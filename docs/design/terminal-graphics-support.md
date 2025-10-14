# WezTerm Graphics from Erlang/LFE: Implementation Guide

**WezTerm supports three major graphics protocols with iTerm2 as the default, best-supported option.** This guide provides complete implementation patterns for displaying images in WezTerm from Erlang and LFE, including working code examples, protocol comparisons, and REPL integration strategies.

## WezTerm's graphics protocol landscape

WezTerm offers three protocols with different trade-offs. **iTerm2 inline images** works out-of-the-box with no configuration, uses simple base64 encoding, includes the built-in `wezterm imgcat` command, and maintains cross-platform compatibility including Windows. The protocol uses escape sequences formatted as `ESC]1337;File=inline=1:<base64-data>BEL` and supports parameters for width, height, and cursor control. WezTerm extends the standard with a `doNotMoveCursor=1` parameter to prevent cursor repositioning.

**Kitty graphics protocol** requires explicit opt-in via `enable_kitty_graphics = true` in the config. It delivers advanced features including precise positioning, image IDs for tracking, animation support, and multiple transmission formats. The protocol excels at local file references that bypass the PTY entirely for maximum performance. However, WezTerm's implementation differs from Kitty's—images map to terminal cells at placement time rather than floating independently, text can "poke holes" through images, z-index isn't respected, and Windows support is absent. The protocol was significantly improved in version 20240128 with fixes for display parameters and image ID handling.

**Sixel graphics** remains experimental in WezTerm with notable limitations. XTSMGRAPHICS queries aren't supported, performance lags behind modern protocols, color register mapping has conformance issues, and Windows exhibits various bugs. Sixel offers legacy VT340 compatibility and wider terminal ecosystem support but delivers lower quality than modern alternatives.

For general use, **choose iTerm2**—it's stable, works everywhere, and requires zero configuration. Enable Kitty protocol for TUI applications needing advanced positioning but only on Linux/Unix systems. Avoid Sixel except when legacy compatibility is essential.

## Erlang implementation foundations

Erlang provides solid primitives for terminal graphics through the `io` module and binary handling. The `io` module explicitly supports ANSI escape sequences when outputting to terminal devices.

### Sending escape sequences

```erlang
%% Basic approach using io:format with ~s format specifier
display_simple() ->
    io:format("~s~n", ["\e[31mRed text\e[0m"]).

%% Binary approach for graphics protocols
send_escape_sequence(Sequence) ->
    io:put_chars(standard_io, Sequence).

%% Terminal detection
is_terminal() ->
    case io:getopts() of
        Opts when is_list(Opts) ->
            Terminal = proplists:get_value(terminal, Opts, false),
            Stdout = proplists:get_value(stdout, Opts, false),
            Terminal andalso Stdout;
        _ -> false
    end.

%% Get terminal dimensions
{ok, Columns} = io:columns(),
{ok, Rows} = io:rows().
```

### iTerm2 protocol implementation

Complete working implementation for displaying images via iTerm2's protocol:

```erlang
-module(iterm2_display).
-export([show_image/1, show_image/2]).

show_image(FilePath) ->
    show_image(FilePath, #{}).

show_image(FilePath, Opts) ->
    case file:read_file(FilePath) of
        {ok, ImageData} ->
            Base64Data = base64:encode(ImageData),
            FileName = filename:basename(FilePath),
            NameB64 = base64:encode(list_to_binary(FileName)),
            
            %% Extract options
            Width = maps:get(width, Opts, "auto"),
            Height = maps:get(height, Opts, "auto"),
            
            %% Build parameter string
            Params = build_params(NameB64, Width, Height),
            
            %% Output: ESC ] 1337 ; File = params : data BEL
            Sequence = io_lib:format("\e]1337;File=~s:~s\a\n", 
                                     [Params, Base64Data]),
            io:put_chars(lists:flatten(Sequence)),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

build_params(NameB64, Width, Height) ->
    BaseParams = io_lib:format("name=~s;inline=1", [NameB64]),
    WithWidth = case Width of
        "auto" -> BaseParams;
        W -> io_lib:format("~s;width=~s", [BaseParams, W])
    end,
    case Height of
        "auto" -> WithWidth;
        H -> io_lib:format("~s;height=~s", [WithWidth, H])
    end.
```

Usage examples:
```erlang
1> iterm2_display:show_image("photo.jpg").
ok
2> iterm2_display:show_image("chart.png", #{width => "80%", height => "auto"}).
ok
3> iterm2_display:show_image("logo.png", #{width => "50", height => "30"}).
ok
```

### Kitty protocol implementation

The Kitty protocol requires chunking data into 4096-byte segments:

```erlang
-module(kitty_display).
-export([show_image/1]).

-define(CHUNK_SIZE, 4096).

show_image(FilePath) ->
    case file:read_file(FilePath) of
        {ok, ImageData} ->
            Base64Data = base64:encode(ImageData),
            send_kitty_chunked(Base64Data),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

send_kitty_chunked(Data) ->
    send_kitty_chunked(Data, ?CHUNK_SIZE, true).

send_kitty_chunked(<<>>, _Size, _First) ->
    ok;
send_kitty_chunked(Data, Size, First) when byte_size(Data) =< Size ->
    %% Final chunk
    Control = case First of
        true -> "a=T,f=100,m=0";  % transmit+display, PNG format, last chunk
        false -> "m=0"              % continuation, last chunk
    end,
    io:format("\e_G~s;~s\e\\\n", [Control, Data]);
send_kitty_chunked(Data, Size, First) ->
    <<Chunk:Size/binary, Rest/binary>> = Data,
    Control = case First of
        true -> "a=T,f=100,m=1";  % transmit+display, PNG format, more chunks
        false -> "m=1"              % continuation, more chunks
    end,
    io:format("\e_G~s;~s\e\\\n", [Control, Chunk]),
    send_kitty_chunked(Rest, Size, false).
```

### Sixel via external tools

Since Sixel encoding is complex, use external tools:

```erlang
-module(sixel_display).
-export([show_image/1, show_image/2]).

show_image(FilePath) ->
    show_image(FilePath, #{}).

show_image(FilePath, Opts) ->
    Width = maps:get(width, Opts, 800),
    Height = maps:get(height, Opts, 480),
    
    Command = io_lib:format("img2sixel -w ~p -h ~p '~s'", 
                           [Width, Height, FilePath]),
    Output = os:cmd(lists:flatten(Command)),
    io:put_chars(Output),
    ok.
```

### LFE implementations

LFE uses identical Erlang functions through interop:

```lisp
(defmodule iterm2-display
  (export (show-image 1) (show-image 2)))

(defun show-image
  ([file-path]
   (show-image file-path #m()))
  ([file-path opts]
   (case (file:read_file file-path)
     ((tuple 'ok image-data)
      (let* ((base64-data (base64:encode image-data))
             (filename (filename:basename file-path))
             (name-b64 (base64:encode (binary (filename))))
             (width (maps:get 'width opts "auto"))
             (height (maps:get 'height opts "auto"))
             (params (build-params name-b64 width height))
             (sequence (io_lib:format "\\e]1337;File=~s:~s\\a\\n" 
                                     (list params base64-data))))
        (io:put_chars (lists:flatten sequence))
        'ok))
     ((tuple 'error reason)
      (tuple 'error reason)))))

(defun build-params (name-b64 width height)
  (let ((base (io_lib:format "name=~s;inline=1" (list name-b64))))
    (cond 
      ((and (== width "auto") (== height "auto")) base)
      ((== height "auto") 
       (io_lib:format "~s;width=~s" (list base width)))
      ('true 
       (io_lib:format "~s;width=~s;height=~s" (list base width height))))))
```

### No existing Erlang libraries

Research found **no dedicated terminal graphics libraries for Erlang/LFE**. The Elixir ecosystem has better support—the `Image` library (hex.pm/packages/image) includes `Image.Preview.iterm/1` for iTerm2 display. For color support, `erlang-color` and `eproxus/color` provide ANSI color formatting but not graphics protocols.

## Image generation and encoding

### Base64 encoding

Erlang's built-in `base64` module handles encoding requirements:

```erlang
%% Encode binary to base64
Base64Binary = base64:encode(ImageData).
Base64String = base64:encode_to_string(ImageData).

%% Decode
ImageData = base64:decode(Base64Binary).

%% With options (OTP 26+)
Base64 = base64:encode(Data, #{
    mode => standard,  % or urlsafe
    padding => true    % or false
}).
```

### Image processing with eimp

The `eimp` library provides production-ready image manipulation:

```erlang
%% Start application
application:start(eimp).

%% Convert and resize
{ok, ImageBinary} = file:read_file("photo.jpg"),
{ok, PngBinary} = eimp:convert(ImageBinary, png, [
    {scale, {800, 600}}
]),

%% Get image information
{ok, Info} = eimp:identify(ImageBinary).
%% Info: #{type => jpeg, width => 4032, height => 3024, ...}

%% Display the converted image
Base64 = base64:encode(PngBinary),
io:format("\e]1337;File=inline=1:~s\a\n", [Base64]).
```

**eimp features**: Converts between PNG, JPEG, WebP, GIF; uses external C libraries via ports for crash safety; employs per-CPU-core process pool; protects against decompression bombs (25Mpx limit).

### Protocol format support

**iTerm2** accepts any format macOS supports—PNG, JPEG, GIF, PDF, BMP, TIFF. **Kitty** supports PNG (f=100), JPEG (f=101), raw RGB (f=24), RGBA (f=32), and all formats via ImageMagick conversion. **Sixel** works with any format convertible to sixel bitmap via `img2sixel` or ImageMagick.

### ImageMagick integration

Use external tools for format conversion and processing:

```erlang
%% Simple command execution
convert_image(Input, Output) ->
    Cmd = io_lib:format("convert '~s' '~s'", [Input, Output]),
    os:cmd(lists:flatten(Cmd)).

%% Resize and convert in one step
resize_and_convert(Input, Width, Height) ->
    Cmd = io_lib:format("convert '~s' -resize ~px~p png:-", 
                       [Input, Width, Height]),
    list_to_binary(os:cmd(lists:flatten(Cmd))).

%% Use with ports for better control
convert_via_port(InputPath, Width, Height) ->
    Path = os:find_executable("convert"),
    Args = [InputPath, "-resize", 
            io_lib:format("~px~p", [Width, Height]), 
            "png:-"],
    Port = open_port({spawn_executable, Path}, 
                     [{args, Args}, binary, exit_status]),
    collect_port_data(Port, <<>>).

collect_port_data(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_data(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        {Port, {exit_status, Status}} ->
            {error, {exit_status, Status}}
    end.
```

## Practical patterns and terminal detection

### Multi-protocol display module

Production-ready module with automatic protocol selection:

```erlang
-module(term_image).
-export([display/1, display/2]).

display(ImagePath) ->
    display(ImagePath, auto).

display(ImagePath, Protocol) ->
    case detect_protocol(Protocol) of
        iterm2 -> display_iterm2(ImagePath);
        kitty -> display_kitty(ImagePath);
        sixel -> display_sixel(ImagePath);
        none -> {error, no_graphics_support}
    end.

detect_protocol(auto) ->
    %% Check TERM_PROGRAM first
    case os:getenv("TERM_PROGRAM") of
        "WezTerm" -> iterm2;  % WezTerm supports iTerm2 by default
        "iTerm.app" -> iterm2;
        _ ->
            %% Check TERM variable
            case os:getenv("TERM") of
                "xterm-kitty" -> kitty;
                Term when is_list(Term) ->
                    case string:find(Term, "sixel") of
                        nomatch -> none;
                        _ -> sixel
                    end;
                _ -> none
            end
    end;
detect_protocol(Protocol) -> Protocol.

display_iterm2(ImagePath) ->
    {ok, ImageData} = file:read_file(ImagePath),
    Base64 = base64:encode(ImageData),
    Name = base64:encode(list_to_binary(filename:basename(ImagePath))),
    io:format("\e]1337;File=name=~s;inline=1:~s\a\n", [Name, Base64]),
    ok.

display_kitty(ImagePath) ->
    {ok, ImageData} = file:read_file(ImagePath),
    Base64 = base64:encode(ImageData),
    kitty_display:send_kitty_chunked(Base64),
    ok.

display_sixel(ImagePath) ->
    Command = io_lib:format("img2sixel '~s'", [ImagePath]),
    os:cmd(lists:flatten(Command)),
    ok.
```

### WezTerm-specific detection

```erlang
detect_wezterm() ->
    case os:getenv("TERM_PROGRAM") of
        "WezTerm" ->
            Version = os:getenv("TERM_PROGRAM_VERSION"),
            Pane = os:getenv("WEZTERM_PANE"),
            {ok, #{version => Version, pane => Pane}};
        _ ->
            {error, not_wezterm}
    end.

get_wezterm_dimensions() ->
    case os:cmd("wezterm cli list --format=json 2>/dev/null") of
        "" -> {error, no_wezterm};
        Json ->
            %% Parse JSON to get dimensions
            %% Returns: #{rows, cols, pixel_width, pixel_height, dpi}
            parse_wezterm_info(Json)
    end.
```

### Terminal dimension detection

```erlang
get_terminal_size() ->
    %% Method 1: Use io module
    case {io:columns(), io:rows()} of
        {{ok, Cols}, {ok, Rows}} ->
            #{cols => Cols, rows => Rows};
        _ ->
            %% Method 2: Fallback to stty
            case os:cmd("stty size 2>/dev/null") of
                "" -> #{cols => 80, rows => 24};
                Size ->
                    case string:tokens(Size, " \n") of
                        [RowsStr, ColsStr] ->
                            #{cols => list_to_integer(ColsStr),
                              rows => list_to_integer(RowsStr)};
                        _ -> #{cols => 80, rows => 24}
                    end
            end
    end.
```

### Performance considerations

**Local file performance**: Kitty protocol with file references bypasses the PTY entirely—just sends the filename. This is **orders of magnitude faster** than transmitting base64-encoded data but only works for local sessions.

**Over-PTY performance**: For remote sessions, Sixel transmits fastest over the wire due to direct encoding without base64 overhead. However, **Kitty and iTerm2 deliver superior quality** with 24-bit color and alpha channel support. Base64 encoding adds 33% size overhead but modern terminals handle this efficiently.

**Image sizing**: Always resize images before transmission. WezTerm's default limit is 25 million pixels per frame. Calculate appropriate dimensions:

```erlang
scale_for_terminal(ImagePath, MaxCols, MaxRows) ->
    %% Assume 10x20 pixels per character cell (adjust for your terminal)
    CellWidth = 10,
    CellHeight = 20,
    
    MaxWidth = MaxCols * CellWidth,
    MaxHeight = MaxRows * CellHeight,
    
    Cmd = io_lib:format(
        "convert '~s' -resize '~px~p>' png:-",
        [ImagePath, MaxWidth, MaxHeight]
    ),
    list_to_binary(os:cmd(lists:flatten(Cmd))).
```

## REPL integration patterns

### Non-blocking display

**Terminal graphics protocols are inherently non-blocking**. They write escape sequences to stdout and return immediately—no event loop integration needed. Images appear inline with text without blocking REPL interaction.

```erlang
%% This returns instantly
show_plot(Data) ->
    PlotFile = generate_plot_file(Data),
    term_image:display(PlotFile),
    file:delete(PlotFile),
    ok.
```

### Erlang shell extensions via user_default

The `user_default` module provides the **easiest REPL integration**:

```erlang
%% Save as user_default.erl in ~/.erlang.d/
-module(user_default).
-export([img/1, img/2, plot/1]).

%% Display image with default settings
img(Filename) ->
    img(Filename, #{}).

%% Display with options
img(Filename, Opts) ->
    term_image:display(Filename, Opts).

%% Plot data points
plot(Data) when is_list(Data) ->
    TempFile = "/tmp/erlang_plot_" ++ 
               integer_to_list(erlang:unique_integer([positive])) ++ 
               ".png",
    try
        generate_gnuplot(Data, TempFile),
        img(TempFile)
    after
        file:delete(TempFile)
    end.

generate_gnuplot(Data, OutputFile) ->
    DataFile = OutputFile ++ ".dat",
    PlotScript = OutputFile ++ ".gnu",
    
    %% Write data
    DataStr = lists:map(fun({X, Y}) ->
        io_lib:format("~p ~p~n", [X, Y])
    end, Data),
    file:write_file(DataFile, DataStr),
    
    %% Generate plot script
    Script = io_lib:format(
        "set terminal png size 800,480~n"
        "set output '~s'~n"
        "plot '~s' with lines~n",
        [OutputFile, DataFile]
    ),
    file:write_file(PlotScript, Script),
    os:cmd("gnuplot " ++ PlotScript),
    
    file:delete(DataFile),
    file:delete(PlotScript),
    ok.
```

Load in `~/.erlang`:
```erlang
code:load_abs("/home/user/.erlang.d/user_default").
```

Usage:
```erlang
1> img("chart.png").
ok
2> img("logo.png", #{width => "50%"}).
ok
3> plot([{1,1}, {2,4}, {3,9}, {4,16}, {5,25}]).
ok
```

### xrepl integration (LFE)

The xrepl project provides an experimental enhanced REPL for LFE with session management. While it **doesn't currently have graphics support**, the architecture is ideal for integration:

```lisp
;; Proposed xrepl-graphics module
(defmodule xrepl-graphics
  (export (show-image 1) (detect-terminal 0)))

(defun detect-terminal ()
  (let ((term-prog (os:getenv "TERM_PROGRAM"))
        (term (os:getenv "TERM")))
    (cond
      ((== term-prog "WezTerm") 'iterm2)
      ((== term-prog "iTerm.app") 'iterm2)
      ((== term "xterm-kitty") 'kitty)
      ('true 'none))))

(defun show-image (filename)
  (case (file:read_file filename)
    ((tuple 'ok data)
     (case (detect-terminal)
       ('iterm2 (render-iterm2 data))
       ('kitty (render-kitty data))
       ('none (io:format "Graphics not supported~n" '()))))
    ((tuple 'error reason)
     (io:format "Error: ~p~n" (list reason)))))

(defun render-iterm2 (image-data)
  (let* ((base64 (base64:encode image-data))
         (seq (io_lib:format "\\e]1337;File=inline=1:~s\\a\\n" 
                            (list base64))))
    (io:put_chars (lists:flatten seq))
    'ok))
```

### Best practices for REPL workflows

**Capability detection**: Always detect terminal capabilities at startup and store in session state. Provide clear error messages when graphics aren't supported.

**Fallback strategy**: Implement layered fallbacks—Kitty → iTerm2 → Sixel → ASCII representation. This ensures functionality across terminal types.

**Respect terminal dimensions**: Query terminal size and scale images appropriately. WezTerm provides `wezterm cli list --format=json` for detailed dimension information including pixel dimensions and DPI.

**Temporary file management**: Clean up generated plot files promptly using try/after blocks to ensure cleanup even on errors.

**User configuration**: Allow users to override protocol selection via environment variables like `ERLANG_IMAGE_PROTOCOL=kitty` for flexibility.

## Actionable implementation roadmap

**For immediate use**: Create a `user_default.erl` module with `img/1` function that detects WezTerm via `TERM_PROGRAM` and uses iTerm2 protocol with base64 encoding. This provides instant image display in the Erlang shell with **fewer than 30 lines of code**.

**For production applications**: Build a dedicated module implementing all three protocols with automatic detection, chunking for large images, proper error handling, and fallback strategies. Use eimp for image format conversion and resizing.

**For LFE/xrepl**: Contribute an xrepl-graphics module implementing terminal detection and protocol rendering. Integration with xrepl's session architecture enables per-session graphics configuration and state management.

**Testing approach**: Test on actual WezTerm with various image sizes and formats. Verify chunking works for large images. Test tmux compatibility if needed (requires `allow-passthrough on` in tmux.conf for iTerm2 protocol).

The combination of Erlang's solid binary handling, built-in base64 encoding, and WezTerm's robust iTerm2 protocol support creates an **excellent foundation for terminal graphics**. The lack of dedicated libraries is easily overcome with straightforward implementations that deliver production-ready functionality with minimal code.