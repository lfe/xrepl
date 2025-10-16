# xrepl Graphics Vision: Vega-Lite Integration Strategy

## Executive Summary

This document establishes the architectural vision for bringing rich, modern visualization capabilities to xrepl, the experimental LFE REPL. The approach leverages Vega-Lite's declarative grammar, VlConvert for rendering, and Erlang's supervision architecture to create a production-ready visualization system that works seamlessly across local development, remote SSH sessions, and saved REPL contexts.

**Core Design Principles:**
- **Declarative specifications** over imperative rendering
- **Data as values** - serialize, save, replay visualizations
- **Environment-aware rendering** - GUI, terminal graphics, or file output
- **Graceful degradation** - from rich interactive to ASCII fallback
- **Zero JavaScript runtime** - leverage compiled tooling instead
- **Supervised processes** - crashes don't kill the REPL

This strategy positions xrepl as the first LFE REPL with native, production-ready visualization support, bridging the gap between text-based evaluation and visual data exploration.

---

## Background: Why Visualization Matters for REPLs

REPL-driven development fundamentally benefits from visual feedback. When exploring data, debugging algorithms, or presenting results, seeing patterns visually accelerates understanding in ways text output cannot match. Modern REPLs in other ecosystems have recognized this:

- **Clojure's Reveal** provides rich JavaFX-based visualization with deep object inspection
- **IPython/Jupyter** pioneered inline graphics in notebooks with pluggable display systems
- **Julia's REPL** supports multiple backends for seamless plotting across environments
- **Elixir's Livebook** integrates Vega-Lite visualizations in web-based notebooks

The Erlang/LFE ecosystem currently lacks mature visualization tools integrated into REPL workflows. While powerful libraries exist for distributed systems, fault tolerance, and concurrency, data visualization has remained an afterthought. This gap creates friction for data analysis, algorithm development, and exploratory programming in LFE.

xrepl has the opportunity to be the first LFE REPL with native visualization support, providing capabilities comparable to modern REPLs in other languages while respecting LFE's functional programming paradigms and Erlang's process supervision model.

---

## Why Vega-Lite? Understanding the Technology Choice

### The Visualization Grammar Landscape

Three primary approaches dominate modern data visualization:

1. **Low-level imperative libraries** (D3.js, Processing, native graphics APIs)
   - Fine-grained control over every pixel
   - Steep learning curves
   - Difficult to serialize or regenerate
   - Code is tightly coupled to rendering

2. **High-level chart libraries** (Chart.js, Plotly, native GUI charting)
   - Easy to use for common chart types
   - Limited flexibility beyond built-in templates
   - Varying levels of customization support
   - Often tightly coupled to specific platforms

3. **Declarative visualization grammars** (Vega, Vega-Lite, ggplot2)
   - Specify *what* to visualize, not *how* to render
   - Specifications are data (JSON, not code)
   - Highly composable and reusable
   - Separates concerns: data → spec → rendering

### Why Reveal Chose Vega-Lite Over D3

Clojure's Reveal visualization system selected Vega-Lite despite D3 being more established and powerful. This decision reveals important architectural insights:

**Declarative specifications are data**: Vega-Lite specs are pure JSON objects that programs can generate, manipulate, and serialize. This aligns perfectly with Clojure's (and LFE's) philosophy of "code as data." You can construct a visualization spec as a map, transform it, save it, and regenerate it later.

**Programmatic generation**: Vega was explicitly designed as a "target language" where computer programs dynamically generate visualizations. D3, while powerful, expects humans to write imperative code. When a REPL needs to generate hundreds of different visualizations automatically, declarative specs win.

**Separation from rendering**: Vega specs describe visualizations independent of the rendering engine. The same spec can render in a browser, server-side to PNG, or even as ASCII art. D3 is tightly bound to DOM manipulation and browser environments.

**Serialization and reusability**: Vega specs can be saved, transmitted over networks, embedded in documents, and reconstructed perfectly. D3 code is imperative JavaScript that requires execution context.

**Maintainability**: Vega handles the complex rendering logic internally. Updates to the Vega engine automatically improve all visualizations without code changes. With D3, every visualization is custom code requiring manual updates.

For xrepl, these same advantages apply. LFE developers can construct Vega-Lite specs as Erlang maps, the REPL can save them with session history, and rendering can happen in whatever environment makes sense—all without writing JavaScript.

### The JavaScript Ecosystem's Innovation Advantage

JavaScript and TypeScript dominate visualization innovation for pragmatic reasons:

**The browser as universal renderer**: The web platform provides sophisticated graphics primitives (SVG, Canvas, WebGL) accessible from JavaScript. This creates a standardized, ubiquitous rendering target.

**Network effects**: Once D3 established JavaScript as the lingua franca of visualization, subsequent innovation naturally occurred in the same ecosystem. Libraries build on each other's foundations.

**Interactive visualizations**: The web's event model and JavaScript's asynchronous nature make it natural to create interactive, responsive visualizations.

**Academic research**: Many visualization research labs (UW Interactive Data Lab, Stanford Vis Group, MIT CSAIL) publish their work as JavaScript libraries, both for accessibility and because browser-based visualizations demonstrate well.

However, this JavaScript dominance doesn't mean LFE/Erlang must adopt JavaScript directly. The ecosystem provides compiled tools like VlConvert that bring Vega capabilities to any language through process boundaries.

---

## Explored Alternatives and Technology Assessment

### Option 1: Direct Terminal Graphics Protocols

**Approach**: Implement Sixel, Kitty Graphics Protocol, or iTerm2 Inline Images directly in Erlang, generating images from scratch or using external tools like gnuplot.

**Evaluation**: 
- ✅ No external dependencies for rendering
- ✅ Works seamlessly over SSH
- ✅ Displays inline with REPL output
- ❌ Limited to static images (no true interactivity)
- ❌ Requires implementing or porting graphics encoders
- ❌ Still need chart generation logic
- ❌ Sixel: limited color depth (typically 256 colors)
- ❌ Kitty: limited terminal adoption, implementation complexity

**Verdict**: Necessary as a display backend, but insufficient alone. Terminal protocols are excellent for *showing* images but don't solve the chart generation problem.

### Option 2: wxErlang Native GUI

**Approach**: Use wxErlang (included in OTP) to create native GUI windows for visualization, potentially embedding wxWebView for Vega rendering.

**Evaluation**:
- ✅ Included in OTP (no external dependencies)
- ✅ True interactive visualizations
- ✅ Native platform look and feel
- ✅ Production-proven (Observer, Debugger use it)
- ❌ Steep learning curve (C++ OO patterns in functional Erlang)
- ❌ Doesn't work over SSH without X11 forwarding
- ❌ Complex memory management (explicit object lifecycle)
- ❌ Still requires chart rendering implementation

**Verdict**: Valuable for local interactive development, but requires fallback for remote use. The complexity is justified only if we need rich interactivity beyond what terminal graphics provide. Best used in combination with other approaches.

### Option 3: Custom Rust Visualization Binary (Plotters)

**Approach**: Build a custom CLI tool using the Plotters Rust library that accepts JSON specs and outputs PNG/SVG files.

**Evaluation**:
- ✅ Extremely fast (trillions of data points)
- ✅ Single compiled binary per platform
- ✅ Full control over features and optimizations
- ✅ Pure Rust with minimal dependencies
- ❌ Must design and maintain custom spec format
- ❌ Limited to chart types we implement
- ❌ Reinventing the wheel (Vega already exists)
- ❌ Smaller ecosystem than Vega

**Verdict**: Excellent for specific high-performance needs or custom visualizations, but shouldn't be the primary system. Could serve as a faster alternative backend for common chart types after the Vega-Lite system is established.

### Option 4: Rerun (Multimodal Visualization SDK)

**Approach**: Use Rerun's Rust-based SDK for visualizing temporal, spatial, and multimodal data streams.

**Evaluation**:
- ✅ Built in Rust with excellent performance
- ✅ Perfect for robotics, computer vision, 3D data
- ✅ Standalone viewer with powerful features
- ✅ Python, C++, and Rust bindings available
- ❌ Focused on spatial/temporal data, not traditional charts
- ❌ Overkill for standard plotting needs
- ❌ Learning curve for domain-specific architecture

**Verdict**: Not suitable as primary visualization system, but worth considering for specialized use cases involving 3D data, point clouds, or sensor streams. Could be a future enhancement for domain-specific applications.

### Option 5: Existing Command-Line Tools (gnuplot, etc.)

**Approach**: Integrate mature command-line plotting tools via Erlang ports.

**Evaluation**:
- ✅ Battle-tested and stable
- ✅ Rich feature sets
- ✅ Well-documented
- ❌ Gnuplot: dated aesthetics, clunky syntax
- ❌ Limited modern alternatives exist
- ❌ Not designed for programmatic generation
- ❌ Difficult to create declarative workflows

**Verdict**: Insufficient for modern needs. The existing plottah library already integrates gnuplot for users who need it, but xrepl should aim higher.

---

## The Chosen Path: Vega-Lite + VlConvert Architecture

### Why This Combination Wins

**Vega-Lite provides the specification layer**: A mature, well-documented visualization grammar with extensive chart types, transformations, and composition capabilities. The JSON specifications map naturally to Erlang/LFE data structures.

**VlConvert eliminates JavaScript runtime**: A standalone Rust binary with embedded Deno that converts Vega-Lite JSON to PNG, SVG, or PDF. No Node.js, no browser, no JavaScript toolchain required in the xrepl build process or runtime.

**Erlang ports provide clean integration**: Spawn VlConvert as a supervised process, pipe JSON specs through stdin, receive rendered images through stdout. Crashes are isolated and recoverable.

**Declarative specs enable saving and replay**: Visualization specifications can be stored alongside REPL session history. Users can replay entire sessions including all visualizations exactly as they appeared originally.

**Multiple rendering backends**: The same Vega-Lite spec can render to wxErlang windows (local GUI), terminal graphics (SSH), or files (reports, documentation). This flexibility serves diverse workflows.

### Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                    xrepl User API                       │
│  (viz:scatter Data Opts) → Erlang Map (Vega-Lite spec) │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────┐
│              Visualization Manager                      │
│  - Detect environment capabilities                      │
│  - Route to appropriate backend                         │
│  - Manage VlConvert process lifecycle                   │
└────────┬──────────────────────────┬─────────────────────┘
         │                          │
    ┌────▼─────┐              ┌────▼─────┐
    │ VlConvert│              │ Direct   │
    │  Port    │              │ Display  │
    │ Process  │              │ Backends │
    └────┬─────┘              └────┬─────┘
         │                          │
         │ PNG/SVG                  │
         ▼                          ▼
┌────────────────┐      ┌──────────────────────┐
│  File Storage  │      │ Terminal Graphics    │
│  (sessions)    │      │ - iTerm2 protocol    │
└────────────────┘      │ - WezTerm support    │
                        │ - Kitty protocol     │
                        │ - Sixel fallback     │
                        │ - ASCII fallback     │
                        └──────────────────────┘
```

### Component Responsibilities

**User API Layer** (`viz.lfe`):
- High-level functions: `(viz:scatter ...)`, `(viz:line ...)`, `(viz:bar ...)`
- Convert LFE data structures to Vega-Lite specs (Erlang maps)
- Provide sensible defaults while allowing full spec customization
- Validate input data and spec structure

**Visualization Manager** (`viz_manager.erl`):
- Singleton supervised process for lifecycle management
- Environment capability detection at startup
- Backend selection based on available features
- Message-based API: `{display, Type, Spec, Options}`
- Maintain VlConvert port process or restart on failure

**VlConvert Integration** (`vlconvert_port.erl`):
- Spawn and supervise VlConvert binary process
- Handle JSON encoding (via `jsx` or `jiffy`)
- Stream specs to VlConvert stdin
- Collect PNG/SVG output from stdout
- Error handling and process monitoring

**Terminal Graphics Backend** (`viz_terminal.erl`):
- Protocol detection (iTerm2, WezTerm, Kitty, Sixel support)
- Base64 encoding for inline protocols
- Escape sequence generation
- Fallback to ASCII art for unsupported terminals
- Terminal dimension detection for sizing

**File Backend** (`viz_file.erl`):
- Save images to temporary or specified locations
- Embed image references in session history
- Support for PNG, SVG, PDF output formats
- Cleanup of temporary files

**wxErlang Backend** (`viz_wx.erl`) [Future Enhancement]:
- Spawn wxFrame windows for interactive display
- Handle image display and zooming
- Multiple window management
- Integration with wxWebView for full Vega interactivity

---

## VlConvert: The Key Technology Enabler

### What is VlConvert?

VlConvert is a Rust-based command-line tool that converts Vega and Vega-Lite specifications to static images or interactive HTML. It embeds a Deno JavaScript runtime to execute the official Vega libraries but packages everything as a single standalone binary.

**Key advantages**:
- **No Node.js required**: Everything is self-contained
- **Cross-platform binaries**: Pre-compiled for macOS (Intel/ARM), Linux (x86_64/aarch64), Windows
- **Multiple output formats**: PNG, SVG, PDF, HTML
- **Production-ready**: Maintained by Vega team, used by various data tools
- **Fast**: Rust performance with JavaScript only for Vega rendering

### Integration Strategy

#### Automatic Binary Management

**Platform detection**: At xrepl startup, detect OS and architecture using Erlang's system information.

```erlang
detect_platform() ->
    OS = os:type(),
    Arch = erlang:system_info(system_architecture),
    case {OS, Arch} of
        {{unix, darwin}, Arch} when string:str(Arch, "aarch64") > 0 ->
            {macos, arm64};
        {{unix, darwin}, _} ->
            {macos, x64};
        {{unix, linux}, Arch} when string:str(Arch, "aarch64") > 0 ->
            {linux, arm64};
        {{unix, linux}, _} ->
            {linux, x64};
        {{win32, _}, _} ->
            {windows, x64}
    end.
```

**Download on first use**: If VlConvert binary is not found in xrepl's priv directory, download it from GitHub releases. Cache the binary for subsequent uses.

```erlang
ensure_vlconvert() ->
    PrivDir = code:priv_dir(xrepl),
    BinaryPath = filename:join([PrivDir, "bin", "vl2png"]),
    case filelib:is_file(BinaryPath) of
        true -> {ok, BinaryPath};
        false -> download_vlconvert(PrivDir)
    end.

download_vlconvert(PrivDir) ->
    {Platform, Arch} = detect_platform(),
    Version = "v1.7.0",  % Or latest
    URL = build_download_url(Platform, Arch, Version),
    
    {ok, {{_, 200, _}, _, Binary}} = httpc:request(get, {URL, []}, [], 
                                                    [{body_format, binary}]),
    
    BinDir = filename:join([PrivDir, "bin"]),
    ok = filelib:ensure_dir(BinDir ++ "/"),
    BinaryPath = filename:join([BinDir, "vl2png"]),
    ok = file:write_file(BinaryPath, Binary),
    ok = file:change_mode(BinaryPath, 8#755),
    
    {ok, BinaryPath}.
```

**Checksum verification**: Verify downloaded binaries against published SHA256 checksums to ensure integrity.

**Bundled option**: For production deployments, include platform-specific binaries in the xrepl release package to avoid runtime downloads.

#### Process Supervision

VlConvert runs as a supervised port process under the visualization manager:

```erlang
-behaviour(gen_server).

init([]) ->
    {ok, BinaryPath} = ensure_vlconvert(),
    Port = open_port({spawn_executable, BinaryPath},
                     [binary, stream, {args, ["--port"]}, 
                      {packet, 4}, exit_status]),
    {ok, #state{port = Port, pending = #{}}}.

handle_call({render, VegaLiteSpec, Format}, From, State) ->
    RequestId = make_ref(),
    Json = jsx:encode(VegaLiteSpec),
    port_command(State#state.port, Json),
    Pending = maps:put(RequestId, {From, Format}, State#state.pending),
    {noreply, State#state{pending = Pending}}.

handle_info({Port, {data, ImageData}}, #state{port = Port} = State) ->
    %% Match request to response, reply to waiting caller
    {{From, Format}, NewPending} = extract_pending_request(State#state.pending),
    gen_server:reply(From, {ok, ImageData}),
    {noreply, State#state{pending = NewPending}};

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    %% Process crashed, restart
    error_logger:warning_msg("VlConvert process exited with status ~p~n", 
                            [Status]),
    {stop, {vlconvert_exit, Status}, State}.

terminate(_Reason, #state{port = Port}) ->
    catch port_close(Port),
    ok.
```

**Supervision strategy**: One-for-one supervision under `viz_sup`. If VlConvert crashes, the supervisor restarts it without affecting the REPL or other visualization processes.

**Graceful degradation**: If VlConvert fails to start or download, visualization manager falls back to ASCII-only rendering with clear error messages explaining what's missing.

---

## User-Facing API Design

### High-Level Convenience Functions

Users should be able to create common visualizations with minimal code:

```lisp
;; Simple scatter plot
(viz:scatter '((x . #(1 2 3 4 5))
               (y . #(2 4 6 8 10))))

;; Line chart with options
(viz:line data 
  '((title . "Temperature Over Time")
    (x-label . "Date")
    (y-label . "Temperature (°C)")
    (color . "steelblue")))

;; Bar chart from key-value data
(viz:bar '((categories . #("A" "B" "C" "D"))
           (values . #(23 45 56 78)))
  '((title . "Sales by Region")))

;; Histogram with automatic binning
(viz:histogram measurements 
  '((bins . 20)
    (color . "orange")))
```

### Direct Vega-Lite Specs for Power Users

For full control, users can provide complete Vega-Lite specifications:

```lisp
(viz:vega-lite
  '#m(
    $schema "https://vega.github.io/schema/vega-lite/v5.json"
    description "A simple bar chart with embedded data"
    data #m(values (#m(category "A" value 28)
                    #m(category "B" value 55)
                    #m(category "C" value 43)))
    mark "bar"
    encoding #m(
      x #m(field "category" type "nominal" axis #m(labelAngle 0))
      y #m(field "value" type "quantitative"))))
```

### Display Control

Users can specify how visualizations are displayed:

```lisp
;; Display inline in terminal (default)
(viz:show plot)

;; Save to file
(viz:save plot "output.png")

;; Open in external viewer (if available)
(viz:view plot)

;; Embed in current REPL session history
(viz:embed plot)
```

### Composing Complex Visualizations

Vega-Lite's composition operators enable sophisticated multi-view displays:

```lisp
;; Layered charts
(viz:layer
  (viz:line data1 '((color . "blue")))
  (viz:scatter data2 '((color . "red"))))

;; Concatenated charts
(viz:hconcat
  (viz:bar sales-by-region)
  (viz:pie market-share))

;; Faceted charts
(viz:facet data '((row . "category")
                  (column . "region")))
```

---

## Terminal Graphics Integration

### Protocol Support Strategy

**Primary: iTerm2 Inline Images Protocol**
- Supported by: iTerm2, WezTerm, mintty
- Simple base64 encoding, single escape sequence
- No configuration required
- Works out-of-the-box on macOS and modern terminals

**Secondary: Kitty Graphics Protocol**
- Supported by: Kitty, WezTerm (with config), Ghostty (partial)
- Advanced features: animations, transparency, layering
- Requires explicit opt-in in WezTerm
- Best performance for local sessions

**Tertiary: Sixel Graphics**
- Supported by: 30+ terminals (xterm, mlterm, WezTerm, Konsole)
- Widest compatibility for older terminals
- Limited color depth (typically 256 colors)
- Use libsixel via port for encoding

**Fallback: ASCII Art**
- Universal compatibility
- Use Unicode box-drawing characters
- Implementations: Simple bar charts, line plots, sparklines
- Useful for quick data inspection even without graphics

### Implementation Details

#### iTerm2 Protocol

```erlang
display_iterm2(ImageData, Opts) ->
    Base64 = base64:encode(ImageData),
    Width = maps:get(width, Opts, "auto"),
    Height = maps:get(height, Opts, "auto"),
    Name = maps:get(name, Opts, "chart"),
    
    NameB64 = base64:encode(list_to_binary(Name)),
    Params = io_lib:format("name=~s;inline=1;width=~s;height=~s", 
                          [NameB64, Width, Height]),
    
    Sequence = io_lib:format("\e]1337;File=~s:~s\a\n", [Params, Base64]),
    io:put_chars(lists:flatten(Sequence)).
```

#### WezTerm Detection

```erlang
detect_wezterm() ->
    case os:getenv("TERM_PROGRAM") of
        "WezTerm" ->
            Version = os:getenv("TERM_PROGRAM_VERSION"),
            {wezterm, Version};
        _ ->
            false
    end.

supports_iterm2_protocol(wezterm, _Version) -> true;
supports_iterm2_protocol(iterm2, _Version) -> true;
supports_iterm2_protocol(_, _) -> false.
```

#### Terminal Dimension Detection

```erlang
get_terminal_size() ->
    case {io:columns(), io:rows()} of
        {{ok, Cols}, {ok, Rows}} ->
            #{cols => Cols, rows => Rows, 
              pixel_width => Cols * 10,   % Approximate
              pixel_height => Rows * 20}; % Approximate
        _ ->
            #{cols => 80, rows => 24,
              pixel_width => 800, pixel_height => 480}
    end.
```

#### Automatic Sizing

Calculate appropriate image dimensions based on terminal size and user preferences:

```erlang
calculate_image_size(Opts) ->
    Terminal = get_terminal_size(),
    
    %% Default: 80% of terminal width, maintain aspect ratio
    DefaultWidth = trunc(maps:get(pixel_width, Terminal) * 0.8),
    DefaultHeight = trunc(DefaultWidth * 0.6),  % 5:3 aspect ratio
    
    Width = maps:get(width, Opts, DefaultWidth),
    Height = maps:get(height, Opts, DefaultHeight),
    
    #{width => Width, height => Height}.
```

### WezTerm-Specific Optimizations

**Use native image protocol**: WezTerm defaults to iTerm2 protocol, which requires no configuration.

**Support wezterm imgcat**: WezTerm provides a built-in `wezterm imgcat` command that xrepl can use as a fallback.

**Detect multiplexer context**: Check for tmux/screen and adjust approach accordingly.

```erlang
in_multiplexer() ->
    case {os:getenv("TMUX"), os:getenv("STY")} of
        {false, false} -> false;
        {Tmux, _} when Tmux =/= false -> {tmux, Tmux};
        {_, Screen} when Screen =/= false -> {screen, Screen}
    end.
```

**Multiplexer compatibility**: iTerm2 protocol works in WezTerm's native multiplexing. For tmux, require `allow-passthrough on` in tmux.conf and provide clear error messages if not configured.

---

## Session Management and Persistence

### Embedding Visualizations in REPL History

A key advantage of declarative Vega-Lite specs is they can be saved and replayed:

```erlang
%% Session history entry structure
-record(history_entry, {
    timestamp,
    input,         % User's LFE expression
    output,        % Evaluation result
    visualizations % List of {Spec, ImageData}
}).

%% Save session
save_session(Filename, History) ->
    SessionData = #{
        version => "1.0",
        timestamp => erlang:system_time(second),
        entries => [serialize_entry(E) || E <- History]
    },
    Encoded = term_to_binary(SessionData, [compressed]),
    file:write_file(Filename, Encoded).

serialize_entry(#history_entry{visualizations = Vizs} = Entry) ->
    %% Store Vega-Lite specs, optionally cache rendered images
    SerializedVizs = [{Spec, cache_or_regenerate(Spec, Image)} 
                      || {Spec, Image} <- Vizs],
    Entry#history_entry{visualizations = SerializedVizs}.
```

### Replay Capabilities

When loading a saved session, regenerate visualizations from specs:

```erlang
replay_session(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    #{entries := Entries} = binary_to_term(Binary),
    
    lists:foreach(fun(Entry) ->
        display_entry(Entry),
        lists:foreach(fun({Spec, _CachedImage}) ->
            %% Regenerate from spec to current terminal
            viz_manager:display(Spec, #{backend => auto})
        end, Entry#history_entry.visualizations)
    end, Entries).
```

**Benefits**:
- Sessions are self-contained and portable
- Visualizations adapt to current terminal capabilities
- Specs can be edited or extended before replay
- Historical data remains accessible

### Export and Sharing

```erlang
%% Export session as HTML report
export_html(Session, OutputFile) ->
    Html = generate_html_report(Session),
    file:write_file(OutputFile, Html).

generate_html_report(Session) ->
    EntryHtml = [entry_to_html(E) || E <- Session#session.entries],
    iolist_to_binary([
        "<html><head><title>xrepl Session</title>",
        "<script src='https://cdn.jsdelivr.net/npm/vega@5'></script>",
        "<script src='https://cdn.jsdelivr.net/npm/vega-lite@5'></script>",
        "<script src='https://cdn.jsdelivr.net/npm/vega-embed@6'></script>",
        "</head><body>", EntryHtml, "</body></html>"
    ]).
```

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-3)

**Week 1: Core Infrastructure**
- Create `viz` application structure
- Implement `viz_manager` supervisor
- Add VlConvert binary detection and download logic
- Write platform-specific download URLs and checksums
- Create basic test suite for binary management

**Week 2: VlConvert Port Integration**
- Implement `vlconvert_port` gen_server
- Handle JSON encoding/decoding with `jsx`
- Set up port communication protocol
- Add error handling and process monitoring
- Test VlConvert rendering with sample specs

**Week 3: Basic Terminal Display**
- Implement iTerm2 protocol rendering
- Add terminal capability detection
- Create ASCII art fallback for unsupported terminals
- Test on iTerm2, WezTerm, standard xterm
- Document terminal requirements

**Deliverable**: Basic `(viz:show-image "file.png")` working in supported terminals.

### Phase 2: Vega-Lite API (Weeks 4-6)

**Week 4: Spec Generation Helpers**
- Design LFE API for common chart types
- Implement `viz:scatter`, `viz:line`, `viz:bar`
- Create data format conversion utilities
- Add Vega-Lite spec validation
- Write examples and documentation

**Week 5: Advanced Charts**
- Implement `viz:histogram`, `viz:heatmap`, `viz:area`
- Add composition functions (`viz:layer`, `viz:hconcat`)
- Support custom Vega-Lite specs via `viz:vega-lite`
- Add common transformations (aggregation, filtering)

**Week 6: Styling and Customization**
- Implement theming system (color schemes, fonts)
- Add size and dimension controls
- Support Vega-Lite encoding channels (color, size, shape)
- Create preset configurations for common use cases

**Deliverable**: Full high-level plotting API with comprehensive examples.

### Phase 3: Session Integration (Weeks 7-8)

**Week 7: REPL Integration**
- Hook visualization into xrepl evaluation loop
- Implement automatic display of plot results
- Add session history tracking for visualizations
- Create `user_default` helper functions

**Week 8: Persistence**
- Implement session save/load with embedded specs
- Add session replay functionality
- Create HTML export feature
- Build session management UI commands

**Deliverable**: Complete REPL integration with save/replay capability.

### Phase 4: Polish and Documentation (Weeks 9-10)

**Week 9: Testing and Error Handling**
- Comprehensive test coverage for all components
- Property-based testing for spec generation
- Error message improvements
- Edge case handling (large data, invalid specs)

**Week 10: Documentation and Examples**
- Complete API reference documentation
- Tutorial: "Getting Started with xrepl Visualization"
- Cookbook: Common visualization patterns
- Integration guide for library authors
- Video demonstrations

**Deliverable**: Production-ready release with complete documentation.

### Phase 5: Advanced Features (Future)

**Weeks 11-14: wxErlang GUI Backend**
- Implement native GUI window support
- Add interactive pan/zoom capabilities
- Multiple window management
- Integration with wxWebView for full Vega interactivity

**Weeks 15+: Additional Enhancements**
- Kitty Graphics Protocol implementation
- Sixel protocol support
- Animation and streaming data support
- Custom Plotters-based backend for performance
- Integration with Livebook (if using Erlang kernel)

---

## Technical Specifications

### Dependencies

**Required**:
- `jsx` or `jiffy` - JSON encoding/decoding
- `httpc` (OTP) - Binary downloads
- VlConvert binary (auto-downloaded)

**Optional**:
- `wxErlang` (OTP) - GUI backend
- `libsixel` - Sixel terminal graphics

### Platform Support

**Tier 1** (Full support, automated testing):
- macOS 11+ (Intel and Apple Silicon)
- Ubuntu 20.04+ / Debian 11+
- Fedora 35+

**Tier 2** (Best-effort support):
- Windows 10+ (via WSL2)
- Other Linux distributions
- FreeBSD

**Terminal Compatibility**:
- ✅ Full support: iTerm2, WezTerm, Kitty
- ⚠️  Partial support: xterm (Sixel), mlterm, Konsole
- ⚠️  Fallback only: Terminal.app, gnome-terminal, standard SSH

### Performance Characteristics

**Rendering times** (estimated on modern hardware):
- Simple chart (100 points): 50-100ms
- Complex visualization (10K points): 200-500ms
- Large dataset (100K+ points): 1-3s

**Memory usage**:
- VlConvert process: ~50-100MB baseline
- Per-visualization overhead: ~1-5MB depending on complexity

**Startup time**:
- First run (download): 5-30s depending on network
- Subsequent runs: <100ms (cached binary)

### Configuration

```erlang
%% xrepl.config
[
  {viz, [
    {default_backend, auto},      % auto | terminal | wx | file
    {vlconvert_binary, auto},     % auto | {path, "/usr/local/bin/vl2png"}
    {terminal_protocol, auto},    % auto | iterm2 | kitty | sixel | ascii
    {image_cache_dir, "/tmp/xrepl_viz"},
    {max_cache_size_mb, 100},
    {theme, default},             % default | dark | colorblind | custom
    {default_width, 800},
    {default_height, 600}
  ]}
].
```

---

## Success Metrics

### MVP Criteria (Phase 1-3)

- ✅ VlConvert automatically downloads and runs on supported platforms
- ✅ Basic charts (scatter, line, bar) render in terminal
- ✅ Visualizations display in iTerm2 and WezTerm without configuration
- ✅ ASCII fallback works in standard terminals
- ✅ Session save/load preserves and replays visualizations
- ✅ No crashes kill the REPL (supervised processes)
- ✅ Clear error messages for missing features

### Production Readiness (Phase 4)

- ✅ Comprehensive documentation and examples
- ✅ Test coverage >80%
- ✅ Performance meets targets (see above)
- ✅ Handles errors gracefully (network, invalid specs, crashes)
- ✅ Works in CI/CD environments (headless rendering)
- ✅ Community feedback incorporated

### Long-term Vision

- First-class LFE REPL visualization experience
- Community contributions of custom visualizations
- Integration with data analysis libraries
- Reference implementation for other LFE tools
- Case studies of real-world usage

---

## Risk Mitigation

### Technical Risks

**Risk**: VlConvert binary download failures
**Mitigation**: 
- Retry logic with exponential backoff
- Multiple mirror support
- Bundled binaries in release packages
- Clear error messages with manual download instructions

**Risk**: Platform-specific binary issues
**Mitigation**:
- Automated testing on all Tier 1 platforms
- Community testing on Tier 2 platforms
- Fallback to Docker/container-based rendering

**Risk**: Terminal protocol incompatibilities
**Mitigation**:
- Conservative protocol detection
- Extensive terminal testing matrix
- ASCII fallback as universal baseline

**Risk**: VlConvert process crashes
**Mitigation**:
- Supervision tree ensures restart
- Request timeout and retry logic
- Graceful degradation to cached images

### Adoption Risks

**Risk**: Learning curve for Vega-Lite specs
**Mitigation**:
- High-level API covers 90% of use cases
- Extensive examples and cookbook
- Progressive disclosure (simple→advanced)

**Risk**: Performance concerns with large datasets
**Mitigation**:
- Data sampling utilities built-in
- Clear documentation of performance characteristics
- Future Plotters backend for high-performance needs

**Risk**: Terminal incompatibility frustration
**Mitigation**:
- Automatic detection prevents failed attempts
- Clear terminal capability reporting
- Recommendation of supported terminals

---

## Comparison to Other Ecosystems

### vs Clojure/Reveal

**Similarities**:
- Declarative Vega-Lite specifications
- REPL integration with evaluation loop
- Session persistence and replay

**Advantages over Reveal**:
- No JavaFX dependency (lighter weight)
- Works over SSH without X11 forwarding
- Terminal graphics for remote development
- Simpler deployment (single binary)

**Reveal advantages**:
- Rich interactive JavaFX GUI
- Deep object inspection
- More mature ecosystem

### vs Python/IPython

**Similarities**:
- Inline visualization in REPL
- Multiple backend support
- Extensible architecture

**Advantages over IPython**:
- Built-in supervision and fault tolerance
- Declarative specs are pure data
- No GIL for concurrent visualization

**IPython advantages**:
- Massive ecosystem (matplotlib, seaborn, plotly)
- Jupyter notebook integration
- More mature tooling

### vs Julia REPL

**Similarities**:
- Multiple plotting backend support
- Backend switching at runtime
- Performance-focused implementation

**Advantages over Julia**:
- Erlang's process model for isolation
- SSH-friendly terminal graphics
- Focus on declarative over imperative

**Julia advantages**:
- Native plotting libraries
- Scientific computing focus
- JIT compilation for performance

---

## Conclusion

The Vega-Lite + VlConvert architecture provides xrepl with a production-ready path to modern visualization capabilities. By choosing declarative specifications over imperative rendering, leveraging compiled tooling over runtime dependencies, and embracing Erlang's supervision model, this approach delivers:

- **Developer experience**: Simple API for common cases, full power when needed
- **Deployment simplicity**: Auto-downloaded binaries, no complex setup
- **Environment flexibility**: GUI, terminal, file output from same specs
- **Reliability**: Supervised processes, graceful degradation, clear errors
- **Future-proof**: Extensible architecture, community standards

This positions xrepl as the first LFE REPL with native, production-ready visualization support—bringing the Erlang/LFE ecosystem to parity with modern REPLs while respecting functional programming principles and leveraging the BEAM's unique strengths.

The 10-week MVP timeline provides a realistic path to initial release, with clear phases for infrastructure, API development, integration, and polish. The modular architecture ensures that advanced features (wxErlang GUI, additional protocols) can be added incrementally without disrupting the core functionality.

Most importantly, this approach makes visualization a first-class citizen in LFE development, enabling data exploration, algorithm debugging, and result presentation with the same ease as other modern programming environments—all while maintaining the declarative, functional style that makes LFE powerful.
