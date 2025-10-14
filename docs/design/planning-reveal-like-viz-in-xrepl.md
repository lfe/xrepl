# Implementing Reveal-like visualizations in LFE xrepl

Modern terminal emulators support rich inline graphics through three competing protocols, enabling REPL visualization systems comparable to Clojure's Reveal. **The most practical implementation path for xrepl combines a wxErlang GUI window with terminal graphics fallback**, using Vega-Lite JSON generation for declarative visualizations. This hybrid approach provides interactive visualizations when running locally while gracefully degrading to terminal graphics over SSH, with an estimated 6-9 week MVP timeline.

Why this matters: REPL-driven development benefits enormously from visual feedback, but the Erlang/LFE ecosystem currently lacks mature visualization tools integrated into REPL workflows. By understanding the architectural patterns from Reveal and the capabilities of modern terminal graphics protocols, xrepl can become the first LFE REPL with native visualization support, bridging the gap between text-based evaluation and rich visual exploration.

The landscape has evolved significantly since 2020. The **Kitty Graphics Protocol** has emerged as the most advanced option for terminal-based rendering, supporting animations, transparency, and GPU acceleration at 60+ FPS. Meanwhile, wxErlang provides a mature GUI framework included in OTP, and the Elixir ecosystem offers production-ready libraries like ContEx and VegaLite for server-side chart generation. The challenge lies in integrating these technologies into a cohesive visualization system that works across different environments.

## Terminal graphics protocols enable rich inline visualization

Three primary protocols dominate modern terminal graphics: Sixel (1980s legacy with wide compatibility), iTerm2 Inline Images (simple and efficient), and Kitty Graphics Protocol (most advanced with GPU acceleration). Each protocol involves sending escape sequences that terminals interpret as image data, but they differ dramatically in capabilities and implementation complexity.

**Sixel remains the most compatible option**, supported by 30+ terminals including xterm, mlterm, WezTerm, and Konsole. The protocol divides images into 6-pixel-high horizontal strips encoded as ASCII characters (63-126 range), using run-length encoding for compression. A complete Sixel sequence looks like `ESC P q #0;2;0;0;0 #1~~@@vv@@~~$ ESC \`, where color registers are defined first, then pixel data encoded strip by strip. However, Sixel suffers from limited true color support (typically 256 colors due to register constraints), slow rendering (100-500ms for full-screen images), and no animation capabilities. Implementation requires either porting to libsixel via Erlang ports or writing a pure Erlang encoder, with estimated complexity of 1-2 weeks.

**The Kitty Graphics Protocol offers the most advanced capabilities** for real-time visualization. Using APC escape sequences (`ESC _G<key>=<value>;<data>ESC \`), it supports PNG/JPEG formats, 32-bit RGBA with alpha blending, pixel-perfect placement, animation frames, and Z-axis control for layering graphics above or below text. Most importantly, Kitty separates image transmission from placement, allowing you to upload an image once and display it multiple times with different parameters. The protocol achieves 60+ FPS through GPU acceleration, shared memory support for zero-copy transfers on local machines, and SIMD parsing. For a REPL visualization system, this enables smooth real-time updates crucial for exploring dynamic data. However, the protocol's complexity requires significant implementation effort (2-3 weeks for basic support), and terminal adoption remains limited primarily to Kitty, WezTerm (partial), and Ghostty (partial).

**iTerm2's inline image protocol strikes a balance** between simplicity and features. The protocol transmits complete image files as base64 in a single escape sequence: `ESC ] 1337 ; File=inline=1:base64_data ^G`. This approach is remarkably simple to implement (potentially under 100 lines of code) and efficient since it delegates image decoding to the OS. iTerm2 supports animated GIFs natively, handles Retina displays properly, and accepts any image format macOS supports. For tmux users, a multipart transmission mode splits large images into 1MB chunks. Implementation complexity is low (3-5 days), and the protocol is supported by iTerm2, WezTerm, and mintty. The primary limitation is macOS focus, though the protocol itself is platform-agnostic.

**Detection and fallback logic is critical** for robustness. Environment variables provide initial hints: `$TERM_PROGRAM` identifies iTerm2 or WezTerm, while `$KITTY_WINDOW_ID` confirms Kitty. The most reliable approach queries terminal capabilities by sending a control sequence and checking the response, though this adds startup latency. A practical detection strategy prioritizes Kitty (check environment), falls back to iTerm2 (query support), then tries Sixel (universal query sequence), finally degrading to ASCII art using Unicode block characters. The Python package `term-image` and Rust library `viuer` both implement this multi-protocol approach successfully and serve as reference implementations.

## Reveal's architecture separates streaming transformation from declarative rendering

Reveal builds on cljfx, a declarative JavaFX wrapper that implements React-inspired functional UI patterns. The rendering pipeline flows from Clojure values through a streaming transformation layer into JavaFX scene graph components. This architecture cleverly maintains object references rather than converting everything to text, enabling deep inspection and navigation through object graphs after rendering.

**The streaming layer** (`vlaaad.reveal.stream`) transforms Clojure data structures into an intermediate representation optimized for visual display. It uses transducers for efficient transformation, preserves syntax highlighting based on types, and crucially maintains references to actual objects so users can interact with rendered data. When you see a map displayed in Reveal, clicking on values opens new inspection windows because Reveal stores the actual objects, not just their printed representations. This "values are sacred" philosophy means printed output can be read back and remains inspectable.

**cljfx provides the declarative UI foundation** through a component lifecycle system. Every UI element is described as an immutable map with `:fx/type` keys, similar to React's JSX but using pure data. When state changes, cljfx diffs the new description against the old one and applies minimal updates to the actual JavaFX nodes. The lifecycle protocol defines three operations: `create` (instantiate from description), `advance` (update existing instance), and `delete` (cleanup). This architecture eliminates manual DOM manipulation and makes UI logic purely functional.

**Vega integration happens through JavaFX's WebView component**, which embeds a full web browser. Reveal generates HTML containing Vega/Vega-Lite JSON specifications and loads it into the WebView. The data flow is: Clojure map → JSON serialization via `clojure.data.json` → HTML generation with CDN-linked Vega libraries → WebView rendering → JavaScript execution. Importantly, Reveal separates specification from data, passing datasets separately to enable fast updates for streaming visualizations without reparsing the entire spec. The system also supports two-way signal binding, allowing Clojure code to react to user interactions in Vega visualizations. This approach is pragmatic but creates a dependency on a heavyweight web rendering engine.

**JavaFX was chosen for specific architectural reasons**: it runs in-process with the application being inspected, provides hardware-accelerated native graphics, includes rich UI components (charts, tables, trees, WebView), and offers consistent cross-platform behavior. Most critically, the scene graph model maps naturally to declarative rendering patterns. However, JavaFX requires careful memory management (explicit object destruction) and has a complex property system that cljfx abstracts away. The REPL integration uses nREPL middleware to intercept evaluation results and forward them to the Reveal window, maintaining actual object references through serialization. Alternative integration modes include socket REPL (prepl format) and Clojure's tap system for non-invasive value streaming.

## A hybrid wxErlang plus terminal graphics approach provides maximum flexibility

Four primary implementation approaches exist for xrepl, each with distinct tradeoffs in complexity, capabilities, and environment compatibility. The recommended hybrid approach combines wxErlang for rich local visualization with terminal graphics fallback for SSH sessions, estimated at 6-9 weeks for a complete MVP implementation.

**Direct terminal rendering using Sixel or Kitty protocols** offers the simplest starting point. No Erlang libraries exist for these protocols, requiring either ports to libsixel (a mature C library) or pure Erlang implementations from scratch. A port-based approach opens an Erlang port to spawn `img2sixel` or `vl2png`, passes image data or Vega-Lite specs through the port, receives encoded output, and writes escape sequences to stdout. This works seamlessly over SSH without X11 forwarding, displays inline with REPL output, and requires no GUI framework. However, it's limited to static images (no true interactivity), constrained by terminal capabilities (Sixel typically 256 colors max), and doesn't work in traditional tmux/screen sessions. Implementation complexity is medium (1-2 weeks for basic support) with the main challenges being terminal capability detection and handling the diversity of terminal emulators.

**wxErlang provides native GUI capabilities** with the significant advantage of being included in OTP by default. The architecture would spawn a supervised visualization server process that manages wxFrames, receives messages from the REPL process containing visualization commands, renders using wx primitives or embedded wxWebView for HTML/Vega content, and persists independently of the REPL (crashes don't kill the REPL). This approach enables true interactivity (zoom, pan, click handlers), multiple windows or tabs, rich widget sets for complex visualizations, and native platform look-and-feel. The Observer and Debugger tools demonstrate wxErlang's production readiness. However, wxErlang has a notoriously steep learning curve due to mapping C++ object-oriented patterns to functional Erlang, requires explicit memory management for wx objects, doesn't work over SSH without X11 forwarding, and involves complex event handling via message passing. Implementation complexity is medium-high (4-5 weeks for MVP) with major time investments in learning the wx API, implementing proper lifecycle management, and handling cross-platform quirks.

**The hybrid approach combines both strategies** through an architecture where xrepl detects the environment at startup (checks for DISPLAY on Linux, GUI availability), then either starts the wxErlang visualization server (GUI mode) or falls back to terminal graphics (SSH mode). A unified message protocol (`{display, Type, Data, Options}`) keeps the REPL code agnostic to rendering method. The visualization manager process decides which backend to use based on capabilities and forwards rendering commands appropriately. This provides the best of both worlds: rich interactive visualizations locally and functional fallback over SSH. However, it doubles implementation complexity since you must maintain two complete rendering paths, and synchronization between REPL and separate GUI window requires careful design. Total implementation time is 6-9 weeks: 2 weeks for infrastructure, 1-2 weeks for terminal rendering, 4-5 weeks for GUI rendering, and 2 weeks for integration and polish.

**Vega-Lite integration through external process ports** provides the most practical path to declarative visualizations. Rather than implementing chart rendering from scratch, xrepl generates Vega-Lite JSON specifications and pipes them to VlConvert (a standalone tool that converts Vega-Lite to PNG/SVG using embedded Deno, no Node.js required). The code flow is straightforward: construct a Vega-Lite spec as an Erlang map, encode to JSON using jsx or jiffy libraries, pipe to `vl2png` via Erlang port, receive PNG binary, and display using wxErlang or terminal protocols. This leverages the mature Vega ecosystem, provides a declarative API that's simple to generate from LFE, enables export to static formats, and avoids implementing chart rendering algorithms. The LFE API could look like:

```lisp
;; High-level plotting API
(viz:scatter '((x . (1 2 3 4)) (y . (10 20 30 40)))
             '((mark . "point") (title . "My Plot")))

;; Or direct Vega-Lite specs for full control
(viz:vega-lite 
  '#m(mark "line"
      data #m(values (#m(x 1 y 2) #m(x 2 y 4)))
      encoding #m(x #m(field "x" type "quantitative"))))
```

Implementation complexity is medium (2-3 weeks): 1 week for JSON generation helpers, 3-5 days for port integration with VlConvert, and 1 week for designing the LFE API. The main limitation is requiring an external dependency (VlConvert binary), though it could be bundled with xrepl distributions.

## Terminal multiplexers present significant challenges for graphics

Traditional terminal multiplexers like tmux and screen fundamentally conflict with graphics protocols because they parse and buffer all terminal output. When tmux intercepts escape sequences for Sixel or Kitty graphics, it typically strips them or stores them in ways that break rendering. This architectural mismatch means graphics sent through standard tmux don't appear in the terminal.

**sixel-tmux represents the most mature workaround** through a fork that adds explicit Sixel support. It operates in two modes: passthrough mode (forward Sixel sequences to supporting terminals) and fallback mode (use "derasterize" to convert Sixels to ASCII art for non-supporting terminals). The fork maintains a separate buffer for graphics and attempts to redraw them when panes switch or scroll. However, multiple issues remain: vertical splits don't work well with passthrough mode, images aren't redrawn when returning from other buffers, and scrollback can corrupt graphics. Several competing forks exist with varying levels of stability, and none are ready for production use. The Kitty graphics protocol faces even worse multiplexer support since tmux would need to understand image IDs, placement semantics, and composition rules.

**Modern alternatives avoid the problem entirely** through different architectures. Kitty and WezTerm both include built-in multiplexing capabilities with native tab, pane, and window management. Since graphics rendering happens in the same process that manages panes, there's no intermediary to break protocols. Graphics work seamlessly in split panes with different rendering modes per pane (one with images, another text-only). The tradeoff is losing tmux's remote persistence—if the terminal closes, sessions end. Zellij, a modern Rust-based multiplexer, has experimental Sixel support and active development toward Kitty protocol compatibility, but graphics support remains immature.

**Split-pane capabilities in native terminals** provide better graphics support than multiplexers. Kitty's native split panes support mixed rendering modes with GPU acceleration, allowing graphics in one pane and text in another. WezTerm similarly provides built-in multiplexing with full graphics protocol support (Sixel, Kitty, iTerm2). Both use Unicode placeholders for image positioning that can coexist with normal text in the same buffer. For xrepl, the practical recommendation is: initially target modern terminals with native split support (Kitty, WezTerm), document that multiplexer support is experimental, and consider sixel-tmux integration as a later enhancement rather than a core requirement. Users needing both multiplexing and graphics should use Kitty or WezTerm instead of tmux.

## Successful REPLs demonstrate diverse architectural patterns for visualization

IPython and Jupyter exemplify the display system architecture pattern that has become standard across modern REPLs. IPython implements a pluggable display stack where multiple renderers register with priority levels. When evaluation produces a result, the display system checks terminal capabilities and routes to the appropriate renderer. Backends include inline terminal graphics (using Sixel/Kitty with appropriate matplotlib backends), separate GUI windows (matplotlib opens X11/Cocoa windows via Qt/GTK), HTML output for Jupyter notebooks, or file saving as fallback. This architecture separates computation (kernel) from presentation (display handlers), enabling the same code to work in diverse environments.

**Euporie represents the state-of-art for terminal-based notebooks**, providing a full TUI-based Jupyter client written in Python using `prompt_toolkit`. It supports all major graphics protocols (Sixel, Kitty, Kitty-unicode, iTerm2) with automatic detection. The architecture is particularly instructive: receive `display_data` messages from Jupyter kernel (ZeroMQ), check terminal capabilities, render images using `pillow` to target format, output appropriate escape sequences, and position using TUI layout. Euporie proves that sophisticated notebook interfaces work entirely in the terminal, including interactive widgets, markdown rendering, and LaTeX. The separation between notebook logic (cell management), kernel communication (Jupyter protocol), and display rendering (pluggable based on capabilities) creates a maintainable architecture that xrepl could adapt.

**Julia's display system demonstrates elegant functional design** through a display stack where multiple displays register as handlers. Objects define `show` methods for different MIME types (`show(io::IO, ::MIME"image/png", p::Plot)`), and the runtime dispatches to appropriate handlers based on environment. Backend switching at runtime enables the same plot command to render as GUI window (GR backend), ASCII art (UnicodePlots), or HTML (PlotlyJS) simply by calling `unicodeplots()` or `gr()` beforehand. This flexibility comes from treating display as a cross-cutting concern rather than coupling it to plot types. For xrepl, a similar abstraction would allow `(plot data)` to work identically whether rendering to wxErlang window, terminal graphics, or file output.

**R's terminalgraphics package shows how to integrate with language-native graphics systems** by implementing standard graphics device interfaces. The package registers `sixel()` and `tgp()` (Kitty) device functions that hook into R's graphics engine. When R code calls `plot()`, the graphics engine sends drawing commands to the active device, which translates them to Sixel or Kitty escape sequences. This approach is architecturally elegant because it requires zero changes to existing plotting code—users just call `sixel()` before plotting. However, it requires deep integration with the graphics subsystem, which LFE lacks. The more practical pattern for xrepl is explicit visualization commands rather than trying to intercept all potentially visual operations.

**Common patterns across successful implementations** include capability detection at startup (query terminal for supported protocols), fallback chains (try best option, progressively degrade), separation of computation from rendering (different processes or threads), message-based communication (Erlang naturally supports this), and pluggable backend architecture (register handlers for different capabilities). For xrepl specifically, the architecture should mirror Euporie's approach: single manager process for visualization decisions, pluggable renderers implementing a common protocol, automatic capability detection, and clear error messages when requested features aren't available.

## The Erlang ecosystem provides building blocks but lacks integrated solutions

The BEAM ecosystem has surprisingly limited support for terminal-based visualization compared to Python or Julia, with most effort focused on web-based approaches through Phoenix LiveView. No Erlang or Elixir projects currently integrate rich visualization into REPL workflows, though several libraries provide components that could be assembled into a solution.

**ContEx represents the most mature server-side visualization library**, written in pure Elixir generating SVG output. It provides bar charts, line plots, scatter plots, Gantt charts, and sparklines through a dataset-oriented API. The declarative approach is similar to D3.js concepts, and integration with Phoenix LiveView is excellent for web dashboards. However, ContEx generates SVG as strings, requiring either a browser to view or conversion to raster formats for terminal display. VegaLite, the Elixir binding to Vega-Lite grammar, offers more comprehensive visualization capabilities with official Livebook integration. It can render using wxWebView (wxErlang's embedded browser component) or save to PNG/SVG through external processes. Tucan builds a high-level matplotlib-like API on top of VegaLite, providing convenient functions for common plot types while maintaining declarative VegaLite specs underneath.

**Terminal UI frameworks exist but focus on text-based interfaces** rather than graphics. Ratatouille implements The Elm Architecture in Elixir, providing declarative terminal UI with panels, tables, labels, and basic ASCII charts using the asciichart library. It powers Toby, a terminal-based Erlang observer, demonstrating production readiness. Kjell enhances the Erlang shell with syntax highlighting, power-line prompts, and extension support, though without graphics capabilities. These frameworks prove that sophisticated terminal interfaces are possible in BEAM, but none integrate bitmap graphics protocols like Sixel or Kitty. The asciichart library provides ASCII line charts using box-drawing Unicode characters, offering a lightweight fallback for basic visualization without requiring any graphics protocol support.

**wxErlang remains the standard GUI framework**, included in OTP and used by Observer and Debugger. It provides mature cross-platform GUI capabilities through wxWidgets bindings, including wxWebView for embedding HTML content. The architecture maps C++ object-oriented patterns to Erlang's functional style through message-passing for events. While complex, wxErlang is production-proven and requires no external dependencies. For xrepl, wxErlang provides the most practical path to native GUI visualization. A supervised process could manage wx frames, receive plot commands via messages, render using either wx primitives or embedded wxWebView for Vega content, and persist independently of the REPL. Example Observer code in OTP demonstrates patterns for wx lifecycle management, event handling, and drawing.

**The minimal viable implementation path** combines proven BEAM ecosystem components: start with ContEx or VegaLite for generating SVG/PNG charts (pure Elixir, well-maintained), add VlConvert for converting Vega-Lite JSON to bitmap formats (no Node.js required), display using wxErlang for GUI mode (included in OTP) or Sixel for SSH mode (via libsixel port), and wrap everything in a clean LFE API. The Elixir Livebook project demonstrates successful integration of these components, though Livebook uses a web-based notebook interface rather than traditional REPL. A production-ready implementation would be:

```erlang
% Phase 1: Generate visualization spec
VegaSpec = #{
    <<"$schema">> => <<"https://vega.github.io/schema/vega-lite/v5.json">>,
    <<"data">> => #{<<"values">> => Data},
    <<"mark">> => <<"bar">>,
    <<"encoding">> => #{
        <<"x">> => #{<<"field">> => <<"a">>, <<"type">> => <<"ordinal">>},
        <<"y">> => #{<<"field">> => <<"b">>, <<"type">> => <<"quantitative">>}
    }
},

% Phase 2: Convert to image
Json = jsx:encode(VegaSpec),
Port = open_port({spawn, "vl2png"}, [binary, {packet, 2}]),
port_command(Port, Json),
PngData = receive {Port, {data, Data}} -> Data end,

% Phase 3: Display via appropriate backend
case xrepl_viz:get_backend() of
    wxerlang -> xrepl_wx:display_image(PngData);
    sixel -> xrepl_sixel:display_image(PngData);
    ascii -> xrepl_ascii:display_fallback(VegaSpec)
end
```

This approach requires approximately 6-9 weeks for MVP: 2-3 weeks for Vega-Lite JSON generation and VlConvert integration, 4-5 weeks for wxErlang window with image display, 1-2 weeks for Sixel fallback, and 2 weeks for LFE API design and integration with xrepl.

## Implementation roadmap prioritizes quick wins then comprehensive features

The practical implementation path starts with low-hanging fruit to provide immediate value, then progressively adds more sophisticated capabilities. Phase 1 focuses on basic ASCII visualization and ANSI colors to enhance xrepl output immediately without requiring graphics protocols or external dependencies.

**Week 1-2 should establish foundations**: Implement ANSI color support by porting Elixir's IO.ANSI concepts to LFE (functions for common colors, cursor positioning, screen clearing), add syntax highlighting to xrepl output using color codes, create basic ASCII chart rendering using box-drawing Unicode characters (implementing simple line and bar charts), and validate that charts display correctly across different terminals. This provides visible improvement to xrepl with minimal risk and approximately 10-15 hours of development time. Example API: `(viz:ascii-plot '(1 2 3 4 5 3 2))` outputs a simple line chart using box-drawing characters.

**Week 3-6 implements Vega-Lite generation**: Design LFE visualization API with common chart types (scatter, line, bar, histogram), implement Vega-Lite JSON spec generation from LFE data structures, integrate VlConvert via Erlang port for converting specs to PNG, add terminal capability detection (check for Sixel/Kitty support), and implement Sixel output via libsixel port with fallback to ASCII charts. This creates a working visualization pipeline end-to-end. The API should feel natural in LFE:

```lisp
;; Simple high-level API
(viz:bar '((categories . #("A" "B" "C"))
           (values . #(10 20 15)))
         '((title . "Sales by Category")))

;; Direct Vega-Lite for advanced users
(viz:vega-lite 
  '#m(mark "point"
      data #m(values (#m(x 1 y 2) #m(x 2 y 4)))
      encoding #m(x #m(field "x") y #m(field "y"))))
```

**Week 7-12 adds wxErlang GUI support**: Implement supervised visualization server process using wxErlang, create basic window with image display panel, add message protocol for REPL to send visualization commands (`{display, image, PngData, Options}`), implement auto-detection of GUI availability (check DISPLAY environment variable, test wx initialization), add startup flag for forcing terminal/GUI mode (`xrepl --viz-mode terminal`), and test across platforms (macOS Aqua, Linux X11, Windows). This provides the rich interactive visualization experience for local development.

**Week 13-15 adds browser-based alternative**: Embed Cowboy HTTP server (optional dependency), implement HTML generation with embedded Vega-Lite JavaScript libraries, auto-open browser for interactive visualizations, add WebSocket support for live updates, and create dashboard view for multiple charts. This provides the best interactive experience without requiring GUI framework. However, this should be considered optional since it adds significant complexity and dependencies.

**Critical design decisions** include API design (balance simplicity for common cases with power for advanced usage), backend selection (automatic vs explicit configuration), error handling (graceful degradation when features unavailable), and data format (native Erlang terms vs standardized formats like JSON). The visualization API should integrate naturally with xrepl's existing shell functions, perhaps as a special command `(viz:* ...)` that gets recognized and dispatched to the visualization system. Documentation must clearly explain limitations (multiplexer graphics, terminal requirements, SSH scenarios) and provide troubleshooting guidance.

The recommended stack consists of: LFE for API surface and integration with xrepl, Erlang/OTP for core infrastructure (ports, supervision, message passing), VlConvert CLI for Vega-Lite rendering (bundle with xrepl or make optional dependency), wxErlang for native GUI (included in OTP, no external dependency), libsixel for terminal graphics (optional dependency, fallback to ASCII if missing), and ContEx or pure Vega-Lite for chart specifications. This stack minimizes external dependencies while maximizing capabilities, with graceful degradation when optional components aren't available.

## Recommendations

**Immediate priorities** focus on establishing basic visualization quickly. Implement ANSI colors and ASCII charts first (2-4 weeks total) to provide immediate value without complex dependencies. Users get syntax-highlighted output and basic in-terminal plots within a month. Then implement Vega-Lite JSON generation with VlConvert integration (2-3 weeks) to unlock declarative specifications and static chart export. This creates a working visualization pipeline that can save PNG files even without display integration.

**Medium-term goals** build comprehensive display support. Add wxErlang GUI window (4-5 weeks) for rich local visualization with the full capabilities of native GUI frameworks. Implement Sixel terminal graphics fallback (1-2 weeks) for SSH scenarios where GUI isn't available. This hybrid approach serves both local development and remote operation, covering the majority of use cases. Total time investment is approximately 9-12 weeks for complete implementation.

**Long-term enhancements** expand capabilities and polish. Consider browser-based rendering via Cowboy for maximum interactivity and responsive design, add animation support for time-series data using Kitty protocol's animation capabilities, implement custom chart types beyond standard Vega-Lite grammar, create integration with xrepl history (visualize previous results), and potentially develop a Livebook kernel for LFE to leverage that ecosystem. These enhancements provide increasing sophistication but aren't necessary for initial release.

**Technical specifications** should explicitly document supported environments, required dependencies, optional dependencies with fallback behavior, terminal compatibility matrix, and known limitations. The README should guide users through installation on different platforms, provide examples of all chart types, explain how to choose backends explicitly, troubleshoot common issues, and explain why certain features don't work in certain environments (especially multiplexers). Clear documentation prevents frustration and helps users understand the architectural tradeoffs.

The path forward is clear: start with ASCII and Vega-Lite for quick wins, add wxErlang for rich local visualization, implement Sixel for SSH scenarios, and create a clean LFE API that hides complexity. This approach delivers a production-ready visualization system for xrepl within 3-4 months while maintaining the flexibility to enhance capabilities over time.