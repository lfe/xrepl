# xrepl

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][version]
[![Tags][github-tags-badge]][github-tags]

[![Project Logo][logo]][logo-large]

*An experimental, general purpose LFE REPL*

##### Table of Contents

* [About](#about-)
* [Dev Process](#dev-process-)
* [Tests](#tests-)
* [Usage](#usage-)
* [License](#license-)

## About [&#x219F;](#table-of-contents)

This project has a long history of not coming into being, despite there being a need for it in the LFE community from fairly early in the language's history. The [initial impetus](https://github.com/lfe/lfe/issues/153) and all subsquent community feature requests have been 100% inspired by what the larger Clojure community has in the [nREPL](https://nrepl.org/nrepl/1.1/index.html) project.

The xrepl project has as its primary mission:

> To explore strange new LFE REPL use cases, to seek out new REPL features and new interactive development possibilities, to boldly go where LFE-proper hasn't gone. Yet.

Anything good that comes out of this project that may be even remotely useful to LFE itself will result in a ticket and/or a PR against the LFE repository for Robert's reading pleasure and discretionary approval.

## Dev Process [&#x219F;](#table-of-contents)

**HIC SVNT DRACONES**

This project is a work in progress -- _very_ early stages!

### Phase 1 Status: Complete ✓

Phase 1 implementation is complete! The following has been implemented:

#### Core Modules

- **xrepl-eval**: LFE evaluation wrapper with error handling and hooks
- **xrepl-env**: Environment management with shell variables, functions, and macros
- **xrepl-io**: I/O handling for stdio transport
- **xrepl-store**: ETS-based session storage (gen_server)
- **xrepl-session**: Individual REPL session management (gen_server with evaluator process)
- **xrepl-session-sup**: Session supervisor (simple_one_for_one)
- **xrepl-sup**: Main application supervisor
- **xrepl**: Main REPL loop and application entry point

#### Features

- ✓ Basic working REPL with single local session
- ✓ Expression evaluation with proper error handling
- ✓ Environment persistence across evaluations
- ✓ Shell history variables (+, ++, +++, -, *, **, ***)
- ✓ Shell functions (pwd, help, i, clear, etc.)
- ✓ Pattern matching with `set`
- ✓ Function and macro definitions
- ✓ Clean supervisor tree for fault tolerance

#### Try It Out

Start the xrepl:

```lfe
(xrepl:start)
```

Evaluate expressions:

```lfe
lfe> (+ 1 2)
3
lfe> (defun factorial (n) (if (== n 0) 1 (* n (factorial (- n 1)))))
factorial
lfe> (factorial 5)
120
lfe> *
120
lfe> (set (list a b c) (list 1 2 3))
(1 2 3)
lfe> (+ a b c)
6
```

There's a lot of work to be done, here -- the bits that have been written down are organised by milestone, here:

* [lfe/xrepl/milestones](https://github.com/lfe/xrepl/milestones?direction=asc&sort=title&state=open)

Specific tickets can be view by clicking on the milestone in question in that link. Keep an eye out for "epic" tickets, as those will provide the most context for the work being done in any given milestone.

If the above notes haven't scared you off and you would like to play (or even contribute), you can run the latest xrepl code using rebar3_lfe by doing the following:

1. `git clone git@github.com:lfe/xrepl.git`
1. `cd xrepl`
1. `mkdir _checkouts && cd _checkouts`
1. `git clone git@github.com:lfe/rebar3.git`
1. `cd rebar3 && git checkout release/0.5.x`
1. `cd ../../../`

At this point, you should be:

1. Back in the xrepl project directory
1. Have a local copy of the latest rebar3 branch
1. Be using said branch when you execute `rebar3 lfe ...` commands

## Tests [&#x219F;](#table-of-contents)

```shell
$ rebar3 as test lfe ltest
```

## Usage [&#x219F;](#table-of-contents)

Assuming you've following the above steps, start up xrepl with the following:

``` shell
$ rebar3 lfe xrepl
```

## License [&#x219F;](#table-of-contents)

``` text
Apache License, Version 2.0

Copyright © 2024, The LFE Community <http://lfe.io>.
```

[//]: ---Named-Links---

[logo]: priv/images/logo-v1-x250.png
[logo-large]: priv/images/logo-v1-x4800.png
[github]: https://github.com/lfe/xrepl
[gitlab]: https://gitlab.com/lfe/xrepl
[gh-actions-badge]: https://github.com/lfe/xrepl/actions/workflows/cicd.yml/badge.svg
[gh-actions]: https://github.com/ORG/xrepl/actions/workflows/cicd.yml
[lfe]: https://github.com/lfe/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-21%20to%2026-blue.svg
[version]: https://github.com/ORG/xrepl/blob/main/.github/workflows/cicd.yml
[github-tags]: https://github.com/ORG/xrepl/tags
[github-tags-badge]: https://img.shields.io/github/tag/lfe/xrepl.svg
[github-downloads]: https://img.shields.io/github/downloads/lfe/xrepl/total.svg
