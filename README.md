# xrepl

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][version]
[![Tags][github-tags-badge]][github-tags]

[![Project Logo][logo]][logo-large]

*An experimental, general purpose LFE REPL*

##### Table of Contents

* [About](#about-)
* [Build](#build-)
* [Start the Project REPL](#start-the-repl-)
* [Tests](#tests-)
* [Usage](#usage-)
* [License](#license-)

## About [&#x219F;](#table-of-contents)

This project has a long history of not coming into being, despite there being a need for it in the LFE community from fairly early in the language's history. The [initial impetus](https://github.com/lfe/lfe/issues/153) and all subsquent community feature requests have been 100% inspired by what the larger Clojure community has in the [nREPL](https://nrepl.org/nrepl/1.1/index.html) project.

The xrepl project has as its primary mission:

> To explore strange new LFE REPL use cases, to seek out new REPL features and new interactive development possibilities, to boldly go where LFE-proper hasn't gone. Yet.

Anything good that comes out of this project that may be even remotely useful to LFE itself will result in a ticket and/or a PR against the LFE repository for Robert's reading pleasure and discretionary approval.

## Build [&#x219F;](#table-of-contents)

```shell
$ rebar3 lfe compile
```

## Start the Project REPL [&#x219F;](#table-of-contents)

```shell
$ rebar3 lfe repl
```

## Tests [&#x219F;](#table-of-contents)

```shell
$ rebar3 as test lfe ltest
```

## Usage [&#x219F;](#table-of-contents)

TBD

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
