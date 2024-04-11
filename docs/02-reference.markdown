# API Reference

The following is a list of all user-facing parts of DBvolve.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `DBVOLVE`

### `EVOLVE` (function)

    (EVOLVE DATABASE EVOLUTIONS-PATH)

Run evolutions at `evolutions-path` against `database`.

  `database` must be a database client object for a supported database client
  system.  Supporting systems (e.g. `dbvolve/sqlite`) must already be loaded.

  Examples:

    (ql:quickload '(:dbvolve :dbvolve/sqlite))
    (defvar *db* (sqlite:connect "foo.sqlite"))
    (evolve *db* "path/to/evolutions"))

  

