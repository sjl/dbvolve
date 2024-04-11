Usage
=====

DBvolve is a lightweight library for evolving a database schema over time.  It
might be called a "database migration library" except that "migrations" are
round-trip, and DBvolve intentionally does not support backwards migrations.

[TOC]

## Overview

DBvolve is a relatively light-weight schema update mechanism for SQL databases.
Updates ("evolutions") are raw SQL files stored in a single directory with
a particular naming scheme, e.g.:

    evolutions/
        000-initial-tables.sql
        001-add-products-table.sql
        002-insert-more-product-types.sql
        003-merge-name-fields.sql

When run, DBvolve will ensure that a table called `dbvolve` exists in your
database to track which evolutions have been applied. This table will be locked
before running any evolutions to ensure that multiple clients can attempt to run
evolutions at the same time, e.g. when restarting a web server.

DBvolve will compare the list of already-run evolutions from the `dbvolve` table
to the evolution files found in the directory and run any with a higher ID than
the last currently-applied evolution.

A transaction will be used when running these evolutions.  If any evolutions
fail to apply, the transaction will be rolled back.  Note that this is *one*
transaction for *all* pending evolutions, so if you have three evolutions to run
and the second evolution fails, *none* of the three will be applied.

You must not begin, commit, or rollback transactions in your evolutions.  This
is not checked.

There is intentionally no support for backwards evolutions.  If you need to undo
something, write a new evolution and roll forward.

## Installation/Loading

The core `dbvolve` system contains only the main API.  You will also need to
load a second `dbvolve/…` system that adds support for your particular database
client.  Currently the following database clients are supported:

* `dbvolve/postmodern`
* `dbvolve/sqlite`

So if you want to e.g. use DBvolve to run schema evolutions against a Postgres
database, your project's `.asd` might look something like this:

    :depends-on (… :dbvolve :dbvolve/postmodern)

## Evolution Files

All schema evolutions must be kept in a single directory.  Any files ending in
`sql` or `dbvolve` (reserved for future use) will be considered to be
evolutions, and must have names of the form `<integer>-<name>.sql`, e.g.:

    evolutions/
        000-initial-tables.sql
        001-add-products-table.sql
        002-insert-more-product-types.sql
        003-merge-name-fields.sql

Evolution IDs must start from 0 and there must be no gaps or duplicates (DBvolve
will verify this before running).

## API

The main API is the `dbvolve:evolve` function, which takes a database client
object and a path to an evolutions directory:

    (defvar *db* (sqlite:connect "db.sqlite"))

    (dbvolve:evolve *db* "path/to/evolutions/")

`evolve` is a generic function that will dispatch to the appropriate method
based on the class of the database client object it receives — you must ensure
that the appropriate DBvolve client support system (e.g. `dbvolve/sqlite`) has
been loaded in advance.

This single function is pretty much the extent of the API.  How you want to use
it is up to you.  One way might be to write a little wrapper script to invoke it
manually when you need to.  Or if you're writing a web application server, you
could have the server run `evolve` every time it starts up — the table locking
will ensure that servers will see a consistent database state even if you
restart many of them at the same time.

## New Clients

If you want to add support for another database client, look at the code in
`src/postmodern.lisp` and `src/sqlite.lisp` to get an idea of what you need to
do.  If it's a relatively common database client that others might find useful
please consider sending a pull request.
