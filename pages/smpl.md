---
title: The smpl Command-line Utility
showtoc: true
---

_Simple_ ships with a command-line utility, `smpl`, that helps bootstrap
applications as well as run then in development mode.

## Synopsis

```
smpl [server] [OPTIONS]
smpl create [OPTIONS] <app_name>
```

## Global Options

```
-? --help           Display help message
-V --version        Print version information
```

## Creating an application

```bash
smpl create [OPTIONS] <app_name>
```

Creates a new _Simple_ application in the directory `app_name`. The Haskell
module for the new application is the _CamelCase_ derivation of `app_name`.
For example, an app with the name `test_app` would be created with the module
name `TestApp`.

By default, only minimal functionality is included in generated apps. The
following options add additional functionality:

  * `--templates` includes support for rendering templates taken from the
    "views" subdirectory.

  * `--postgresql` adds support for creating models that serialize to
  PostgreSQL, as well as convenience methods for accessing a global PostgreSQL
  connection.

  * `--sessions` adds the ability to interact with a cookie-based session map.

  * `--all` includes all of the above.

All of these options modify the `[AppName].Common` module by adding various
instances for `AppSettings` type, as well as add dependencies on necessary
packages in the app's cabal file. The `--templates` flag also changes the
default app handler to use render a placeholder view instead of a simple
string.

## Running an application

```bash
smpl server
```

Running the above command from within an application directory invokes
`wai-handler-devel` to run an application in development mode. In particular,
it expects `app` function from the `Application` module to have the type
signature `(Application -> IO()) -> IO ()` and passes in a continuation to run
the `Application`. The server will watch for changed files, and recompile and
reload the application as necessary.

By default, the server runs on port 3000, but this can be changed with `-p PORT`
or `--port=PORT`. If your `app` function resides in a different module, use
`--module=MODULE` to specify it.

The server sets the `ENV` environment variable to `development`. This can be
used by the application that it is running in development mode. For example,
`simple-postgresql-orm` uses this to start a PostgreSQL server in a
subdirectory of the app as well as run all migrations when in development mode.

