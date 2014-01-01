---
title: Home
---

Simple is &quot;framework-less&quot; web framework for Haskell web
applications. Unlike many other frameworks, Simple does not enforce a
particular structure or paradigm for web applications. Rather, Simple
provides enough infrastructure to help you create your own patterns (or
re-create existing ones). Simple is minimalist, providing a lightweight
base. Everything else (e.g. sessions, controllers, persistence,
caching) is provided in composable units, so you can include only the
ones you need in your app, and easily replace with your own components.

##Quick Start

If you already have Haskell setup, you can install and create your first
application with just a few simple commands:

```bash
$ cabal install simple
$ smpl create test_app
$ cd test_app
$ cabal run
```

You can now navigate to your web app at
[http://localhost:3000](http://localhost:3000)!

You can expand your app with a persistent datastore, session data, templates
and more. Build a fully function blog in the
[Getting Started Tutorial](tutorial.html)!

