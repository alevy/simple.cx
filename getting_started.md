---
title: Getting Started
showtoc: true
---

This getting started tutorial will get you up and running with a basic web
application. At the end you will know

1. how install Simple

2. how to create a Simple application from scratch

3. the structure of a Simple application

4. how to add persistence with PostgreSQL.

## Guide Assumptions

This guide assumes you have a working version of the GHC Haskell compiler, the
cabal package manager and an up-to-date version of the PostgreSQL database. The
best way to get GHC and cabal setup is by installing the
[Haskell platform](http://www.haskell.org/platform). Most linux distributions
have PosgreSQL in their package repositories (e.g.
`apt-get install postgresql`, `pacman -S postgresql`).
Mac OS X comes with PostgreSQL, however, some of the utilities that this guide
relies on (like `pg_ctl`) are not shipped by default. However, you installing
from Homebrew will install the approriate utilities.

The guide also assumes you have a basic understand of Haskell programming, web
programming and preferably have built a few web applications in the past. For a
good starting guide to Haskell see
[Learn You a Haskell for Great Good!](http://learnyouahaskell.org)
and/or [Real World Haskell](http://book.realworldhaskell.org/).

## Creating a new Simple app

### Installing Simple

Open up a terminal. Commands prefaced with a dollar sign ($) should be run in
terminal. Use cabal to instal Simple:

```bash
$ cabal install simple
```

To verify that Simple installed properly run the following command:

```bash
$ smpl --help
```

which should print out the subcommands and options available for the `smpl`
utility that comes with Simple.

### Creating the Blog application

The `smpl` utility helps you create a new, blank Simple application from the
command-line. To begin creating our application, open a terminal, navigate to a
folder where you would like to create the project (for example `cd ~/hack`) and
create a new Simple project called "blog":

```bash
$ smpl create --all blog
```

This will create a new subdirectory called "blog" containing your app. The
"--all" flag tells `smpl` to include boilerplate code for templates,
cookie-based sessions and PostgrSQL based models. The directory contains a
ready-to-run app with the following structure:

| File/Folder    | Purpose                                                    |
|----------------|------------------------------------------------------------|
| Blog/          | Parent directory for app specific modules                  |
| Blog/Common.hs | A base module that defines the application type            |
| db/            | An empty directory for defining the database schema        |
| templates/     | The default folder for defining view templates             |
| views/         | The default folder for defining views                      |
| Application.hs | Initial setup and route configuration                      |
| blog.cabal     | Used by cabal to resolve package dependencies and compile the app|
| Main.hs        | Contains the main function used to start the application   |

## Getting up and running

Our application is ready to start working on. Now we'll get a server up and
running and start adding functionality to our application.

### Starting the server

Simple apps are built on top of the WAI package, and can be run with any WAI
compatible server, like [warp](http://hackage.haskell.org/package/warp). The
`smpl` utility helps you run your application in development mode. First,
install the package `wai-handler-devel`:

```bash
$ cabal install wai-handler-devel
```

Now we can run the Simple development server. From your application directory,
run:

```bash
$ smpl server
```

This will fire up `wai-handler-devel` on port _3000_, and set the environment
variable "ENV" to "development". To see your application in action, open a
browser and navigate to [http://localhost:3000/](http://localhost:3000). You
should see the default generated home page:

![](images/screenshot-hello.png "\"Hello World\" Screenshot")

You can of course compile and run your application without the `smpl` utility
(in fact, this is probably how you will deploy your app, as wai-handler-devel
is not suitable for production).

```bash
$ cabal run
```

This will compile and run `Main.hs`, which by default runs your application
using warp on port _3000_. You can specify a different port by setting the "PORT"
environment variable, for example, to run the application on port _8080_:

```bash
$ PORT=8080 cabal run
```

### Adding a Route

The default generated application isn't very interesting, displaying only a
boilerplate homepage for every request.
