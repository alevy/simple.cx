---
title: Getting Started
showtoc: true
---

This getting started tutorial will get you up and running with a basic web
application. At the end you will know (1) how install Simple, (2) how to create
a Simple application from scratch, (3) the structure of a Simple application
and (4) how to add persistence with PostgreSQL.

## 1. Guide Assumptions

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

## 2. Installation

Install Simple using cabal:

```bash
$ cabal install simple
```

