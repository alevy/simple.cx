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

This will create a new subdirectory called "blog" containing our app. The
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
`smpl` utility helps run applications in development mode. First,
install the package `wai-handler-devel`:

```bash
$ cabal install wai-handler-devel
```

Now we can run the Simple development server. From our application directory,
run:

```bash
$ smpl server
```

This will fire up `wai-handler-devel` on port _3000_, and set the environment
variable "ENV" to "development". To see the application in action, open a
browser and navigate to [http://localhost:3000/](http://localhost:3000). You
should see the default generated home page:

![](images/screenshot-hello.png "\"Hello World\" Screenshot")

<aside>

You can of course compile and run applications without the `smpl` utility
(in fact, this is probably how you will deploy apps, as wai-handler-devel
is not suitable for production).

```bash
$ cabal run
```

This will compile and run `Main.hs`, which by default runs the application
using warp on port _3000_. You can specify a different port by setting the "PORT"
environment variable, for example, to run the application on port _8080_:

```bash
$ PORT=8080 cabal run
```
</aside>

### Adding a Controller

The default generated application isn't very interesting, displaying only a
boilerplate homepage. Since we don't have any content for our
blog yet, let's start by displaying a form for creating posts. Create a new
module `Blog.Controllers.Posts` in `Blog/Controllers/Posts.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.Posts where

import Blog.Common
import Web.Frank
import Web.Simple

postsController :: Controller AppSettings ()
postsController = do
  get "new" $ do
    respond $ okHtml "Placeholder for new posts form"
```

The above code responds to a GET request for the sub-path `/new/` with the
plaintext "Placeholder for new posts form". `okHtml` simply wraps this text in
an HTTP response with status code _200 OK_ and content type "text/html".

We imported `Blog.Common` to get access to `AppSettings`. `Web.Frank` gives us a
[Sinatra](http://sinatrarb.com)-like DSL for routing. Finally `Web.Simple`
exposes `Controller`, `respond` and `okHtml`

<aside>

`Controller` is the main type used in _Simple_. It contains the request
(accessible through the monadic `request` function), as well as an
application-specific value (in our case, of type `AppSettings`). A `Controller`
falls-through unless it explicitly `respond`s to a request.

</aside>

We must also modify `Application.hs` to actually route to our new controller:

```haskell
...other imports...
import Blog.Controllers.Posts

app :: (Application -> IO ()) -> IO ()
app runner = do
  settings <- newSettings

  runner $ controllerApp settings $ do
    routeTop $ render "index.html" ()
    routeName "posts" postsController
```

We've simply imported our posts controller module and added a line inside the
routing block that routes all requests witha path beginning with "/posts/" to
the `postsController`. Now, pointing your browser
[http://localhost:3000/posts/new](http://localhost:3000/posts/new) should
show our placeholder for the form.

Of course, we actually want to display a form, not just some random text. We
can add a form easily using `Web.Templates`. Create a new template in the file
`views/posts/new.html`:

```html
<h2>New Post</h2>
<form action="/posts/" method="POST">
  <p>
    <label for="title">Title</label>
    <input type="text" name="title" id="title"/>
  </p>
  <p>
    <label for="body">Body</label>
    <textarea name="body" id="body"></textarea>
  </p>
  <p><input type="submit" value="Create"/></p>
</form>
```
Next, we'll modify the posts controller to render this template as opposed to
the placeholder text. In `Blog/Controllers/Posts.hs`, add the import
`Web.Simple.Templates` and change the line that beings with `respond` to
`render "posts/index.html" ()`, to get:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.Posts where

import Blog.Common
import Web.Frank
import Web.Simple
import Web.Simple.Templates

postsController :: Controller AppSettings ()
postsController = do
  get "new" $ do
    render "posts/new.html" ()
```

`render` finds the specified template under the _views_ directory, renders the
template (passing in the third parameter as an argument) and calls `respond`
with the result. Now, navigating to the same page
[http://localhost:3000/posts/new](http://localhost:3000/posts/new) should
display:

![](images/screenshot-new-post.png "New Post Screenshot")

<aside>

Notice that we didn't specify any of the page style, main heading or any
boilerplate HTML for that matter, yet the generate page still displays it.
That's because `Web.Simple.Templates` renders all templates within
the layout in `layouts/main.html`. This behavior can be customized
for a particular page by calling `renderLayout` with a specific layout or
`renderPlain` to render a template with no layout. It can also be changed
globally by modifying the instance of `HasTemplates` in `Blog.Common`.

</aside>
