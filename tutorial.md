---
title: Getting Started Tutorial
showtoc: true
---

This getting started tutorial will get you up and running with a basic web
application. At the end you will know

1. how install _Simple_

2. how to create a _Simple_ application from scratch

3. the structure of a _Simple_ application

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

## Creating a new _Simple_ app

### Installing _Simple_

Open up a terminal. Commands prefaced with a dollar sign ($) should be run in
terminal. Use cabal to instal _Simple_:

```bash
$ cabal install simple
```

To verify that _Simple_ installed properly run the following command:

```bash
$ smpl --help
```

which should print out the subcommands and options available for the `smpl`
utility that comes with _Simple_.

### Creating the Blog application

The `smpl` utility helps you create a new, blank _Simple_ application from the
command-line. To begin creating our application, open a terminal, navigate to a
folder where you would like to create the project (for example `cd ~/hack`) and
create a new _Simple_ project called "blog":

```bash
$ smpl create --templates blog
```

This will create a new subdirectory called "blog" containing our app. The
"--templates" flag tells `smpl` to include boilerplate code for templates.
We could specify "--all" to get PostgreSQL ORM and cookie-based session
support, but we won't be using that for this tutorial. The directory contains a
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

## Starting the server

Our application is ready for us to get to work. Now we'll get a server up and
running and start adding functionality to our application.

_Simple_ apps are built on top of the WAI package, and can be run with any WAI
compatible server, like [warp](http://hackage.haskell.org/package/warp). The
`smpl` utility helps run applications in development mode. First,
install the package `wai-handler-devel`:

```bash
$ cabal install wai-handler-devel
```

Now we can run the _Simple_ development server. From our application directory,
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

## Displaying Content

The default generated application isn't very interesting, displaying only a
boilerplate homepage. We'll start by adding some content. For simplicity we can
store blog posts in the filesystem. Let's create some dummy data:

```
$ mkdir data
$ cat > data/00001
The Title of Your First Post on a Single Line
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Etiam vitae interdum sapien. In congue... 

$ cat > data/00002
The Title of Your Second Post
Aliquam tempor varius justo vitae bibendum! Duis vitae rutrum
neque. Sed ut sed...
```

### List posts

We'll use the filename to order posts, the first line of the file for post
title and the rest for the post body. Now that we have some data to play with,
let's list and display our posts. Modify "Application.hs":

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Application where

import Control.Monad
import Control.Monad.IO.Class
import Blog.Common
import Data.Aeson
import Data.List
import Network.HTTP.Types
import System.Directory
import System.FilePath
import System.IO
import Web.Simple
import Web.Simple.Templates

app :: (Application -> IO ()) -> IO ()
app runner = do
  settings <- newAppSettings
  runner $ controllerApp settings $ do
    routeMethod GET $ do
      -- Respond to the root route
      routeTop $ do
        posts <- liftIO $ do
          dataDir <- getDirectoryContents "data"
          let postFiles = sort $
                filter (not . isPrefixOf ".") dataDir
          forM postFiles $ \postFile -> do
            withFile ("data" </> postFile) ReadMode $ \h -> do
              title <- hGetLine postHandle
              return $ object ["id" .= postFile
                              , "title" .= title]
        render "index.html" $ object ["posts" .= posts]
```

<aside>

`Controller` is the main type used in _Simple_. It contains the request
(accessible through the monadic `request` function), as well as an
application-specific value (in our case, of type `AppSettings`). A `Controller`
falls-through unless it explicitly `respond`s to a request.

</aside>

The above code responds to a GET request for the root route ("/"), rendering
the template "views/index.html" and passing it an
[aeson](http://hackage.haskell.org/packages/aeson) value containing a list of
post objects. Each post contains an _id_ and _title_. `routeTop` ensures that
the route is only invoked if there is no path remaining to consume. The rest of
the code simply reads the first line (the title) of each file in the "data"
directory.

If you are using cabal to build your application, you also need to add the
packages `aeson`, `directory`, `filepath`, `http-types` and `transformers` as
dependencies (they should already be installed since _Simple_ depends on them).

<aside>

Most of the imports we added were for reading the posts from the filesystem
(`System.Directory`, `System.FilePath` and System.IO) or for basic Monad
manipulations (`Control.Monad` and `Control.Monad.IO.Class`). `Data.Aeson` is
needed to construct a value to pass to the template. `Data.List` is used to
manipulate the list of files returned from the "data" directory. Finally,
`Network.HTTP.Types` imports `GET` (as well as `POST`, `PUT` etc) used in
`routeMethod`.

</aside>

Let's now modify "views/index.html" to make use of the posts:

```html
$if(null(posts))$
No posts.
$else$
<ul>
$for(post in posts)$
<li><a href="/$post.id$">$post.title$</a></li>
$endfor$
</ul>
$endif$
```

_Simple_ templates are embedded templates -- meaning they are embedded inside
HTML, or whichever relevant output format. Template directives (like control
statments and variable expansions) are surrounded by dollar signs ($). Our
template lists the post titles and links to the post itself at "/:post.id".

![](images/screenshot-index.png "Posts index Screenshot")

<aside>

Notice that we didn't specify any of the page style, main heading or any
boilerplate HTML for that matter, yet the generate page still displays it.
That's because `Web.Simple.Templates` renders all templates within
the layout in `layouts/main.html`. This behavior can be customized
for a particular page by calling `renderLayout` with a specific layout or
`renderPlain` to render a template with no layout. It can also be changed
globally by modifying the instance of `HasTemplates` in `Blog.Common`.

</aside>

### Show post

If we click on any of the links now, we'll get a 404 (not found) page. That's
because we still need to add the route for displaying individual posts:

```haskell
-- Repond to "/:post_id"
routeVar "post_id" $ routeTop $ do
  postId <- queryParam' "post_id"
  let postFile = "data" </> (takeFileName postId)
  post <- liftIO $ do
    h <- openFile postFile ReadMode
    title <- hGetLine h
    body <- hGetContents h
    return $ object ["title" .= title, "body" .= body]
  render "show.html" post
```

<aside>

`routeVar` consumes the next directory in the request path, but instead of
matching a particular string (like `routeName`), it sticks the directory name
in the given query parameters. So, for example, `routeVar "post_id"` will match
paths will at least one directory left, and make the name of the directory
available in the `post_id` query parameter from within the controller.

</aside>

and add a view template in "views/show.html":

```html
<h2>$title$</h2>
<p>
$body$
</p>
```

Now, if we click on a link from the main page, our app will display the post
body:

![](images/screenshot-post.png "Show Post Screenshot")

We nearly have a complete (albiet minimal) blog application. We're just missing
a way to generate the content in the first place...

## Creating content

### New post form

Our first step towards being able to author new posts is to display an HTML
form. This is fairly straight forward since it involves no dynamic content.
We'll add the route "/new" which will simply render the form:

```haskell
-- Render new post form
routeName "new" $ routeTop $ do
  render "new.html" ()
```

It's imperitive that this route appears before the route for displaying posts.
That's because routes are evaluated in order, and `routeVar "post_id"` would
match "/new", which we don't want.

Finally, we need to add a template in "views/new.html":

```html
<form action="/" method="POST">
  <p>
    <label for="title">Title</label>
    <input type="text" name="title" id="title"/>
  </p>
  <p>
    <textarea name="body"></textarea>
  </p>
  <p><input type="submit" value="Create"/>
</form>
```
![](images/screenshot-new.png "New Post Screenshot")

### Parsing the form

Submitting the form above will perform a "POST" request to the root path with
a URL-encoded body containing the contents of the form. In order to store the
new post, we need to parse the form and ensure that the data is valid (i.e. the
title and body fields are non-empty).

The monadic `parseFrom` function parses a form into a list of parameters (each
a pair of strict `ByteString`s for the key and value) and a list of `FileInfo`s
(`FileInfo` represents an uploaded file, but we won't go into that now as it's
not relevant for our example).

`parseForm` lets us save new posts relatively easily:

```haskell
...
import qualified Data.ByteString.Char8 as S8
...

-- Create form
routeMethod POST $ routeTop $ do
  (params, _) <- parseForm
  let notNull = not . S8.null
  let mpost = do
        title <- notNull `mfilter` lookup "title" params
        body <- notNull `mfilter` lookup "body" params
        return (title, body)
  case mpost of
    Nothing -> redirectBack
    Just (title, body) -> liftIO $ do
      files <- filter (\(c:_) -> c /= '.') `fmap`
        getDirectoryContents "data"
      let lastFileNum = show $ length files + 1
      let fileName =
            take (5 - length lastFileNum)
              [z | _ <- [0..], let z = '0'] ++
            lastFileNum
      withFile ("data" </> fileName) WriteMode $ \h ->
        S8.hPutStrLn h title
        S8.hPutStr h body
  respond $ redirectTo "/"
```

Once we've extracted the parameters from the request body, we lookup the "title"
and "body" fields (note that these just correspond to the "name" attribute we
gave the inputs in our HTML form) and ensure they are not empty (whith
`notNull` and `mfilter). If this fails (i.e. if "title" or "body" are either
not present or empty), we redirect to the referer (the new post form). In a
real application, we'd probabaly want to give the user some hint as to what
went wrong. If the form is complete, we store the post and redirect to the post
listings.

<aside>
Note that we've imported `Data.ByteString.Char8`. To build with cabal, you'll
need to add the `bytestring` package as a dependency in `blog.cabal`.
</aside>

We're basically done! Our blog app, while very simple, is totally functional!

## Bonus! Routing DSLs

The `route*` combinators are very expressive and are, therefore, great for
customizing exactly how to route a request. However, in the common case, where
an application follows a simple pattern, they can get a bit cumbersome to use.
_Simple_ ships with two DSLs on top of the `route*` combinators that make
common routing tasks easy. Let's use one of these DSLs, "Frank", to rewrite out
blog application more concisely.

"Frank" exposes an interface based on the [Sinatra](http://sinatrarb.com)
framework for Ruby. For example, the route:

```haskell
get "/:post_id" $ do
  ...
```

will match GET requests which have exactly one uncomsumed directory in the path
and use its contents for the "post_id" query parameters. The route is
equivilant to (and in fact implemented as):

```haskell
routeMethod GET $ routePattern "/:post_id" $ routeTop
```

There are similar methods for `post`, `put` and `delete`.

Once we import `Web.Frank`, we can rewrite our application much more cleanly
using this interface. The full listing is:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Application where

import Control.Monad
import Control.Monad.IO.Class
import Blog.Common
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.List
import Network.HTTP.Types
import System.Directory
import System.FilePath
import System.IO
import Web.Simple
import Web.Simple.Templates

app :: (Application -> IO ()) -> IO ()
app runner = do
  settings <- newAppSettings

  runner $ controllerApp settings $ do
    get "/" $ do
      posts <- liftIO $ do
        dataDir <- getDirectoryContents "data"
        let postFiles = sort $
              filter (not . isPrefixOf ".") dataDir
        forM postFiles $ \postFile -> do
          withFile ("data" </> postFile) ReadMode $ \h -> do
            title <- hGetLine h
            return $ object ["id" .= postFile
                            , "title" .= title]
      render "index.html" $ object ["posts" .= posts]

    -- Respond to "/new"
    get "/new" $ do
      render "new.html" ()

    -- Repond to "/:post_id"
    get "/:post_id" $ routeTop $ do
      postId <- queryParam' "post_id"
      let postFile = "data" </> (takeFileName postId)
      post <- liftIO $ do
        h <- openFile postFile ReadMode
        title <- hGetLine h
        body <- hGetContents h
        return $ object ["title" .= title, "body" .= body]
      render "show.html" post

    -- Create form
    post "/" $ do
      (params, _) <- parseForm
      let notNull = not . S8.null
      let mpost = do
            title <- notNull `mfilter` lookup "title" params
            body <- notNull `mfilter` lookup "body" params
            return (title, body)
      case mpost of
        Nothing -> redirectBack
        Just (title, body) -> liftIO $ do
          files <- filter (\(c:_) -> c /= '.') `fmap`
            getDirectoryContents "data"
          let lastFileNum = show $ length files + 1
          let fileName =
                take (5 - length lastFileNum)
                  [z | _ <- [0..], let z = '0'] ++
                lastFileNum
          withFile ("data" </> fileName) WriteMode $ \h -> do
            S8.hPutStrLn h title
            S8.hPutStr h body
      respond $ redirectTo "/"
```

## Next Steps

  * [Building Models with PostgreSQL](postgresql-orm.html)
  * [_Simple_ template system in depth](templates.html)
  * [Cookie-based session management](sessions.html)
  * [In depth reference](api.html)

