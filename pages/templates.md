---
title: Simple Templates
showtoc: true
---

_Simple_ comes with support for the embedded templating language defined in the
package `simple-tempalates`. Templates let you embed dynamic content in HTML,
JSON or any other text format you return in your responses.

## Using with a _Simple_ app

Adding template support in your app is as easy as declaring an instance of
`HasTemplates` for your app settings:

```haskell
import Web.Simple.Templates

data MyAppSettings = ...

instance HasTemplates MyAppSettings
```

`HasTemplate` has default definitions for all of its methods. However, in most
cases you'll probably want to override at least:

_TODO_

## Language Description

A template may contain plain-text, which is reproduced as is, as well as
blocks of code, denoted by encapsulating them with dollar-signs (\$),
supporting variable expansion, function invokation, conditionals and loops. For
example, given a global variable "answer" with the value *42*,

     The answer to the universe is $answer$.

would expand to

     The answer to the universe is 42.

Since the dollar-sign is used to denote code sections, it must be
escaped in plaintext sections by typing two dollar-signs. For example,
to reproduce the lyrics for *Bonzo Goes to Bitburg*, by The Ramones:

     Shouldn't wish you happiness,
     wish her the very best.
     $$50,000 dress
     Shaking hands with your highness

### Primitive Types

_Booleans_, _Numbers_, _Strings_, _Arrays_ and _Null_ values can be typed as
literals:

* __Booleans__ are the lower-case `true` and `false`
* __Numbers__ are rationals, parsed according to the rules in attoparsec's
[rational](http://hackage.haskell.org/package/attoparsec-0.10.4.0/docs/Data-Attoparsec-Text.html#v:rational)
parser (roughly, decimal numbers with an optional decimal point and optional
exponent)


>     Pi is approximately $3.14159$


* **String** literalss are surrounded by double-quotes (\"). Double-quotes inside
  a string can be escaped by proceeding it with a backslash (\\\"),
  however backslashes themselves do not need to be escaped:

>     And then, Dr. Evil said:
>     $"Mini Me, stop humping the \"laser\"."$

* **Arrays** are surrounded by square-brackets ([ ]) and elements are
    comma separated. Elements can be literals, variables or function
    invokations, and do not have to be the same type. Spaces bewteen elements
    are ignored:


>     $["Foo", 42, ["bar", "baz"], length([1, 2, 3, 6])]$

* **Null** is typed as the literal *null* (in lower case):

>     $null$

* **Objects** map from String keys to values of any type. Objects cannot be
  typed literally.

### Variable substitution

Templates are evaluated with a single global variable called `@`. For
example, you can refernce the global in your template like so:

     The value in my global is $@$.

If the global is an _Object_, it can be indexed using dot-notation:

     The Sex Pistols' bassist was $@.bassist.name.first$

In this case, you may also discard the `@` global reference and simply
name the field in the global object, for example:

     Field 'foo' is $foo$.
     Field 'bar.baz' is $bar.baz$.

Strings, Numbers and Booleanss are meaningful when evaluated to text
in a template. Objects and Arrays render as a string representing their types
(e.g. "[object]"). Null renders the empty string. However, values of all types
can be used as arguments to functions, or in conditionals and loops.

### Function Invokation

Functions are invoked with similar syntax to imperative languages:

     $myfunc(arg1, arg2, arg3)$

where arguments can be literals, variables or other function calls --
basically anything that can be evaluated can be an argument to a
function. Function names are in a separate namespace than variables, so
there can be a function and variable both named *foo* and they are
differentiated by their use. For example:

     $mysymbol$

is a variable expansion, whereas

     $mysymbol()$

is a function invokation.

### Conditionals

Branching is supported through the common *if* statement with an
optional *else* branch. Conditions can be any expression. *false* and
*null* are evaluated as *false*, while everything else is evaluated as
*true*.

*if* blocks are surround by an *if*-statement and and *endif*, each
surrounded separately by dollar signs. Optionally, the *else* branch is
declared by with "\$else\$". The blocks themselves are templates and may
contain regular text as well as evaluable expressions.

     Should I stay or should I go?
     $if(go)$
     Trouble will be $trouble$.
     $else$
     Trouble will be $double(trouble)$
     $endif$

### Loops

For loops iterate over collections, setting a variable name to one
element in the collection for each iteration of the loop. Collections
are usually `Array`s, however non-false expressions (e.g., `String`s and
`Number`s) are treated as collections with one element. A loop starts
with a *for*-statement surrounded by dollar-signs and end with an
"\$endfor\$":

     <h1>The Clash</h1>
     <ul>
     $for(member in band)$
     <li>$member.name$ played the $member.instrument$</li>
     $endfor$
     </ul>

There is also an optional "\$sep\$" (for *separator*) clause, which is
rendered *between* iterations. So if I have a collection with three
items, the *sep* clause will be rendered after the first and second, but
not third elements:

     <h1>Grocery list</h1>
     <p>
     $for(item in groceries)$
     $item.quantity$ $item.name$(s).
     $sep$
     <br/>
     $endfor$
     </p>

Will render something like:

     <h1>Grocery list</h1>
     <p>
     2 MC(s).
     <br/>
     1 DJ(s)
     <br/>
     </p>
