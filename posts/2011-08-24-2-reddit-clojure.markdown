---
title: Wallpapers in Clojure
tags: clojure,network,io,binary
---

# What are we going to do?

This is part two of a series of programming posts. In part
[one](/posts/2011-08-24-1-reddit-haskell.html) I wrote a program in Haskell
which downloads a random wallpaper from Reddit. See the first post to see the
motivation for this.

In this post I'm going to write a similar program in Clojure. A disclaimer is
in order. I haven't known Clojure for long, only for perhaps less than a month,
but the learning process seems to be going well. I still don't know why `a` is
better than `b` and if theres a function `c` that does `d e f g`, so keep that
in mind when reading this.

I'm also going to write a bit about the learning process I went through when
writing it. Also a good point to note is that unlike the Haskell version which
I wrote a couple of months ago, I wrote the Clojure version just hours ago, so
I'm still blind for the potential problems.

# Implementation

I was using [leiningen](https://github.com/technomancy/leiningen) for the
project dependency management. Not that there is a lot of dependencies, but the
program is easy enough to use even in such small projects. It also provides a
way to build a complete jar package that has all the dependencies included.

Haskell has GHCi which is a nice REPL which good autocompletion and readline
(haskeline) support. It also supports debugging and whatelse. The Clojure repl
is in its infancy, with no readline support, no debugging support, no
autocompletion support. There are programs that give readline support like
[rlwrap](https://github.com/andreyvit/rlwrap), but nothing that provides
autocompletion, which is definitely a minus. Also, since Clojure is a lisp
derivative, it has brackets, a lot of them. It can be a pain to keep them
balanced, which is of course no problem in a proper editor, but in a bare bones
REPL it's no fun.

Even still the REPL is useful, or rather there are a couple of funcions/macros
that make the REPL infinitely useful. Getting documentation out of a function
couldn't be easier than `(doc function-name)`. This is in theory doable with
GHCi, but even then it would require a browser. Haskell type signatures provide
a lot of the information I get from the `doc` function, so it's not that bad
with Haskell, especially with [duckduckgo](http://www.duckduckgo.com) bang
patterns, finding documentation is nearly a joy (this is worthy of a rant
post).

~~~
user=> (doc reduce)
-------------------------
clojure.core/reduce
([f coll] [f val coll])
  f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called.
~~~

Another quite nice function is the `source` function, which provides the
source code of the function. I haven't used it as much as I have used the
documentation, but during my year with Haskell, I have come upon situations
where knowing the source would have been helpful. If nothing else, the function
allows me to read standard library source code directly from the REPL.

~~~~
user=> (source reduce)
(def
    ^{:arglists '([f coll] [f val coll])
      :doc "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called."
      :added "1.0"}    
    reduce
     (fn r
       ([f coll]
             (let [s (seq coll)]
               (if s
                 (r f (first s) (next s))
                 (f))))
       ([f val coll]
          (let [s (seq coll)]
            (if s
              (if (chunked-seq? s)
                (recur f 
                       (.reduce (chunk-first s) f val)
                       (chunk-next s))
                (recur f (f val (first s)) (next s)))
              val)))))
nil
~~~~

If I'm sure what a function is called, or whether a function exists, I can use
[clojuredocs](http://clojuredocs.org) which is similar to hayoo or hoogle. It
did have a bug a while back where it couldn't find functions with hyphens, and
as the Clojure way is to create functions such as `with-open`, it was quite
problematic. Also [duckduckgo](http://www.duckduckgo.com) gives me bang patterns to
find Clojure documentation just like it does for Haskell.

As for the program source code, I'll go through it in the order it is written.
It might not make a good story telling, but it works.

The first think is the namespace declaration. Namespaces are like Java
packages. You can combine importing to the namespace declaration. The
`(:gen-class)` creates a named class, which is required for an executable
application. For more information about it, see [Clojure
documentation](http://clojure.org/compilation). The `(:require)` directive
tells Clojure to import those Clojure modules. They would be fully qualified,
unless for the `:as` directive, which gives a short-hand name for them.

~~~{.sourceCode .clojure}
(ns redditwallpaper.core
  (:gen-class)
  (:require [clojure.contrib.duck-streams :as duck]
            [clojure.java [io :as io] [shell :as shell]]
            [clojure.string :as string]
            [clojure.contrib.json :as json]))
~~~

I wasn't able to find a filename function for Clojure (not that I tried too
hard), so I created my own. The function splits a path by forward slash,
meaning that the function only works on unix. Then the function returns the
last item, which hopefully is the file name.

~~~{.sourceCode .clojure}
(defn filename [uri]
  (last (string/split uri #"/")))
~~~

The next function is for filtering nsfw content. Clojure keywords (prepended
with :) also behave as functions that take map as an argument. Also to note is
that Clojure has similar notation of falsy values as javascript. Everything but
false and nil are truthful values, therefore if the tag does not exist in the
json it returns nil and is therefore false. If the tag does exist, it should be
either true or false, meaning that false is false and true is true.

Another way to look at it is that `over_18` function is exactly the same as
`:over_18` but without the colon. Which begs to differ why I wrote that
function. I could've written it as a filter already at this point. I believe
this is a good example how writing blog posts help to improve your skills.

~~~{.sourceCode .clojure}
(defn over_18 [x]
  (:over_18 x))
~~~

The posts function filters the child posts from the json. The `get-in` function
takes  a 'path' through a deep associative map. For example `(get-in {:foo
{:bar 1}} [:foo :bar])` would return 1. After getting the child posts, we pluck
the real post data out of them by mapping the data keyword through each item.
See the first post to see how it was done in Haskell.

~~~{.sourceCode .clojure}
(defn posts [x]
  (map :data (get-in x [:data :children])))
~~~

The next function is for filtering real images from the list. Like I mentioned
in the previous post, I'm not in the mood for creating parsers for imgur etc.
so I filter just the urls that have real images. I do this with regular
expressions. In Clojure regular expressions are created with `#""` reader
macro. The `re-find` function returns the found match or nil if the regular
expression doesn't match. This is another example of the falsy values; if nil,
then false.

~~~{.sourceCode .clojure}
(defn plain-image [x]
  (re-find #"(.jpg|.png)$" (:url x)))
~~~

The url is in a variable unlike in the Haskell version, because I was testing
in the REPL and didn't want to keep writing the url over and over again.

~~~{.sourceCode .clojure}
(def url "http://www.reddit.com/r/wallpaper.json")
~~~

Next I save the parsed json into a variable. I'm not sure whether `slurp` is
lazy or not, but it doesn't really matter with this program. `slurp` reads the
content of the file/socket/url given to it. I believe it uses Clojure
[multimethods](http://clojure.org/multimethods) to know what kind of reader to
create. The `read-json` lives in the `json` namespace as declared in the
namespace macro. The function parses the string into a Clojure map with keys
transformed into keywords.

~~~{.sourceCode .clojure}
(def wallpaperjson (json/read-json (slurp url)))
~~~

I struggled with the next function for quite a bit. The images are binary
files, and I tried `io/copy` function with `reader` and what-else. For binary
IO we need streams, which the functions `input-stream` and `output-stream`
provide us. They too use multimethods to figure out what kind of stream to
instantiate.

Thanks to raek in #Clojure for pointing out that `io/copy` can handle binary
files when given streams. Like I said, I tried with readers but it didn't work
as they work with string data. Another kudos for him for mentioning that no,
`io/copy` does not close the handles, but `with-open` does. This fixed a bug
where not all of the image got downloaded. And one more kudos for him for
mentioning that `io/copy` does interleaved IO, which I couldn't get from the
documentation.

~~~{.sourceCode .clojure}
(defn copy [uri file]
  (with-open [in (io/input-stream uri)
              out (io/output-stream file)]
    (io/copy in out)))
~~~

I ranted a bit about the Haskell system module. I haven't got enough experience
how Clojure behaves, but the module only provides a single function `sh`.
Otherwise it behaves similarly to the Haskell version, taking a program name
and arguments. However, thanks to (reverse?) polish notation we don't need to
create a list specifically for this purpose, we just give the command arguments
as function arguments.

~~~{.sourceCode .clojure}
(defn set-background [file]
  (shell/sh "feh" "--bg-scale" file))
~~~

Then comes the main. I believe this is overriding the main method provided by
`(:gen-class)` earlier. I've been reading Stuart Halloways [Programming
Clojure](http://pragprog.com/book/shcloj/programming-clojure) 
without doing the exercices, so I might remember wrong, but I believe
he wrote that appending a hyphen is the way to override class methods with
`(gen-class)`

The weird `[& args]` syntax means that "all the rest of the function arguments
goes to a variable named args" which is a part of destructuring. Destructuring
is similar to pattern matching, but less powerful. The destructuring only
matches sequence items. For example `(let [[a b & rest] [1 2 3 4]])` would put
1 and 2 to `a` and `b` respectively. 3 and 4 would go to `rest`.

For easier reading, instead of doing a lot of function application, I use
intermediary values. I probably could chain or thread through the functions,
but I believe it would just make the part harder to read, especially for a
beginner such as myself. The random image is taken this time by taking the
first item of a shuffled list. I originally had a similar implementation as the
Haskell version, but somebody on IRC recommended the first shuffle version.

I believe that the `do` function is unnecessary in functions as they have
implicit do blocks, but it too increases readability in my opinion. At this
point I have all the information I already need, the random image, the image
url and the image filename. All that's left is to print some text, download the
file and set it as the background.

One oddity at the end of the block is the `shutdown-agents` function. I noticed
that when running the jar, the last line (at that time the "Done" line)
executed quite fast, but the program hang for a minute or so. Raek on IRC
suggested trying the `shutdown-agents` function, but also said that it's weird
as if the function helps, it shouldn't have originally completed at all. I also
asked about this from StackOverflow, but haven't got an answer at this time.

~~~{.sourceCode .clojure}
(defn -main [& args]
  (let [posts (posts wallpaperjson)
        safe (filter (complement over_18) posts)
        images (filter plain-image safe)
        image (:url (first (shuffle images)))
        fname (filename image)]
    (do
      (println (format "Downloading '%s'" image))
      (copy image fname)
      (println (format "Setting background image to '%s'" fname))
      (set-background fname)
      (println "Done")
      (shutdown-agents))))
~~~

# Thoughts

As Clojure is the language I'm trying to learn now, of course I'm going to say
that I love it. But I'll try to be more analytical.

The REPL is still in its infancy, but I'm able to live with just a readline
support. I'm hoping that one day we have a REPL as good as iPython or GHCi.

The fact that I can get documentation and source straight from the REPL is
invaluable for a command-line junkie as myself. I don't mind using browser to
find documentation, but I do mind when I just want to reference the function
documentation, which is most of the time. I usually remember the function name,
but not necessarily the function arguments or the order of arguments. I also
believe that having the source code available is going to be useful in the
future.

Clojure has a pretty printer library, which is not similar to the pretty
printer libraries in Haskell, but rather pretty prints Clojure data structures.
This was a great help when reading the json, because the raw json wasn't
formatted at all. I could get the json, parse it as Clojure map and pretty
print it without killing the terminal. Very useful especially I didn't remember
the json format by heart.

I also like the minimalistic syntax, and the power provided by dynamic
languages. The Haskell version was 82 lines long and the Clojure version only
36 lines. A lot of this was because as a dynamic language, Clojure was able to
parse the json as a Clojure map, and therefore I could use the map like any
other map, without any custom parsers.

Not all of it was roses though. A couple of times, I tried to call a java
function on a class that wasn't suitable. For example calling a function
provided by streams, with readers. This can only be catched at runtime, which
is really not nice.

Another con is from the JVM. It's great that Clojure has full access to full
Java economy and libraries, but the startup time is horrible. I'm a console
junkie and I can't imagine that the simplest of my tools would take more than
half a second to start, not to mention the two seconds that this program takes.
With this program it's sort of acceptable since there is also network involved,
but compared to Haskell, it's horrible. I did a quick and dirty benchmark and
compiled Haskell took less than a second to change the background. Clojure took
a little less than three seconds. Unless you count the full minute without
killing the agents.
