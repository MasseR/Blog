---
title: Coding environment
tags: general, programming, vim, haskell
---

A good coding environment is paramount to productivity and to keep your nerves
intact. If your environment fights you back for for every thing, your doomed to
either do bad job or stop entirely.

I am a Vim user. I will not go into details of what Vim is, you either know
about it or not, but the simple description that it is an old, actively
developed, highly configurable programming editor/ide. I don't like eclipse or
similar, but there was a thing in it, that made me more productive; key
shortcuts to compile and run the program.

For a long time my Vim experience was to code with Vim, drop to shell with
`^Z`, compile and run. This later turned to compiling within Vim with `:make`.
This in turn evolved into a shortcut bound to `<F5>`. Sometimes I bound `<F6>`
to run the program, but it was usually done manually for every instance.

While studying Haskell, I become to appreciate the GHCi program. The writer of
[Learn You a Haskell](http://learnyouahaskell.com/) mentioned, that his
workflow was to code, and have GHCi open, and every now and then `:load` the
file into the interpreter. I too did this for a while, but this required me to
have at least two windows open when developing, and it wasn't entirely pleasing
with a small netbook screen and a tiling window manager.

To solve this, I bound `<F5>` to compile the active file, and `<F6>` to open
the active file in the GHCi interpreter. This was such a success that I made
similar bindings to Python ([iPython](http://ipython.scipy.org/moin/)), scala,
java (jdb) and PHP. The experience with the latter two wasn't too good.
Debugger is never as good as an interpreter, but jdb is even a bad debugger in
my opinion. I don't need much to be pleased, just a command history and
something similar to readline. The PHP interpreter, well, I don't have much
experience with it, but it seems to have some weird quirks.

These bindings for GHCi was sufficient for a while, but it required me to be
physically in the source root directory. For example if the hierarchy was
something like `project/src/{sub1,sub2,sub3}`, I had to be in the src
directory. This dilemma made me learn a bit of VimL, the Vim scripting
language. I made a script that build a string from the relative file path, to
the directory I was in. For example, if I was editing `project/src/foo/bar.hs`,
it would create a string ".:src:src/foo", which some of you might notice is the
format for import directories `-i`. So no matter where I was, the source root
would be included.

Time goes by and my haskell package cache gets corrupted over and over again.
This effect was later on dubbed the cabal butterfly effect, and the solution
was to use sandboxed builds with
[cabal-dev](http://hackage.haskell.org/package/cabal-dev). I tried the tool,
but this would break my workflow, since the de-facto way to use the tool is to
let the tool do everything for you. I would need to drop back to shell, and use
the tool to compile or to open the interpreter. This was a no-go and I never
really adopted `cabal-dev` and just kept on suffering the broken cache.

The change would come later with a school assignment. The project required two
packages, that would have somewhat incompatible dependencies. If you installed
the two packages separately, you would be sure to have a corrupted package
cache. Installing the two packages together did work however, so I could
continue developing. Later I noticed that installing a third package, not
involved in the project, would still break the package cache because it had a
similar dependency to the project. This was the turning point, the point where
the cons of having a single package cache outweighed the trouble to get a
proper cabal-dev workflow.

Luckily the solution was simple. I created a simple cabal file for the
assignment, which basically just declared the project dependencies. I then ran
`cabal-dev install --dependencies-only`, which would pull the dependencies and
install them locally for the project. There is no way that the global package
cache would corrupt this local cache. But then I needed to solve workflow
problem. Cabal-dev creates a `cabal-dev` directory within the project root
which contains `cabal.config` file which in turn declares the local package
cache directory. All that is needed, is to find the cabal-dev directory, which
is similar to creating the include path string, read the config file, parse the
package cache directory, and append `-package-cache dir` to
`b:ghc_static_options` variable. The solution, which for now is just a quick
hack, and requires revisiting, is in my
[github](https://github.com/MasseR/dotvim) repository for all the world to see.

I can now again `<F5>` to compile and `<F6>` to examine the file in GHCi
interpreter, with the added benefit of having a sandboxed environment.
