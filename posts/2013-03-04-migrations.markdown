---
title: Two-way database migrations
---
**Note**: This is a direct copy from my scribbles.

## Two-way database migrations

I've been preaching about the importance of code with migrations together with
the fact the sequential migrations are unfeasible with a larger project. Mostly
people agree with these (at least with point 2.), but I haven't been able to
sell the dependency-chain idea. I propose here an idea on how to manage
migrations. It is based on clojure which I'm not too familiar with, nor have I
spent too much time on thinking this through.

The idea is based on two key points:

1. migrations are held in separate clojure files (for example in `migrations/`)
2. migrations are declared as clojure source code with a domain specific language

### Separate files

The migrations are held in separate files. This brings me to the choice of
clojure. Clojure is a functional language with easy(ish) syntax, is dynamic and
it's easy to load dynamically^[Compared to Haskell for example]. A programming
language is chosen because we want to allow custom logig in there^[Not verified
this is feasible with a dsl].

### Domain specific language

I chose domain specific language, instead of sql, as the means of
implementation because it gives us greater flexibility with less code. The
domain specific language outputs an abstract syntax tree, describing the
migration operation. But because it is a tree, we can also modify it. We can
have every operation to have an inverse^[Not verified this claim], meaning that
we can have two-way migrations.

Another benefit for domain specific languages is that the same code can be used
with multiple databases. It all depends on the printing library for the
database in question.

### Examples

The following examples are pseudo-code, trying to resemble clojure.

    (migrate
      (:depends [])
      (create "foo"
              [:column 'id :int :primary :autoincrement
               :column 'username :string :unique
               :column 'hash :string
              ])))
