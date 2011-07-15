---
title: Example on using HaskellDB
tags: database, haskell
---

### HaskellDB

A while ago we had a database course in the university. We decided to do it
with Haskell, and found out about
[HaskellDB](http://hackage.haskell.org/package/haskelldb), which is a
high-level database wrapper based on relational algebra. Therefore we felt it
as a best choice for showing how functional languages can handle database
queries. I had beforehand done some testing with HDBC, which is a database
wrapper influenced by Perl, and felt that using long query strings within
Haskell was, if nothing else, ugly.

We felt that HaskellDB would provide us a nice declarative interface and type
safety. Little did we know about the journey it would take us into, and the
perils that waited us.

The first hurdle we faced, was that HaskellDB was badly documented. I felt that
I needed some high-level overview of the library before I could dive in into
the API documentation, but sadly there was practically no documentation like
that. Two papers from the original creator was found from
[here](http://haskelldb.sourceforge.net/haskelldb.pdf) and
[here](http://www.cse.chalmers.se/alumni/bringert/publ/haskelldb/haskelldb-db-2005.pdf).
First document was from 2004 and the second from 2005, but the library hasn't
been updated that much since, and the papers were still valid. They
complemented each other, but even then they both focused on queries, even
though HaskellDB required the layout to be defined. I found no examples of how
to define the DBLayout, until one lucky search which found a `Setup.hs` example
from [hpaste](http://hpaste.org). Unfortunately, I no longer can find the
paste, if it even exists.

In this post, I will write an example (two actually) on how to set up HaskellDB
so that you can run your queries. I am by no means an expert in HaskellDB, but
I felt that I should document the journey I went through, so that the people
after me might find it easier.

### Creating the layout

I have created a github repository for the example. The repository can be found
[here](https://github.com/MasseR/haskelldb-example). Please download it, or follow through
github. I recommend downloading/cloning however since, then you can try it out
yourself.

You may notice that the repository has two branches. This is because I feel
that the default way to describe the database layout in HaskellDB is too
complex and template haskell makes it simpler. The `haskelldb-th` requirement
is for the `th` branch of the repository.

I didn't feel like creating a cabal package for the examples, for reasons I
will explain later. Therefore you need to install the following packages manually.

- [HDBC](http://hackage.haskell.org/package/HDBC)
- [HDBC-sqlite3](http://hackage.haskell.org/package/HDBC-sqlite3)
- [haskelldb](http://hackage.haskell.org/package/haskelldb)
- [haskelldb-hdbc-sqlite3](http://hackage.haskell.org/package/haskelldb-hdbc-sqlite3)
- Optionally [haskelldb-th](http://hackage.haskell.org/package/haskelldb-th)

Let's get started then shall we? We are going to create a database for a simple
blog. The database consists of two posts and comments table.

![ER-model](/static/img/er.png)

HaskellDB requires a layout module that describes the table and the columns in
it. This is how you do it:

~~~{.sourceCode .haskell}
{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack44 #-}
module Blog.Comments where

import Database.HaskellDB.DBLayout


type Comments =
    (RecCons Id (Expr Int)
     (RecCons Email (Expr String)
      (RecCons Comment (Expr String)
       (RecCons Post (Expr Int) RecNil))))

comments :: Table Comments
comments = baseTable "comments" $
           hdbMakeEntry Id #
           hdbMakeEntry Email #
           hdbMakeEntry Comment #
           hdbMakeEntry Post


data Id = Id

instance FieldTag Id where fieldName _ = "id"

xid :: Attr Id Int
xid = mkAttr Id


data Email = Email

instance FieldTag Email where fieldName _ = "email"

email :: Attr Email String
email = mkAttr Email


data Comment = Comment

instance FieldTag Comment where fieldName _ = "comment"

comment :: Attr Comment String
comment = mkAttr Comment


data Post = Post

instance FieldTag Post where fieldName _ = "post"

post :: Attr Post Int
post = mkAttr Post
~~~

Holy.. You can't be serious, right? Not entirely no. This is the layout file
that is generated for you. I won't delve too deep into it, but there are some
important notes to take, especially when we head to the template haskell
version, when we can't see the source for the layout.

The type alias `Comments` describes the record type for the table. You can see
that the type is like a list. Columns are consed after each other, and ended
with a `Nil`. The point I'm trying to make is that `Comments` is described as
an id followed by email, followed by comment, followed by post. This means that
when doing inserts, you need to insert all of these, and in the same order.

There are two ways of generating the layout information. The first one doesn't
require any extra packages, and is done by `DBDirect`. `DBDirect` takes a
`DBInfo` datatype and produces a module from the information provided.

A top-level module is created for the database, and beneath it are modules for
each table, ones that look like the previous example.

~~~{.sourceCode .haskell}
dbdescr = DBInfo "Blog" (DBOptions False mkIdentPreserving) [
    TInfo "posts" [
        CInfo "id" (IntT, False) -- numeric and not null
      , CInfo "author" (StringT, False)
      , CInfo "title" (StringT, False)
      , CInfo "content" (StringT, False)
    ]
  , TInfo "comments" [
        CInfo "id" (IntT, False) -- numeric and not null
      , CInfo "email" (StringT, False)
      , CInfo "comment" (StringT, False)
      , CInfo "post" (IntT, False) -- The same type as post id
    ]
  ]
~~~

    Blog.hs
    Blog/
       |/Posts.hs
       |/Comments.hs

I want to make a point of this. `DBDirect` creates new modules for you at
runtime. You can create the `DBInfo` information directly from a database
schema or create database schema from `DBInfo`. I don't recommend creating the
schema from `DBInfo`, because as you can see, the description language is
extremely limited. Of you can do like I just did, create `DBInfo` manually.


There is however one point to make. DBInfo must exist, and must have been
compiled to layout modules _before_ the main module is compiled. To advance,
just run the `createLayout.hs` file and see how the modules are created. You
can safely remove the toplevel `Blog.hs` file. I have no idea what it's
supposed to do, it just seems to mirror the `DBInfo` we created earlier, and
doesn't even compile.

This requirement complicates the build procedure. In our assignment we created
a pre-build rule to create the modules and after that compile the main program.
This was the best I could do with my knowledge, and even then it was a
two-stage compilation for no good reason, and this is why I looked into the
template haskell solution.
[This](https://gitorious.org/koulu/tietokannatii/commit/8733d6bd1c4bf595676c4289c2e4b0ffc4a84eb1)
is a commit in our assignment, in which I created the two-stage compilation.

You can also check the template haskell branch. See how I have added the layout
modules to version control, and how simple they look? Best of all, they are
handled compile-time, so that our two-stage compilation is turned into
single-stage compilation.

~~~{.sourceCode .haskell}
{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fcontext-stack44 #-}
module Blog.Comments where

import Database.HaskellDB.DBLayout
import Database.HaskellDB.CodeGen
mkDBDirectTable "Comments" [
    ("id", [t|Int|])
  , ("email", [t|String|])
  , ("comment", [t|String|])
  , ("post", [t|Int|])
  ]
~~~

### A couple of queries

How does the layout tie in to queries. Also check the two documents I linked
earlier, they will give you the theory behind these queries, much better than I
can. Let's create a query that just saves a post. First of all, we need to
import the posts module, and while we can do it unqualified now, I recommend
importing it qualified, as most likely you will end up having multiple tables
with same column names.

~~~{.sourceCode .haskell}
import qualified Blog.Posts as P
~~~

Then to the query. `insert` takes the database connection, table and record.
Remember when I mentioned records earlier? They all need to be present and in
the same order. Failure to do so, will give you an error you do not want.

~~~{.sourceCode .haskell}
savePost ::  String -> String -> String -> Database -> IO ()
savePost author title content db = insert db P.posts $
      P.xid << _default
    # P.author <<- author
    # P.title <<- title
    # P.content <<- content
~~~

If you check the generated Posts module, you will see that the `id` is named as
`xid`, for not overriding the normal `id` function. You might also notice that
for the id we used `(<<)` and `(<<-)` for all the rest. If you check their
[type
signatures](http://hackage.haskell.org/packages/archive/haskelldb/2.1.1/doc/html/Database-HaskellDB.html),
you will notice that the other one takes an expression and the other one takes
a value. The `(<<-)` is a short way to write `f << constant a`.

I mentioned errors that you do not want. Let's see an example for such. Modify
the `savePost` function so that some of the fields are in wrong order.

~~~{.sourceCode .haskell}
-- WRONG
savePost ::  String -> String -> String -> Database -> IO ()
savePost author title content db = insert db P.posts $
      P.xid << _default
    # P.title <<- title
    # P.author <<- author
    # P.content <<- content
~~~

The error you would get for it, is something horrible:

~~~
Main.hs:31:36:
    Couldn't match type `P.Author' with `P.Title'
    When using functional dependencies to combine
      Database.HaskellDB.Query.InsertRec
        (Database.HaskellDB.HDBRec.RecCons f (e a) r)
        (Database.HaskellDB.HDBRec.RecCons f (Expr a) er),
        arising from the dependency `r -> er'
        in the instance declaration at <no location info>
      Database.HaskellDB.Query.InsertRec
        (Database.HaskellDB.HDBRec.RecCons
           P.Author
           (Expr String)
           (Database.HaskellDB.HDBRec.RecCons
              P.Content (Expr String) Database.HaskellDB.HDBRec.RecNil))
        (Database.HaskellDB.HDBRec.RecCons
           P.Title
           (Expr String)
           (Database.HaskellDB.HDBRec.RecCons
              P.Content (Expr String) Database.HaskellDB.HDBRec.RecNil)),
        arising from a use of `insert' at Main.hs:31:36-41
    In the expression: insert db P.posts
    In the expression:
        insert db P.posts
      $     P.xid << _default
        #     P.title <<- title
          #     P.author <<- author # P.content <<- content

Main.hs:31:36:
    Couldn't match type `P.Title' with `P.Author'
    When using functional dependencies to combine
      Database.HaskellDB.Query.InsertRec
        (Database.HaskellDB.HDBRec.RecCons f (e a) r)
        (Database.HaskellDB.HDBRec.RecCons f (Expr a) er),
        arising from the dependency `r -> er'
        in the instance declaration at <no location info>
      Database.HaskellDB.Query.InsertRec
        (Database.HaskellDB.HDBRec.RecCons
           P.Title
           (Expr String)
           (Database.HaskellDB.HDBRec.RecCons
              P.Author
              (Expr String)
              (Database.HaskellDB.HDBRec.RecCons
                 P.Content (Expr String) Database.HaskellDB.HDBRec.RecNil)))
        (Database.HaskellDB.HDBRec.RecCons
           P.Author
           (Expr String)
           (Database.HaskellDB.HDBRec.RecCons
              P.Title
              (Expr String)
              (Database.HaskellDB.HDBRec.RecCons
                 P.Content (Expr String) Database.HaskellDB.HDBRec.RecNil))),
        arising from a use of `insert' at Main.hs:31:36-41
    In the expression: insert db P.posts
    In the expression:
        insert db P.posts
      $     P.xid << _default
        #     P.title <<- title
          #     P.author <<- author # P.content <<- content
~~~

How about a query that selects something from database. Query that gets top n
posts and formats it nicely. When doing the assignment, we noticed that this
was a recurring pattern; make a query, map over it and select fields from the
record. If you don't map over the result after the query, the return type will
be a list of records.

~~~{.sourceCode .haskell}
fquery f = fmap (map f)

getTopNPosts ::  Int -> Database -> IO [String]
getTopNPosts n db = fquery (\r -> r!P.title) $
  query db $ do
    posts <- table P.posts
    top n
    project $ copyAll posts
~~~

