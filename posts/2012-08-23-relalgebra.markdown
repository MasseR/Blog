---
title: Relational algebra
tags: haskell, databases
---

I've been obsessing over [relational
algebra](https://en.wikipedia.org/wiki/Relational_algebra) for the past year or
more. Relational algebra is a mathematical language for defining query
operations. I first came to know it during our 'Database II' course in
university, and at the time I wondered why on earth should I spend time
studying this as clearly SQL is more relevant. We ended up doing our exercise
project with [HaskellDB](http://hackage.haskell.org/package/haskelldb) which is
an embedded domain specific language with roots in relational algebra, and I've
been in love hate relationship with relational algebra ever since. For
introduction to relational algebra read the wikipedia article. It took me some
while to grok it, but it's simple in heart.

Why not raw SQL?
----------------

So what's wrong with raw SQL? I'm not a great articulator so I'll quote on
Michael Snoyman from his [Yesod book](http://www.yesodweb.com/book).

    Unfortunately, using Haskell isn't enough. The web, by its very nature, is not
    type safe. Even the simplest case of distinguishing between an integer and
    string is impossible: all data on the web is transferred as raw bytes, evading
    our best efforts at type safety. Every app writer is left with the task of
    validating all input. I call this problem the boundary issue: as much as your
    application is type safe on the inside, every boundary with the outside world
    still needs to be sanitized.

Web programming is a 3-tier architecture. There is the frontend-tier,
logic-tier and database-tier. If you try to bring another tier to your current
tier, problems arise. Let's take frontend-tier as an example. Let's imagine
that you're echoing html/css/javascript as strings. Frontend-tier is a good
example, as it benefits the least from native tooling ^[I just found an article
about compiling mustache templates at
https://singpolyma.net/2012/08/compiling-mustache-templates/]. It's difficult
to combine different parts together so that they make sense. What we could
instead do is to separate the frontend-tier into a separate entity from
logic-tier (templating) or create a library that handles small-scale html
operations for us. Let's write some examples on these so that you can make your
own mind of it.

````php
$html = '';
$html .= '<div>';
$html .= '  <ul>';
foreach($items as $item)
    $html .= '      <li>' . escape($item) . '</li>';
$html .= '  </ul>';
$html .= '</div>';
````

------------

````html
<div>
    <ul>
        {{#items}}
            <li>{{#item}}</li>
        {{/items}}
    </ul>
</div>
````

------------

````haskell
let html = H.div $
                H.ul $
                    mconcat $ [H.li item | item <- items]
````

So what's this got to do with raw SQL? The same applies to SQL. SQL is a
powerful domain specific language, no doubt about it. But building SQL-queries
by string is just asking for trouble. We as programmers want the errors to show
up fast. If the errors are shown only after we are doing the actual SQL-query
it's already "too slow". What would be optimal is that the compiler would
complain about the query as soon as we save the file. Not many IDEs can handle
this, especially since strings can be quite complex. It's more difficult to
check `$sql = "select foo from " . $tablename . "where " . $where` than `$sql =
'select * from foo where id=2'`.

Why not ORM?
------------

Object relational mappers are an abstraction layer over SQL. Like the name
suggests they map between objects and relational data. They solve pretty much
all the problems with query building, but they come at a cost ^[Entirely my own
opinion]. The abstraction layer they provide is too high, you have little
control over the queries, and they limit your expressibility with some queries
^[Some kind of reference needed]. There is a speed benefit on writing SQL, but
there is a semantic benefit on ORMs. If your choice is SQL vs ORM, choose on
what you need, not what your personal opinion is about them, as both have their
benefits and cons.

How relational algebra fits the bill
------------------------------------

SQL is based on relational algebra ^[and/or relational calculus. Couldn't find
any reference quickly, so take it with a grain of salt], but it has a bit more
operations than relational algebra ^[As an example, aggregates aren't part of
relational algebra]. Some of SQL extras can be extended into relational
algebra, so we can assume that relational algebra and SQL are more or less
equivalent in their capabilities. The killer feature against ORMs is that
relational algebra queries map almost one-to-one into SQL queries. This means
that we have the high abstraction over queries, while still maintaining full
control over the queries ^[This is implementation specific. HaskellDB generates
suboptimal queries, but they have the benefit of being _correct_]. And the
obvious benefit over raw SQL is that it's not strings being concatenated.

Another great benefit of relational algebra is that is composable. Every
operation takes effect on a relation and returns a relation. It means that we
can build a query part by part and they are all relations which can be composed
further. In real life I had a situation where I had an extremely complex query.
It spanned over multiple tables and took time and effort to assert that it is
_correct_. After breaking it to multiple lines and indenting, it takes the
better part of my screen, and is not easy on the eyes. There are gotchas
because it uses SQL features that don't have 'native support' ^[difference
operator can be simulated in SQL, but it's a bit complex]. Anyways, it's a
query that you write once and then spend considerable effort to make sure that
later modifications don't break it. Now imagine that a while later you need to
take that query and do some further massaging of the data, like take some
aggregate value of it. You have two choices, first; forget modifying the query,
take the result and calculate the aggregate on the logic-tier, or the second;
copypaste the query and modify it. Both have their cons; it's good to keep the
queries on the database-tier because it's made for it and can perhaps optimize
the query. Not to mention there is less data going through the network. The
second has more dangerous problems; after copypasting and modifying the second
query, what if the original query needs to be modified. Both of the queries
need to be modified, and there are no assertions that the second query will
ever be modified. Then we have two logically different queries where the other
one is probably _incorrect_.

Because in relational algebra everything is a relation, you can just take that
intermediary query and both execute it and further refine the query and execute
that. As both queries stem from the same origin, modifying the origin modifies
both of the queries, assuring that both the queries are _correct_. This alone
is in my opinion a great incentive to use relational algebra.


HaskellDB
---------

[HaskellDB](http://hackage.haskell.org/package/haskelldb) is an EDSL for
running queries which has taken influence from relational algebra. It is a
powerful library which is in need of tender loving care, but has great
potential. I will return to the deficits ^[deficits for me] at the last
section.

Besides being a relational algebra EDSL and having the benefits mentioned
above, it also has the benefit of being _type-safe_. Type-safety brings safety
and assurance to our queries, we can be more sure ^[why not 'sure'? Examples on
this in the last section] that our queries are _correct_. For example it is a
compiler error to restrict that a string field has a value of `3`. Also it
means that we can't by accident try to join two incompatible tables together.

I'll go through little tutorial-like examples on HaskellDB queries to show how
the syntax is and how we can benefit from composable queries. The queries are
from my pet project, which is still heavily under way and therefore the schema
and queries might be suboptimal. I'm writing the project in yesod and I'm using
the [persistent](http://hackage.haskell.org/package/persistent-1.0.0) ORM
library in it, so I will write comparisons with the persistent library to some
queries. The schema I'm using in my HaskellDB tests is a little different to
the one persistent creates for me ^[namely, persistent creates numeric id
fields for every table, but for the HaskellDB schema there are numeric ids only
where it makes sense]. Go see the [schema](/static/img/graphs/schema.png).

Another difference is that HaskellDB queries operate in a pure query monad,
whereas persistent queries are immediately IO effects. But let's start with a
simple example. Find a household by user. For all the queries, I'm listing
first persistent version, then HaskellDB version, then how I would write the
raw SQL and then how HaskellDB writes the query.


``````haskell
selectList [HouseholdOwner ==. userId] []
``````

------------

`````haskell
h <- table household
restrict $ h!householdOwner .==. constant userId
return h
`````

------------

````sql
select * from Household where householdOwner = ?
````

------------

````sql
SELECT householdOwner,
       householdName,
       householdId
FROM Household as T1
WHERE householdOwner = 'john@example.com'
````

As we can see, there is not much benefit with HaskellDB over persistent. But
then again someone who has never seen the two libraries is more likely to
understand what happens in the second query. A bit more verbose, but also a bit
more intuitive.

Let's have a bit more complex query next. Persistent doesn't handle joins by
default, so we resort into a sort of trickery. Let's try getting all shopping
list item names when given a shopping list id.

````haskell
listItemIds <- map (shoppinglistItemItem . entityVal) <$>
    (selectList [ShoppinglistItemList ==. shoppinglistId,
                 ShoppinglistItemCount >. 0] [])
map (itemName . entityVal) <$>
    (selectList [ItemId <- listItemIds] [])
````

------------


````haskell
i <- table item
l <- table shoppinglistItem
restrict $
    i!itemId .==. l!shoppinglistItemItem .&&.
    l!shoppinglistItemCount .>. (constant 0) .&&.
    l!shoppinglistItemList .==. constant shoppinglistId
project $ itemName << i!itemName
````

------------

````sql
SELECT itemName FROM item AS i
JOIN shoppinglistItem AS l
ON l.shoppinglistItem = i.itemId
WHERE l.shoppinglistItemCount > 0 AND l.shoppinglistItemList = ?
````

------------

````sql
SELECT itemName1 as itemName
FROM (SELECT shoppinglistItemList as shoppinglistItemList2,
             shoppinglistItemItem as shoppinglistItemItem2,
             shoppinglistItemCount as shoppinglistItemCount2
      FROM ShoppinglistItem as T1) as T1,
     (SELECT itemName as itemName1,
             itemId as itemId1
      FROM Item as T1) as T2
WHERE shoppinglistItemList2 = 1 AND
    shoppinglistItemCount2 > 0 AND
    itemId1 = shoppinglistItemItem2
````

Again, the relational algebra version is more verbose, but it is also more
intuitive. Also we don't need to do two separate queries for simulating joins.

For the last query, let's count the total sum of the item costs. I don't have
this query on my project yet, so I have no idea how to use persistent to do
this. I'm not sure if it's even feasible.

````haskell
i <- table item
l <- table shoppinglistItem
restrict $
    i!itemId .==. l!shoppinglistItemItem .&&.
    l!shoppinglistItemCount .>. (constant 0) .&&.
    l!shoppinglistItemList .==. constant shoppinglistId
project $ itemCost << (i!itemCost .*. l!shoppinglistItemCount)
````

------------

``````sql
SELECT (i.itemCost * l.shoppinglistItemCount) as itemCost FROM item AS i
JOIN shoppinglistItem AS l
ON l.shoppinglistItem = i.itemId
WHERE l.shoppinglistItemCount > 0 AND l.shoppinglistItemList = ?
``````

------------

``````sql
SELECT itemCost1 * shoppinglistItemCount2 as itemCost
FROM (SELECT shoppinglistItemList as shoppinglistItemList2,
             shoppinglistItemItem as shoppinglistItemItem2,
             shoppinglistItemCount as shoppinglistItemCount2
      FROM ShoppinglistItem as T1) as T1,
     (SELECT itemCost as itemCost1,
             itemId as itemId1
      FROM Item as T1) as T2
WHERE shoppinglistItemList2 = 1 AND
    shoppinglistItemCount2 > 0.0 AND
    itemId1 = shoppinglistItemItem2
``````

There is a caveat in the HaskellDB query, but I'll get to it in the last
section. For now, we could do some refactoring. If you are observant, you can
notice that the two latest queries are almost the same, only the `project`
lines differ. Let's refactor the queries so that we can reuse the queries.

`````haskell
let baseQuery shoppinglistId = do
        i <- table item
        l <- table shoppinglistItem
        restrict $
            i!itemId .==. l!shoppinglistItemItem .&&.
            l!shoppinglistItemCount .>. (constant 0) .&&.
            l!shoppinglistItemList .==. constant shoppinglistId
        project $
            itemName << i!itemName #
            itemCost << i!itemCost #
            shoppinglistItemCount << l!shoppinglistItemCount
    sumQuery shoppinglistId = do
        r <- baseQuery shoppinglistId
        project $ (r!itemCost .*. r!shoppinglistItemCount)
    nameQuery shoppinglistId = do
        r <- baseQuery shoppinglistId
        project $ itemName << r!itemName
`````

See how we created a base query which is then reused in the subsequent queries.
This way we can be assured that the queries are _correct_ if the original query
is correct.


Problems
--------

I mentioned that I'm writing a project in Yesod with the persistent library.
Why didn't I choose HaskellDB instead? Well first of all, I wanted to take the
route of least hassle, and integrating HaskellDB into yesod might not be so
simple. I wanted to get something out as fast as possible, and refactor later
if necessary. Another reason is that persistent is _excellent_ in database
migrations. I like the QuasiQuoter EDSL syntax for declaring database schemas.
I don't really like the query syntax for persistent, but meh, I can live with
it. Another thing I like with persistent is that it creates a `TypeId` for
every table, and even if it's `Int`, `Int` and `TypeId` are considered
different types when doing queries. This has immense benefits. With SQL and
HaskellDB I can create join-queries where the type-invariant is held, but the
FK-invariant is not. As long as a number is  a number the join is considered
valid. This is not the case with persistent, which makes sure that you don't
try to join shoppinlists and shoppinglist counts by accident.

The caveat I mentioned before came from type-safety. I'm not sure whether I
like this or not, but in haskell it is invalid to multiplicate integers and
doubles. They might both be numeric values but they are different types. If for
some reason there is a situation where I need to do that, I can always 'cast'
the types so that they match. It's a bit cumbersome but it's doable. But with
HaskellDB I couldn't find a way to change the type to another during
projection. Originally I had marked the cost field as double and count field as
integer, but I couldn't multiply them, so I changed the type from integer to
double in the field declarations. I feel like I had to loosen type invariants
to satisfy compiler.

Other notes
-----------

I'm starting my baschelors thesis this year. I've been throwing ideas around of
what to do, but many of them have somehow related to relational algebra. My
latest idea has been 'updating' HaskellDB and some related libraries. I've been
thinking of doing the following:

* QQ EDSL for creating schemas and HaskellDB DBDirect tables
    - Should handle migrations
* Last inserted id support for connection libraries.
* Improving datetime support
* Support for sum types (enum?)
* Support for Text

What do you readers think of this? Is this 'academic' enough? Would this be
useful? Is this enough? What would you like to see instead? Have in mind that
it also involves writing the thesis part and studying. As a comparison, this is
worth 10 study points (two months of financial aid), whereas a two period
course of (harder) mathematics is 8 study points.
