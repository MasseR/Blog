---
title: Distributed database migrations
---

Database migrations are an important aspect. There is no system where the
original database schema persists all the way through projects lifetime. I have
heard of migration tools, but unfortunately I have done no research on them.
This post is my take on it.

## Background

There are other approaches, but this is nice and simple. The premise is that
there are no ORM libraries, no database<->object mappers etc. This means that
we can't have the model information from the source code and figure out the
diff from that. Why this premise? First and foremost, I want to have explicit
control over my queries, and secondly I'm using PHP for this, and I'm not too
fond of docstring metadata declarations. There are situations, where you want
to have fine grained control over your schema, and either you need to have
extremely complex metadata handling, or you just create the DDL queries by
hand.

## Naive solution

You can write your database migrations by hand linearly. Last time I did
something with Drupal, it also had this model. For example, you can have
`update1()`, `update2()`, `update3()`, which are then ran automatically
linearly in increasing order. On database side, you keep track of your current
version, and the next time there are updates, you just run all the updates from
'last+1->latest'. It's consistent and easy to implement, but it breaks when
your commits and migrations are distributed.

![Linear chain](/static/img/graphs/migrations_1.png)

For example with git, one developer might have multiple feature-branches, which
are then merged into dev/release afterwards. Every branch might have their own
migrations, but their numbering must start from the same root. You can't skip
numbers, because then the database is an inconsistent state, thinking that it's
in a higher version, without having executed the previous migrations.

![Break in the chain](/static/img/graphs/migrations_2.png)

## Possible solution

I haven't implemented any of this, and I doubt I have time to do it anytime
soon, so take anything I write with a big grain of salt.

Let's tackle the first problem, numerically linear execution. Instead of having
numerical migrations, we can have short-descriptions or tokens on the
migrations. These tokens are saved to the database and if all the tokens are in
database, it's consistent. If new tokens are introduced, they are automatically
executed.

This introduces two problems. The first one is that we need to keep track of
the active migrations, but this is easy to solve. Instead of keeping track of a
single version number, insert a new row for each migration. The tokens
differentiate the different migrations. If the token does not exist in the
database, it has not been yet executed, and we are not in the latest state. The
other problem is a more difficult one. The migrations are rarely independent,
but instead depend on some earlier state.

![Dependent migrations](/static/img/graphs/migrations_3.png)

This dependency problem is more difficult to solve, but not impossible. As an
outline, we have two problems to solve, marking dependencies and solving it.
Marking the dependencies can be done with simple docstring metadata, and
probably even with normal method calls, without losing readability. The second
part is solving the dependency chain and linearizing it. The dependency chain
can be linearized with a simple [topological sorting
algorithm](http://en.wikipedia.org/wiki/Topological_sort). Once we got the
linear dependency chain we can start executing.  Start from the top and go
down, if the migration hasn't yet been executed, execute it now.

![Back to linear migrations](/static/img/graphs/migrations_4.png)
