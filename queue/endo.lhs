I wrote this [gist](https://gist.github.com/MasseR/4749508) a while ago. A
couple of days Ollie Charles wrote [Quick and Easy DSLs with Writer
Endo](http://ocharles.org.uk/blog/posts/2013-02-12-quick-dsls-with-endo-writers.html).
This post is my attempt to open my gist and how I got to it.

Haskell structures and terms are scary and difficult concepts for many. Many of
them are documented as their mathematical counterpart and this abstract
description does not give pointers how and when the structures might be used.
This is sad because many of them are widely applicable to many situations, but
you need the experience to see where they might fit. This post uses
endomorphisms, monoids and monads. I will try to unwrap them into their
pragmatic counterparts.

## Endo monoid

~~~{.haskell}
data Endo a = Endo { appEndo :: a -> a }
~~~

Wikipedia describes [endomorphisms](https://en.wikipedia.org/wiki/Endomorphism)
as "In mathematics, an endomorphism is a morphism (or homomorphism) from a
mathematical object to itself". Hackage desribes
[`Endo`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Monoid.html#v:Endo)
as "The monoid of endomorphisms under composition". I don't know about you, but
those descriptions won't open the structure and its uses to me. Rather than the
description, let's cheat and use some prior knowledge. For me it was google+,
for you it's hopefully me. If you look at the data definition, you see that the
structure takes one argument, a function `a -> a`. This can be seen as
modifying the underlying type.

Before going further, let's step back and see some simpler monoids. Let's take
the `Sum` monoid which sums values together. The type and monoid instance is
simple and probably doesn't require any explanation.

~~~{.haskell}
newtype Sum a = Sum { getsum :: a}
instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0
  (Sum a) `mappend` (Sum b) = Sum (a + b)

Sum 3 `mappend` Sum 2 -- Sum 5
~~~

We can simulate the `Sum` monoid with `Endo`. If you look at the `Sum`
definition above, you can see that the binary operator modifies the original
value with $+$ operator.

~~~~{.haskell}
add x = Endo ((+x))

appEndo (add 2 `mappend` add 3) 0 -- 5
~~~~

Before going to the pragmatic example, let's take another simple and
interesting example. We can define difference lists with `Endo`. Difference
lists are lists with appending $O(1)$ and their type is `[a] -> [a]`.

~~~~{.haskell}
type DiffList a = Endo [a]

fromList :: [a] -> DiffList a
fromList xs = Endo $ (xs ++)

toList :: DiffList a -> [a]
toList d = appEndo d []
~~~~

Now it's time for the pragmatic example. I'm going to use the same example as
the gist. I'm wanting to simulate yesod style widget monad. The idea behind it
is that a page consists of a finite number of sections where we are using:
header, footer, body, title, scripts and styles.

~~~~{.haskell}
data Template = Template {
    header :: Html
  , footer :: Html
  , body :: Html
  , addScript :: String
  , addStyle :: String
  , title :: String
  }
~~~~

We want to be able to update this structure partially. Therefore we need to
think what is a partial `Template` and what the updates consist of. Luckily for
us, each of the underlying types has a monoid instance so we can create an
empty `Template` easily. We could also use `Maybe` for each field, but I find
it ugly. Besides, we always have a body, it just might be empty (notice the
difference from nothing or null).

~~~~{.haskell}
emptyTemplate = Template mempty mempty mempty mempty mempty mempty
~~~~

## Writer monad

## Wrapping up
