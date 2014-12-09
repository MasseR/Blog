---
title: Example of type level literals
author: Mats Rauhala
---

I've been reading on type operators, type families and dependent types today,
and before I forget everything, I thought I'd write about it. This is mostly
just for myself so I won't be writing anything too big.

- DataKinds promotes type constructors to kinds
  - Prefix with '
  - Only constructors without arguments
- ScopedTypeVariables allow the render instance below and the proxy types in it
- TypeFamilies allow sort of dependent type programming (not in this example)
- PolyKinds allow the `(path :: k)` syntax
- See also [servant](http://alpmestan.com/posts/2014-12-09-rethinking-webservices-apis-haskell.html)

> {-# Language DataKinds #-}
> {-# Language TypeOperators #-}
> {-# Language TypeFamilies #-}
> {-# Language GADTs #-}
> {-# Language AllowAmbiguousTypes #-}
> {-# Language PolyKinds #-}
> {-# Language UndecidableInstances #-}
> {-# Language ScopedTypeVariables #-}

> import GHC.TypeLits (symbolVal, KnownSymbol)

Proxy type with phantom type. Used when given explicit type signature.

> data Proxy a = Proxy

Type operator for combining types. Used for combining type level literals for
building an uri. Note the `infixr`, which is required because of type inference
rules(?)

> data (path :: k) :/ b = Proxy path :/ b
> infixr 9 :/

End clause for recursive combinators.

> data End

Example recursive path.

> type Path = "foo" :/ "bar" :/ "baz" :/ End

Rendering type class. The type class is used for getting the type information
for the rendering function. I'm not sure if it could be done without type
classes, but this is simple enough.

> class Render path where
>   render :: Proxy path -> String

> instance Render End where
>   render _ = "/"

*Note:* `ScopedTypeVariables` is required for the following to work.

> instance (KnownSymbol path, Render subpath) => Render (path :/ subpath) where
>   render p = "/" ++ symbolVal proxy ++ render (Proxy :: Proxy subpath)
>     where proxy = Proxy :: Proxy path

> path :: String
> path = render (Proxy :: Proxy Path)
