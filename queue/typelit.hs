{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language GADTs #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language PolyKinds #-}
{-# Language UndecidableInstances #-}
{-# Language ScopedTypeVariables #-}

import GHC.TypeLits

data Proxy a = Proxy
data End

data (path :: k) :/ b = Proxy path :/ b
infixr 9 :/

type Path = "foo" :/ "bar" :/ "baz" :/ End

foo p = symbolVal p

class Render path where
  type R path :: *
  render :: Proxy path -> String

instance Render End where
  render _ = ""

instance (KnownSymbol path, Render subpath) => Render (path :/ subpath) where
  type R (path :/ subpath) = R subpath
  render p = symbolVal proxy ++ "/" ++ render (Proxy :: Proxy subpath)
    where proxy = Proxy :: Proxy path

