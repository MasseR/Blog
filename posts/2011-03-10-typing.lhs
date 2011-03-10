---
title: Static typing for better security
tags: haskell,web,security,typing
---

Everybody who has done some web development knows that user data should not be
trusted, ever. I've been doing some PHP work lately, and found myself longing
for Haskells' great type system.

I haven't done that much web development and haven't yet gained any good
escaping routines. I started having trouble when I had to decide at what point
to escape user data. At first I was going to escape before inserting to
database, but there could be some important data in it. Instead I opted to
escape at the very last moment, when rendering the template.

However after a while I noticed a lot of unescaped content. I had simply
forgotten to make the templates escape the variables. This forgetfulness made
me think about Haskell and how I could leverage the type system to aid my
hopeless memory.

Ideally we would like to have the compiler warn us when trying to output unsafe
strings, but still be easy to use. I had just read about the ghc generalized
newtype deriving pragma, which we could use together with overloaded strings
pragma. This allows us to use the safe strings as we would use regular strings.
We would also like to explicitly mark strings as safe, as well as have unsafe
strings automatically sanitized.

The example is a bit contrived but the use case would be something like the
following:

~~~~{.haskell}
webPutStrLn "Hello world"
webPutStrLn $ safeString "Hello /> world!"
fmap safeString getDataFromWeb >>= webPutStrLn
~~~~

The pragmas are for ease of use. Allows us to use our new type like we would
use Strings.

> {-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

We import Data.String for the IsString instance. The type class is used by the
OverloadedStrings pragma and any type with that instance can be used like
Strings.

> import Data.String ()

For the example we wouldn't really need any proper sanitizing, but let's make
the example and testing a bit more exciting and use an existing sanitizer
library. The module is from the [xss-sanitize](http://hackage.haskell.org/package/xss-sanitize-0.2.6) package.

> import Text.HTML.SanitizeXSS (sanitize)

Our `SafeString` is created with the newtype keyword which is faster than ADT and
allows us to use the GeneralizedNewtypePragma. The new type is just a wrapper
around `String`.

> newtype SafeString = SafeString String deriving (IsString)

The `safeString` method is a safe constructor for our `SafeString` type. It
automatically sanitizes the argument and returns our safe string.

> safeString :: String -> SafeString
> safeString = SafeString . escape
>   where escape = sanitize

And since we wanted to explicitly be able to mark strings as safe, why not make
it as annoying as possible? The next function does just that.

> yesImTotallySureThatThisStringIsSafe :: String -> SafeString
> yesImTotallySureThatThisStringIsSafe = SafeString

> webPutStrLn :: SafeString -> IO ()
> webPutStrLn (SafeString x) = putStrLn x

> getDataFromWeb :: IO String
> getDataFromWeb = return "hello\" /> world"
