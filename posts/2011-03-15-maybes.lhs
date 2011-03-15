---
title: Web input
tags: haskell, php
---

Like mentioned in a earlier post, I've been doing some web development lately.
All developers know that data should be validated properly. This includes
sanitating data like mentioned in the previous post, and making sure whether
there is data at all or not.

Especially with web development it is crucial to check what data you have
available, as users might not fill all the fields. It is also necessary to
check the data in the backend, even if there are checks and validation on the
user side, as malicious users or hackers might create their own requests and
bypass all the user interface checks.

I've been coding with PHP for a few weeks now, and the way I've checked the
user entered fields has been bugging me lately. The more there is user data,
the deeper the if-else fields go, and I don't like it. I also write for myself
a shopping list application. I kept it as simple as possible, and as such all
the functionality is around 70 lines, including the template. User data
sanitation is handled by the template, and the field validation takes maybe 3
or 4 lines.

In this post I'll write different ways for checking an imaginary form
containing name, age and location describing a person. All of these fields are
required and missing any of them means error.

The idiomatic way to write in php could be like the following function. As you
can see, the function goes deeper and deeper as there are more fields to check.
This is okay for a small number of fields, but it is still painful. Every
fields existance is checked explicitly and the age is validated as a number.
Only at the deepest level the `$person` variable is set with a value.

~~~~~~~~{.sourceCode .php}
function mkPerson()
{
  $person = null;
  if(isset($_POST['name']))
  {
    if(isset($_POST['age']) && is_numeric($_POST['age']))
    {
      if(isset($_POST['location']))
      {
        $person = array(
          "name" => $_POST['name'],
          "age" => $_POST['age'],
          "location" => $_POST['location'],
      }
    }
  }
  return $person;
}
~~~~~~~~

If we have a large number of mandatory fields, one could validate the fields
with `array_reduce`. The solution is a bit complex and ugly with php syntax,
but works. This doesn't behave exactly the same as the one above, as it doesn't
check the numeric value. This is also too complex for three fields, but any
more and it might do just the trick.

~~~~~~{.sourceCode .php}
$required_fields = array("name", "age", "location");
$valid = array_reduce(
  $required_fields,
  function($acc, $key) use($_POST) {
    return $acc && in_array($key, array_keys($_POST));
  }, true);
$person = null;
if($valid)
{
  $person = array();
  foreach($required_fields as $key)
    $person[$key] = $_POST[$key];
}
~~~~~~

So how does Haskell fare? There are libraries for forms and great frameworks
for overall web development, but I think it's fairest if we keep it as bare
bones as possible. For this experiment I'm using only the
[CGI](http://hackage.haskell.org/package/cgi) package. The `Maybe` transformer
is from the [MaybeT](http://hackage.haskell.org/package/MaybeT) package.

> import Control.Applicative
> import Control.Monad.Maybe
> import Control.Monad
> import Network.CGI

Our `Person` datatype consists of String for name, Int for age and a String for
location. Our purpose is to fill this datatype with data from a form.

> data Person = Person String Int String deriving Show

The CGI package provides a function called `getVar` which has the following
type signature: `MonadCgI m => String -> m (Maybe String)`. As you can see, the
function returns a `Maybe a` value, which means that we should have a way to
use monads for our advantage. The `Maybe` monad short-circuits as soon as a
`Nothing` is found. For example `Nothing >>= Just 2` returns a `Nothing`.

But the function is in a `MonadCGI m` monad, how do we take advantage of the
`Maybe` behaviour? This is what the MaybeT package was for, we just lift the
`getVar` function into `MaybeT m String`.

> getVarT :: (MonadCGI m) => String -> MaybeT m String
> getVarT = MaybeT . getVar

Now that the getter is in a proper monad, we can take full advantage of it. The
next function shows how to use idiomatic haskell do-notation for filling out
the `Person` datatype. As you can see, there is no explicit checking for field
existence, but instead it is handled by the `Maybe` monad.

The solution is much shorter than the PHP version, and scales for larger number
of fields. This solution is still quite clunky and we should see whether we can
clean it up a bit.

> mkPerson :: CGI (Maybe Person)
> mkPerson = runMaybeT $ do
>   name <- getVarT "name"
>   age <- read `fmap` getVarT "age"
>   location <- getVarT "location"
>   return $ Person name age location

The `Control.Applicative` module usually provides for a more compact solution.
However the MaybeT package doesn't provide an Applicative instance for the
`MaybeT` transformer. However as monads and applicative functors are closely
related, it is trivial to make a monad an instance of `Applicative`. The
`Applicative` requires you to create functions `pure` and `(<*>)` for a minimal
complete definition.

> instance (Functor m, Monad m) => Applicative (MaybeT m) where
>   pure = return
>   (<*>) = ap

With the `Applicative` module, we can chain functors nicely and still get the
same functionality as with monads. This time however the function is not
aesthetically pleasing, at least to my eyes, but is a bit shorter and more
concise.

> mkApplicativePerson :: CGI (Maybe Person)
> mkApplicativePerson = runMaybeT $ Person
>   <$>  getVarT "name"
>   <*> (read <$> getVarT "age")
>   <*> getVarT "location"
