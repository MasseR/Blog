---
title: Woes of functional programming with PHP
tags: fp, php
---

### A very fast introduction to lambdas and closures

Lambdas, also known as anonymous functions are functions with no name. Normally
you would create a function like this:

~~~{.sourceCode .php}
function doSomething() {
    return "Fire ze missiles!";
}
~~~~~

With lambdas, the same function could be written as:

~~~~~{.sourceCode .php}
$doSomething = function() { return "Fire ze missiles!"; }
~~~~~

That's all fine and dandy, but I haven't said anything about closures. Closures
close over a function, trapping the variables inside. This is useful in many
ways, and this is but an example:

~~~~{.sourceCode .php}
function doSomething($userid, $groups) {
    $bind = array_map(function($groupid) use($userid) {
        return array("uid" => $userid, "gid" => $groupid);
    };
}
~~~~

The variable `$userid` is trapped inside the anonymous function. The function
maps over the groups array and creates a new array which might be something
like `array(array("uid" => 1, "gid" => 1), array("uid" => 1, "gid" => 5))`. The
result could be used to bind values into a SQL statement.


### Lambdas

I believe functional programming paradigms have a use in the real world. The
abstractions they provide enable the programmer to develop faster and create
more readable and understandable code. I would give anything to be able to earn
money by writing Haskell, but as of now it is not a possibility, and my day job
is writing PHP.

Haskell is a functional nature by heart, and seems to have one of the best
syntaxes for what it does. I haven't seen many more as minimalistic languages
as Haskell, although Clojure comes close. I have chosen Haskell as a comparison
mainly for this reason, but because of this, all the examples need to be taken
with a grain of salt. The examples are at home with Haskell, but on foreign
territory with PHP.

As of PHP 5.3, PHP has supported anonymous functions, lambdas, but their syntax
is somewhat horrible, and the benefits they provide are minimal if any. Take
for example the following snippet from Haskell. Imagine that we have a data
type describing a user, and we want to get their names, but only if they have
children.

In haskell, we might represent the employer as a simple algebraic datatype. In
PHP we could either create an associative array or by creating a class. In this
example I'm going to write it as a class.

~~~~{.sourceCode .haskell}
data Employer = Employer {
   surname :: String
 , forename :: String
 , age :: Int
 , children :: [Person]
}
~~~~

~~~~~{.sourceCode .php}
class Employer {
    private $surname = null;
    private $forename = null;
    private $age = null; // Dangerous by itself, as age is numeric, and null can be considered as 0
    private $children = array();
    // Constructor
    // Setters
    function getSurname()  { return $this->surname;             }
    function getForename() { return $this->forename;            }
    function getAge()      { return $this->age;                 }
    function getChildren() { return $this->children;            }
    function hasChildren() { return count($this->children) > 0; }
}
~~~~~

To get the employers who have children, we can just filter them and then format
as a tuple the name. Or maybe even concatenate into a "$forename $surname"
string. The algorithm is read as "map as tuple the values that were filtered having children".

~~~~{.sourceCode .haskell}
employersWithChildren = map (liftM2 (,) surname forename) . filter ((> 0) . length . children)
~~~~

This small example already showed two uses for higher order functions and
lambdas, namely `map` and `filter`. It's a bit long line, but it is still
readable and understandable.

The idiomatic way to do the same with PHP would be to loop over the
employers and add the result to an array if there is a match. This is such a
simple example that it's still extremely readable, but in general mutable
variables, make it more difficult to keep track of what's happening.

~~~~{.sourceCode .php}
$employersWithChildren = array();
foreach($employers as $e)
    if($e->hasChildren())
        $employersWithChildren[] = array($e->getSurname(), $e->getForename());
~~~~

Seeing how simple the Haskell example was, you'd expect that the same would
hold true for PHP. Let's see how it could be written with higher order
functions in PHP. The algorithm is exactly the same as the Haskell example.

~~~~~{.sourceCode .php}
array_map(function($x) {
    return array($x->getSurname(), $x->getForename());
}, array_filter($employers, function($x) {
    return $x->hasChildren();
});
~~~~~

There's many reasons why it's difficult to read it. First of all, the signature
for `array_map` is `(a -> b) -> [a] -> [b]`, whereas the signature for
`array_filter` is `[a] -> (a -> Bool) -> [b]`. Two higher order functions, but
the other takes the callback as first argument, and the array as second, and
the second vice-versa. Actually, `array_map` is the oddity here, as it's the
only one which takes the callback first. This is unfortunate because, the
higher-order function you're most likely to use is mapping. So you get nice and
comfortable with map, and when you try to filter or reduce, you're wondering
what's gone wrong. It is sort of understandable however that map has the
callback as first, as it can take multiple arrays at once.

The other reason why it's difficult to read is that we transformed the simple 4
line algorithm that had 3 lines of program logic, into 5 lines of code with 2
lines of program logic. Instead of removing boilerplate, we increased it.

### Closures

I showed earlier how higher-order functions, lambdas and closures could be used
to create an array to bind into a SQL statement. The same algorithm could be
represented in Haskell with the following:

~~~{.sourceCode .haskell}
-- Pointfree
doSomething = flip (zipWith (flip (,))) . repeat
-- Explicit arguments
doSomething' uid gids = zipWith (flip (,)) gids (repeat uid)
~~~

Although the first version is shorter, it's not necessarily as readable as the
second. Even the first function might be a difficult read for someone who's not
done much with Haskell. The function takes two arguments. The first argument is
a single user id value, and the second is a list of group ids'. The function
then repeats the user id as many times as necessary, creating a new list of
user id values. The zipWith function takes the to lists and combines them with
`flip (,)` which flips the order of arguments for the `(,)` function creating a
tuple of uid and group id. The user id is the first value in the tuple and the
group id is the second.

Another example could be validating values. Imagine a hyphotetical situation
where you have many inputs, which need to be validated similarly. For example,
validate that they are string values, they don't have any illegal characters,
they can be parsed into a date, etc.

For this example, I need to introduce a new higher-order function, `all`. The
function takes a function and an array. The function should return a boolean.
If and only if all the values are returned as true, the function returns true.
For a simple demonstration, I'll also introduce the function `id` which returns
the same argument as was given to it. In PHP it could be written as `function
id($x) { return $x; }`.

~~~{.sourceCode .php}
all(function($x) { return $x; }, array(
    true,
    true,
    true)); // => true
all(function($x) { return $x; }, array(
    true,
    false,
    true)); // => false
~~~

Another function I need to introduce is `apply` which takes a function and an
argument and calls the function with the argument. It's not necessary with the
PHP example, but the haskell version has a symbol `$` in it, which means
applying.

Let's start by describing the function that we need. We need a function that
takes a list of validator-functions and the value to validate. I'll say that
again; it takes a list of _functions_ and the input to validate. We could of
course describe it the other way around, a function that takes a validator
function and a list of inputs and filters them. However if we want more general
functionality and reusability, the first one is the way to go.

~~~{.sourceCode .php}
// Imagine that $validators is in scope
$validate = function($input) use ($validators) {
    return all($validators, function($y) use($input) {
        return $y($input);
    }
};
~~~

It's a mouthful. Four lines, two lambdas, and extremely difficult to read. How
does Haskell fare against it?

~~~{.sourceCode .haskell}
-- Imagine validators are in scope
validate input = all ($ input) validators
~~~

One line? Is that really all? Yup, it's that simple. There's also some currying
involved, but I'll explain that in the next chapter.

### Currying

Currying is a technique in which a function is transformed so that it can be
called with single arguments. This is one part where Haskell really shines, as
by default every function in Haskell is curried.

In Haskell every function is a function that takes a single arguments and
returns either a function or result. Let's take for example the function `add a
b = a + b`, which seemingly takes two arguments, but in reality it takes an
arguments and returns a function that takes an argument and returns a function.
The same can be said with type signatures. Initially the function has the type
of `a -> a -> a` which can be read as a function of a to a to a. After giving
the function an argument, the signature transforms into `a -> a`. And after
giving one more arguments the type signature turns into `a`.

~~~{.sourceCode .haskell}
add :: Int -> Int -> Int
add a b = a + b


add 1 -- Int -> Int (add b = 1 + b)
add 1 2 -- Int (add = 1 + 2)
~~~

PHP has the worst syntax I know of when creating curried functions and invoking
them. It's just not a pain to create the function, but the invoking is horrible
too.

~~~{.sourceCode .php}
function add($a) {
 return function($b) use($a) {
    return $a + $b;
 };
}
~~~

The function add takes an arguments and returns a function, just like haskell
above, but done explicitly. Not only that but the `use` statement makes it more
difficult to read, and parse. But that's just the function, and I mentioned
that invoking the function is difficult too. Let's see how it's done.

~~~{.sourceCode .php}
$curried = add(1);
$result = $curried(2);
$illegal = add(1)(2); // Doesn't work
~~~

So to invoke the curried functions, we need to temporarily set the function
into a variable. We can't just invoke it in one go. Even with vanilla
Javascript where we have to curry manually, we can still do it in two lines.

~~~{.sourceCode .js}
var add = function(a) { return function(b) { return a+b; }; }
add(1)(2);
~~~

### Calling functions

Calling callbacks in PHP is a total mess. In this section I will not write a
single line of Haskell, and focus entirely on PHP.

When declaring callbacks in PHP, you have a multitude of ways in your arsenal.
Too many if you ask me. You can have anonymous functions, normal functions,
classes, static classes, classes with magic methods, and anonymous functions
created with `create_function`.

~~~{.sourceCode .php}
function() { return null; } // Anonymous / lambda function
create_user_func('$x', 'return $x'); // Anonymous / lambda function
function id($x) { return $x; } // Normal function
class Foo {
    function normal() { return null; } // Method in a function
    static function s_function() { return null; } // Static method in a function
    function __invoke() { return null; } // Makes the class callable
}
~~~

We have two functions for calling functions (that itself is a subject of wtf I
think). The first function is `call_user_func`, which takes the callback and
parameters. The second function is `call_user_func_array` which takes the
callback and parameters in an array.

Let's take all the different kinds of callbacks, and walk through how we can
call them. Let's start with anonymous functions.

~~~{.sourceCode .php}
// Our example function is just id
$fun = function($x) { return $x; }

// We can call it like any other function, but remember when currying that you
// can't use ()().
$fun(1);

// We can also call it with call_user_func
call_user_func($fun, 1);

// And with call_user_func_array
call_user_func_array($fun, array(1));
~~~

~~~{.sourceCode .php}
// Before 5.3 you had to create anonymous function with create_function
$fun = create_function('$x', 'return $x;');

// You can't call it like a normal function, but must instead use the calling
// functions
call_user_func($fun, 1);
call_user_func_array($fun, array(1));
~~~

~~~{.sourceCode .php}
function id($x) { return $x; }

// As you know, you can call it like a normal function, since it is a normal function
id(1);

// And both the calling functions work, but there is a gotcha. Unlike in sane
// languages, you aren't giving a reference to the object, but instead you are
// giving the name of the function as a string. This is bad, really really bad,
// and I'll come back with an example later.
call_user_func('id', 1);
call_user_func_array('id', array(1));
~~~

~~~{.sourceCode .php}
// A normal public method in a class.
class Foo {
    function id($x) { return $x; }
}

// You can call the function normally, but you need to have the object
// instance. My trouble with this is that you can't chain calls when creating an
// object. new Foo(); does not return an object as you'd expect, but does
// something else (I should probably check documentation what it really does)
//
$foo = new Foo();
$foo->id(1);
// new Foo()->id(1); Illegal but I wish it would work. Does in python and javascript

// You can also use the calling functions, but there is another gotcha. The
// callback argument is an array where the first value is the object, and the
// second argument is the method as a string. The first value can also be a
// statement of new Foo(), or a string which both do the same thing; instantiates
// a new object.
call_user_func( array ($foo, 'id'), 1      );
call_user_func( array (new Foo(), 'id'), 1 );
call_user_func( array ("Foo", 'id'), 1     );

call_user_func_array ( array ($foo, 'id'),      array (1) );
call_user_func_array ( array (new Foo(), 'id'), array (1) );
call_user_func_array ( array ("Foo", 'id'),     array (1) );
~~~~

~~~{.sourceCode .php}
// A static method in a class.
class Foo {
    static function id($x) { return $x; }
}

// You can call the function normally. As it's static, you don't need to have
// an object instance, but you _can_ have an object instance.
Foo::id(1);
$foo = new Foo();
$foo::id(1);

// You can use the calling functions, and all the same styles as when calling
// normal methods, can be used to call static methods. You can also call the
// static method, like you would call a normal function.
call_user_func( array ($foo, 'id'), 1      );
call_user_func( array (new Foo(), 'id'), 1 );
call_user_func( array ("Foo", 'id'), 1     );
call_user_func( "Foo::id", 1); // New

call_user_func_array ( array ($foo, 'id'),      array (1) );
call_user_func_array ( array (new Foo(), 'id'), array (1) );
call_user_func_array ( array ("Foo", 'id'),     array (1) );
call_user_func_array( "Foo::id", array(1)); // New
~~~

~~~{.sourceCode .php}
// A callable class. I don't have any experience with this, so I might have
// missed something
class Id {
    function __invoke($x) { return $x; }
}

// You need to have an object instance. After that, you can call it like an
// anonymous function.
$id = new Id();
$id(1)

// All the same methods that could be used for normal methods, can be used with
// __invoke. But you can also call it like a a lambda function
call_user_func(array($id, "__invoke"), 1);
call_user_func(array(new Id(), "__invoke"), 1);
call_user_func(array("Id", "__invoke"), 1);
call_user_func($id, 1);

call_user_func_array(array($id, "__invoke"), array(1));
call_user_func_array(array(new Id(), "__invoke"), array(1));
call_user_func_array(array("Id", "__invoke"), array(1));
call_user_func_array($id, array(1));
~~~

That's a whole lot of different kinds of callbacks and ways to call them. When
calling callables, you either always need to use the helper functions with ugly
syntax, or create a wrapper, which creates a nice closure. For an example,
let's write a custom map implementation and a wrapper that always returns a
nice callable function.

~~~{.sourceCode .php}
function callable($f) {
    return function() use($f) {
        return call_user_func_array($f, func_get_args());
    };
}
~~~

It's a nice wrapper that returns a function that calls the callable with the
arguments given for the inner function. We could use it like this:

~~~{.sourceCode .php}
$id = callable('id');
$id(1);
~~~

But there's a problem with this implementation. If we supply something other
than callbacks, it will fail with php warning. An exception would be better. We
have a function `is_callable` at our disposal, which returns true only if the
function is callable. Let's edit the function to throw exception when callable
is not a callable.

~~~{.sourceCode .php}
function callable($f) {
    if(!is_callable($f))
        throw new Exception("Not callable");
    return function() use($f) {
        return call_user_func_array($f, func_get_args());
    };
}
~~~

Great, now we have some rudimentary error handling and we can go forward to
create the custom map implementation. The `array_map` function fails if the
array is null. How about making a map that returns an empty array if given null
as an argument.

~~~{.sourceCode .php}
function safeMap($f, $arr)
{
    if(is_null($arr))
        return array();
    return array_map($f, $arr);
}
~~~

Now we have a function that does the sane thing if given an empty value. We no
longer need to explicitly check that the array is not null when mapping over
values. We might also like some lazy loading, which we can do with callbacks.

~~~{.sourceCode .php}
function safeMap($f, $arr)
{
    if(is_null($arr))
        return array();
    try {
        $g = callable($arr);
        $arr = $g();
    } catch(Exception e) { }
    return array_map($f, $arr);
}
~~~

The last one is a bit of a stretch, but it's for the error I was talking about
earlier. Imagine that we are getting some data from a database, and mapping
over it. Do you see the error now? Let's imagine that the data we received is
`array("foo", "bar")`. The `is_callable` function tries to find a class with
the name `foo` with method `bar`. How about we inadvertly try map over a
string. That too can be thought as a callable, causing an error.
