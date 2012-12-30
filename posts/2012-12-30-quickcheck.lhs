---
title: QuickCheck notes
author: Mats Rauhala
tags: haskell, notes
---
Quickcheck is one of those tools that I want to use, but I'm not comfortable using them. It's not that I don't know how to use the tools, but the problem comes with the basics of QuickCheck. QuickCheck is a testing tool that checks the properties of the function. For some functions it's more easy to think of properties than for others. In this document I try to record some of the properties for random functions that I have thought of.

Guidelines
----------

- Try to think what it means to be that function
- QuickCheck requires concrete types
- test-framework is a life-saver

Grouping
--------

A simple grouping function could be typed like this. The types tell that we probably somehow join the elements together into sublists, wherein the name tells us that the joining is grouping. Aside from that, I'll give another restriction; all the elements must be in their buckets, or in other words, every sublist must be unique.

~~~{.haskell}
group :: (Ord a) => [a] -> [[a]]
~~~

There is a trivial implementation for this function, but I'll leave it to later, as there is one property in particular that fails with the default `Data.List.group` function.

Let's start by declaring the properties (that I thought of when walking to car).

- All the elements that were present in the original list, must be present in the resulting list
- All the elements in buckets must be the same
- The buckets must contain all of their corresponding elements.
    - This implies that all the buckets are unique

Let's try to tackle the properties one by one.

~~~{.haskell}
prop_all_elements xs = sort xs == (sort . concat . group) xs
  where types = (xs :: [Int])
~~~

The thing with the types part is that QuickCheck can't infer the types polymorphically. You need to have concrete types. Other than that the property is simple. If we concatenate and sort the grouped list, we should end up with the same list as sorted original list. All the elements that are in the original list are in the grouped list.

~~~{.haskell}
prop_grouped_are_grouped xs = all same grouped
  where grouped = group (xs :: [Int])
        same [] = True
        same (y:ys) = all (== y) ys
~~~

Aside from being our next property, it's a nice example of higher-order functions. `all` is a specialized higher-order function that specializes fold. All it requires is a `a -> Bool` function which are then folded into `Bool`. As for the property, it's pretty much a mathematical notation for it. Very simple. As for why I wrote a base case for the pattern matching is that we can't be sure that QuickCheck doesn't provide us with empty lists, actually we can be pretty sure there is going to be a couple of empty lists in the mix.

~~~{.haskell}
prop_all_elements_in_group xs = all allElementsInGroups grouped
  where grouped = group (xs :: [Int])
        totalElements x = length $ filter (== x) xs
        allElementsInGroups [] = True
        allElementsInGroups (y:ys) length (y:ys) == totalElements y
~~~

Generally this doesn't differ from the previous property test much. The skeleton is the same `all something grouped`, but the testing function is different. What we test here is that the number of elements in a bucket must be the same as the total number of elements in the original list.

What can we say in general about these tests? They all play an important role in defining the function. They all are a part of "what makes group function what it is". If we go through them in different order, we see how they relate to each other. The last property is not enough alone, because we aren't sure whether all the elements in a bucket are the same, and if we can't be sure of that, the entire property is bogus, failing entirely. However the second property rectifies this. Now we can be sure that all the elements in a bucket are the same, and therefore we can check that the number of elements in a bucket matches the number of that element in the original list.

 The first property is a separate, but important. Where as the two latter properties check that the buckets are valid and the elements haven't travelled to other buckets, they do not make sure, that for example the group function doesn't return `[]`, or otherwise minified list.


What about the grouping function? Let's see how we could trivially write it. It's a contrived example, but it demonstrates how QuickCheck can find problems that you didn't think of.

~~~{.haskell}
group :: (Ord a) => [a] -> [[a]]
group = L.group
~~~

Let's see how it fares against our properties.

    *Main> quickCheck prop_all_elements
    +++ OK, passed 100 tests.

So far so good. It doesn't take out or add extra elements to the list.


    *Main> quickCheck prop_grouped_are_grouped 
    +++ OK, passed 100 tests.

So far so good. All the elements consist only of the same elements. Didn't expect anything else.


    *Main> quickCheck prop_all_elements_in_group 
    *** Failed! Falsifiable (after 11 tests and 3 shrinks):     
    [-6,0,-6]

Oops, we got an error. Even though this is random testing, we get an extremely informative error and example out of it. Let's see what our group function would do if it was given that list.


    *Main> group [-6, 0, -6]
    [[-6],[0],[-6]]

A-ha! The default group function groups the adjacent elements. This is of course mentioned in the documentation, but it's a good example nonetheless. So let's modify the grouping function to suit our definition.

    group :: (Ord a) => [a] -> [[a]]
    group = L.group . L.sort

Instead of going through each of the properties one by one, I'll just use the test-framework package and put the tests there.


~~~{.haskell}
module Main where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Grouping (group)
import Data.List (sort)

main = defaultMain tests

tests = [
    testGroup "Grouping" [
          testProperty "All original elements present" prop_all_elements
        , testProperty "All grouped elements are the same" prop_grouped_are_grouped
        , testProperty "All elements are in their buckets" prop_all_elements_in_group
      ]
  ]

prop_all_elements xs = sort xs == (sort . concat . group) xs
  where types = (xs :: [Int])

prop_grouped_are_grouped xs = all same grouped
  where grouped = group (xs :: [Int])
        same [] = True
        same (x:xs) = all (== x) xs

prop_all_elements_in_group xs = all allElementsInGroups grouped
  where grouped = group (xs :: [Int])
        totalElements x = length $ filter (== x) xs
        allElementsInGroups [] = True
        allElementsInGroups (y:ys) = length (y:ys) == totalElements y
~~~

And now the results

    *Main> :main
    Grouping:
      All original elements present: [OK, passed 100 tests]
      All grouped elements are the same: [OK, passed 100 tests]
      All elements are in their buckets: [OK, passed 100 tests]
    
             Properties  Total      
     Passed  3           3          
     Failed  0           0          
     Total   3           3          
