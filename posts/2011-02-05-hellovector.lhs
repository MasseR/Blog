---
title: Hello genetic algorithms
tags: haskell,ai,ga
---
I'm doing a series of posts about me studying Haskell and various algorithms.
This is so that I can later remember what was going on in my mind when doing
them and why and how I did them. If other people can gain something from these,
then all the better. These posts are all literate haskell and can be copied and
executed directly.

Genetic algorithms is a subset of artificial intelligence. It tries to mimic
the real world evolution with keywords population, generation, mating and
mutation. Genetic algorithms are good for finding optimizations for difficult
problems.

There are many good articles and tutorials on the net, so I won't explain it
deeply. Probably the most helpful link with java applets was
[this](http://www.obitko.com/tutorials/genetic-algorithms/). This algorithm is
heavily based on a genetic algorithm tutorial from
[generation5](http://www.generation5.org/content/2003/gahelloworld.asp). The
basic gist is this. At first we create the initial population with random
values. Then for every generation we sort them through their fitness values,
have them mate giving higher probability for the better individuals and then
mutating the chromosomes.

Usually the data is fitted into binary representation, but I have kept the
chromosomes as a vector of characters. The mating is done by selecting randomly
two individuals and a random point inside the string. The fathers chromosomes
are taken until the point, and the rest are the mothers. For example if we have
the words  "hello zyrkd" and "hhljo world" and the split point is 5, the child
would be "hello world". This is simple and fast, but might not be as efficient
as some other methods.

Actually "efficient" is a difficult concept with this, as the population is
great and it takes some time to run every generation, fast algorithms can be
worse than slower but more _efficient_ algorithms. For example, for my first
implementation C I tried with a simple fitness function and levenshtein
distance. When using the levenshtein distance, the generation converged a lot
sooner, but the cost to calculate the distance was too great.

Using binary representation really pays off when doing mutation as you can just
flip the bit. With our representation we need to split the string, create a
random character, and insert between the splitted string.

I am not an expert in Haskell, and most of the algorithms I'm showing are
something new and shiny for me and I might have a proper understanding of them
when writing these posts. Always do your own research when reading these. Let's
start with the the mandatory imports.

> import System.Random
> import Control.Monad.State
> import Data.Ord
> import Data.Vector (Vector, (!))
> import qualified Data.Vector as V
> import Data.List (sortBy)

I'm using Vector from the [vector](http://hackage.haskell.org/package/vector)
package. It's fast and does stream fusion so it's a good structure for getting
some speed improvements. The `Maybe Int` is such because there is a high chance
that we have already calculated the fitness for the word.

> type StringVector = Vector Member
> data Member = Member (Maybe Int) (Vector Char)

We are hardcoding the target string. We could have it taken from arguments, but
that would make this unnecessary complex. And as the title suggests, this is a
"Hello world" tutorial.

> target = V.fromList "Hello world"
> population = 1000

Haskell is famous for making easy things difficult and using random numbers is
definitely complex. Haskell random number generator is pure, meaning it returns
always the same value with the same input. The random generator takes a
generator and returns a tuple with the new generator and the random value.
Saving the generator and giving it as an argument would be painstaking, so we
create a stateful representation of random number generators. For details see
[lyah](http://learnyouahaskell.com/for-a-few-monads-more#state).

> randomSt :: (RandomGen g, Random a) => State g a
> randomSt = state random
> randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
> randomRSt = state . randomR

The next function is interesting. If you take a uniform random function
generator, like for example a 1d6 dice, you'd get with equal chance the numbers
1 to 6. However when you have two dices, you'd get random numbers from 2 to 12,
with 2 being less probable than 6. You'd need to get two ones if you want to
get 2. However to get 6, you'd need 1 and 5, 2 and 4, 3 and 3 etc. If you were
actually to graph it, you'd get a triangular histogram with the point being in
the middle.

But we wanted the beginning of the population to have better probability than
the middle, so how would we move it to the beginning? By subtracting from it.

> wrandomRSt :: (RandomGen g) => Int -> State g Int
> wrandomRSt n =
>   let s = liftM2 (+) (randomRSt (0.0, 1.0)) (randomRSt (0.0, 1.0)) :: (RandomGen g) => State g Float
>       n' = fromIntegral n
>   in liftM (flip div 2 . floor . abs . subtract n' . (n' *)) s

> randomword :: (RandomGen g) => State g (Vector Char)
> randomword = V.replicateM (V.length target) (randomRSt (' ', 'Z'))

Next we create the initial population. Remember that population was an integer
set to 1000, so we replicate a thousand random words lifted into the Member
datatype.

> initialPopulation :: (RandomGen g) => State g StringVector
> initialPopulation = V.replicateM population (Member Nothing `fmap` randomword)

The fitness function calculates the distance between two words. The algorithm
isn't as good as levenshtein distance, but gives us a wider range of fitness
values and is good enough. It calculates the distance by summing the distance
between two characters. For example the distance between 'a' and 'c' would be 2
and 'c' and 'd' 1.

> fitness ::  (Enum t) => Vector t -> Int
> fitness x' = V.sum (V.map (\(a,b) -> abs $ fromEnum a - fromEnum b) $ V.zip target x')

The vector package doesn't have any sort implementation, and I didn't find it
important enough to implement it by myself, instead I basically convert to
list, use the builtin sort function and convert back into a vector. During
conversion to list I check whether member has any fitness data on it, and if it
doesn't we calculate the fitness. This is where I got a lot of improvement from
the previous version. In the profile data from previous version, the program
spent >50% of the time in fitness function, but now that we cache the fitness
data, it's been reduce to around 10%.

> sortByFitness xs = V.fromList $ sortBy (\(Member a _) (Member b _) -> compare a b) $ calculateFitnesses xs
>   where calculateFitnesses = V.foldl' calculateMaybeFitness []
>	  calculateMaybeFitness :: [Member] -> Member -> [Member]
>	  calculateMaybeFitness acc m@(Member (Just _) _) = m : acc
>	  calculateMaybeFitness acc (Member Nothing v) = Member (Just (fitness v)) v : acc

Like explained above, the mating function finds random father and random mother
and combines their chromosomes.

> mate :: (RandomGen g) => StringVector -> State g StringVector
> mate xs = V.replicateM population (step xs)
>   where
>     step :: RandomGen g => StringVector -> State g Member
>     step xs =
>       let mom = liftM (xs !) (wrandomRSt population)
>	    dad = liftM (xs !) (wrandomRSt population)
>	    split = randomRSt (0, V.length target - 1)
>       in do
> 	(Member _ m) <- mom
> 	(Member _ d) <- dad
> 	s <- split
> 	return (Member Nothing (V.take s m V.++ V.drop s d))

Not all genetic algorithms use elitism but it prevents us from going
"backwards". With elitism we leave the first 100 untouched, but they are still eligible to be parents.

> elite = population `div` 10
> elitism :: (RandomGen g) => StringVector -> State g StringVector
> elitism xs = let
>   a = V.take elite xs
>   children = V.take (population - elite) `fmap` mate xs
>   in do
>     b' <- children >>= mutate
>     let xs' = (a V.++ b')
>     return xs'

Mutation is a simple act, but the implementation looks daunting. The basic idea is that we only mutate a certain percentage. When a member is mutated, it is splitted from a random point. The pre and post blocks are saved and the middle is discarded. A new random middle character is taken and inserted in the middle of the pre and post blocks.

> mutate :: (RandomGen g) => StringVector -> State g StringVector
> mutate xs = V.mapM mut xs
>   where
>     mut :: (RandomGen g) => Member -> State g Member
>     mut xs' = do
>       r <- randomRSt (0,100) :: ((RandomGen g) => State g Int)
>       if r < 15 then mutate' xs' else return xs'
>     mutate' :: (RandomGen g) => Member -> State g Member
>     mutate' member@(Member _ xs') = do
>       x <- randomRSt (0, V.length xs'-1)
>       n <- randomRSt (' ', 'z')
>       let a = V.take x xs'
>	    b = V.drop (x+1) xs'
>	    c = (Member Nothing (a V.++ V.singleton n V.++ b))
>       return c

The evolve function steps through every generation until we reach the desired
value. As this is a simple example, we know exactly when we've reached the
optimal value, but this is not always the case. For example when using genetic
algorithms to solve the traveling salesman problem, how do we know that our
solution is worse than a solution 1000 generations later. In these situations
it's perfectly acceptable to use some heuristics to determine when to stop, or
just stop after n iterations. For example when I tested genetic algorithms for
solving tsp, I evolve into next generation until there's been no improvement
for 50 generations.

> evolve :: (RandomGen g) => State g [(Int, String, String)]
> evolve = do
>   p <- initialPopulation
>   step p
>   where
>     step p =
>       let (Member _ best) = V.head sorted
>	    (Member _ worst) = V.last sorted
>	    sorted = sortByFitness p
>       in
> 	if best == target
> 	   then return [(fitness best, V.toList best, V.toList worst)]
> 	   else do
> 	     pp <- elitism sorted
> 	     next <- step pp
> 	     return ((fitness best, V.toList best, V.toList worst) : next)

> main = do
>   g <- getStdGen
>   mapM_ print $ evalState evolve g

Example output could be:

    (288,"3RZNT6VNKTN",")$!A'@%4&0'")
    (288,"3RZNT6VNKTN","+0.Y,\\/9 W>")
    (265,"LUUV4-QUVSo","U8OD#=W Z,8")
    (240,"PYVPX4URZOY","<VCNSoUR\"Q*")
    ...
    (1,"Helln world","Hellnzworld")
    (1,"Helln world","Helln &orld")
    (1,"Helln world","Hellnsworld")
    (0,"Hello world","Helln wo ld")


*Edit:* Clarified the paragraph about random numbers.
