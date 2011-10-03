---
title: Haskell game architecture part I
tags: haskell, games, tests
---
I've had a big break from Haskell and it has allowed me to ferment the basic
ideas of it. I think that before I tried to do too much with monads and the
code wasn't clear nor easy to reason. Reading [a beginners musings about
writing an elemental system in
Haskell](http://alfredodinapoli.wordpress.com/2011/09/12/lets-make-an-elemental-type-system-in-haskell-part-i/)
made me think how I would do the same thing. After sleeping a night over it, I
also began to think, how the overall game world could be represented and
advanced.

At first I imagined using a state monad and updating it with every action. This
would require me keeping the entire state with me all the time which would
reduce clarity. Instead I remembered reading the sourcecode of a Haskell irc
server, which used the writer monad and 'actions'. Also reading [purely
functional retrogames](http://prog21.dadgum.com/23.html) mentioned that
threading the bare minimum is the way to go.

Another thing I tried to do was to think in types, meaning that I'll try to
write the types _before_ writing the implementation. Of course this being a
written post, all you'll see is the completed result, but hopefully behind the
scenes, I managed to keep this up.

The system I'm trying to model is somewhat similar to what a roguelike might
have. Turn based actions, turns having duration. Because of the duration,
faster actions should be done first. This prompts for a sorted list, or even
better, a priority queue. One turn is equal to player speed. This means that
for every turn we execute the actions that have their duration less than player
speed.

> import qualified Data.PQueue.Min as P
> import qualified Data.Map as M
> import Data.Map (Map)
> import Control.Monad.Writer
> import Control.Arrow

> type BaseDamage = Int
> type Hp = Int
> type Mp = Int
> type Ac = Int
> type Speed = Int
> type TargetEntity = Entity
> type FromEntity = Entity
> type Name = String
> type Location = (Int, Int)

The entity means for example the player, but it can also mean the monster. For
now I try to keep this simple and not think about inventories or status
effects. The entity consists of a player name, their health and mana. If this
first try is a success, I might implement a proper entity and magic system,
which I've been thinking for quite a while now, but this is enough for proof of
concept.

> data Entity = Entity {
>     entityName :: Name
>   , entityHp :: Hp
>   , entityMp :: Mp
>   , entityLoc :: Location
> } deriving (Eq, Show, Ord)
> data Direction = MoveLeft | MoveRight | MoveUp | MoveDown deriving (Eq, Ord, Show)

The `Event` is the thing that's happening. These are the things we're collecting
the the priority queue, but we'll wrap them in the `Action`. By separating the
two, we can use the type system to enforce the invariant that every event needs
to have a speed associated with it.

> data Event = Move Entity Direction
>            | PhysicalAttack FromEntity TargetEntity BaseDamage deriving (Eq, Ord, Show)
> data Action = Action Speed Event deriving (Eq, Show)

Sorting the events is important, so we need to instantiate the Ord class for
the action datatype. We could automatically derive the instance because the
speed is the first parameter for the type, but this way is more explicit, and
we don't get odd bugs if for some reason at some time we change the order of
the parameters.

> instance Ord Action where
>   (Action a _) `compare` (Action b _) = a `compare` b

Let's test this out by creating some entities and actions and testing how they
work out.

~~~
ghci> foldr (P.insert) P.empty [physAtk, escape]
fromAscList [Action {speed = 3, event = PhysicalAttack (Entity "Idefix" 20 0) (Entity "Masse" 100 0) 10},Action {speed = 10, event = Move (Entity "Masse" 100 0) MoveLeft}]
ghci> foldr (P.insert) P.empty [escape, physAtk]
fromAscList [Action {speed = 3, event = PhysicalAttack (Entity "Idefix" 20 0) (Entity "Masse" 100 0) 10},Action {speed = 10, event = Move (Entity "Masse" 100 0) MoveLeft}]
~~~

Seems like that in both cases, attacking first or escaping first, the attack comes first.

> masse = Entity "Masse" 100 0 (0,0)
> dog = Entity "Idefix" 20 0 (0,1)
> physAtk = Action 3 $ PhysicalAttack dog masse 10
> escape = Action 10 $ Move masse MoveLeft

We intended to use the writer monad for threading through the actions. For the
writer monad to work, our target type needs to have a monoid instance, which
PQueue luckily has.

> type EventQueue = P.MinQueue Action
> type Events a = Writer EventQueue a

We don't have any modifiers for attacks, nor their speed, so we can create
simple functions to test our monad instance.

> attack :: FromEntity -> TargetEntity -> BaseDamage -> Events ()
> attack from trg dmg = tell $ P.singleton $ Action 3 $ PhysicalAttack from trg dmg
> move :: Entity -> Direction -> Events ()
> move e d = tell $ P.singleton $ Action 10 $ Move e d

Let's try it again to make sure that everything is working as expected.

~~~
ghci> execWriter  $ (attack dog masse 10 >> move masse MoveLeft)
fromAscList [Action 3 (PhysicalAttack (Entity "Idefix" 20 0) (Entity "Masse" 100 0) 10),Action 10 (Move (Entity "Masse" 100 0) MoveLeft)]
~~~

Next we're going to implement the part that takes the list of events, and
modifies the game state accordingly. For this we need to have a speed of the
round, and somewhere to keep the results. The state I present here, is not the
best possible. First of all, our game world is empty and the entities are held
in a map of `String -> Entity`.

> data GameState = GameState {
>     entities :: Map String Entity
>   , player :: String
> } deriving Show

We can see that our advance function should have the type `GameState ->
EventQueue -> (GameState, EventQueue)`, but let's break it down to smaller
functions first. First of all, we haven't properly implemented the player speed
yet, so let's create a simple constant for now.

> playerSpeed = 7

We also need a function that takes all the events that happen in the current
turn and modifies the rest of the actions so that their duration is decreased.
The `happeningEvents` function does just that. The `actionEvent` function is
just a getter for the actual event from the action wrapper. The
`happeningEvents` function uses arrows to run the two methods, for the first
item it maps the `actionEvent` and on the second item it maps the `passTime`
function.

> actionEvent :: Action -> Event
> actionEvent (Action _ e) = e

> passTime :: Speed -> Action -> Action
> passTime s (Action s' e) = Action (s' - s) e

> happeningEvents :: Speed -> EventQueue -> ([Event], EventQueue)
> happeningEvents s = (map (actionEvent) *** P.map (passTime s)) . P.span (\(Action s' _) -> s' <= s)

The `changeLoc` function is for changing the entities
coordinates. I use a traditional coordinate system, where x is the horizontal
line and y is the vertical. Negative x is on the left and negative y is on the
bottom. The x-coordinate is the first item in the tuple and y is the second.

> changeLoc :: Direction -> Location -> Location
> changeLoc MoveLeft  = first pred
> changeLoc MoveRight  = first succ
> changeLoc MoveUp  = second succ
> changeLoc MoveDown  = second pred

For the actual event processing we need to go through each and every event and
run their effects. For the effects, we create the function `runEvent` which
uses pattern matching to update the game state.

> runEvent :: Event -> GameState -> GameState
> runEvent event (GameState m p) = GameState (doEvent event m) p
>   where
>     doEvent (PhysicalAttack _ to dmg) m = M.adjust (\p -> p{ entityHp = (entityHp p - dmg) }) (entityName to) m
>     doEvent (Move ent dir) m = M.adjust (\p -> p{ entityLoc = changeLoc dir (entityLoc p) }) (entityName ent) m

And now we finally get to the advance function. I use the `first` function to
fold the event effects to the game state, and the remaining event queue is the
one left from the `happeningEvents` function.

> advance :: GameState -> EventQueue -> (GameState, EventQueue)
> advance state queue = first (foldr runEvent state) $ happeningEvents playerSpeed queue

For testing the functions, I created an initial game state containing the two
entities defined earlier and a couple of actions. For the result we should see
that only one of the actions is ran and that the movement action should have a
duration of 3. For the effects, we should see that the hp for masse has reduced
to 90, and otherwise nothing should have happened.

> gameState = GameState (M.fromList $ map (\x -> (entityName x, x)) [masse, dog]) "masse"
> actions = execWriter $ (attack dog masse 10 >> move masse MoveLeft)

~~~~
ghci> gameState
GameState {entities = fromList [("Idefix",Entity {entityName = "Idefix", entityHp = 20, entityMp = 0, entityLoc = (0,1)}),("Masse",Entity {entityName = "Masse", entityHp = 100, entityMp = 0, entityLoc = (0,0)})], player = "masse"}
ghci> advance gameState actions
(GameState {entities = fromList [("Idefix",Entity {entityName = "Idefix", entityHp = 20, entityMp = 0, entityLoc = (0,1)}),("Masse",Entity {entityName = "Masse", entityHp = 90, entityMp = 0, entityLoc = (0,0)})], player = "masse"},fromAscList [Action 3 (Move (Entity {entityName = "Masse", entityHp = 100, entityMp = 0, entityLoc = (0,0)}) MoveLeft)])
~~~~
