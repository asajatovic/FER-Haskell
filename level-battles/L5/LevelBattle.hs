-- ========================================================================== --
{- |
= PUH Level 5 Battle

  Welcome to your fifth PUH Battle. You know the drill. Solve **all tasks**
  to win and follow the instructions. If you think something is ambiguous, **ask
  for clarification in the main slack channel (#haskell)**. Follow the type
  signatures and don't modify them.

  Hope you had a good rest over the holidays, let's do this!

-}
-- ========================================================================== --


module LevelBattle where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified System.Random as R

  --

{- ** LB 5.1 -}

{-
  People love winning stuff. But there are different types of people. We should define
  for each type of person the winning criterie. Sounds as an use case for custom type classes.
  We defined a `Person` and `SlightlyMoreLuckyPerson` data types. 

  1) Define a custom typeclass called `Winnable a` whose instances can be winners.
      It should only have one method `isWinner` with type signature `a -> Bool`.
  2) Define an instance of `Winnable` for `Person` and `SlightlyMoreLuckyPerson`.
    2a) `Person` wins if it's `number` is `3`.
    2b) `SlightlyMoreLuckyPerson` wins if it's `name` is longer than 3.
-}

data Person = Person { personId :: Int, name :: String, number :: Int} deriving (Show, Eq)
data SlightlyMoreLuckyPerson = SlightlyMoreLuckyPerson { luckyName :: String } deriving Show
  
-- class Winnable a where

-- instance Winnable Person where

-- instance Winnable SlightlyMoreLuckyPerson where


{- ** LB 5.2 -}

{-
  Let's take a more serious approach towards winning stuff. First we would like to keep the list of
  our potential winners in a file. Of course we would need a file format first.

  Let's create our function for parsing a `;` separated string and returning a `Person` instance.
  1) Define the `parseSsv` function that splits a string on `;` char.
  2) Define an instance of the `Read` typeclass that utilizes that function to enable reading `Person`
     from string.
     - Start with `instance Read Person where` and define the `readPrec` function.
     - How `readPerc` works can be found http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:readsPrec
       - readsPrec :: Read a => Int -> String -> [(a, String)]
       - First parameter isn't important for this task. The second parameter is the raw input string.
       - `readsPrec` returns a singleton list containing a tuple with our `Person` and a `String`.
       - `String` is the leftover which could not be parsed. For our purpose, always returning `""` is OK.
  How do you know you are done?
  It would enable you to write `read "1;Zoran Sumadinac;5" :: Person` and get a `Person`.
-}

parseSsv :: String -> [String]
parseSsv s = undefined

-- instance Read Person where


{- ** LB 5.3 -}

{- 
  Okay. Now we know how to parse a string and get a `Person`. It's time to load people from a file.

  1) Define a function `getsPeople` that takes a file path and returns a `Map` of `Person` mapped by
     their `personId`.
  
  `Data.Map.fromList` is your friend here. Don't forget we can `read` a `Person` from the line. Example `list.txt`
  can be loaded by `getsPeople "./list.txt"`.
-}

getsPeople :: FilePath -> IO (Map Int Person)
getsPeople path = undefined

{- ** LB 5.4 -}

{-
  Since we have a `Map` of people why not choose a random `Person` to win some stuff?

  1) Define a function `randomListElement` based on a list gives you an random element.
  2) Define a function `selectRandomPerson` that gets a `Map` of `Person` and gives you a
      random winner. It should do it by select a random key first from all keys.
-}

randomListElement :: [a] -> IO a
randomListElement xs = undefined

selectRandomPerson :: Map Int Person -> IO (Maybe Person)
selectRandomPerson p = undefined

{- ** LB 5.5 -}

{-
  Let's do some validation with Maybe monad. The behaviour we are looking for is: exit as soon as possible.

  1) Write `notNegative` function that validates if a number is negative.
    It returns `Just Int` if OK or `Nothing`.
  2) Write `notEmpty` function that validate if a string is empty.
    It returns `Just String` if OK or `Nothing`.
  3) Write `validateBattle` function that validates a battle.
    3a) There should be 3 or more solved tasks.
    3b) Merge request name should be "level-5"
  4) Write a `createBattle` function that receives an `Int` and `String` and returns `Just Battle` if OK or `Nothing`.
    4a) Write the function using a `do` block syntax sugar.
    4b) Validate first the number of tasks then merge request name and finally validate the battle.

  Checking out the desugared versions of a `do` block in the lectures will help you alot in understanding how to write this
  to exit as *soon as possible*. 
-}

data Battle = Battle {solvedTasks :: Int, mergeRequestName :: String} deriving (Show, Eq)

notNegative :: Int -> Maybe Int
notNegative n = undefined

notEmpty :: String -> Maybe String
notEmpty s = undefined

validateBattle :: Battle -> Maybe Battle
validateBattle b = undefined

createBattle :: Int -> String -> Maybe Battle
createBattle st' mrn' = undefined