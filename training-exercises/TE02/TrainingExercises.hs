-- =============================================================================== --
{- |
  Welcome to the extra segment of your second Haskell training.

  This one is meant to fully prepare you for your upcoming level battle.
  You do not have to do it, but if you want to practice a bit more before taking
  on the battle, feel free to give it your best.

  As always, ask your TA if you need any help.
-}
-- =============================================================================== --
--
module TrainingExercises where
--
import Data.List hiding (lookup)
import Prelude hiding (lookup)
import Data.Char
--

{- * 2 DIY KEY-VALUE STORAGE

   In this exercise, you will have to implement some polymorphic utility functions
   for working on lists of key-value pairs where keys are strings. Think of
   the list as a simple key-value storage.

   When solving the problem, look at the provided type signatures and try to
   figure out how the function should work. This time you don't have any tests
   to help you.

   Remember: in functional programming, objects are immutable. When "adding" an
   element to a list, you're actually creating a new list with that new object.
 -}

-- ** TE 2.1
--
-- | Write a function for getting a pair with a certain key which should return
-- a list with a single element as a result (or no elements if the key doesn't
-- exist):
findItem :: [(String, a)] -> String -> [(String, a)]
findItem list key =
  if null list 
    then list
    else if fst (head list) == key 
      then [head list] 
      else findItem (tail list) key

-- ** TE 2.2
--
-- | Write a function that checks if a list contains an element with a certain key:
contains :: [(String, a)] -> String -> Bool
contains list key = not $ null (findItem list key)

-- ** TE 2.3
--
-- | Write a function that tries to retrieve a value with a certain key or throws an error if
-- the key doesn’t exist (example of error function usage : error "I’m an error
-- message"):
lookup :: [(String, a)] -> String -> a
lookup list key =
  if null list
    then error "I’m an error message"
    else if fst (head list) == key 
      then snd (head list) 
      else lookup (tail list) key

-- ** TE 2.4
--
-- | Write a function that inserts a new key value pair. If key already exists than do nothing:
insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert list pair = if not (contains list (fst pair)) then pair : list else list

-- ** TE 2.5
--
-- | Write a function that removes a key value pair with the certain key:
remove :: [(String, a)] -> String -> [(String, a)]
remove list key = [pair | pair <- list, key /= fst pair ]

-- ** TE 2.6
--
-- | Write a function that updates the value of a certain key (if the key doesn’t exist,
-- the function does nothing) :
update :: [(String, a)] -> String -> a -> [(String, a)]
update list key value = [if key == fst pair then (key, value) else pair  | pair <- list]
