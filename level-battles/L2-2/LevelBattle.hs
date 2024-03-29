-- ========================================================================== --
{- |
# PUH Level 2 Battle

  So you failed your second PUH Battle...

  Don't worry yet, there's still hope. This is your second chance.

  The rules are the same, tasks similar, the idea the same.

  This battle consists of 3 tasks. You must successfully solve all of them in
  order to win the battle. Every task has a single function in the form of
  `lb2x`, e.g. for the first task you have to write a function `lb21`.

  The output will be tested both automatically and manually, which means you must
  take care of the output formatting (spaces, punctuation etc.)

  Do not change provided type signatures! They're here to help you. Use them
  wisely.

  Finally, you are allowed (encouraged, even) to **create helper functions** as
  you see fit. However, every helper function **must** have type signatures
  defined.

  The deadline is Saturday, 10 November 2018 at 12pm (noon).


  You can use cabal or GHCi.

  To use cabal, run

  >> cabal repl

  which will run repl. To reload, type `:r`.

  If that's not working for you, run

  >> ghci

  and load the Level Battle:

  >> :l LevelBattle


  Although you are not allowed to ask for help in solving the exercises, if you
  have any trouble with infrastructure, running repl, or anything similar,
  **please ask for help on Slack** (but do it in public channels, e.g. `#haskell`).

  Good luck!
-}
-- ========================================================================== --

module LevelBattle where
--
import Data.List
import Data.Char
--

-- * L 2.1
--
-- | Write a tail recursive function counting the vowels (defined as a, e, i, o and u) in a sentence.
-- Example:
-- lb21 "A short sentence" = 5


lb21 :: String -> Int
lb21 xs = vowCount xs 0
  where vowCount []     n = n
        vowCount (x:xs) n = case toLower(x) of 
            'a' -> vowCount xs (n + 1)
            'e' -> vowCount xs (n + 1)
            'i' -> vowCount xs (n + 1)
            'o' -> vowCount xs (n + 1)
            'u' -> vowCount xs (n + 1)
            _   -> vowCount xs n

-- ** L 2.2
--
-- | Write a function for converting a positive integer number (and 0) to a binary string.
-- Example:
-- lb22 9 = "1001"
-- lb22 -3 = error "Converting negative numbers to decimal is too much of a hassle"

lb22 :: Int -> String
lb22 n 
  | n <  0 = error "Converting negative numbers to decimal is too much of a hassle"
  | n == 0 = "0"
  | otherwise = toBinary n
  where toBinary 0 = ""
        toBinary n = (toBinary $ n `div` 2) ++ (show $ n `mod` 2)

-- ** L 2.3
--
-- | Implement a tail recursive function for integer division in terms of substraction.
-- This implementation should work correctly for positive numbers and 0 (don't worry
-- about negative numbers). In case of division by 0, throw an error.

lb23 :: Int -> Int -> Int
lb23 a b = divSub a b 0
  where divSub _ 0 _ = error "divide by zero"
        divSub a b c
          | a < b     = c
          | otherwise = divSub (a-b) b (c+1)