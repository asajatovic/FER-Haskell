-- =============================================================================== --
{- |
  Welcome to your tenth Haskell training.

  You should know the drill by now - solve the exercises in `TrainingExercises.hs`,
  run the tests with Cabal, push to `training-10`, create a Merge Request,
  and assign it to your TA.

  Keep in mind that **all exposed functions and instance definitions must have
  type signatures**.
  (helper functions that are defined in local definitions don't)!

  When defining class type instances, replace the `TODO instance...` with your
  definition.

  Also there aren't any extra tasks - all tasks are mandatory.

  Don't worry about the performance of your code.

  As always, ask your TA if you need any help.

  Happy holidays!
-}
-- =============================================================================== --
--
module TrainingExercises where
--
import Data.Char
import Data.List
import Data.Ord
--

{- * 10.1 Recursive data types and type class instances  -}

-- ** TE 10.1.1

-- Take a look at the definition of MyList data type from the lecture:
data MyList a = Empty | Cons a (MyList a) deriving Show

-- Define an 'Eq' instance for the MyList type so that two lists are considered
-- equal if they have the same elements regardless of their order.
-- Hint: think about a helper function that could help you here.

-- TODO instance ...

{- No order solution
instance Eq a => Eq (MyList a) where  
  Empty     == Empty       = True
  Cons x xs == Cons x' xs' = x == x' && xs == xs'
  _         == _           = False 
-}

instance (Ord a, Eq a) => Eq (MyList a) where
  l1 == l2 = (sort (myListToList l1)) == (sort (myListToList l2))

myListToList :: MyList a -> [a]
myListToList Empty       = []
myListToList (Cons y ys) = y:(myListToList ys)
  
-- ** TE 10.1.2

-- Take a look at the Christmas Tree data type - it's your normal tree, except
-- that it's got a new skin so the leafs have ornaments, and nodes have lights.
data ChristmasTree a = Ornament
            | Light { value :: a,
                    left  :: ChristmasTree a,
                    right :: ChristmasTree a }
  deriving Show

-- Define a function that takes a Christmas tree and returns a list containing
-- all elements in their in-order traversal.

treeToList :: ChristmasTree a -> [a]
treeToList = convertTreeToList [] 
  where convertTreeToList xs Ornament = xs
        convertTreeToList xs node     = (convertTreeToList xs (left node))++[value node]++(convertTreeToList xs (right node))

-- ** TE 10.1.3
-- Let's say that two trees are equal if they contain the same elements
-- regardless of their order. Define an 'Eq' instance for the ChristmasTree type
-- that implements that notion of equality.

-- TODO instance ...

{- No order solution
instance Eq a => Eq (ChristmasTree a) where
  Ornament               == Ornament                  = True
  Light value left right == Light value' left' right' = 
    (value == value' && left == left' && right == right') ||
    (value == value' && left == right' && right == left')
  _                      == _                         = False
-}

instance (Ord a, Eq a) => Eq (ChristmasTree a) where
  t1 == t2 = (sort (treeToList t1)) == (sort (treeToList t2))
