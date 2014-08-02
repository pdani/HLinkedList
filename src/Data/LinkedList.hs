{-# OPTIONS_GHC -F -pgmF htfpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LinkedList
-- Copyright   :  (c) Daniel Pek 2014
-- License     :  see LICENSE
--
-- Maintainer  :  pekdaniel@gmail.com
--
-- Simple implementation of a simply-linked list with reverse
--
-----------------------------------------------------------------------------

module Data.LinkedList
    ( LinkedList (..)
    , reverse
    , reverseTail
    , htf_thisModulesTests
    ) where

import Prelude hiding (reverse)
import qualified Data.Foldable as F
import qualified Data.List as L

import Test.Framework

-- | 'LinkedList' is an abstract datatype representing a recursive data
-- structure which is isomorphic with Haskell lists.
--
data LinkedList a = Node { value :: a
                         , next :: LinkedList a
                         }
                  | Nil
  deriving (Show, Eq)

-- By implementing foldr, we get foldl and a bunch of other methods for free
instance F.Foldable LinkedList where
    foldr _ z Nil = z
    foldr f z (Node x xs) = f x (F.foldr f z xs)

-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- @xs@ must be finite.
reverse :: LinkedList a -> LinkedList a
reverse = F.foldl' (flip Node) Nil

-- | 'reverseTail' is the explicit tail-recursive implementation of 'reverse'
reverseTail :: LinkedList a -> LinkedList a
reverseTail Nil = Nil
reverseTail xs = helper xs Nil
  where helper Nil a = a
        helper (Node x xs) a = helper xs (Node x a)

-- | 'fromList' @xs@ takes the list @xs@ and transforms it into a LinkedList
fromList :: [a] -> LinkedList a
fromList = (L.foldl' (flip Node) Nil) . L.reverse

-- Tests

-- Needed for QuickCheck properties
instance Arbitrary a => Arbitrary (LinkedList a) where
    arbitrary = fromList `fmap` (arbitrary :: Arbitrary a => Gen [a])
    
-- With concrete input values on 'reverse'
test_11reverse_nil = assertEqual (reverse Nil) (Nil :: LinkedList Int)
test_12reverse_single = assertEqual (reverse $ Node 1 Nil) $ Node 1 Nil
test_13reverse_multiple = assertEqual (reverse $ Node 1 $ Node 2 $ Node 3 $ Nil) $ Node 3 $ Node 2 $ Node 1 $ Nil
test_14reverse_twice = assertEqual (reverse $ reverse $ Node 1 $ Node 2 $ Node 3 $ Nil) $ Node 1 $ Node 2 $ Node 3 $ Nil

-- With random input values on 'reverse'
prop_21reverse_nil = Nil == reverse (Nil :: LinkedList Int)
prop_22reverse_single :: Int -> Bool
prop_22reverse_single x = Node x Nil == (reverse $ Node x Nil)
prop_23reverse_multiple :: [Int] -> Bool
prop_23reverse_multiple xs = (fromList $ L.reverse xs) == (reverse $ fromList xs)
prop_24reverse_twice :: LinkedList Int -> Bool
prop_24reverse_twice xs = xs == (reverse $ reverse xs)

-- With concrete input values on 'reverseTail'
test_31reverseTail_nil = assertEqual (reverseTail Nil) (Nil :: LinkedList Int)
test_32reverseTail_single = assertEqual (reverseTail $ Node 1 Nil) $ Node 1 Nil
test_33reverseTail_multiple = assertEqual (reverseTail $ Node 1 $ Node 2 $ Node 3 $ Nil) $ Node 3 $ Node 2 $ Node 1 $ Nil
test_34reverseTail_twice = assertEqual (reverseTail $ reverseTail $ Node 1 $ Node 2 $ Node 3 $ Nil) $ Node 1 $ Node 2 $ Node 3 $ Nil

-- With random input values on 'reverseTail'
prop_41reverseTail_nil = Nil == reverseTail (Nil :: LinkedList Int)
prop_42reverseTail_single :: Int -> Bool
prop_42reverseTail_single x = Node x Nil == (reverseTail $ Node x Nil)
prop_43reverseTail_multiple :: [Int] -> Bool
prop_43reverseTail_multiple xs = (fromList $ L.reverse xs) == (reverseTail $ fromList xs)
prop_44reverseTail_twice :: LinkedList Int -> Bool
prop_44reverseTail_twice xs = xs == (reverseTail $ reverseTail xs)
