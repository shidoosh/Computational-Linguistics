module Memoization (memoize, memoize2, lift0, lift1, lift2, liftMany, TableBased) where

import Control.Monad
import qualified Data.Map as Map

---------------------------------------------
-- Here's everything you need to know about the interface 
-- for using this module.

-- The type 'TableBased c v a' is a fancy version of the 
-- type 'a'. More specifically, a thing of this type is 
-- like a thing of type 'a', except it's calculated in 
-- such a way that it makes use of a memoized lookup 
-- table that maps things of type 'c' (for "cells") 
-- to things of type 'v' (for "values"). For short, 
-- think of 'TableBased c v a' as "memoization-based a".

-- The 'lift' functions let you take functions that you've written 
-- for working with "normal" things and use them for working with 
-- memoization-based things.
--   lift0 :: x -> TableBased c v x
--   lift1 :: (x -> y) -> TableBased c v x -> TableBased c v y
--   lift2 :: (x -> y -> z) -> TableBased c v x -> TableBased c v y -> TableBased c v z
--   liftMany :: ([a] -> a) -> [TableBased c v a] -> TableBased c v a

-- In particular the 'lift' functions are useful for writing 
-- "de-recursive-ized" versions of recursive functions. Then 
-- you can create memoized versions of the recursive function 
-- using the 'memoize' functions. These 'memoize' functions 
-- "tie up" a de-recursive-ized function to make it recursive, 
-- like just 'fix' does, but they add in the memoization 
-- along the way.
--   type Booster a = (a -> a)
--   fix :: Booster a -> a
--   memoize :: (Ord c) => Booster (c -> TableBased c a a) -> c -> a
--   memoize2 :: (Ord c1, Ord c2) => Booster (c1 -> c2 -> TableBased (c1,c2) a a) -> c1 -> c2 -> a

---------------------------------------------
-- A demo example: fibonacci

fib :: Int -> Int
fib n = case (n <= 1) of
        True -> 1
        False -> fib (n-1) + fib (n-2)

fastFib :: Int -> Int
fastFib = memoize derecFib

derecFib :: (Int -> TableBased c v Int) -> (Int -> TableBased c v Int)
derecFib smaller n = case (n <= 1) of
                     True ->  lift0 1
                     False -> lift2 (\x -> \y -> x + y) (smaller (n-1)) (smaller (n-2))

---------------------------------------------
---------------------------------------------
---------------------------------------------
---------------------------------------------
---------------------------------------------
-- IMPLEMENTATION DETAILS FROM HERE ON
---------------------------------------------

data TableBased c v a = MkTableBased (Map.Map c v -> (a, Map.Map c v))

instance Functor (TableBased c v) where
    fmap = liftM
instance Applicative (TableBased c v) where
    pure x = MkTableBased (\n -> (x,n))
    (<*>) = ap
instance Monad (TableBased c v) where
    -- DIY state monad
    (MkTableBased fa) >>= k =
        MkTableBased $ \n ->
            let (a,n') = fa n in
            let (MkTableBased fb) = k a in
            let (b,n'') = fb n' in
            (b,n'')

---------------------------------------------

lift0 :: x -> TableBased c a x
lift0 = pure

lift1 :: (x -> y) -> TableBased c a x -> TableBased c a y
lift1 = liftM

lift2 :: (x -> y -> z) -> TableBased c a x -> TableBased c a y -> TableBased c a z
lift2 = liftM2

lift3 :: (w -> x -> y -> z) -> TableBased c a w -> TableBased c a x -> TableBased c a y -> TableBased c a z
lift3 = liftM3

liftMany :: ([a] -> a) -> [TableBased c a a] -> TableBased c a a
liftMany f xs = liftM f (sequence xs)

---------------------------------------------

tryRetrieveElse :: (Ord c) => (c -> TableBased c a a) -> c -> TableBased c a a
tryRetrieveElse f c =
    let yield x = MkTableBased (\tbl -> (x, Map.insert c x tbl)) in
    MkTableBased (\tbl -> (Map.lookup c tbl, tbl)) >>= (\r ->
        case r of
        Just x -> pure x
        Nothing -> (f c) >>= yield
    )

goFromEmptyTable :: TableBased c a a -> a
goFromEmptyTable (MkTableBased f) = fst (f Map.empty)

type Booster a = (a -> a)

fix :: Booster a -> a
fix f = let x = f x in x

memoize :: (Ord c) => Booster (c -> TableBased c a a) -> c -> a
memoize f = \x -> goFromEmptyTable (fix (tryRetrieveElse . f) x)

memoize2 :: (Ord c1, Ord c2) => Booster (c1 -> c2 -> TableBased (c1,c2) a a) -> c1 -> c2 -> a
memoize2 f = curry (memoize (uncurry . f . curry))

