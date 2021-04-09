module Assignment02 where

-- Imports everything from the Recursion module.
import Recursion

-- Imports just a few things that we have seen from the standard Prelude module.
-- (If there is no explicit 'import Prelude' line, then the entire Prelude
-- module is imported. I'm restricting things here so a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), (++), not, Bool(..), Char)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

-- 1. Recursive functions on the Numb type:

-- A. Computes the product of two Numbs
mult :: Numb -> Numb -> Numb
mult = \n -> \m -> case n of
              --if first number is Z, then return Z
            Z -> n
              -- if first number is greater than Z,
              -- then add it to itself m times
            S n' -> case m of
                    -- if m is Z, then return m
                    Z -> m
                    -- if m is greater than Z, add n to itself m times
                    S m' -> add (mult n m') n

-- B. Computes the sum of all Numbs less than or equal to the given number
sumUpTo :: Numb -> Numb
sumUpTo = \n -> case n of
            -- if 0, add 0
            Z -> n
            -- if non-zero, then add n to the rest of n
            S n' -> add n (sumUpTo n')

-- C. Returns True if the two numbers given are equal, False otherwise
equal :: Numb -> Numb -> Bool
equal = \n -> \m -> case n of
            -- if n is Z...
            Z -> (case m of
                    -- if m is also Z, return true.
                    -- Recursion will end here is n is equal to m
                    Z -> True
                    -- if m is not Z, return false
                    S m' -> False)
            -- if n is greater than Z...
            S n' -> (case m of
                    -- if m is Z, return false
                    Z -> False
                    -- Evaluate n' and m'
                    -- if equal, eventually n' and m' will both be Z
                    S m' -> equal n' m')

-- 2. Recursive functions on lists

-- D. Returns (in the form of a Numb) the number of elements in the given list
-- for which the given argument returns True
count :: (a -> Bool) -> [a] -> Numb
count = \f -> \l -> case l of
            [] -> Z
            -- if l in NonEmpty, Evaluate x applied to f
            x : rest -> case (f x) of
                    -- if f returns true, add 1 (add S, because Numb)
                    -- Continue counting for the rest of the list
                    -- Note: Parentheses are important!! Otherwise, f will
                    -- try to take rest as an argument, as opposed to count's!!
                    True -> S ((count f) rest)
                    -- if f return false, do not add anything
                    -- Continue counting for the rest of the list
                    False -> (count f) rest

-- E. Returns a list containing the given element the
-- given number of times (and nothing else)
listOf :: Numb -> a -> [a]
listOf = \n -> \x -> case n of
            Z -> []
            -- Decrement Numb by passing n', pass x to add same given element
            S n' -> [x] ++ (listOf n') x

-- F. Returns a list that has an addition occurence of a to [a]
addToEnd :: a -> [a] -> [a]
addToEnd = \n -> \l -> case l of
            -- at end of the list, so add n to l
            [] -> l ++ [n]
            -- Recurse on l until at end of list
            -- Important to add [x] back in, so function "remembers"
            x : rest -> [x] ++ (addToEnd n) rest

-- G. Returns a modified version of [a], such that all elements which f
-- returns True is removed
remove :: (a -> Bool) -> [a] -> [a]
remove = \f -> \l -> case l of
            [] -> []
            x : rest -> case (f x) of
                    -- f returns true on x, so "forget" x
                    True -> (remove f) rest
                    -- f returns true on x, so keep x in l
                    False -> [x] ++ (remove f) rest

-- H. Returns the list containing the first n elements of list;
-- or, if n is greater than the length of list, returns list as it is.
prefix :: Numb -> [a] -> [a]
prefix = \n -> \l -> case n of
            Z -> []
            S n' -> case l of
                    [] -> []
                    -- n is at least one, so add one element to the rest of l
                    x : rest -> [x] ++ (prefix n') rest


-- 3. Recursive functions on the RegExp type

-- I. Returns the number of occurences of the star operator in the given RegExp
countStars :: RegExp -> Numb
countStars = \r -> case r of
            ZeroRE -> Z
            OneRE -> Z
            Lit r' -> Z
            Star r' -> S (countStars r')
            -- Concat and Alt take 2 RegExp, need to search both and add
            Concat r' r'' -> (add (countStars r')) (countStars r'')
            Alt r' r'' -> (add (countStars r')) (countStars r'')

-- J. Returns the length of the longest root-to-leaf sequence of nodes in this tree
-- for the given regular expression,
-- i.e. the depth of the most deeply-embedded leaf of the tree
depth :: RegExp -> Numb
depth = \r -> case r of
            ZeroRE -> S Z
            OneRE -> S Z
            Lit r' ->  S Z
            Star r' -> S (depth r')
            -- Concat and Alt take 2 RegExp, need to search both, take bigger
            Concat r' r'' -> S ((bigger (depth r')) (depth r''))
            Alt r' r'' -> S ((bigger (depth r')) (depth r''))


-- K. Returns the string — i.e. the list of characters — representing the given
-- regular expression in the notation we used in class
reToString :: RegExp -> [Char]
reToString = \r -> case r of
            ZeroRE -> ['0']
            OneRE -> ['1']
            Lit r' -> [r']
            Star r' -> reToString(r') ++ ['*']
            -- Concat and Alt take 2 RegExp, need to "stringify" both
            Concat r' r'' -> ['('] ++ reToString(r') ++ ['.'] ++ reToString(r'') ++ [')']
            Alt r' r'' -> ['('] ++ reToString(r') ++ ['|'] ++ reToString(r'') ++ [')']
