{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Assignment06 where

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringCFG

data Tree nt t = Leaf nt t | NonLeaf nt (Tree nt t) (Tree nt t) deriving Show

tree1 :: Tree Cat String
tree1 = NonLeaf VP (NonLeaf VP (Leaf V "watches") (Leaf NP "spies"))
                   (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes"))

tree2 :: Tree Cat String
tree2 = NonLeaf VP (Leaf V "watches")
                   (NonLeaf NP (Leaf NP "spies") (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes")))

probToBool :: GenericCFG nt t Double -> GenericCFG nt t Bool
probToBool (nts, ts, i, r) =
    let newi = \nt -> (i nt > 0) in
    let newr = \rule -> (r rule > 0) in
    (nts, ts, newi, newr)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
root :: Tree nt t -> nt
root tree =
    case tree of
    Leaf nonterm term -> nonterm
    NonLeaf nonterm l1 r1 -> nonterm


-- 1. Leftmost derivations
-- A. Computes the list of rewrite rules, in order, that are used in the leftmost
-- derivation corresponding to the given tree
treeToDeriv :: Tree nt t -> [RewriteRule nt t]
treeToDeriv tree = case tree of
      Leaf nonterm term -> [TRule nonterm term]
      NonLeaf nonterm l1 r1 -> [NTRule nonterm (root l1, root r1)] ++ treeToDeriv l1 ++ treeToDeriv r1


-- 2. Semirings and CFGs
-- B. Computes, in a semiring-general way, the “complete” value that the given
-- grammar assigns to the given string
f :: (Ord nt, Ord t, Semiring a) => GenericCFG nt t a -> [t] -> a
f c l = let (nonterms, terminals, i, rules) = c in
  gen_or (map (\nonterminal -> (i nonterminal) &&& fastInside c l nonterminal) nonterms)

-- C. Define semiring operations for Int so that the resulting
-- grammar can be used with f to calculate the number of distinct analyses (i.e. tree structures) for a string
instance Semiring Int where
  x &&& y = x * y
  x ||| y = x + y
  gtrue = 1
  gfalse = 0

boolToCount :: GenericCFG nt t Bool -> GenericCFG nt t Int
boolToCount (nonterms, terminals, i, r) =
-- I want to pass nt to i, when it is false, return gfalse. else grtrue. Same for rules
  let newi = \nt -> (case i nt of
                        True -> gtrue
                        False -> gfalse) in
  let newr = \rule -> (case r rule of
                        True -> gtrue
                        False -> gfalse) in
  (nonterms, terminals, newi, newr)


-- D. Define semiring operations for [Double] so that the resulting
-- grammar can be used with f to calculate the probabilities of the individual tree structures for a string
instance Semiring [Double] where
    x &&& y = (liftA2 (\a -> \b -> a * b) x y)
    x ||| y = x ++ y
    gtrue = [1.0]
    -- Should be [] or [0.0]?
    gfalse = []

probToProbList :: GenericCFG nt t Double -> GenericCFG nt t [Double]
probToProbList (nonterms, terminals, i, r) =
  let newi = \nt -> (case (i nt > 0) of
                    True -> [i nt]
                    False -> gfalse) in
  let newr = \rule -> (case (r rule > 0) of
                    True -> [r rule]
                    False -> gfalse) in
  (nonterms, terminals, newi, newr)


-- E. Define semiring operations for [[RewriteRule nt t]] so that the resulting grammar can be
-- used with f to find the rule sequences used in the leftmost derivations of a string.
instance Semiring [[RewriteRule nt t]] where
    x &&& y = (liftA2 (\a -> \b -> a ++ b) x y)
    x ||| y = x ++ y
    gtrue = [[]]
    gfalse = []

boolToDerivList :: GenericCFG nt t Bool -> GenericCFG nt t [[RewriteRule nt t]]
boolToDerivList (nonterms, terminals, i, r) =
  let newi = \nt -> (case i nt of
                        True -> gtrue
                        False -> gfalse) in
  let newr = \rule -> (case r rule of
                        True -> [[rule]]
                        False -> gfalse) in
  (nonterms, terminals, newi, newr)


-- F. Define semiring operations for [(Double, [RewriteRule nt t])] so that the resulting
-- grammar can be used with f to find the probability and rule sequence for each derivation of a string
gen_andDoubleRule ::(Double, [RewriteRule nt t]) -> (Double, [RewriteRule nt t]) -> (Double, [RewriteRule nt t])
gen_andDoubleRule x y =
    let (doubleX, rulesX) = x in
    let (doubleY, rulesY) = y in
    (doubleX * doubleY, rulesX ++ rulesY)


instance Semiring [(Double, [RewriteRule nt t])]  where
    x &&& y = (liftA2 (\a -> \b -> gen_andDoubleRule a b) x y)
    x ||| y = x ++ y
    gtrue = [(1.0, [])]
    gfalse = []


probToProbDerivList :: GenericCFG nt t Double -> GenericCFG nt t [(Double, [RewriteRule nt t])]
probToProbDerivList (nonterms, terminals, i, r) =
  let newi = \nt -> (case (i nt > 0) of
                  True -> [(i nt, [])]
                  False -> gfalse) in
  let newr = \rule -> (case (r rule > 0) of
                  True -> [(r rule, [rule])]
                  False -> gfalse) in
  (nonterms, terminals, newi, newr)
