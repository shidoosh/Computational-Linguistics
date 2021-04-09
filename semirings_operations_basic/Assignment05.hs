{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lhs :: (Semiring v) => v -> v -> v -> v
distrib_lhs x y z = x &&& (y ||| z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

-- 1 Writing functions that work for any semiring
--A.
distrib_rhs :: (Semiring v) => v -> v -> v -> v
distrib_rhs x y z = (x &&& y) ||| (x &&& z)

--B. Computes what we might call the “semiring dot product”
dotprod :: (Semiring v) => [v] -> [v] -> v
dotprod s1 s2 =
  let newList = zipWith (&&&) s1 s2 in
  gen_or newList

--C. Computes what we might call the “semiring exponential”:
-- given an element x of a semiring and a number n,
-- it should combine n “copies” of x via generalized conjunction
expn :: (Semiring v) => v -> Numb -> v
expn s n =
  case n of
    Z -> gtrue
    S n' -> s &&& expn s n'

-- 2. Now for semiring-based FSAs
-- D. Computes semiring-general backward values
backward :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> st -> v
backward g w q =
  let (states, syms, i, f, delta) = g in
  case w of
    [] -> f q
    x:rest -> gen_or (map (\q1 -> delta (q,x,q1) &&& backward g rest q1) states)


-- E. Computes values of the function fM , using the equation in (28) in this week’s handout.
f :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> v
f g w =
  let (states, syms, i, f, delta) = g in
  gen_or (map (\q0 -> i q0 &&& backward g w q0) states)


--3. Adding the cost semiring
--F. Adds up two costs. Adding anything to an infinite cost produces an infinite cost.
addCost :: Cost -> Cost -> Cost
addCost x y = let  (TheInt a) = x in let (TheInt b) = y in
  case x of
  Inf -> Inf
  _-> case y of
    Inf -> Inf
    _->TheInt (a+b)


--G.  selects the smaller of two costs
-- (or, if the two given costs are the same, returns that cost). An infinite cost is larger than any other cost.
minCost :: Cost -> Cost -> Cost
minCost x y = let  (TheInt a) = x in let (TheInt b) = y in
  case x of
    Inf -> case y of
            Inf -> Inf
            -- x is inf, return y
            _-> TheInt b
    -- x is not inf
    _-> case y of
            -- y is inf, return x
            Inf -> TheInt a
            -- y is not inf
            _-> case a > b of
              True -> TheInt b
              False -> case a == b of
                True -> TheInt b
                False -> TheInt a

--H. make Cost an instance of the Semiring class.
instance Semiring Cost where
  x &&& y = addCost x y
  x ||| y = minCost x y
  gtrue = TheInt 0
  gfalse = Inf



--4. Adding the set-of-strings semiring
--
-- I. Now do the same thing for [String] as you did above for the type Cost:
-- add the type [String] to the Semiring class
instance Semiring [String] where
  x &&& y = liftA2 (\a -> \b -> a ++ b) x y
  x ||| y = x ++ y
  gtrue = [""]
  gfalse = []

-- J. gfsa13 :: GenericAutomaton Int Char [String]
-- (13) on the handout
gfsa13 :: GenericAutomaton Int Char [String]
gfsa13 = makeGFSA [] ([1,2,3], ['C','V'],
                       [(1, [""])], [(1, [""])],
                       [((1,'V',1), ["V"]),
                        ((1,'C',2), ["C"]),
                        ((1,'V',3), ["V"]),
                        ((2,'V',1), ["VV"]),
                        ((2,'V',3), ["VV"]),
                        ((3,'C',1), [""])])

--K.
gfsa_flap :: GenericAutomaton Int Char [String]
gfsa_flap = makeGFSA [] ([0, 1, 2],
                          ['a', 'n', 't', 'T'],
                          [(0, [""])],
                          [(0, [""]), (1, [""]), (2, ["t"])],
                          [((0,'n',0), ["n"]),
                           ((0,'t',0), ["t"]),
                           ((0,'a',1), ["a"]),
                           ((1,'n',0), ["n"]),
                           ((1,'a',1), ["a"]),
                           ((1,'t',2), [""]),
                           ((2,'a',1), ["ta", "Ta"]),
                           ((2,'n',0), ["tn"]),
                           ((2,'t',0), ["tt"])])
