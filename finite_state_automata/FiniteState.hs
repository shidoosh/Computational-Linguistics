module FiniteState where

-- A type we'll use for the symbols of some FSAs.
-- By ``deriving Eq'' we are linking the default/obvious
-- equality function on this type to the name (==).
data SegmentCV = C | V deriving (Show, Eq)

-- This line defines `State' as a synonym for `Int', just for readability
type State = Int

-- In class on Thu 1/23 we saw a version of this type which had SegmentCV
-- ``hard-coded'' as the symbol type. This is a more flexible version
-- which allows symbols to be of any type. So `Automaton SegmentCV' is
-- a type for FSAs with symbols of type SegmentCV, `Automaton Char' is
-- a type for FSAs with symbols of type Char, etc.
type Automaton a = ([State], [a], [State], [State], [(State,a,State)])

-- Here's the FSA from (4) on the handout
fsa_handout4 :: Automaton SegmentCV
fsa_handout4 = ([40,41,42,43], [C,V], [40], [43], [(40, C, 40),
                                                   (40, V, 40),
                                                   (40, C, 41),
                                                   (40, V, 42),
                                                   (41, C, 43),
                                                   (42, V, 43),
                                                   (43, C, 43),
                                                   (43, V, 43)])

-- Here's the FSA from (5) on the handout
fsa_handout5 :: Automaton SegmentCV
fsa_handout5 = ([1,2,3], [C,V], [1], [3], [(1, C, 1),
                                           (1, V, 1),
                                           (1, V, 2),
                                           (2, C, 3)])

-- This corresponds transparently to (24) on the handout.
-- This function can only be used with automata whose symbol
-- type has an equality function defined on it, because it
-- calls the function `elem' which has this restriction.
-- (The type `(Int,a,Int)' is a member of Eq iff `a' is a member of Eq.)
backward :: (Eq a) => Automaton a -> [a] -> State -> Bool
backward m w q =
    let (states, syms, i, f, delta) = m in
    case w of
    [] -> elem q f          -- F(q) on the handout
    (x:rest) -> or (map (\q1 -> elem (q,x,q1) delta && backward m rest q1) states)

-- This corresponds transparently to (21) on the handout.
-- (And this function can only be used with symbol types
-- that belong to the class Eq, because it calls `backward'
-- which already has this requirement.)
generates :: (Eq a) => Automaton a -> [a] -> Bool
generates m w =
    let (states, syms, i, f, delta) = m in
    or (map (\q0 -> elem q0 i && backward m w q0) states)

-- NB: There's nothing particularly special about (&&), it's just predefined
-- for us like this:
--      (&&) :: Bool -> Bool -> Bool
--      x && y = case x of {False -> False; True -> y}
