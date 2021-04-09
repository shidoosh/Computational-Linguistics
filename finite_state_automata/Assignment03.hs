module Assignment03 where

-- Imports everything from the FiniteState module
import FiniteState

-- Another type we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | WB deriving (Show,Eq)

-- A list-like type that will be useful for computing forward values
data SnocList a = ESL | (SnocList a) ::: a deriving Show

-- The word ``hello'' encoded as a snoc list of characters
sl :: SnocList Char
sl = ((((ESL ::: 'h') ::: 'e') ::: 'l') ::: 'l') ::: 'o'

-- Checks that all states and symbols mentioned in the transition
-- table (i.e. delta) come from the provided lists of states and symbols.
fsaSanityCheck :: (Eq a) => Automaton a -> Bool
fsaSanityCheck m =
    let (states, syms, i, f, delta) = m in
    let validTransition (q1,x,q2) = elem q1 states && elem x syms && elem q2 states in
    and (map validTransition delta)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.


-- 1. Encoding finite-state automata formally
fsa_countVs :: Automaton SegmentCV
fsa_countVs = ([54, 73, 21, 38], [C, V], [54], [38], [(54, C, 54),
                                                   (54, V, 73),
                                                   (73, C, 73),
                                                   (73, V, 21),
                                                   (21, V, 54),
                                                   (21, C, 21),
                                                   (21, V, 38),
                                                   (38, C, 38)])

-- 2. “Snoc lists”
--A. Write a function addToFront :: a -> SnocList a -> SnocList a so that addToFront x l returns a snoc list
--   that is like l but has an additional occurrence of x as its leftmost element.
addToFront :: a -> SnocList a -> SnocList a
addToFront y s =  case s of
              ESL -> s ::: y
              rest:::x -> ((addToFront y) rest) ::: x

--B. Write a function toSnoc :: [a] -> SnocList a that produces
--   the snoc list corresponding to the given “normal list” (i.e. cons list).
toSnoc :: [a] -> SnocList a
toSnoc l = case l of
              [] -> ESL
              -- prints backwards, but (toSnoc rest):::x fails...
              -- why??
              --x:rest -> x ::: (toSnoc rest)
              x:rest -> ((addToFront x) (toSnoc rest))

-- 3. Forward Values
-- C. Write a forward :: (Eq a) => Automaton a -> SnocList a -> State -> Bool
-- which computes forward values
-- forward m w q should evaluate to True iff there’s a way to get
-- from an initial state of the automaton m to the state q that
-- produces the symbols of w.
forward :: (Eq a) => Automaton a -> SnocList a -> State -> Bool
forward m w q =
  let (states, syms, i, f, delta) = m in
    case w of
      ESL -> elem q i          -- I(q)
      -- switch elem (q, x, q1) to elem(q1, x, q)
      -- since we care if we can get to initial state from q1
      -- i.e. initial state -> q
      (rest:::x) -> or (map (\q1 -> elem (q1,x,q) delta && forward m rest q1) states)

-- D. Write a function generates2 :: (Eq a) => Automaton a -> [a] -> Bool
-- which checks whether the given automaton generates the given string of symbols.
generates2 :: (Eq a) => Automaton a -> [a] -> Bool
generates2 m w =
    let (states, syms, i, f, delta) = m in
    -- F(q) instead of I(q)
    or (map (\q0 -> elem q0 f && forward m (toSnoc w) q0) states)



-- 4. Designing finite-state automata
-- E.
fsa_twoCs :: Automaton SegmentCV
fsa_twoCs = ([1, 2, 3], [C, V], [1], [3], [(1, C, 2),
                                           (1, V, 1),
                                           (2, C, 3),
                                           (2, V, 2),
                                           (3, C, 3),
                                           (3, V, 3)])



fsa_oddEven :: Automaton SegmentCV
fsa_oddEven = ([1, 2, 3, 4], [C, V], [1], [4], [(1, C, 4),
                                                (1, V, 2),
                                                (2, C, 3),
                                                (2, V, 1),
                                                (3, C, 2),
                                                (3, V, 4),
                                                (4, V, 3),
                                                (4, C, 1)])

fsa_thirdC :: Automaton SegmentCV
fsa_thirdC = ([1, 2, 3, 4, 5], [C, V], [1], [4], [(1, C, 2),
                                               (1, V, 2),
                                               (2, C, 3),
                                               (2, V, 3),
                                               (3, C, 4),
                                               (3, V, 5),
                                               (4, C, 4),
                                               (4, V, 4),
                                               (5, C, 5),
                                               (5, V, 5)])

-- Can I do NFA??
fsa_thirdlastC :: Automaton SegmentCV
fsa_thirdlastC = ([1, 2, 3, 4], [C, V], [1], [4], [(1, C, 1),
                                                  (1, V, 1),
                                                  (1, C, 2),
                                                  (2, C, 3),
                                                  (2, V, 3),
                                                  (3, C, 4),
                                                  (3, V, 4)])

fsa_harmony :: Automaton SegmentPKIU
fsa_harmony = ([1, 2, 3, 4], [P, K, I, U, WB], [1], [1, 2, 4], [(1, P, 1),
                                                             (1, K, 1),
                                                             (1, WB, 1),
                                                             (1, I, 2),
                                                             (1, U, 4),
                                                             (2, P, 2),
                                                             (2, K, 2),
                                                             (2, I, 2),
                                                             (2, U, 3),
                                                             (2, WB, 1),
                                                             (3, P, 3),
                                                             (3, K, 3),
                                                             (3, I, 3),
                                                             (3, U, 3),
                                                             (3, WB, 3),
                                                             (4, P, 4),
                                                             (4, K, 4),
                                                             (4, U, 4),
                                                             (4, I, 3),
                                                             (4, WB, 1)])

fsa_PU :: Automaton SegmentPKIU
fsa_PU = ([1, 2, 3], [P, K, I, U], [1], [1, 2], [(1, K, 1),
                                                 (1, I, 1),
                                                 (1, U, 3),
                                                 (1, P, 2),
                                                 (2, P, 2),
                                                 (2, K, 2),
                                                 (2, I, 2),
                                                 (2, U, 2),
                                                 (3, P, 3),
                                                 (3, K, 3),
                                                 (3, I, 3),
                                                 (3, U, 3)])

fsa_adjacentPU :: Automaton SegmentPKIU
fsa_adjacentPU = ([1, 2, 3, 4], [P, K, I, U], [1], [1, 2, 3], [(1, K, 1),
                                                            (1, I, 1),
                                                            (1, U, 4),
                                                            (1, P, 2),
                                                            (2, K, 1),
                                                            (2, I, 1),
                                                            (2, U, 3),
                                                            (2, P, 2),
                                                            (3, K, 1),
                                                            (3, I, 1),
                                                            (3, U, 4),
                                                            (3, P, 2),
                                                            (4, K, 4),
                                                            (4, I, 4),
                                                            (4, U, 4),
                                                            (4, P, 4)])
