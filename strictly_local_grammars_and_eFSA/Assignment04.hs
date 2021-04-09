module Assignment04 where

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List(nub)

import FiniteStatePart2

---------------------------------------
-- Setup for section 1

type SLG sy = ([sy], [sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Show)

slg1 :: SLG SegmentCV
slg1 = ([C,V], [C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 2

data DisjointUnion a b = This a | That b deriving (Show,Eq)

data RegExp sy = Lit sy
               | Alt (RegExp sy) (RegExp sy)
               | Concat (RegExp sy) (RegExp sy)
               | Star (RegExp sy)
               | ZeroRE
               | OneRE
               deriving Show

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3))

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

-- 1.1 Recognizing strings generated by an SLG
backwardSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
backwardSLG m w =
      let (syms, i, f, delta) = m in
      case w of
        [] -> False
        x:rest -> case rest of
          -- 1 element list, check for self loop
          [] -> elem (x, x) delta
          _ -> elem (x, (head rest)) delta && backwardSLG m rest


generatesSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
generatesSLG m w =
      let (syms, i, f, delta) = m in
      case w of
        [] -> False
        _-> elem (head w) i && backwardSLG m w && elem (last w) f


-- 1.2 Conversion to FSAs
syToConstructed l =
  case l of
    [] -> []
    x:rest -> [StateForSymbol x] ++ syToConstructed rest

makeState l =
  case l of
    []->[]
    x:rest -> let (y, z) = x in [(StateForSymbol y, z, StateForSymbol z)] ++ makeState rest

slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA m =
  let (syms, i, f, delta) = m in
  let initialState = ExtraState in
  let converted = syToConstructed syms in
  let newStates = [initialState] ++ converted in
  let first = (initialState, head i, (StateForSymbol (head i)))  in
  let newDelta = [first] ++ makeState delta in
  let newEnd = syToConstructed f in
   (newStates, syms, [initialState], newEnd, newDelta)


-- 2. Converting regular expressions into ε-FSAs
-- A. Produces a new version of the given FSA, with the state labels “updated” according to the given function.
getMaybe l = case l of
  [] -> []
  x:rest -> let (a, b, c) = x in [b] ++ getMaybe rest

mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates f e =
  let (states, symbols, i, end, delta) = e in
  let newStates = map f states in
  let newInitial = map f i in
  let newEnd = map f end in
  let newSyms = getMaybe delta in
  let newDelta = map (\(q1,x,q2) -> (f q1, x, f q2)) delta in
  (newStates, symbols, newInitial, newEnd, newDelta)


-- B. Squashes all values of type DisjointUnion Int Int back into the type Int
-- without ever “collapsing distinctions”, i.e. without creating any “collisions”
flatten :: DisjointUnion Int Int -> Int
flatten x = case x of
  -- always even
  This a -> a * 2
  -- always odd
  That a -> a * 2 + 1

-- C. Given an automaton that generates the stringset L and an automaton that generates the stringset
-- L', produces a new automaton that generates the stringset L ∪ L'.
unionFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (DisjointUnion st1 st2) sy
unionFSAs m m' =
      let (states, syms, i, f, delta) = m in
      let (states', syms', i', f', delta') = m' in
      let newStarts = map (\x -> This x) i ++ map (\x -> That x) i' in
      let newEnds = map (\x -> This x) f ++ map (\x -> That x) f' in
      let newDelta = map (\(a,b,c) -> (This a, b, This c)) delta ++ map (\(a,b,c) -> (That a, b, That c)) delta' in
      let newSyms = nub (syms ++ syms') in
      let thism1 = mapStates (\x -> This x) m in
      let (states1, syms1, i1, f1, delta1) = thism1 in
      let thatm2 = mapStates (\x -> That x) m' in
      let (states2', syms2', i2', f2', delta2') = thatm2 in
      let newStates = states1 ++ states2' in
      (newStates, newSyms, newStarts, newEnds, newDelta)

-- D. Given an automaton that generates the stringset L
-- and an automaton that generates the stringset L′, produces a new
-- automaton that generates the stringset {u + v | u ∈ L, v ∈ L′}.
concatFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (DisjointUnion st1 st2) sy
concatFSAs m m' =
  let (states, syms, i, f, delta) = m in
  let (states', syms', i', f', delta') = m' in
  let newSyms = nub (syms ++ syms') in
  let newStarts = map (\x -> This x) i in
  let newEnds = map (\x -> That x) f' in
  -- Step 1: Get transitions in m that end with final state
  let mEnds = map (\x -> This x) f in
  -- Step 2: Get transitions in m' that begin with initial state
  let m2Starts = map (\x -> That x) i' in
  -- Step 3: Hook up each one in Step 1 with each one in Step 2
  -- Use epsilon as symbol, i.e. Nothing
  let connects = liftA2 (\x -> \y -> (x,Nothing,y)) mEnds m2Starts in
  -- Step 4: Add in newDelta
  -- need to remove transitions??
  let newDelta = connects ++ map (\(a,b,c) -> (This a, b, This c)) delta ++ map (\(a,b,c) -> (That a, b, That c)) delta' in
  let thism1 = mapStates (\x -> This x) m in
  let (states1, syms1, i1, f1, delta1) = thism1 in
  let thatm2 = mapStates (\x -> That x) m' in
  let (states2', syms2', i2', f2', delta2') = thatm2 in
  let newStates = states1 ++ states2' in
  (newStates, newSyms, newStarts, newEnds, newDelta)


-- E. Given an automaton that generates the stringset L,
-- produces a new automaton that generates all strings producible but concatenating together zero or more strings from L.starFSA :: EpsAutomaton st sy -> EpsAutomaton (DisjointUnion Int st) sy
starFSA :: EpsAutomaton st sy -> EpsAutomaton (DisjointUnion Int st) sy
starFSA d =
  let thatD = mapStates (\x -> That x) d in
  let (states, syms, i, f, delta) = thatD in
  let newStates = [(This 1)] ++ states in
  let newEnds = [(This 1)] ++ f in
  let newStart = [(This 1)] in
  -- Step 1: Connect new state to original inital states
  let connectThatState = liftA2 (\x -> \y -> (x, Nothing, y)) [(This 1)] i in
  -- Step 2: Connect original final states to original inital states
  let connectfinaltoInitial = liftA2 (\x -> \y -> (x, Nothing, y)) f i in
  let (states1, syms1, i1, f1, delta1) = d in
  let deltaFix = map (\(a,b,c) -> (That a, b, That c)) delta1 in
  -- Step 3: Add transitions from Step 1 and Step 2
  let newDelta = deltaFix ++ connectThatState ++ connectfinaltoInitial in
  (newStates, syms, newStart, newEnds, newDelta)


-- F. Produces an automaton that generates the stringset that is the
-- denotation of the given regular expression.
reToFSA :: (Eq sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA r =
  case r of
    Lit r1 -> ([1,2], [r1], [1], [2], [(1, Just r1, 2)])
    Alt r1 r2 -> mapStates flatten (unionFSAs (reToFSA r1) (reToFSA r2))
    Concat r1 r2 -> mapStates flatten (concatFSAs (reToFSA r1) (reToFSA r2))
    Star r1 -> mapStates flatten (starFSA (reToFSA r1))
    ZeroRE -> ([1], [], [1], [], [])
    OneRE -> ([1,2], [], [1], [2], [(1, Nothing, 2)])