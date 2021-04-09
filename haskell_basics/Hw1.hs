module Hw1 where 
--import Text.Show.Functions
import Debug.Trace

data Shape = Rock | Paper | Scissors deriving Show 

data Result = Win Shape | Draw deriving Show

n=1

f = \s -> case s of {Rock -> 334; Paper -> 138; Scissors -> 99}

g = \z -> z + 4

whatItBeats = \s -> case s of {Rock -> Scissors; Paper -> Rock; Scissors -> Paper}
 
--e s x = case s of {Rock -> x * x; Paper -> x * x * x; Scissors -> x}
--e = \s -> \x -> case s of {Rock -> x * x; Paper -> x * x * x; Scissors -> x}
--myadd = \n -> \m -> (n + m)
myadd n m = n + m 


