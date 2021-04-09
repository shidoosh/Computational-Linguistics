module Recursion where

data Form = T | F | Neg Form | Cnj Form Form | Dsj Form Form deriving Show

f1 = Dsj (Neg T) (Cnj F T)

removeNegs = \form -> case form of
                      T -> T
                      F -> F
                      Neg phi -> removeNegs phi
                      Cnj phi psi -> Cnj (removeNegs phi) (removeNegs psi)
                      Dsj phi psi -> Dsj (removeNegs phi) (removeNegs psi)

-- The type 'Bool' is defined under the hood like this.
-- data Bool = False | True deriving Show

-- The function 'not' is defined under the hood like this.
-- not = \b -> case b of {True -> False; False -> True}

-- Implements the definition in (4) on the handout.
denotation = \form -> case form of
                      T -> True
                      F -> False
                      Neg phi -> case (denotation phi) of {True -> False; False -> True}
                      Cnj phi psi -> case (denotation phi) of True -> denotation psi
                                                              False -> False
                      Dsj phi psi -> case (denotation phi) of True -> True
                                                              False -> denotation psi

---------------------------------------------------

data Numb = Z | S Numb deriving Show

one = S Z
two = S one
three = S two
four = S three
five = S four

lessThanTwo :: Numb -> Bool
lessThanTwo = \x -> case x of
                    Z -> True
                    S y -> case y of {Z -> True; S z -> False}

double :: Numb -> Numb
double = \n -> case n of
               Z -> Z
               S n' -> S (S (double n'))

dbl :: Int -> Int
dbl = \n -> case (n == 0) of {True -> 0; False -> 2 + dbl (n-1)}

isOdd :: Numb -> Bool
isOdd = \n -> case n of
              Z -> False
              S n' -> not (isOdd n')

add :: Numb -> Numb -> Numb
add = \n -> (\m -> case n of
                   Z -> m
                   S n' -> (add n') (S m)     --- or: S ((add n') m)
            )

-- Same as 'add', but written in a way that makes clear that this is a 
-- function that takes a number and produces a function.
add2 :: Numb -> (Numb -> Numb)
add2 = \n -> case n of
             Z -> (\m -> m)
             S n' -> (\m -> add n' (S m))

lessThanOrEq :: Numb -> (Numb -> Bool)
lessThanOrEq = \n -> \m -> case n of
                Z -> True
                S n' -> case m of
                        Z -> False
                        S m' -> (lessThanOrEq n') m'

bigger :: Numb -> (Numb -> Numb)
bigger = \n -> \m -> case n of
                     Z -> m
                     S n' -> case m of
                             Z -> n
                             S m' -> S ((bigger n') m')

------------------------------------------------

-- (9) on the handout
data IntList = Empty | NonEmpty Int IntList deriving Show

-- (11) on the handout
total :: IntList -> Int
total = \l -> case l of
              Empty -> 0
              NonEmpty x rest -> x + total rest

-- (13) on the handout
total2 :: [Int] -> Int
total2 = \l -> case l of
               [] -> 0
               x : rest -> x + total2 rest

contains :: (a -> Bool) -> ([a] -> Bool)
contains = \f -> \l -> case l of
                       [] -> False
                       x : rest -> case (f x) of
                                   True -> True
                                   False -> contains f rest

------------------------------------------------

-- Corresponds to the definition in (16) on the handout
data RegExp = Lit Char | Alt RegExp RegExp | Concat RegExp RegExp | Star RegExp | ZeroRE | OneRE deriving Show

-- See (17) on the handout
re17a = Alt (Lit 'a') (Lit 'b')
re17b = Concat re17a (Lit 'c')
re17c = Star re17b

