{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module Assignment08 where

import Control.Applicative(liftA, liftA2, liftA3)


import TreeGrammars

plainWords = ["John","Mary","ate","bought","an","apple","books","yesterday","C","laughed","because"]
whWords = ["who","what","why"]
qWords = ["Q"]

-- (1a)/(2a) `C John ate an apple'
tree_1a :: Tree String
tree_1a =
    Node "*" [
        Node "C" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "ate" [],
                Node "*" [Node "an" [], Node "apple" []]
            ]
        ]
    ]

-- (1b)/(2b) `Q John ate what'
tree_1b :: Tree String
tree_1b =
    Node "*" [
        Node "Q" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "ate" [],
                Node "what" []
            ]
        ]
    ]

-- (3a) `Q John ate an apple'
tree_3a :: Tree String
tree_3a =
    Node "*" [
        Node "Q" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "ate" [],
                Node "*" [Node "an" [], Node "apple" []]
            ]
        ]
    ]

-- (3b) `C John ate what'
tree_3b :: Tree String
tree_3b =
    Node "*" [
        Node "C" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "ate" [],
                Node "what" []
            ]
        ]
    ]

tree_13 :: Tree String
tree_13 =
    Node "*" [
        Node "Q" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "laughed" [],
                Node "**" [
                    Node "because" [],
                    Node "*" [
                        Node "Mary" [],
                        Node "*" [
                            Node "*" [Node "bought" [], Node "books" []],
                            Node "why" []
                        ]
                    ]
                ]
            ]
        ]
    ]

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-- A. Returns the number of occurrences of the given symbol in the given tree
count :: (Eq a) => a -> Tree a -> Int
count x (Node y daughters) =
  case x == y of
    True -> 1 + sum (map (count x) daughters)
    False -> sum (map (count x) daughters)

-- B. Returns the sequence of symbols we find by following
-- a root-to-leaf path downwards through given tree,
-- taking the leftmost branch at each point.
leftEdge :: Tree a -> [a]
leftEdge (Node y daughters) =
  case daughters of
    [] -> [y]
    t:rest -> [y] ++ (leftEdge (head daughters))

-- C. Produces all lists of the given length, made out of elements
-- of the given list.
concatr x rest = map (x:) rest

makeList l = case l of
  [] -> []
  x:rest -> [[x]] ++ makeList rest

allHelp l1 l2 = concat (liftA (\a -> concatr a l2) l1)

allNum n l1 l2 =
  case n > 2 of
    False -> []
    True -> let newN = n-1 in
            let newL2 = liftA2 (\a -> \b -> a ++ b) l1 l2 in
            newL2 ++ (allNum newN l1 newL2)

allLists :: Int -> [a] -> [[a]]
allLists n l =
  case n == 0 of
    True -> [[]]
    False -> case n < 2 of
              True -> makeList l
              False -> case n > 2 of
                        False -> let l1 = makeList l in
                                  allHelp l l1
                        True ->  let l1 = makeList l in
                                 let l2 = allHelp l l1 in
                                 filter (\x -> length x == n) (allNum n l1 l2)

-- D. Implements the definition in (17) on the class handout.
under :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> st -> Bool
under m t q =
  let (states, symbols, ends, delta) = m in
  let (Node x ts) = t in
  let f qs = elem (qs, x, q) delta && and (zipWith (under m) ts qs) in
  or (map f (allLists (length ts) states))

-- E. Checks whether the given finite-state tree automaton generates the given tree.
generates :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> Bool
generates m t =
  let (states, symbols, ends, delta) = m in
  let (Node x ts) = t in
  -- or (map (\q0 -> elem q0 i && backward m w q0) states)
  or (map (\q0 -> elem q0 ends && under m t q0) states)


data WhStatus = Wh | LicWh | WhOK deriving (Eq,Show)
fsta_wh1 = ([Wh, LicWh, WhOK], plainWords ++ whWords ++ qWords ++ ["*"], [WhOK], [])

fsta_wh2 = ([Wh, LicWh, WhOK], plainWords ++ whWords ++ qWords ++ ["*"], [WhOK], [])
