{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module TreeGrammars where

-- Corresponds to the definition in (5) on the handout
data Tree sy = Node sy [Tree sy] deriving Show

-- Corresponds to the tree in (7) on the handout
t7 :: Tree Char
t7 = Node 'b' [Node 'c' [Node 'a' []], Node 'a' [Node 'b' [], Node 'b' []]]

-- Corresponds to the definition in (10) on the handout
type Automaton st sy = ([st], [sy], [st], [([st],sy,st)])

--------------------------------------------------------------------------------
-- A couple of example functions illustrating how to work with this sort of tree. 
-- (These functions have nothing directly to do with tree automata.)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Node x ts) = Node (f x) (map (treeMap f) ts)

contains :: (Eq a) => a -> Tree a -> Bool
contains x (Node y ts) = (x == y) || or (map (contains x) ts)

--------------------------------------------------------------------------------
-- The example from section 2.3.1 on the handout

t12 :: Tree Char
t12 = Node 'a' [Node 'b' [Node 'b' [Node 'a' []], Node 'a' [Node 'b' [], Node 'a' []]]]

data Parity = Even | Odd deriving (Show,Eq)

fsta_even :: Automaton Parity Char
fsta_even = ([Even,Odd], ['a','b'], [Even],
             [ ([Even,Even], 'a', Odd),     ([Even,Even], 'b', Even), 
               ([Even,Odd],  'a', Even),    ([Even,Odd],  'b', Odd), 
               ([Odd,Even],  'a', Even),    ([Odd,Even],  'b', Odd), 
               ([Odd,Odd],   'a', Odd),     ([Odd,Odd],   'b', Even), 
               ([Even],      'a', Odd),     ([Even],      'b', Even), 
               ([Odd],       'a', Even),    ([Odd],       'b', Odd), 
               ([],          'a', Odd),     ([],          'b', Even)
             ])

--------------------------------------------------------------------------------
-- The example from section 2.3.3 on the handout

t16 :: Tree String
t16 =
    Node "*" [
        Node "*" [
            Node "that" [], 
            Node "*" [ Node "nothing" [], Node "*" [Node "ever" [], Node "happened" []] ]
        ] ,
        Node "*" [Node "surprised" [], Node "us" [] ]
    ]

-- These three states are called "NEG", "LIC" and "0" respectively on the handout (sorry)
data NegStatus = Neg | LicNeg | NegOK deriving (Show,Eq)

fsta_npi :: Automaton NegStatus String
fsta_npi = let npis = ["anything", "ever", "even"] in
           let licensors = ["nothing", "not"] in
           let otherwords = ["that", "happened", "surprised", "us"] in
           ([NegOK, LicNeg, Neg],
            ["*"] ++ npis ++ licensors ++ otherwords,
            [NegOK, LicNeg],
            [([Neg,    Neg],    "*", Neg), 
             ([NegOK,  Neg],    "*", Neg), 
             ([Neg,    NegOK],  "*", Neg), 
             ([NegOK,  NegOK],  "*", NegOK), 
             ([LicNeg, Neg],    "*", NegOK), 
             ([LicNeg, NegOK],  "*", NegOK), 
             ([NegOK,  LicNeg], "*", NegOK), 
             ([LicNeg, LicNeg], "*", NegOK)
            ] ++ map (\s -> ([], s, NegOK)) otherwords 
              ++ map (\s -> ([], s, LicNeg)) licensors
              ++ map (\s -> ([], s, Neg)) npis
           )

