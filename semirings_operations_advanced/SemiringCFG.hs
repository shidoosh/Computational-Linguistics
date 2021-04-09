{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module SemiringCFG where

import Control.Applicative(liftA, liftA2, liftA3)

import qualified Memoization as M

data Cat = S | NP | VP | PP | V | P deriving (Show, Eq, Ord)

data RewriteRule nt t = NTRule nt (nt,nt) | TRule nt t deriving (Show, Eq)

type CFG nt t = ([nt], [t], [nt], [RewriteRule nt t])

-----------------------------------------------------------
-- CFGs generalized to allow non-boolean result values

type GenericCFG nt t v = ([nt], [t], nt -> v, RewriteRule nt t -> v)

-- Feel free to ignore the details of this function.
makeGCFG :: (Eq nt, Eq t) => v -> ([nt], [t], [(nt,v)], [(RewriteRule nt t, v)]) -> GenericCFG nt t v
makeGCFG def (nts, ts, starts, rules) =
    let mylookup l x = case lookup x l of {Just y -> y; Nothing -> def} in
    (nts, ts, mylookup starts, mylookup rules)

-----------------------------------------------------------
-- Familiar semiring stuff, same as last week

class Semiring a where
    (&&&) :: a -> a -> a
    (|||) :: a -> a -> a
    gtrue :: a
    gfalse :: a

gen_or :: Semiring a => [a] -> a
gen_or list = case list of {[] -> gfalse; (x:xs) -> x ||| (gen_or xs)}

gen_and :: Semiring a => [a] -> a
gen_and list = case list of {[] -> gtrue; (x:xs) -> x &&& (gen_and xs)}

instance Semiring Bool where
    x &&& y = x && y
    x ||| y = x || y
    gtrue = True
    gfalse = False

instance Semiring Double where
    x &&& y = x * y
    x ||| y = x + y
    gtrue = 1.0
    gfalse = 0.0

-----------------------------------------------------------
-- Some example grammars

-- cfg1 :: GenericCFG Cat String Bool
-- cfg1 = makeGCFG False ([S,NP,VP,PP,V,P], ["fish","sheep","deer","near","with"],
--                        [(S,True)],
--                        [(NTRule S (NP,VP), True),
--                         (NTRule NP (NP,NP), True),
--                         (NTRule NP (NP,PP), True),
--                         (NTRule VP (V,NP), True),
--                         (NTRule VP (VP,PP), True),
--                         (NTRule PP (P,NP), True),
--                         (TRule NP "fish", True),
--                         (TRule NP "sheep", True),
--                         (TRule NP "deer", True),
--                         (TRule V "near", True),
--                         (TRule V "fish", True),
--                         (TRule P "near", True),
--                         (TRule P "with", True)
--                        ]
--                       )
--
-- cfg2 :: GenericCFG Cat String Bool
-- cfg2 = makeGCFG False ([S,NP,VP,PP,V,P], ["fish","sheep","deer","near","with"],
--                        [(S,True), (NP,True)],
--                        [(NTRule S (NP,VP), True),
--                         (NTRule NP (NP,NP), True),
--                         (NTRule NP (NP,PP), True),
--                         (NTRule VP (V,NP), True),
--                         (NTRule VP (VP,PP), True),
--                         (NTRule PP (P,NP), True),
--                         (TRule NP "fish", True),
--                         (TRule NP "sheep", True),
--                         (TRule NP "deer", True),
--                         (TRule V "near", True),
--                         (TRule V "fish", True),
--                         (TRule P "near", True),
--                         (TRule P "with", True)
--                        ]
--                       )
--
-- -- Probabilistic version of cfg1
-- cfg3 :: GenericCFG Cat String Double
-- cfg3 = makeGCFG 0 ([S,NP,VP,PP,V,P], ["fish","sheep","deer","near","with"],
--                    [(S,1.0)],
--                    [(NTRule S (NP,VP), 1.0),
--                     (NTRule NP (NP,NP), 0.1),
--                     (NTRule NP (NP,PP), 0.2),
--                     (NTRule VP (V,NP), 0.7),
--                     (NTRule VP (VP,PP), 0.3),
--                     (NTRule PP (P,NP), 1.0),
--                     (TRule NP "fish", 0.2), (TRule NP "sheep", 0.4), (TRule NP "deer", 0.1),
--                     (TRule V "near", 0.4), (TRule V "fish", 0.6),
--                     (TRule P "near", 0.5), (TRule P "with", 0.5)
--                    ]
--                   )

-- From (7) on the handout
cfg7 :: GenericCFG Cat String Bool
cfg7 = makeGCFG False ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"],
                       [(VP,True)],
                       [(NTRule VP (V,NP), True),   (NTRule NP (NP,PP), True),     (NTRule PP (P,NP), True),
                        (NTRule VP (VP,PP), True),  (TRule NP "telescopes", True),
                        (TRule VP "watches", True), (TRule NP "watches", True),    (TRule P "with", True),
                        (TRule VP "spies", True),   (TRule NP "spies", True),      (TRule V "watches", True)
                       ]
                      )

-- A probabilistic version of cfg7
cfg8 :: GenericCFG Cat String Double
cfg8 = makeGCFG 0.0 ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"],
                     [(VP,1.0)],
                     [(NTRule VP (V,NP), 0.4),   (NTRule NP (NP,PP), 0.2),     (NTRule PP (P,NP), 1.0),
                      (NTRule VP (VP,PP), 0.3),  (TRule NP "telescopes", 0.3),
                      (TRule VP "watches", 0.2), (TRule NP "watches", 0.3),    (TRule P "with", 1.0),
                      (TRule VP "spies", 0.1),   (TRule NP "spies", 0.2),      (TRule V "watches", 1.0)
                     ]
                    )

-- Another probabilistic version of cfg7, with starting probability
-- split evenly between VP and NP
cfg9 :: GenericCFG Cat String Double
cfg9 = makeGCFG 0.0 ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"],
                     [(VP,0.5), (NP,0.5)],
                     [(NTRule VP (V,NP), 0.4),   (NTRule NP (NP,PP), 0.2),     (NTRule PP (P,NP), 1.0),
                      (NTRule VP (VP,PP), 0.3),  (TRule NP "telescopes", 0.3),
                      (TRule VP "watches", 0.2), (TRule NP "watches", 0.3),    (TRule P "with", 1.0),
                      (TRule VP "spies", 0.1),   (TRule NP "spies", 0.2),      (TRule V "watches", 1.0)
                     ]
                    )

-----------------------------------------------------------
-- Functions for inside values

insideBool :: GenericCFG nt t Bool -> [t] -> nt -> Bool
insideBool cfg str n =
    let (nts, ts, i, r) = cfg in
    case str of
    [] -> False
    (x:[]) -> r (TRule n x)
    (_:(_:_)) -> let conjResult i ld rd = r (NTRule n (ld,rd)) &&
                                          insideBool cfg (take i str) ld &&
                                          insideBool cfg (drop i str) rd in
                 or (liftA3 conjResult [1 .. length str - 1] nts nts)

inside :: (Semiring v) => GenericCFG nt t v -> [t] -> nt -> v
inside cfg str n =
    let (nts, ts, i, r) = cfg in
    case str of
    [] -> gfalse
    (x:[]) -> r (TRule n x)
    (_:(_:_)) -> let conjResult i ld rd = r (NTRule n (ld,rd)) &&&
                                          inside cfg (take i str) ld &&&
                                          inside cfg (drop i str) rd in
                 gen_or (liftA3 conjResult [1 .. length str - 1] nts nts)

fastInside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> [t] -> nt -> v
fastInside cfg str n =
    let (nts, ts, i, r) = cfg in
    M.memoize2
    (\smaller -> \str -> \n ->
        case str of
        [] -> M.lift0 gfalse
        (x:[]) -> M.lift0 (r (TRule n x))
        (_:(_:_)) -> let conjResult i ld rd = M.liftMany gen_and [M.lift0 (r (NTRule n (ld,rd))),
                                                                  smaller (take i str) ld,
                                                                  smaller (drop i str) rd] in
                     M.liftMany gen_or (liftA3 conjResult [1 .. length str - 1] nts nts)
    ) str n

---------------------------------------------------------------------------------
-- This combination of two expressions, where we're very careful we make sure to
-- cover all our cases systematically:
--
--     case str of
--     [] -> ...
--     (x:rest) -> case rest of
--                 [] -> ...
--                 (y:rest2) -> ...
--
-- is equivalent to this shorter version, which is essentially what we're using
-- in the inside functions above:
--
--     case str of
--     [] -> ...
--     (x:[]) -> ...
--     (x:(y:rest2)) -> ...
--
