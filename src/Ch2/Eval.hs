{-# LANGUAGE LambdaCase #-}
module Ch2.Eval where

import Ch2.TM ( S(B), Q, D(..), Delta, Program, Tape )

eval :: Program -> Tape -> Tape
eval (state, δ) tape = exec δ (state, tape)

exec :: Delta -> (Q, Tape) -> Tape
exec δ = \ case
    (q, t@(ls, h, rs)) -> case lookup (q, h) δ of
        Nothing         -> t
        Just (q', s, d) -> exec δ (q', move d (ls, s, rs))

move :: D -> Tape -> Tape
move = \ case
    L -> moveL
    R -> moveR

moveL :: Tape -> Tape
moveL = \ case
    (ls, h, rs) -> (tl ls, hd ls, cons (h, rs))

moveR :: Tape -> Tape
moveR = \ case
    (ls, h, rs) -> (cons (h, rs), hd rs, tl rs)

hd :: [S] -> S
hd = \ case
    []  -> B
    s:_ -> s

tl :: [S] -> [S]
tl = \ case
    []   -> []
    _:ss -> ss

cons :: (S, [S]) -> [S]
cons = \ case
    (B, ss) -> ss
    (s, ss) -> s : ss


