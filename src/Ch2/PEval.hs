{-# LANGUAGE LambdaCase #-}
module Ch2.PEval where

import Ch2.PM 

eval :: Program -> Tape -> Tape
eval (state, δ) tape = exec δ (state, tape)

exec :: Delta -> (Q, Tape) -> Tape
exec δ (q, t@(ls, h, rs)) = case lookingUp (q, h) δ of
    []  -> t
    qas -> exec δ (foldl phi (q, t) qas)
    where
        phi (q, t) = \ case
            (q', Write s) -> (q', write s t)
            (q', Move d)  -> (q', move d t)

lookingUp :: Eq a => a -> [(a, b)] -> [b]
lookingUp k = \ case
    [] -> []
    (k',v) : kvs
        | k == k'   -> v : lookingUp k kvs
        | otherwise -> lookingUp k kvs

write :: S -> Tape -> Tape
write s = \ case
    (ls,_,rs) -> (ls,s,rs)

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


