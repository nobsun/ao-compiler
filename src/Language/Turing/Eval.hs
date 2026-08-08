-- # Language.Turing.Eval
-- 
-- ## 言語拡張と`module`宣言
-- 
{-# LANGUAGE GHC2024 #-}
module Language.Turing.Eval
    ( eval
    ) where

import Data.Map ((!?))
import Language.Turing.TM
import Debug.Trace

eval :: Program -> Tape -> Tape
eval (σ, δ) tape = exec δ (σ, tape)

exec :: Delta -> (Q, Tape) -> Tape
exec δ = \ case
    (q,tp@(ls,h,rs)) -> trace (show (q,tp)) $ case δ !? (q,h) of
        Nothing         -> tp
        Just (q', s, d) -> exec δ (q', move d (ls, s, rs))

move :: D -> Tape -> Tape
move = \ case
    L -> moveL
    R -> moveR

moveL :: Tape -> Tape
moveL = \ case
    (ls,h,rs) -> (tl ls, hd ls, cons (h,rs))

moveR :: Tape -> Tape
moveR = \ case
    (ls,h,rs) -> (cons (h,ls), hd rs, tl rs)

hd :: [S] -> S
hd = \ case
    []  -> B
    s:_ -> s

tl :: [S] -> [S]
tl = drop 1

cons :: (S,[S]) -> [S]
cons = \ case
    (B,ss) -> ss
    (s,ss) -> s : ss
