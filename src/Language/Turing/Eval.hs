-- # Language.Turing.Eval
-- 
-- ## 言語拡張と`module`宣言
-- 
{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Language.Turing.Eval
    where

import Data.Map qualified as M
import Language.Turing.TM
{- $setup
>>> :set -XOverloadedStrings
-}

{- | 
-}

eval :: Program -> Tape -> Tape
eval (σ, δ) tape = exec δ (σ, tape)

exec :: Delta -> (Q, Tape) -> Tape
exec δ = \ case
    (q,tp@(ls,h,rs)) -> case δ M.!? (q,h) of
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
    (ls,h,rs) -> (cons (h,rs), hd rs, tl ls)

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
