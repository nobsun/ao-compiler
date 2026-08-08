-- # Language.Turing.TM
-- 
-- ## 言語拡張と`module`宣言
-- 
{-# LANGUAGE GHC2024 #-}
module Language.Turing.TM
    ( Program
    , Q(..)
    , Delta
    , S(..)
    , D(..)
    , Tape
    ) where

import Data.Map ( Map )

type Program = (Q, Delta)

data Q
    = M
    | H
    | S
    | P
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

type Delta = Map (Q, S) (Q, S, D)

data S
    = B
    | I
    | O
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data D
    = L
    | R
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

type Tape = ([S], S, [S])
