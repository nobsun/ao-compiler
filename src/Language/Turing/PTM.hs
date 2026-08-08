-- # Language.Turing.PTM
-- 
-- ## 言語拡張と`module`宣言
-- 
{-# LANGUAGE GHC2024 #-}
module Language.Turing.PTM
    ( Program
    , Q(..)
    , Delta
    , A(..)
    , S(..)
    , D(..)
    , Tape
    ) where

import Data.Map ( Map )
import Data.Map qualified as M

{- $setup
>>> :set -XOverloadedStrings
-}

type Program = (Q, Delta)

data Q
    = S
    | P
    | M
    | W
    | H
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

type Delta = Map (Q, S) (Q, A)

data A
    = Move D
    | Write S
    deriving (Eq,Show,Read)

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

