-- # Language.Turing.TM
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
module Language.Turing.TM
    where

import Data.Map ( Map )
import Data.Map qualified as M

{- $setup
>>> :set -XOverloadedStrings
-}

type Program = (Q, Delta)

data Q
    = M
    | H
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

pAddOne :: Program
pAddOne = (M, δ) where
    δ = M.fromList 
      [((M, I), (M, O, L))
      ,((M, O), (H, I, L))
      ,((M, B), (H, I, L))]
