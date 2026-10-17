-- # FA.FA
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
module FA.FA
    ( NFA (..)
    , DFA (..)
    , toDFA
    ) where

import Data.Bool
import Data.Map qualified as M
import Data.Set qualified as S

type Q  = Int
type S  = String
ε :: S
ε = ""
type Qs = S.Set Q
type Σ  = S.Set S
type Ω  = M.Map S Qs
type Δ  = M.Map Q Ω

type QQs = S.Set Qs
type ΔΔ  = M.Map Qs Ω

data NFA
    = NFA
    { nQs :: Qs
    , nΣ  :: Σ
    , nδ  :: Δ
    , nq₀ :: Q
    , nFs :: Qs
    }
    deriving (Eq, Show)

data DFA
    = DFA
    { dQQs :: QQs
    , dΣ   :: Σ
    , dΔ   :: ΔΔ
    , dQs₀ :: Qs
    , dFs  :: Qs
    }
    deriving (Eq, Show)

{- $ setup
>>> let ?nfa = sampleNFA
-}
toDFA :: NFA -> DFA
toDFA nfa = let ?nfa = nfa in
    let { aa       = εCl (S.singleton nfa.nq₀)
        ; (qqs,δδ) = subsets (S.singleton aa, S.empty, M.empty)
        ; ff       = undefined
        } 
    in  DFA { dQQs = qqs
            , dΣ   = nfa.nΣ
            , dΔ   = δδ
            , dQs₀ = aa
            , dFs  = ff
            }

subsets :: (?nfa :: NFA) => (QQs, QQs, ΔΔ) -> (QQs, ΔΔ)
subsets = undefined

εCl :: (?nfa :: NFA) => Qs -> Qs
εCl qs = qs `S.union` foldl' phi S.empty qs
    where
        δ = ?nfa.nδ
        phi s q = case δ M.!? q of
            Nothing -> s
            Just o  -> case o M.!? ε of
                Nothing  -> s
                Just rs  -> rs
{-
>>> closure 
-}
closure :: (?nfa :: NFA) => Qs -> Qs
closure p = εCl (undefined (εCl p))

sampleNFA :: NFA
sampleNFA = NFA
    { nQs = S.fromList [0 .. 6]
    , nΣ  = S.fromList [ε,"a","b"]
    , nδ  = M.fromList [(0, M.fromList [("a", S.fromList [1])])
                       ,(1, M.fromList [(ε,   S.fromList [2,4])])
                       ,(2, M.fromList [("a", S.fromList [3])
                                       ,("b", S.fromList [3])])
                       ,(3, M.fromList [(ε,   S.fromList [2,4])])
                       ,(4, M.fromList [("a", S.fromList [5])])
                       ,(5, M.fromList [("b", S.fromList [6])])
                       ]
    , nq₀ = 0
    , nFs = S.fromList [6]
    }