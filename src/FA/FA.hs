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
import Data.Maybe
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

{-
>>> toDFA sampleNFA
-}
toDFA :: NFA -> DFA
toDFA nfa = let { ?nfa = nfa } in
    let { ?aa      = εCl (S.singleton nfa.nq₀) } in
        let { (qqs,δδ) = subsets (S.singleton ?aa, S.empty, M.empty)
            ; ff       = undefined
            } 
        in  DFA { dQQs = qqs
                , dΣ   = nfa.nΣ
                , dΔ   = δδ
                , dQs₀ = ?aa
                , dFs  = ff
                }

subsets :: (?nfa :: NFA, ?aa :: Qs) => (QQs, QQs, ΔΔ) -> (QQs, ΔΔ)
subsets = \ case
    (qq, qq2, dd)
        | S.null qq -> (qq2, dd)
        | otherwise -> case qq `S.difference` (S.singleton ?aa) of
            qq1 -> subsets (addQ ?aa (qq1,qq2,dd))
        
addQ :: (?nfa :: NFA) => Qs -> (QQs,QQs,ΔΔ) -> (QQs,QQs,ΔΔ)
addQ aa = \ case
    (qq1,qq2,dd) -> case foldl' phi (qq1, M.empty) ?nfa.nΣ of
        (qq1',o') -> (qq1', S.insert aa qq2, M.insert aa o' dd)
        where
            phi (qq, o) s = addS (aa, s) (qq, qq2, o)

addS :: (?nfa :: NFA) => (Qs, S) -> (QQs, QQs, Ω) -> (QQs, Ω)
addS (aa, s) (qq1,qq2,o) = (qq1', M.insert s aa' o)
    where
        qq1' = bool (S.insert aa' qq1) qq1
                    (S.member aa' (S.unions [S.singleton aa, qq1, qq2 ])) 
        aa'  = δD aa s
{-
>>> qs0 = S.singleton sampleNFA.nq₀
>>> let ?nfa = sampleNFA in εCl qs0
fromList [0]
>>> let ?nfa = sampleNFA in εCl (S.singleton 1)
fromList [1,2,4]
-}
εCl :: (?nfa :: NFA) => Qs -> Qs
εCl qs = iter qs qs
    where
        iter as bs = case foldl' phi S.empty bs of
            bs'       -> case S.difference bs' as of
                bs'' | S.null bs'' -> as
                     | otherwise   -> iter (S.union as bs'') bs''
        phi xs x = S.union xs (transition x ε)
{-
-}
transition :: (?nfa :: NFA) => Q -> S -> Qs
transition q s = maybe S.empty (fromMaybe S.empty . (M.!? s)) (?nfa.nδ M.!? q)

δD :: (?nfa :: NFA) => Qs -> S -> Qs
δD p s = εCl (foldl' phi S.empty (εCl p))
    where
        phi a q = S.union a (transition q s)

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
