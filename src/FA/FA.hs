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

import Debug.Trace qualified as Debug

_DEBUG :: Bool
_DEBUG = () == ()

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
DFA {dQQs = fromList [fromList [0],fromList [1,2,4],fromList [2,3,4],fromList [2,3,4,5],fromList [2,3,4,6]], dΣ = fromList ["","a","b"], dΔ = fromList [(fromList [0],fromList [("a",fromList [1,2,4])]),(fromList [1,2,4],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4])]),(fromList [2,3,4],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4])]),(fromList [2,3,4,5],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4,6])]),(fromList [2,3,4,6],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4])])], dQs₀ = fromList [0], dFs = fromList [2,3,4,6]}
-}
toDFA :: NFA -> DFA
toDFA nfa = let { ?nfa = nfa } in
    let { (qqs,δδ) = subsets (S.singleton aa, S.empty, M.empty)
        ; aa       = εCl (S.singleton nfa.nq₀)
        ; ff       = foldl' phi S.empty qqs
            where
                phi a qs = bool (a `S.union` qs) a (S.null (S.intersection nfa.nFs qs))
        } 
    in  DFA { dQQs = qqs
            , dΣ   = nfa.nΣ
            , dΔ   = δδ
            , dQs₀ = aa
            , dFs  = ff
            }
{- $setup
>>> nfa0 = sampleNFA
>>> a0   = let ?nfa = nfa0 in εCl (S.singleton nfa0.nq₀)
>>> aa0  = S.singleton a0
-}

{-
>>> let ?nfa = nfa0 in subsets (aa0, S.empty, M.empty)
(fromList [fromList [0],fromList [1,2,4],fromList [2,3,4],fromList [2,3,4,5],fromList [2,3,4,6]],fromList [(fromList [0],fromList [("a",fromList [1,2,4])]),(fromList [1,2,4],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4])]),(fromList [2,3,4],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4])]),(fromList [2,3,4,5],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4,6])]),(fromList [2,3,4,6],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4])])])
-}
subsets :: (?nfa :: NFA) => (QQs, QQs, ΔΔ) -> (QQs, ΔΔ)
subsets (qq, qq2, dd) = bool (subsets (addQ aa (qq1,qq2,dd))) (qq2, dd) (S.null qq)
    where
        (aa, qq1) = S.deleteFindMin qq
{-
>>> let ?nfa = nfa0 in addQ a0 (S.empty, S.empty, M.empty)
(fromList [fromList [1,2,4]],fromList [fromList [0]],fromList [(fromList [0],fromList [("a",fromList [1,2,4])])])
-}        
addQ :: (?nfa :: NFA) => Qs -> (QQs,QQs,ΔΔ) -> (QQs,QQs,ΔΔ)
addQ aa = \ case
    (qq1,qq2,dd) -> case foldl' phi (qq1, M.empty) (S.drop 1 ?nfa.nΣ) of
        (qq1',o')    -> (qq1', S.insert aa qq2, M.insert aa o' dd)
        where
            phi (qq, o) s = addS (aa, s) (qq, qq2, o)

addS :: (?nfa :: NFA) => (Qs, S) -> (QQs, QQs, Ω) -> (QQs, Ω)
addS (as, s) (qq1,qq2,o) = (qq1', bool (M.insert s as' o) o notarget)
    where
        qq1'     = bool (S.insert as' qq1) qq1
                    (notarget || S.member as' (S.unions [S.singleton as, qq1, qq2 ])) 
        as'      = δD as s
        notarget = S.null as'
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

{-
toDFA sampleNFA → 

DFA {dQQs  = fromList [fromList [0],fromList [1,2,4],fromList [2,3,4],fromList [2,3,4,5],fromList [2,3,4,6]]
    , dΣ   = fromList ["","a","b"]
    , dΔ   =  fromList [(fromList [0]      ,fromList [("a",fromList [1,2,4])])
                       ,(fromList [1,2,4]  ,fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4])])
                       ,(fromList [2,3,4]  ,fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4])])
                       ,(fromList [2,3,4,5],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4,6])])
                       ,(fromList [2,3,4,6],fromList [("a",fromList [2,3,4,5]),("b",fromList [2,3,4])])]
    , dQs₀ = fromList [0]
    , dFs  = fromList [2,3,4,6]}
-}


{- debug print -}
_trace :: String -> a -> a
_trace | _DEBUG    = Debug.trace
       | otherwise = const id

_tracing :: Show a => a -> a
_tracing = _trace . show <*> id


