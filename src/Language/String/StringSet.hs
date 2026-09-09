-- # Language.String.StringSet
-- 
-- ## 言語拡張と`module`宣言
-- 
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE NPlusKPatterns #-}
module Language.String.StringSet
    ( 
    ) where

import Data.Semigroup
import Control.Arrow
import Data.List
import Data.Set ( Set )
import Data.Set qualified as S

type Σ     = Set Char
type Σstar = Set NStr
type NStr  = (Int,[Char])

ε :: NStr
ε = (0,[])

nΣstar :: [Char] -> Int -> [NStr]
nΣstar sgm = \ case
    n+1 -> [ (fst cs+1,c:snd cs) | c <- sgm, cs <- css] where
        css = nΣstar sgm n
    _   -> [ ε ]

gΣstar :: Σ -> Int -> Σstar
gΣstar sgm n = S.fromAscList $ concatMap (nΣstar ss) [0 .. n] where
    ss  = S.toList sgm
            
{- ^
>>> sgm = S.fromList "abc" :: Σ
>>> gΣstar sgm 0
fromList [(0,"")]
>>> gΣstar sgm 1
fromList [(0,""),(1,"a"),(1,"b"),(1,"c")]
>>> gΣstar sgm 2
fromList [(0,""),(1,"a"),(1,"b"),(1,"c"),(2,"aa"),(2,"ba"),(2,"ca"),(2,"ab"),(2,"bb"),(2,"cb"),(2,"ac"),(2,"bc"),(2,"cc")]
>>> gΣstar sgm 3
fromList [(0,""),(1,"a"),(1,"b"),(1,"c"),(2,"aa"),(2,"ba"),(2,"ca"),(2,"ab"),(2,"bb"),(2,"cb"),(2,"ac"),(2,"bc"),(2,"cc"),(3,"aaa"),(3,"baa"),(3,"caa"),(3,"aba"),(3,"bba"),(3,"cba"),(3,"aca"),(3,"bca"),(3,"cca"),(3,"aab"),(3,"bab"),(3,"cab"),(3,"abb"),(3,"bbb"),(3,"cbb"),(3,"acb"),(3,"bcb"),(3,"ccb"),(3,"aac"),(3,"bac"),(3,"cac"),(3,"abc"),(3,"bbc"),(3,"cbc"),(3,"acc"),(3,"bcc"),(3,"ccc")]
-}

instance Semigroup Int where
    (<>) :: Int -> Int -> Int
    (<>) = (+)

instance Monoid Int where
    mempty :: Int
    mempty = 0

instance {-# OVERLAPPING #-} Semigroup Σstar where
    (<>) :: Σstar -> Σstar -> Σstar
    (<>) = cat

instance {-# OVERLAPPING #-} Monoid Σstar where
    mempty :: Σstar
    mempty = S.singleton ε

expn :: Σstar -> Int -> Σstar
expn a = \ case
    n+1 -> expn a n <> a
    _   -> mempty

{- ^
>>> expn S.empty 0
fromList [(0,"")]

>>> expn S.empty 1
fromList []

>>> expn S.empty 2
fromList []
-}

cat :: Σstar -> Σstar -> Σstar
cat as bs = S.fromList [ a <> b | a <- as', b <- bs' ] where
    as' = S.toList as
    bs' = S.toList bs

