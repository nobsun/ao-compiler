-- # Main
-- 「チューリング言語」
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
module Main 
    ( main
    ) where

import Data.Map qualified as M

import Language.Turing.TM
import Language.Turing.Eval

main :: IO ()
main = print (tp0, eval pAddOne tp0)
    >> print (tp1, eval pAddOne tp1)
    >> putStrLn "-- ex 2.2 --"
    >> print (tp0', eval pAddOne' tp0')
    >> print (tp1', eval pAddOne' tp1')

tp0 :: Tape
tp0 = ([I, I, I], I, [])

tp1 :: Tape
tp1 = ([I, O, I, I], I, [])

tp0' :: Tape
tp0' = ([], B, [I,I,I,I])

tp1' :: Tape
tp1' = ([], B, [I,O,I,I,I])

pAddOne :: Program
pAddOne = (M, δ) where
    δ = M.fromList 
      [((M, I), (M, O, L))
      ,((M, O), (H, I, L))
      ,((M, B), (H, I, L))]

pAddOne' :: Program
pAddOne' = (S, δ) where
    δ = M.fromList 
      [((S, B), (P, B, R))
      ,((P, I), (P, I, R))
      ,((P, O), (P, O, R))
      ,((P, B), (M, B, L))
      ,((M, I), (M, O, L))
      ,((M, O), (H, I, L))
      ,((M, B), (H, I, L))
      ,((H, I), (H, I, L))
      ,((H, O), (H, O, L))
      ]
