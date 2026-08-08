-- # Main
-- Postスタイル「チューリング言語」
{-# LANGUAGE GHC2024 #-}
module Main 
    ( main
    ) where

import Data.Map qualified as M

import Language.Turing.PTM
import Language.Turing.PEval

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
      [((M, I), (W, Write O))
      ,((M, O), (H, Write I))
      ,((M, B), (H, Write I))
      ,((W, O), (M, Move L))
      ]

pAddOne' :: Program 
pAddOne' = (S, δ) where
    δ = M.fromList
      [((S, B), (P, Move R))
      ,((P, I), (P, Move R))
      ,((P, O), (P, Move R))
      ,((P, B), (M, Move L))
      ,((M, I), (W, Write O))
      ,((M, O), (H, Write I))
      ,((M, B), (H, Write I))
      ,((W, O), (M, Move L))
      ,((H, I), (H, Move L))
      ,((H, O), (H, Move L))
      ]
