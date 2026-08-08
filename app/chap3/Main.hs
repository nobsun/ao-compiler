-- # Main 雛形
--
{-# LANGUAGE GHC2024 #-}
module Main 
    ( main
    ) where

import Data.List
import System.Environment

main :: IO ()
main =   putStr . unlines  . words 
     =<< maybe getContents (readFile . fst) . uncons
     =<< getArgs where
