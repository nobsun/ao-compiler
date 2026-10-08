-- # Simple Lexer using ReadP
--
{-# LANGUAGE GHC2024 #-}

module Main 
    ( main
    ) where

import System.Environment
import Lexer.Simple

main :: IO ()
main =  getArgs 
    >>= phi
    >>= mapM_ print . init . simpleLexer
    where
        phi = \ case
            []  -> getContents
            a:_ -> readFile a
