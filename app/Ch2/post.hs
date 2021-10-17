module Main where

import Ch2.PEval ( eval )
import Ch2.PM ( S(I), Tape, p )

main :: IO ()
main = print (tape, eval p tape)

tape :: Tape
tape = ([I,I,I], I, [])
