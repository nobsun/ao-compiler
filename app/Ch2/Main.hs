module Main where

import Ch2.Eval ( eval )
import Ch2.TM ( S(I), Tape, p )

main :: IO ()
main = print (tape, eval p tape)

tape :: Tape
tape = ([I,I,I], I, [])
