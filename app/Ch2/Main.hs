module Main where

import Ch2.Eval
import Ch2.TM

main :: IO ()
main = print (tape, eval p tape)

tape :: Tape
tape = ([I,I,I], I, [])
