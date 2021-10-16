{-# LANGUAGE LambdaCase #-}
module Ch2.TM where

data D
    = L
    | R
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data S
    = B
    | I
    | O
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Q
    = M
    | H
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

type Delta = [((Q, S), (Q, S, D))]

type Program = (Q, Delta)

type Tape = ([S], S, [S])

p :: Program
p = (M, [((M, I), (M, O, L))
        ,((M, O), (H, I, L))
        ,((M, B), (H, I, L))])