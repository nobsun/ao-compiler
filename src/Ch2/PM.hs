{-# LANGUAGE LambdaCase #-}
module Ch2.PM where

data S
    = B
    | I
    | O
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data D
    = L
    | R
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Q
    = M
    | H
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data A
    = Move D
    | Write S

type Delta = [((Q, S), (Q, A))]

type Program = (Q, Delta)

type Tape = ([S], S, [S])
{-
p = (M, [((M, I), (M, O, L))
        ,((M, O), (H, I, L))
        ,((M, B), (H, I, L))])
-}
p :: Program
p = (M, [((M, I), (M, Write O))
        ,((M, I), (M, Move L))
        ,((M, O), (M, Write I))
        ,((M, O), (H, Move L))
        ,((M, B), (M, Write I))
        ,((M, B), (H, Move L))])