-- # Lexer.Simple
--
-- ## 言語拡張と`module`宣言
--
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
module Lexer.Simple
    ( Token (..)
    , simpleLexer
    ) where

import Control.Arrow hiding ((+++))
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP
import Debug.Trace

data Token
    = EOF
    | UNDERBAR
    | ID String
    | STRING String
    | REAL String
    | SPECIAL String
    deriving (Eq, Show)

simpleLexer :: String -> [Token]
simpleLexer = firstParse . parse rTokens

rTokens :: ReadP [Token]
rTokens = many1 rToken

rToken :: ReadP Token
rToken =  foldr1 phi 
    [rUNDERBAR, rID, rSTRING, rREAL, rSPECIAL, rEOF]
    where
        phi r r' = (skipSpaces *> r) +++ r' 

rEOF :: ReadP Token
rEOF = EOF <$ (skipSpaces *> eof)

rUNDERBAR :: ReadP Token
rUNDERBAR = UNDERBAR <$ char '_'

rID :: ReadP Token
rID = ID <$> ((:) <$> satisfy isAlpha <*> munch isAlphaNum)

rSTRING :: ReadP Token
rSTRING = STRING <$> between (char '"') (char '"') (munch (/= '"'))

rREAL :: ReadP Token
rREAL = phi <$> quest (string "-")
            <*> rNum <*> quest rFrac <*> quest rExp 
    where
        phi a b c d = REAL $ concat $ catMaybes [a,Just b, c, d]

rNum :: ReadP String
rNum = munch1 isDigit

rFrac :: ReadP String
rFrac = (:) <$> char '.' <*> rNum

rExp :: ReadP String
rExp = (:) <$> (char 'e' <++ char 'E')
           <*> (phi <$> quest (char '-') <*> rNum)
    where
        phi = maybe id (:)

quest :: ReadP a -> ReadP (Maybe a)
quest p = (Just <$> p) <++ pure Nothing

rSPECIAL :: ReadP Token
rSPECIAL = SPECIAL <$> munch1 (not . flip any ps . flip ($))
    where
        ps = [ isAlphaNum
             , isSpace
             , flip elem ("_" :: String)
             ]

parse :: ReadP a -> String -> [(a, String)]
parse = readP_to_S

firstParse :: [([Token], String)] -> [Token]
firstParse rs =  case [ ts | (ts,"") <- rs ] of
    []   -> error "no parse"
    us:_ -> us
