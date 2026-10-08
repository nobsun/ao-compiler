{
module Lexer.AlexSimple
    ( alexSimpleLexer
    ) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$alphanum = [$alpha $digit]

@id = $alpha $alphanum*
@num = $digit+
@frac = \. @num
@exp = [eE] (\-?) @num
@real = (\-?) @num @frac? @exp?

tokens :-

  $white+           ;
  \"[^\"]*\"        {\ s -> STRING (trim s) }
  \_                {\ _ -> UNDERBAR }
  @id               {\ s -> ID s}
  @real             {\ s -> REAL s}
  .                 {\ s -> SPECIAL s}

{
data Token
    = EOF
    | UNDERBAR
    | ID String
    | STRING String
    | REAL String
    | SPECIAL String
    deriving (Eq, Show)

trim :: String -> String
trim = reverse . drop 1 . reverse . drop 1

alexSimpleLexer :: String -> [Token]
alexSimpleLexer = alexScanTokens
}