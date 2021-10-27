{
module Lexer (Token(..), scanTokens) where
}

-- We don't bother with a fancy interface. Just gimme tokens!
%wrapper "basic"

-- The lexer's job is to turn strings into lexical tokens that the parser consumes
-- The following describes the classes of tokens we manipulate. Some are
-- parameterized by a value (when they describe a whole class)
-- The lexer for integer literals extracts the integer value using read.
tokens :-
  $white+                       ;
  \(                            { \_ -> TLParen       }
  \)                            { \_ -> TRParen       }
  \{                            { \_ -> TLBrace       }
  \}                            { \_ -> TRBrace       }
  true                          { \_ -> TTrue         }
  false                         { \_ -> TFalse        }
  \+                            { \_ -> TPlus         }
  \*                            { \_ -> TStar         }
  \-                            { \_ -> TMinus        }
  \=                            { \_ -> TEq           }
  \<\=                          { \_ -> TLe           }
  \!                            { \_ -> TBang         }
  \/\\                          { \_ -> TWedge        }
  \:\=                          { \_ -> TAssign       }
  if                            { \_ -> TIf           }
  then                          { \_ -> TThen         }
  else                          { \_ -> TElse         }
  while                         { \_ -> TWhile        }
  a-z [a-z A-Z 0-9 \_ \']*      { \s -> TId $ s       }
  [0-9]+                        { \n -> TInt $ read n }

{
data Token =
    TId String
  | TInt Integer
  | TTrue   | TFalse
  | TLParen | TRParen
  | TLBrace | TRBrace
  | TPlus   | TStar   | TMinus
  | TEq     | TLe
  | TBang   | TWedge
  | TAssign
  | TIf     | TThen   | TElse
  | TWhile
  deriving (Eq, Show)

scanTokens = alexScanTokens
}
