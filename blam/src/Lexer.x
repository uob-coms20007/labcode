{
module Lexer (Token(..), PToken(..), tkVar, tkNum, tkPos,
              alexScanTokens) where
}

-- We don't bother with a fancy interface. Just gimme tokens!
%wrapper "posn"

-- The lexer's job is to turn strings into lexical tokens that the parser consumes
tokens :-
  $white+                       ;
  PUSH   | push                 { \p _ -> MkToken p $ TPush          }
  ADD    | add                  { \p _ -> MkToken p $ TAdd           }
  MUL    | mul                  { \p _ -> MkToken p $ TMul           }
  SUB    | sub                  { \p _ -> MkToken p $ TSub           }
  TRUE   | true                 { \p _ -> MkToken p $ TBool True     }
  FALSE  | false                { \p _ -> MkToken p $ TBool False    }
  EQ     | eq                   { \p _ -> MkToken p $ TEq            }
  LE     | le                   { \p _ -> MkToken p $ TLe            }
  NOT    | not                  { \p _ -> MkToken p $ TNot           }
  AND    | and                  { \p _ -> MkToken p $ TAnd           }
  LOAD   | load                 { \p _ -> MkToken p $ TFetch         }
  STORE  | store                { \p _ -> MkToken p $ TStore         }
  GOTO   | goto                 { \p _ -> MkToken p $ TGoto          }
  GOTOF  | gotof                { \p _ -> MkToken p $ TGotoF         }
  NOOP   | noop                 { \p _ -> MkToken p $ TNoop          }
  \;                            { \p _ -> MkToken p $ TSemiC         }
  a-z [a-z A-Z 0-9 \_ \']*      { \p s -> MkToken p $ TVar $ s       }
  \-? [0-9]+                    { \p n -> MkToken p $ TNum $ read n  }

{
data Token =
    TVar String | TNum Integer | TBool Bool
  | TPush       | TAdd         | TMul   | TSub
  | TEq         | TLe          | TNot   | TAnd
  | TFetch      | TStore
  | TGoto       | TGotoF       | TNoop
  | TSemiC

instance Show Token where
  show (TVar v)  = v
  show (TNum n)  = show n
  show (TBool b) = show b
  show TPush     = "PUSH"
  show TAdd      = "ADD"
  show TMul      = "MUL"
  show TSub      = "SUB"
  show TEq       = "EQ"
  show TLe       = "LE"
  show TNot      = "NOT"
  show TAnd      = "AND"
  show TFetch    = "LOAD"
  show TStore    = "STORE"
  show TGoto     = "GOTO"
  show TGotoF    = "GOTOF"
  show TNoop     = "NOOP"
  show TSemiC    = ";"

data PToken = MkToken AlexPosn Token

instance Show PToken where
  show ptk@(MkToken _ tk) =
    let (l, c) = tkPos ptk in
    show tk ++ " (l." ++ show l ++ ", c." ++ show c ++ ")"

tkVar :: PToken -> String
tkVar (MkToken _ (TVar v)) = v
tkVar _                    = undefined

tkNum :: PToken -> Integer
tkNum (MkToken _ (TNum n)) = n
tkNum _                    = undefined

tkPos :: PToken -> (Int, Int)
tkPos (MkToken (AlexPn _ lin col) _) = (lin, col)
}
