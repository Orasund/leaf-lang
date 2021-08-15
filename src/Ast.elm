module Ast exposing
    ( BuildIn(..)
    , Closure
    , Exp(..)
    , Number(..)
    , Statement(..)
    , Value(..)
    )

import Dict exposing (Dict)


type Number
    = IntNum Int
    | FloatNum Float


type Value
    = NullVal
    | BoolVal Bool
    | NumberVal Number
    | ListVal (List Value)
    | ObjectVal (Dict String Value)
    | FunctionVal (Maybe String) Exp


type BuildIn
    = --------------------------------------------------------------------------
      -- Null Type
      --------------------------------------------------------------------------
      IsNull Exp
      --------------------------------------------------------------------------
      -- Bool Type
      --------------------------------------------------------------------------
    | If Exp Exp Exp
    | Eq Exp Exp
    | IsBool Exp
      --------------------------------------------------------------------------
      -- Number Type
      --------------------------------------------------------------------------
    | Plus Exp Exp
    | Mult Exp Exp
    | DivBy Exp Exp
    | Floor Exp
    | IsNumber Exp
    | IsInt Exp
    | IsFloat Exp
      --------------------------------------------------------------------------
      --type ListExp =
      --------------------------------------------------------------------------
    | Head Exp
    | Tail Exp
    | Prepend Exp Exp
    | Append Exp Exp
    | Length Exp
    | IsList Exp
      --------------------------------------------------------------------------
      --ObjectVal
      --------------------------------------------------------------------------
    | Insert String Exp Exp
    | Remove String Exp
    | Get String Exp
    | Size Exp
    | IsObject Exp
      --------------------------------------------------------------------------
      -- FunctionExp
      --------------------------------------------------------------------------
    | IsFunction Exp


type Exp
    = Variable String
    | Constant Value
    | ClosureExp Closure
    | Apply Exp Exp
    | BuildInFun BuildIn


type Statement
    = Let String Exp
    | Mut String Exp
    | Set String Exp


type alias Closure =
    { statements : List Statement, return : Exp }
