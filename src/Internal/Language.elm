module Internal.Language exposing
    ( Block
    , Exp(..)
    , Statement(..)
    , Value(..)
    )

import Dict exposing (Dict)


type Value
    = NullVal
    | StringVal String
    | BoolVal Bool
    | IntVal Int
    | FloatVal Float
    | ListVal (List Value)
    | ObjectVal (Dict String Value)
    | FunctionVal (Maybe String) Exp
    | ExtensionVal (Value -> Result String Value)


type Exp
    = Variable String
    | NullExp
    | StringExp String
    | BoolExp Bool
    | IntExp Int
    | FloatExp Float
    | ListExp (List Exp)
    | ObjectExp (Dict String Exp)
    | FunctionExp (Maybe String) Exp
    | BlockExp Block
    | Apply Exp Exp


type Statement
    = Let String Exp
    | Mut String Exp
    | Set String Exp


type alias Block =
    { statements : List Statement, return : Exp }
