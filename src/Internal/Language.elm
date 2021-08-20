module Internal.Language exposing
    ( Closure
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
    | StringVal String
    | BoolVal Bool
    | NumberVal Number
    | ListVal (List Value)
    | ObjectVal (Dict String Value)
    | FunctionVal (Maybe String) Exp
    | ExtensionVal (Value -> Result String Value)


type Exp
    = Variable String
    | NullExp
    | StringExp String
    | BoolExp Bool
    | NumberExp Number
    | ListExp (List Exp)
    | ObjectExp (Dict String Exp)
    | FunctionExp (Maybe String) Exp
    | ClosureExp Closure
    | Apply Exp Exp


type Statement
    = Let String Exp
    | Mut String Exp
    | Set String Exp


type alias Closure =
    { statements : List Statement, return : Exp }
