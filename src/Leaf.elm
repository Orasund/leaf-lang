module Leaf exposing
    ( Number(..), Value(..), eval
    , Field, field, mutField, unaryFun, binaryFun, trinaryFun
    , untyped, typed, asNull, asBool, asString, asFloat, asInt, asNumber, asNullable, asAnyList, asList, asObject, asFunction, asExtension, Exp
    , toString, toFloat
    )

{-| This is the main module for working with Leaf scripts.


# Basic Evaluation

This package provides a simple way to evaluate Leaf scripts.

```elm
import Leaf exposing (Value(..))

"\"Hello World\""
  |> El.eval Dict.empty
  --> (StringVal "\"Hello World\"",Dict.empty)
```

@docs Number, Value, eval


# Context-Sensitive Evaluation

You can also evaluate context sensitive Leaf scripts.

```
import Leaf exposing (Value(..))

let
  context =
    [ ("name", Elm.field (StringVal "World")
    , ("append"
      , (\s2 s1 -> s1 ++ s2)
          |> El.binaryFunction (Leaf.typed El.asString) (Leaf.typed El.asString)
          |> Elm.field
      )
    ]
      |> Dict.fromList
in
"\"Hello \".append name"
  |> El.eval context
  --> (StringVal "\"Hello World\"",context)
```

@docs Field, field, mutField, unaryFun, binaryFun, trinaryFun

@docs untyped, typed, asNull, asBool, asString, asFloat, asInt, asNumber, asNullable, asAnyList, asList, asObject, asFunction, asExtension, Exp

# Utility Functions

@docs toString, toFloat
-}

import Dict exposing (Dict)
import Internal.Language as Language exposing (Exp)
import Internal.Semantics as Semantics exposing (Access(..))
import Internal.Syntax as Syntax
import Internal.Type as Type
import Internal.Util as Util

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


type alias Field =
    Semantics.Field


type alias Exp =
    Language.Exp


untyped : (Value -> Value) -> Value
untyped fun =
    ExtensionVal (fun >> Ok)


typed : (Value -> Result String a) -> (a -> Value) -> Value
typed mapper fun =
    ExtensionVal (mapper >> Result.map fun)


unaryFun : ((a -> Value) -> Value) -> (a -> Value) -> Value
unaryFun a fun =
    a fun


binaryFun : ((a -> Value) -> Value) -> ((b -> Value) -> Value) -> (a -> b -> Value) -> Value
binaryFun first second fun =
    first <|
        \a ->
            second <|
                \b ->
                    fun a b


trinaryFun :
    ((a -> Value) -> Value)
    -> ((b -> Value) -> Value)
    -> ((c -> Value) -> Value)
    -> (a -> b -> c -> Value)
    -> Value
trinaryFun first second thrid fun =
    first <|
        \a ->
            second <|
                \b ->
                    thrid <|
                        \c ->
                            fun a b c


field : String -> Value -> ( String, Field )
field string value =
    ( string
    , { value = internalToValue value
      , access = Read
      }
    )


mutField : String -> Value -> ( String, Field )
mutField string value =
    ( string
    , { value = internalToValue value
      , access = ReadWrite
      }
    )


eval : Dict String Field -> String -> Result String Value
eval context =
    Syntax.parse
        >> Result.andThen
            (Semantics.eval context)
        >> Result.map internalFromValue


asNull : Value -> Result String ()
asNull =
    internalToValue >> Type.null


asString : Value -> Result String String
asString =
    internalToValue >> Type.string


asBool : Value -> Result String Bool
asBool =
    internalToValue >> Type.bool


asInt : Value -> Result String Int
asInt =
    internalToValue >> Type.int


asFloat : Value -> Result String Float
asFloat =
    internalToValue >> Type.float


asNumber : Value -> Result String Number
asNumber =
    internalToValue >> Type.number >> Result.map internalFromNumber


asNullable : (Value -> Result String a) -> Value -> Result String (Maybe a)
asNullable fun =
    internalToValue >> Type.nullable (internalFromValue >> fun)


asAnyList : Value -> Result String (List Value)
asAnyList =
    internalToValue >> Type.list Type.any >> Result.map (List.map internalFromValue)


asList : (Value -> Result String a) -> Value -> Result String (List a)
asList fun =
    internalToValue >> Type.list (internalFromValue >> fun)


asObject : Value -> Result String (Dict String Value)
asObject =
    internalToValue >> Type.dict Type.any >> Result.map (Dict.map (\_ -> internalFromValue))


asFunction : Value -> Result String ( Maybe String, Exp )
asFunction =
    internalToValue >> Type.function


asExtension : Value -> Result String (Value -> Result String Value)
asExtension =
    internalToValue
        >> Type.extension
        >> Result.map
            (\fun ->
                internalToValue >> fun >> Result.map internalFromValue
            )


toString : Value -> String
toString =
    internalToValue >> Util.valueToString


toFloat : Number -> Float
toFloat =
    internalToNumber >> Util.numToFloat

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------


internalToNumber : Number -> Language.Number
internalToNumber n =
    case n of
        IntNum int ->
            Language.IntNum int

        FloatNum float ->
            Language.FloatNum float


internalFromNumber : Language.Number -> Number
internalFromNumber n =
    case n of
        Language.IntNum int ->
            IntNum int

        Language.FloatNum float ->
            FloatNum float


internalToValue : Value -> Language.Value
internalToValue value =
    case value of
        NullVal ->
            Language.NullVal

        StringVal string ->
            Language.StringVal string

        BoolVal bool ->
            Language.BoolVal bool

        NumberVal number ->
            Language.NumberVal (internalToNumber number)

        ListVal list ->
            Language.ListVal (list |> List.map internalToValue)

        ObjectVal dict ->
            Language.ObjectVal (dict |> Dict.map (\_ -> internalToValue))

        FunctionVal maybe exp ->
            Language.FunctionVal maybe exp

        ExtensionVal fun ->
            (internalFromValue >> fun >> Result.map internalToValue)
                |> Language.ExtensionVal


internalFromValue : Language.Value -> Value
internalFromValue value =
    case value of
        Language.NullVal ->
            NullVal

        Language.StringVal string ->
            StringVal string

        Language.BoolVal bool ->
            BoolVal bool

        Language.NumberVal number ->
            NumberVal (internalFromNumber number)

        Language.ListVal list ->
            ListVal (list |> List.map internalFromValue)

        Language.ObjectVal dict ->
            ObjectVal (dict |> Dict.map (\_ -> internalFromValue))

        Language.FunctionVal maybe exp ->
            FunctionVal maybe exp

        Language.ExtensionVal fun ->
            (internalToValue >> fun >> Result.map internalFromValue)
                |> ExtensionVal
