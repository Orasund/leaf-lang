module Leaf exposing
    ( Exp, Value(..), run
    , Field, field, mutField, unaryFun, binaryFun, trinaryFun
    , untyped, typed, asNull, asBool, asString, asFloat, asInt, asNullable, asAnyList, asList, asObject, asFunction, asExtension
    , toString, addPackage, addExposed
    )

{-| This is the main module for working with Leaf scripts.


# Basic Evaluation

To run this script in Elm you need to call `Leaf.run`.

    import Leaf exposing (Value(..))

    "\"Hello World\""
        |> Leaf.run Dict.empty
        --> Ok (StringVal "Hello World",Dict.empty)

@docs Exp, Value, run


# Context-Sensitive Evaluation

You can also evaluate context sensitive Leaf scripts.

    import Leaf exposing (Value(..))
    import Dict exposing (Dict)
    import Leaf.Core as Core

    context : Dict String Field
    context =
        [ StringVal "World" |> Leaf.field "name"
        , (\s2 s1 -> StringVal (s1 ++ s2))
            |> Leaf.binaryFun (Leaf.typed Leaf.asString)
                (Leaf.typed Leaf.asString)
            |> Leaf.field "append"
        ]
            |> Dict.fromList
            |> Leaf.addExposed Core.package

    "\"Hello \" .append name"
        |> Leaf.run context
        --> Ok (StringVal "Hello World",context)

@docs Field, field, mutField, unaryFun, binaryFun, trinaryFun

@docs untyped, typed, asNull, asBool, asString, asFloat, asInt, asNullable, asAnyList, asList, asObject, asFunction, asExtension


# Utility Functions

@docs toString, addPackage, addExposed

-}

import Dict exposing (Dict)
import Internal.Language as Language exposing (Exp)
import Internal.Semantics as Semantics exposing (Access(..))
import Internal.Syntax as Syntax
import Internal.Type as Type
import Internal.Util as Util


{-| Every Leaf script evaluates to a Value
-}
type Value
    = NullVal
    | StringVal String
    | BoolVal Bool
    | FloatVal Float
    | IntVal Int
    | ListVal (List Value)
    | ObjectVal (Dict String Value)
    | FunctionVal (Maybe String) Exp
    | ExtensionVal (Value -> Result String Value)


{-| Internal type for fields
-}
type alias Field =
    Semantics.Field


{-| A leaf script is internally translated into an Exp
-}
type alias Exp =
    Language.Exp


{-| States that an extension function takes an untyped value

    import Leaf.Core as Core

    (Core.isNull >> BoolVal)
        |> Leaf.unaryFun Leaf.untyped
        |> Leaf.field "isNull"

-}
untyped : (Value -> Value) -> Value
untyped fun =
    ExtensionVal (fun >> Ok)


{-| States that an extension function takes a typed value

    import Leaf.Core as Core

    Core.if_
        |> Leaf.trinaryFun (Leaf.typed Leaf.asBool) Leaf.untyped Leaf.untyped
        |> Leaf.field "if"

-}
typed : (Value -> Result String a) -> (a -> Value) -> Value
typed mapper fun =
    ExtensionVal (mapper >> Result.map fun)


{-| Turns an Elm function with one argument into a Leaf extension

    import Leaf.Core as Core

    (Core.isNull >> BoolVal)
        |> Leaf.unaryFun Leaf.untyped
        |> Leaf.field "isNull"

-}
unaryFun : ((a -> Value) -> Value) -> (a -> Value) -> Value
unaryFun a fun =
    a fun


{-| Turns an Elm function with two argument into a Leaf extension

    import Leaf.Core as Core
    import Leaf exposing (Field)

    (\v1 v2 -> Core.equal v1 v2 |> BoolVal)
        |> Leaf.binaryFun Leaf.untyped Leaf.untyped
        |> Leaf.field "equal"

-}
binaryFun : ((a -> Value) -> Value) -> ((b -> Value) -> Value) -> (a -> b -> Value) -> Value
binaryFun first second fun =
    first <|
        \a ->
            second <|
                \b ->
                    fun a b


{-| Turns an Elm function with three argument into a Leaf extension

    import Leaf.Core as Core
    import Leaf exposing (Field)

    (\v1 v2 -> Core.equal v1 v2 |> BoolVal)
        |> Leaf.binaryFun Leaf.untyped Leaf.untyped
        |> Leaf.field "equal"

-}
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


{-| Constructs a context field. Use this in combination with Dict.fromList
-}
field : String -> Value -> ( String, Field )
field string value =
    ( string
    , { value = internalToValue value
      , access = Read
      }
    )


{-| Constructs a mutable context field. Use this in combination with Dict.fromList
-}
mutField : String -> Value -> ( String, Field )
mutField string value =
    ( string
    , { value = internalToValue value
      , access = ReadWrite
      }
    )


{-| Evaluates a Leaf script. The dictionary may contain fields that can be called from inside the Leaf script.

    import Leaf exposing (Value(..))
    import Dict exposing (Dict)

    context : Dict String Field
    context =
        [ StringVal "World" |> Leaf.field "name"
        , (\s2 s1 -> StringVal (s1 ++ s2) )
            |> Leaf.binaryFun
                (Leaf.typed Leaf.asString)
                (Leaf.typed Leaf.asString)
            |> Leaf.field "append"
        ]
        |> Dict.fromList

    "\"Hello \" .append name"
        |> Leaf.run context
        --> Ok (StringVal "Hello World",context)

-}
run : Dict String Field -> String -> Result String ( Value, Dict String Field )
run context =
    Syntax.parse
        >> Result.andThen
            (Semantics.eval context)
        >> Result.map (Tuple.mapFirst internalFromValue)


{-| Converts a Leaf null into an Elm unit
-}
asNull : Value -> Result String ()
asNull =
    internalToValue >> Type.null


{-| Converts a Leaf string into an Elm string
-}
asString : Value -> Result String String
asString =
    internalToValue >> Type.string


{-| Converts a Leaf bool into an Elm bool
-}
asBool : Value -> Result String Bool
asBool =
    internalToValue >> Type.bool


{-| Converts a Leaf int into an Elm int
-}
asInt : Value -> Result String Int
asInt =
    internalToValue >> Type.int


{-| Converts a Leaf float into an Elm float
-}
asFloat : Value -> Result String Float
asFloat =
    internalToValue >> Type.float


{-| converts a Leaf value into an Elm maybe type
-}
asNullable : (Value -> Result String a) -> Value -> Result String (Maybe a)
asNullable fun =
    internalToValue >> Type.nullable (internalFromValue >> fun)


{-| converts a Leaf list into an Elm list of values
-}
asAnyList : Value -> Result String (List Value)
asAnyList =
    internalToValue >> Type.list Type.any >> Result.map (List.map internalFromValue)


{-| converts a Leaf list into an Elm list type
-}
asList : (Value -> Result String a) -> Value -> Result String (List a)
asList fun =
    internalToValue >> Type.list (internalFromValue >> fun)


{-| converts a Leaf object into an Elm dict
-}
asObject : Value -> Result String (Dict String Value)
asObject =
    internalToValue >> Type.dict Type.any >> Result.map (Dict.map (\_ -> internalFromValue))


{-| converts a Leaf functions into an Elm tuple
-}
asFunction : Value -> Result String ( Maybe String, Exp )
asFunction =
    internalToValue >> Type.function


{-| converts a Leaf extension into an Elm function
-}
asExtension : Value -> Result String (Value -> Result String Value)
asExtension =
    internalToValue
        >> Type.extension
        >> Result.map
            (\fun ->
                internalToValue >> fun >> Result.map internalFromValue
            )


{-| returns a readable string representation of a value
-}
toString : Value -> String
toString =
    internalToValue >> Util.valueToString


{-| Adds a package to a context and applies the naming convention `Package::functionName`.
-}
addPackage : String -> Dict String Field -> Dict String Field -> Dict String Field
addPackage name package =
    Dict.union
        (package
            |> Dict.toList
            |> List.map (\( k, v ) -> ( name ++ "::" ++ k, v ))
            |> Dict.fromList
        )


{-| Adds a package and exposes all function. This should only be used for essential packages.

    import Dict

    addExposed
        --> Dict.union

-}
addExposed : Dict String Field -> Dict String Field -> Dict String Field
addExposed =
    Dict.union



--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------


internalToValue : Value -> Language.Value
internalToValue value =
    case value of
        NullVal ->
            Language.NullVal

        StringVal string ->
            Language.StringVal string

        BoolVal bool ->
            Language.BoolVal bool

        IntVal number ->
            Language.IntVal number

        FloatVal number ->
            Language.FloatVal number

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

        Language.IntVal number ->
            IntVal number

        Language.FloatVal number ->
            FloatVal number

        Language.ListVal list ->
            ListVal (list |> List.map internalFromValue)

        Language.ObjectVal dict ->
            ObjectVal (dict |> Dict.map (\_ -> internalFromValue))

        Language.FunctionVal maybe exp ->
            FunctionVal maybe exp

        Language.ExtensionVal fun ->
            (internalToValue >> fun >> Result.map internalFromValue)
                |> ExtensionVal
