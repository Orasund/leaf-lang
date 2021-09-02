module Leaf.Core exposing
    ( package
    , equal, notEqual, if_
    , isBool, isExtension, isFloat, isFunction, isInt, isList, isNull, isObject, isString
    )

{-| This is the core package, it contains all essential functions.

@docs package


# Equality and If-Statement

@docs equal, notEqual, if_


# Type checking

@docs isBool, isExtension, isFloat, isFunction, isInt, isList, isNull, isObject, isString

-}

import Dict exposing (Dict)
import Leaf exposing (Field, Value(..))


{-| Checks if a value is null
-}
isNull : Value -> Bool
isNull =
    Leaf.asNull >> internalIsOk


{-| Checks if a value is a boolean
-}
isBool : Value -> Bool
isBool =
    Leaf.asBool >> internalIsOk


{-| Checks if a value is a string
-}
isString : Value -> Bool
isString =
    Leaf.asString >> internalIsOk


{-| Checks if a value is a float
-}
isFloat : Value -> Bool
isFloat =
    Leaf.asFloat >> internalIsOk


{-| Checks if a value is an int
-}
isInt : Value -> Bool
isInt =
    Leaf.asInt >> internalIsOk


{-| Checks if a value is a list. Does not check if all values of the list have the same type.
-}
isList : Value -> Bool
isList =
    Leaf.asAnyList >> internalIsOk


{-| Checks if a value is an object. This is not useful if you want an object with a specific structure, write your own checker instead.
-}
isObject : Value -> Bool
isObject =
    Leaf.asObject >> internalIsOk


{-| Checks if a value is a function.
-}
isFunction : Value -> Bool
isFunction =
    Leaf.asFunction >> internalIsOk


{-| Checks if a value is an extension function.
-}
isExtension : Value -> Bool
isExtension =
    Leaf.asExtension >> internalIsOk


{-| Checks if two values are (structually) equal.
-}
equal : Value -> Value -> Bool
equal a b =
    a == b


{-| Checks if two values are (structually) different.
-}
notEqual : Value -> Value -> Bool
notEqual a b =
    a /= b


{-| If the first value is true, it returns the second value, else it returns the third value.
-}
if_ : Bool -> Value -> Value -> Value
if_ b v1 v2 =
    if b then
        v1

    else
        v2


{-| Dict containing all functions in this package, read to be used in Leaf.
-}
package : Dict String Field
package =
    [ (isNull >> BoolVal) |> Leaf.unaryFun Leaf.untyped |> Leaf.field "isNull"
    , (isBool >> BoolVal) |> Leaf.unaryFun Leaf.untyped |> Leaf.field "isBool"
    , (isString >> BoolVal) |> Leaf.unaryFun Leaf.untyped |> Leaf.field "isString"
    , (isFloat >> BoolVal) |> Leaf.unaryFun Leaf.untyped |> Leaf.field "isFloat"
    , (isInt >> BoolVal) |> Leaf.unaryFun Leaf.untyped |> Leaf.field "isInt"
    , (isList >> BoolVal) |> Leaf.unaryFun Leaf.untyped |> Leaf.field "isList"
    , (isObject >> BoolVal) |> Leaf.unaryFun Leaf.untyped |> Leaf.field "isObject"
    , (isFunction >> BoolVal) |> Leaf.unaryFun Leaf.untyped |> Leaf.field "isFunction"
    , (isExtension >> BoolVal) |> Leaf.unaryFun Leaf.untyped |> Leaf.field "isExtension"
    , (\v1 v2 -> equal v1 v2 |> BoolVal)
        |> Leaf.binaryFun Leaf.untyped Leaf.untyped
        |> Leaf.field "equal"
    , (\v1 v2 -> notEqual v1 v2 |> BoolVal)
        |> Leaf.binaryFun Leaf.untyped Leaf.untyped
        |> Leaf.field "notEqual"
    , if_
        |> Leaf.trinaryFun (Leaf.typed Leaf.asBool) Leaf.untyped Leaf.untyped
        |> Leaf.field "if"
    ]
        |> Dict.fromList



--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------


internalIsOk : Result a b -> Bool
internalIsOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
