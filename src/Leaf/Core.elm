module Leaf.Core exposing (equal, if_, isBool, isExtension, isFloat, isFunction, isInt, isList, isNull, isObject, isString, notEqual, package)

import Dict exposing (Dict)
import Leaf exposing (Field, Value(..))


internalIsOk : Result a b -> Bool
internalIsOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


isNull : Value -> Bool
isNull =
    Leaf.asNull >> internalIsOk


isBool : Value -> Bool
isBool =
    Leaf.asBool >> internalIsOk


isString : Value -> Bool
isString =
    Leaf.asString >> internalIsOk


isFloat : Value -> Bool
isFloat =
    Leaf.asFloat >> internalIsOk


isInt : Value -> Bool
isInt =
    Leaf.asInt >> internalIsOk


isList : Value -> Bool
isList =
    Leaf.asAnyList >> internalIsOk


isObject : Value -> Bool
isObject =
    Leaf.asObject >> internalIsOk


isFunction : Value -> Bool
isFunction =
    Leaf.asFunction >> internalIsOk


isExtension : Value -> Bool
isExtension =
    Leaf.asExtension >> internalIsOk


equal : Value -> Value -> Bool
equal a b =
    a == b


notEqual : Value -> Value -> Bool
notEqual a b =
    a /= b


if_ : Bool -> Value -> Value -> Value
if_ b v1 v2 =
    if b then
        v1

    else
        v2


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
