module El.Core exposing (notEqual, package)

import Dict exposing (Dict)
import El
import El.Internal.Semantics exposing (Access(..), Field)
import El.Language exposing (Value(..))
import El.Type as Type


internalIsOk : Result a b -> Bool
internalIsOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


isNull : ( String, Value )
isNull =
    ( "isNull"
    , El.unaryFun (Type.null >> internalIsOk >> BoolVal)
        El.any
    )


isBool : ( String, Value )
isBool =
    ( "isBool"
    , El.unaryFun (Type.bool >> internalIsOk >> BoolVal)
        El.any
    )


isString : ( String, Value )
isString =
    ( "isString"
    , El.unaryFun (Type.string >> internalIsOk >> BoolVal)
        El.any
    )


isNumber : ( String, Value )
isNumber =
    ( "isNumber"
    , El.unaryFun (Type.number >> internalIsOk >> BoolVal)
        El.any
    )


isFloat : ( String, Value )
isFloat =
    ( "isFloat"
    , El.unaryFun (Type.float >> internalIsOk >> BoolVal)
        El.any
    )


isInt : ( String, Value )
isInt =
    ( "isInt"
    , El.unaryFun (Type.int >> internalIsOk >> BoolVal)
        El.any
    )


isList : ( String, Value )
isList =
    ( "isList"
    , El.unaryFun (Type.list Type.any >> internalIsOk >> BoolVal)
        El.any
    )


isObject : ( String, Value )
isObject =
    ( "isObject"
    , El.unaryFun (Type.dict Type.any >> internalIsOk >> BoolVal)
        El.any
    )


isFunction : ( String, Value )
isFunction =
    ( "isFunction"
    , El.unaryFun (Type.function >> internalIsOk >> BoolVal)
        El.any
    )


isExtension : ( String, Value )
isExtension =
    ( "isExtension"
    , El.unaryFun (Type.extension >> internalIsOk >> BoolVal)
        El.any
    )


equal : ( String, Value )
equal =
    ( "equal"
    , El.binaryFun (\a b -> a == b |> BoolVal)
        El.any
        El.any
    )


notEqual : ( String, Value )
notEqual =
    ( "notEqual"
    , El.binaryFun (\a b -> a /= b |> BoolVal)
        El.any
        El.any
    )


ifFun : ( String, Value )
ifFun =
    ( "if"
    , El.trinaryFun
        (\b v1 v2 ->
            if b then
                v1

            else
                v2
        )
        (El.typed Type.bool)
        El.any
        El.any
    )


package : Dict String Field
package =
    [ isNull
    , isBool
    , isString
    , isNumber
    , isFloat
    , isInt
    , isList
    , isObject
    , isFunction
    , isExtension
    , equal
    , notEqual
    , ifFun
    ]
        |> List.map (Tuple.mapSecond (\v -> { value = v, access = Read }))
        |> Dict.fromList
