module El.Core.Number exposing (package)

import Dict exposing (Dict)
import El
import El.Internal.Semantics exposing (Access(..), Field)
import El.Language exposing (Number(..), Value(..))
import El.Type as Type
import El.Util as Util


plus : ( String, Value )
plus =
    ( "plus"
    , El.binaryFun
        (\v1 v2 ->
            case ( v1, v2 ) of
                ( IntNum n1, IntNum n2 ) ->
                    NumberVal (IntNum (n1 + n2))

                ( FloatNum n1, FloatNum n2 ) ->
                    NumberVal (FloatNum (n1 + n2))

                ( n1, n2 ) ->
                    NumberVal (FloatNum (Util.numToFloat n1 + Util.numToFloat n2))
        )
        (El.typed Type.number)
        (El.typed Type.number)
    )


minus : ( String, Value )
minus =
    ( "minus"
    , El.binaryFun
        (\v1 v2 ->
            case ( v1, v2 ) of
                ( IntNum n1, IntNum n2 ) ->
                    NumberVal (IntNum (n2 - n1))

                ( FloatNum n1, FloatNum n2 ) ->
                    NumberVal (FloatNum (n2 - n1))

                ( n1, n2 ) ->
                    NumberVal (FloatNum (Util.numToFloat n2 - Util.numToFloat n1))
        )
        (El.typed Type.number)
        (El.typed Type.number)
    )


mult : ( String, Value )
mult =
    ( "mult"
    , El.binaryFun
        (\v1 v2 ->
            case ( v1, v2 ) of
                ( IntNum n1, IntNum n2 ) ->
                    NumberVal (IntNum (n1 * n2))

                ( FloatNum n1, FloatNum n2 ) ->
                    NumberVal (FloatNum (n1 * n2))

                ( n1, n2 ) ->
                    NumberVal (FloatNum (Util.numToFloat n1 * Util.numToFloat n2))
        )
        (El.typed Type.number)
        (El.typed Type.number)
    )


divBy : ( String, Value )
divBy =
    ( "divBy"
    , El.binaryFun
        (\v1 v2 ->
            case ( v1, v2 ) of
                ( IntNum n1, IntNum n2 ) ->
                    NumberVal (IntNum (n2 // n1))

                ( FloatNum n1, FloatNum n2 ) ->
                    NumberVal (FloatNum (n2 / n1))

                ( n1, n2 ) ->
                    NumberVal (FloatNum (Util.numToFloat n2 / Util.numToFloat n1))
        )
        (El.typed Type.number)
        (El.typed Type.number)
    )


modByFun : ( String, Value )
modByFun =
    ( "modBy"
    , El.binaryFun
        (\n1 n2 ->
            NumberVal (IntNum (n2 |> modBy n1))
        )
        (El.typed Type.int)
        (El.typed Type.int)
    )


absFun : ( String, Value )
absFun =
    ( "abs"
    , El.unaryFun
        (\v1 ->
            case v1 of
                IntNum n ->
                    NumberVal (IntNum (abs n))

                FloatNum n ->
                    NumberVal (FloatNum (abs n))
        )
        (El.typed Type.number)
    )


roundFun : ( String, Value )
roundFun =
    ( "round"
    , El.unaryFun
        (\n ->
            NumberVal (IntNum (round n))
        )
        (El.typed Type.float)
    )


floorFun : ( String, Value )
floorFun =
    ( "floor"
    , El.unaryFun
        (\n ->
            NumberVal (IntNum (floor n))
        )
        (El.typed Type.float)
    )


ceilingFun : ( String, Value )
ceilingFun =
    ( "ceiling"
    , El.unaryFun
        (\n ->
            NumberVal (IntNum (ceiling n))
        )
        (El.typed Type.float)
    )


toFloat : ( String, Value )
toFloat =
    ( "toFloat"
    , El.unaryFun
        (\n ->
            NumberVal (FloatNum (toFloat n))
        )
        (El.typed Type.int)
    )


package : Dict String Field
package =
    [ plus
    , minus
    , mult
    , divBy
    , modByFun
    , absFun
    , roundFun
    , floorFun
    , ceilingFun
    , toFloat
    ]
        |> List.map (Tuple.mapSecond (\v -> { value = v, access = Read }))
        |> Dict.fromList
