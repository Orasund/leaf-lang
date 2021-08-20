module Leaf.Number exposing (..)

{--module El.Core.Number exposing (package)

import Dict exposing (Dict)
import Leaf
import El.Internal.Semantics exposing (Access(..), Field)
import El.Language exposing (Number(..), Value(..))
import Internal.Type as Type
import El.Util as Util


plus : ( String, Value )
plus =
    ( Leaflus"
    , El.binaryFun
        (\v1 v2 ->
            case ( v1, v2 ) of
                ( IntNum n1, IntNum n2 ) ->
                    NumberVal (IntNum (n1 + n2))

                ( FloatNum n1, FloatNum n2 ) ->
                    NumberVal (FloatNum (n1 + n2))

                ( n1, n2 ) ->
                    NumberVal (FloatNum (Util.numToFloat n1 + Util.numToFloat n2))
        )Leaf
        (Leaf.typed Type.number)
        (El.typed Type.number)
    )


minus : ( String, Value )
minus =
    ( Leafinus"
    , El.binaryFun
        (\v1 v2 ->
            case ( v1, v2 ) of
                ( IntNum n1, IntNum n2 ) ->
                    NumberVal (IntNum (n2 - n1))

                ( FloatNum n1, FloatNum n2 ) ->
                    NumberVal (FloatNum (n2 - n1))

                ( n1, n2 ) ->
                    NumberVal (FloatNum (Util.numToFloat n2 - Util.numToFloat n1))
        )Leaf
        (Leaf.typed Type.number)
        (El.typed Type.number)
    )


mult : ( String, Value )
mult =
    ( Leafult"
    , El.binaryFun
        (\v1 v2 ->
            case ( v1, v2 ) of
                ( IntNum n1, IntNum n2 ) ->
                    NumberVal (IntNum (n1 * n2))

                ( FloatNum n1, FloatNum n2 ) ->
                    NumberVal (FloatNum (n1 * n2))

                ( n1, n2 ) ->
                    NumberVal (FloatNum (Util.numToFloat n1 * Util.numToFloat n2))
        )Leaf
        (Leaf.typed Type.number)
        (El.typed Type.number)
    )


divBy : ( String, Value )
divBy =
    ( LeafivBy"
    , El.binaryFun
        (\v1 v2 ->
            case ( v1, v2 ) of
                ( IntNum n1, IntNum n2 ) ->
                    NumberVal (IntNum (n2 // n1))

                ( FloatNum n1, FloatNum n2 ) ->
                    NumberVal (FloatNum (n2 / n1))

                ( n1, n2 ) ->
                    NumberVal (FloatNum (Util.numToFloat n2 / Util.numToFloat n1))
        )Leaf
        (Leaf.typed Type.number)
        (El.typed Type.number)
    )


modByFun : ( String, Value )
modByFun =
    ( LeafodBy"
    , El.binaryFun
        (\n1 n2 ->
            NumberVal (IntNum (n2 |> modBy n1))
        )Leaf
        (Leaf.typed Type.int)
        (El.typed Type.int)
    )


absFun : ( String, Value )
absFun =
    ( Leafbs"
    , El.unaryFun
        (\v1 ->
            case v1 of
                IntNum n ->
                    NumberVal (IntNum (abs n))

                FloatNum n ->
                    NumberVal (FloatNum (abs n))
        )Leaf
        (El.typed Type.number)
    )


roundFun : ( String, Value )
roundFun =
    ( Leafound"
    , El.unaryFun
        (\n ->
            NumberVal (IntNum (round n))
        )Leaf
        (El.typed Type.float)
    )


floorFun : ( String, Value )
floorFun =
    ( Leafloor"
    , El.unaryFun
        (\n ->
            NumberVal (IntNum (floor n))
        )Leaf
        (El.typed Type.float)
    )


ceilingFun : ( String, Value )
ceilingFun =
    ( Leafeiling"
    , El.unaryFun
        (\n ->
            NumberVal (IntNum (ceiling n))
        )Leaf
        (El.typed Type.float)
    )


toFloat : ( String, Value )
toFloat =
    ( "toFloat"
    , El.unaryFun
      Leaf(\n ->
            NumberVal (FloatNum (toFloat n))
        )
        (El.typed Type.int)
    )Leaf


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
--}
