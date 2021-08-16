module El exposing (any, binaryFun, trinaryFun, typed, unaryFun)

import El.Language exposing (Exp(..), Number(..), Value(..))


any : (Value -> Value) -> Value
any fun =
    ExtensionVal (fun >> Ok)


typed : (Value -> Result String a) -> (a -> Value) -> Value
typed mapper fun =
    ExtensionVal (mapper >> Result.map fun)


unaryFun : (a -> Value) -> ((a -> Value) -> Value) -> Value
unaryFun a fun =
    fun a


binaryFun : (a -> b -> Value) -> ((a -> Value) -> Value) -> ((b -> Value) -> Value) -> Value
binaryFun fun first second =
    first <|
        \a ->
            second <|
                \b ->
                    fun a b


trinaryFun :
    (a -> b -> c -> Value)
    -> ((a -> Value) -> Value)
    -> ((b -> Value) -> Value)
    -> ((c -> Value) -> Value)
    -> Value
trinaryFun fun first second thrid =
    first <|
        \a ->
            second <|
                \b ->
                    thrid <|
                        \c ->
                            fun a b c
