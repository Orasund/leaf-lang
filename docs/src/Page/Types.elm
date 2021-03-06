module Page.Types exposing (chapter)

import Data.Shared exposing (Model)
import Data.Test.Types as Test
import ElmBook.Chapter as Chapter
import ElmBook.ElmUI exposing (Chapter)
import View.Chapter as Chapter


chapter : ( String, List (Chapter Model) )
chapter =
    ( "Expressions"
    , [ Chapter.view "Introduction" introduction
      , Chapter.view "Null" null
      , Chapter.view "Bool" bool
      , Chapter.view "String" string
      , Chapter.view "Int" int
      , Chapter.view "Float" float
      , Chapter.view "List" list
      , Chapter.view "Object" object
      , Chapter.view "Function" function
      , Chapter.view "Extension Functions" extensionFunctions
      ]
    )


introduction : String
introduction =
    """
Leaf is dynamically typed (just like JavaScript), this means values have types, but variables don't.

<component with-label="DynamicallyTyped" />

### Expression

![Syntax for Expressions](https://orasund.github.io/leaf-lang/syntax/expression.svg)

### Constructor

![Syntax for Expressions](https://orasund.github.io/leaf-lang/syntax/constructor.svg)
"""


null : String
null =
    """
Null is a type with a single value (itself). It behaves the same as `()` in Elm.

<component with-label="NullValue" />

You can use `null` similar to `Nothing`.

<component with-label="NullableValue" />

In our definition of `append` we have specified that we only allow strings. So, if we try appending `null`, we get a runtime error.

### Null

![Syntax for Null](https://orasund.github.io/leaf-lang/syntax/null.svg)"""


bool : String
bool =
    """Booleans can either be `true` or `false`.

<component with-label="TrueValue" />

<component with-label="FalseValue" />

### Boolean

![Syntax for Booleans](https://orasund.github.io/leaf-lang/syntax/bool.svg)"""


string : String
string =
    """Strings are the same as Strings in Elm. Leaf has neither exceptions nor a result type. Instead you can define an error variable that contains your error message.

<component with-label="StringValue" />

You can check the value of  `error` after the script has run.
``` elm
let
  (value,context) =
    script
      |> Leaf.run Core.package
in
case context |> Dict.get "error |> Maybe.withDefault NullVal of
  NullVal ->
    Ok value
  errVal ->
    Err errVal
```

### String

![Syntax for Strings](https://orasund.github.io/leaf-lang/syntax/string.svg)"""


int : String
int =
    """Leaf has a integers same as Elm. Though it does not have chars.

<component with-label="IntValue" />"""


float : String
float =
    """Floats are similar to Elm's floats, yet different: Nan, Inf and -Inf are not numbers so when dividing by zero you should return an error instead. 

<component with-label="FloatValue" />"""


list : String
list =
    """Lists can have any size and any content. You think of it as a resizable tuple in Elm.

<component with-label="ListValue" />

Lists do no allow a random access, so therefore it takes linear time to get or update an entry.

### List

![Syntax for Lists](https://orasund.github.io/leaf-lang/syntax/list.svg)"""


object : String
object =
    """Objects are the same as objects in javascript. They contain key-value pairs that may be inserted or removed. Behind the scenes its just a Elm Dict of type `Leaf.Value`.

<component with-label="ObjValue" />

`{}` is the empty object, note that it looks like a block without content.

<component with-label="EmptyObj" />

To get the value inside an object you have to define your own extension function.

```
""" ++ Test.objectGetterString ++ """
```

### Object

![Syntax for objects](https://orasund.github.io/leaf-lang/syntax/object.svg)"""


function : String
function =
    """Functions are primitive types in Leaf.

<component with-label="FunctionValue" />

The function may be mutable. In this case the function can be overwritten by another value (or function).

<component with-label="MutFunValue" />

Function are curried so you can partially apply a function. This also works for extension functions.

<component with-label="CurryFun" />

### Function

![Syntax for Functions](https://orasund.github.io/leaf-lang/syntax/function.svg)"""


extensionFunctions : String
extensionFunctions =
    """You may also check if something is an extension function.

<component with-label="ExtensionValue" />"""
