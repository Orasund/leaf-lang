module Page.Statements exposing (chapter)

import Data.Shared exposing (Model)
import ElmBook.Chapter as Chapter
import ElmBook.ElmUI exposing (Chapter)
import View.Chapter as Chapter


chapter : ( String, List (Chapter Model) )
chapter =
    ( "Statements"
    , [ Chapter.view "Introduction" introduction
      , Chapter.view "Let" letContent
      , Chapter.view "Mut & Set" mutContent
      , Chapter.view "Comments" comments
      , Chapter.view "Blocks" blocks
      ]
    )

introduction : String
introduction =
    """
All examples up until now where expressions. So we will now look into statements.

### Statement

![Syntax for Statement](https://orasund.github.io/leaf-lang/syntax/statement.svg)
"""

letContent : String
letContent =
    """
The keyword `let` specifies a constant value. Same as in Elm, this value can not be changed afterwards.

<component with-label="LetExample" />

The variable must start with an lowercase letter (`a,b,c`) followed by
* Letters (`A,a,B,b,C,c,...`)
* Numerals (`0,1,2,3,...`)
* Underscore (`_`)"""

mutContent : String
mutContent =
    """
The keyword `mut` specifies a value that can be change using `set`.

<component with-label="MutExample" />
"""

comments : String
comments =
    """Leaf supports both single- and multi-line comments.

<component with-label="Comments" />"""

blocks : String
blocks =
    """
You can turn a group of statements into a statement using blocks. Just like a Leaf script, bocks consist of statements followed by a single expression (the return value).

<component with-label="BlockBasic" />

You can use a block anywhere you'd usually use an expression.

<component with-label="BlockAdvanced" />

Any variables that were constructed inside a block get destroyed ones the block is over. 
Additionally, variables outside the block can only be read, not written!

<component with-label="BlockUnmutable" />
"""
