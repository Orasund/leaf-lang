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
All examples up until now were expressions. We will now take a look at statements.

### Statement

![Syntax for Statement](https://orasund.github.io/leaf-lang/syntax/statement.svg)
"""

letContent : String
letContent =
    """
The keyword `let` specifies a constant value. Same as in Elm, this value can not be changed afterwards.

<component with-label="LetExample" />

### Variable

![Syntax for Let](https://orasund.github.io/leaf-lang/syntax/variable.svg)

### Let

![Syntax for Let](https://orasund.github.io/leaf-lang/syntax/let.svg)"""

mutContent : String
mutContent =
    """
The keyword `mut` specifies a value that can be change using `set`.

<component with-label="MutExample" />

### Mut

![Syntax for Mut](https://orasund.github.io/leaf-lang/syntax/mut.svg)

### Set

![Syntax for Set](https://orasund.github.io/leaf-lang/syntax/set.svg)"""

comments : String
comments =
    """Leaf supports both single- and multi-line comments.

<component with-label="Comments" />

### Comment

![Syntax for Comment](https://orasund.github.io/leaf-lang/syntax/comment.svg)

### SingleLineComment

![Syntax for Single Line Comment](https://orasund.github.io/leaf-lang/syntax/singleLineComment.svg)

### MultiLineComment

![Syntax for Multi Line Comment](https://orasund.github.io/leaf-lang/syntax/multiLineComment.svg)"""

blocks : String
blocks =
    """
You can turn group of statements into an expression using blocks. Bocks consist of statements followed by a single expression (the return value).

<component with-label="BlockBasic" />

You can use a block anywhere you'd usually use an expression.

<component with-label="BlockAdvanced" />

Any variables that were constructed inside get destroyed at the end of the block. 
Additionally, variables outside the block can only be read, not written!

<component with-label="BlockUnmutable" />
"""
