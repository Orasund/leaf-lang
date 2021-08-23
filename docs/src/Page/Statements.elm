module Page.Statements exposing (chapter)

import Data.Shared exposing (Model)
import ElmBook.Chapter as Chapter
import ElmBook.ElmUI exposing (Chapter)
import View.Chapter as Chapter


chapter : Chapter Model
chapter =
    content
        |> Chapter.view "Statements & Blocks"


content : String
content =
    """
A Leaf script is composed out of a list of statements, followed by an expression. All examples up until now where expressions. So we will now look into statements.
## Let
The keyword `let` specifies a constant value. Same as in Elm, this value can not be changed afterwards.

<component with-label="LetExample" />

## Mut
The keyword `mut` specifies a value that can be change.

<component with-label="MutExample" />

## Comments 
Leaf supports both single- and multi-line comments.

<component with-label="Comments" />

"""
