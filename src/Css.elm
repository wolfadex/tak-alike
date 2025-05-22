module Css exposing (gameplayArea, board, boardSpace, boardSpaceBackground, boardSpaceGrid, sizeSelecttion, stoneCounts, selectedStack, selectedStackDisplay, selectedStackPieces, stackSelectionInput, stackPiece)

import Html
import Html.Attributes


gameplayArea : Html.Attribute msg
gameplayArea =
    Html.Attributes.class "gameplayArea"


board : Html.Attribute msg
board =
    Html.Attributes.class "board"


boardSpace : Html.Attribute msg
boardSpace =
    Html.Attributes.class "boardSpace"


boardSpaceBackground : Html.Attribute msg
boardSpaceBackground =
    Html.Attributes.class "boardSpaceBackground"


boardSpaceGrid : Html.Attribute msg
boardSpaceGrid =
    Html.Attributes.class "boardSpaceGrid"


sizeSelecttion : Html.Attribute msg
sizeSelecttion =
    Html.Attributes.class "sizeSelecttion"


stoneCounts : Html.Attribute msg
stoneCounts =
    Html.Attributes.class "stoneCounts"


selectedStack : Html.Attribute msg
selectedStack =
    Html.Attributes.class "selectedStack"


selectedStackDisplay : Html.Attribute msg
selectedStackDisplay =
    Html.Attributes.class "selectedStackDisplay"


selectedStackPieces : Html.Attribute msg
selectedStackPieces =
    Html.Attributes.class "selectedStackPieces"


stackSelectionInput : Html.Attribute msg
stackSelectionInput =
    Html.Attributes.class "stackSelectionInput"


stackPiece : Html.Attribute msg
stackPiece =
    Html.Attributes.class "stackPiece"
