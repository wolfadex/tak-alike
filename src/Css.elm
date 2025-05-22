module Css exposing (menu, menuJoinOption, menuOptions, gameplayArea, board, boardSpace, boardSpaceBackground, boardSpaceGrid, sizeSelecttion, stoneCounts, selectedStack, selectedStackDisplay, selectedStackPieces, stackSelectionInput, stackPiece, columnWithAction)

import Html
import Html.Attributes


menu : Html.Attribute msg
menu =
    Html.Attributes.class "menu"


menuJoinOption : Html.Attribute msg
menuJoinOption =
    Html.Attributes.class "menuJoinOption"


menuOptions : Html.Attribute msg
menuOptions =
    Html.Attributes.class "menuOptions"


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


columnWithAction : Html.Attribute msg
columnWithAction =
    Html.Attributes.class "columnWithAction"
