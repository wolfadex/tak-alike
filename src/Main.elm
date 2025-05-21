module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board
    , size : Int
    , turn : Player
    , selectedPieceWhite : Piece
    , selectedPieceBlack : Piece
    , pieceToMove : Maybe Int
    }


type alias Board =
    Dict Int (List ( Piece, Player ))


type Piece
    = Stone
    | Wall
    | CapStone


type Player
    = Black
    | White


init : () -> ( Model, Cmd Msg )
init _ =
    let
        size =
            5
    in
    ( { board =
            List.repeat (size * size) []
                |> List.indexedMap Tuple.pair
                |> Dict.fromList
      , size = size
      , turn = White
      , selectedPieceWhite = Stone
      , selectedPieceBlack = Stone
      , pieceToMove = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = SpaceSelected Int
    | SetSelectedPiece Piece


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSelectedPiece piece ->
            case model.turn of
                White ->
                    ( { model | selectedPieceWhite = piece }, Cmd.none )

                Black ->
                    ( { model | selectedPieceBlack = piece }, Cmd.none )

        SpaceSelected index ->
            case model.pieceToMove of
                Nothing ->
                    updateNoPieceSelected index model

                Just idx ->
                    if List.member index (adjacentIndices model.size idx) then
                        case boardGet idx model.board of
                            [] ->
                                ( model, Cmd.none )

                            pp :: _ ->
                                ( { model
                                    | board =
                                        model.board
                                            |> boardInsert index pp
                                            |> boardRemove idx
                                    , pieceToMove = Nothing
                                    , turn =
                                        case model.turn of
                                            White ->
                                                Black

                                            Black ->
                                                White
                                  }
                                , Cmd.none
                                )

                    else
                        updateNoPieceSelected index model


updateNoPieceSelected : Int -> Model -> ( Model, Cmd Msg )
updateNoPieceSelected index model =
    case boardGet index model.board of
        [] ->
            ( { model
                | board =
                    boardInsert index
                        ( case model.turn of
                            White ->
                                model.selectedPieceWhite

                            Black ->
                                model.selectedPieceBlack
                        , model.turn
                        )
                        model.board
                , turn =
                    case model.turn of
                        White ->
                            Black

                        Black ->
                            White
                , pieceToMove = Nothing
              }
            , Cmd.none
            )

        _ ->
            ( { model
                | pieceToMove = Just index
              }
            , Cmd.none
            )


boardGet : Int -> Board -> List ( Piece, Player )
boardGet index board =
    board
        |> Dict.get index
        |> Maybe.withDefault []


boardInsert : Int -> ( Piece, Player ) -> Board -> Board
boardInsert index pp board =
    Dict.insert index [ pp ] board


boardRemove : Int -> Board -> Board
boardRemove index board =
    Dict.insert index [] board


view : Model -> Browser.Document Msg
view model =
    { title = "Tak-Alike"
    , body =
        [ Html.h1 [] [ Html.text "Tak-Alike" ]
        , Html.h3 []
            [ Html.text <|
                case model.turn of
                    White ->
                        "White"

                    Black ->
                        "Black"
            ]
        , model.board
            |> Dict.toList
            |> List.map (viewBoardSpace model.pieceToMove)
            |> Html.div
                [ Html.Attributes.style "display" "grid"
                , Html.Attributes.style "grid-template-columns" ("repeat(" ++ String.fromInt model.size ++ ", 10rem)")
                , Html.Attributes.style "grid-template-rows" ("repeat(" ++ String.fromInt model.size ++ ", 10rem)")
                , Html.Attributes.style "gap" "4px"
                ]
        ]
    }


to2d : Int -> Int -> ( Int, Int )
to2d size index =
    ( index |> modBy size
    , index // size
    )


from2d : Int -> ( Int, Int ) -> Int
from2d size ( x, y ) =
    y * size + x


adjacentIndices : Int -> Int -> List Int
adjacentIndices size index =
    let
        ( x, y ) =
            to2d size index
    in
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]
        |> List.filter (\( x_, y_ ) -> x_ >= 0 && x_ < size && y >= 0 && y < size)
        |> List.map (from2d size)


viewBoardSpace : Maybe Int -> ( Int, List ( Piece, Player ) ) -> Html Msg
viewBoardSpace pieceToMove ( index, pieceStack ) =
    let
        ps p =
            case p of
                Black ->
                    "B"

                White ->
                    "W"
    in
    Html.button
        [ Html.Events.onClick (SpaceSelected index)
        , Html.Attributes.style "border-style" "solid"
        , Html.Attributes.style "border-width" "3px"
        , Html.Attributes.style "border-color" <|
            case pieceToMove of
                Nothing ->
                    "gray"

                Just idx ->
                    if idx == index then
                        "cornflowerblue"

                    else
                        "gray"
        , Html.Attributes.style "background-color" <|
            case pieceStack of
                [] ->
                    ""

                ( _, White ) :: _ ->
                    "rgb(245, 245, 245)"

                ( _, Black ) :: _ ->
                    "rgb(10, 10, 10)"
        , Html.Attributes.style "color" <|
            case pieceStack of
                [] ->
                    ""

                ( _, White ) :: _ ->
                    "rgb(10, 10, 10)"

                ( _, Black ) :: _ ->
                    "rgb(245, 245, 245)"
        ]
        [ case pieceStack of
            [] ->
                Html.text " "

            ( Stone, player ) :: _ ->
                Html.text ("S-" ++ ps player)

            ( Wall, player ) :: _ ->
                Html.text ("W-" ++ ps player)

            ( CapStone, player ) :: _ ->
                Html.text ("C-" ++ ps player)
        ]
