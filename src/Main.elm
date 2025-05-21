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
    , stoneCountWhite : Int
    , stoneCountBlack : Int
    , capstoneCountWhite : Int
    , capstoneCountBlack : Int
    , winner : Maybe ( Player, List Int )
    }


type alias Board =
    Dict Int (List ( Piece, Player ))


type Piece
    = Stone
    | Wall
    | Capstone


type Player
    = Black
    | White


init : () -> ( Model, Cmd Msg )
init _ =
    ( initGame, Cmd.none )


initGame =
    let
        size =
            3

        stoneCount =
            if size == 3 then
                10

            else if size == 4 then
                15

            else if size == 5 then
                21

            else if size == 6 then
                30

            else if size == 8 then
                50

            else
                0

        capstoneCount =
            if size == 3 then
                0

            else if size == 4 then
                0

            else if size == 5 then
                1

            else if size == 6 then
                1

            else if size == 8 then
                2

            else
                0
    in
    { board =
        List.repeat (size * size) []
            |> List.indexedMap Tuple.pair
            |> Dict.fromList
    , size = size
    , turn = White
    , selectedPieceWhite = Stone
    , selectedPieceBlack = Stone
    , pieceToMove = Nothing
    , stoneCountWhite = stoneCount
    , stoneCountBlack = stoneCount
    , capstoneCountWhite = capstoneCount
    , capstoneCountBlack = capstoneCount
    , winner = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NewGame
    | SpaceSelected Int
    | SetSelectedPiece Piece


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( initGame, Cmd.none )

        SetSelectedPiece piece ->
            case model.turn of
                White ->
                    ( { model | selectedPieceWhite = piece }, Cmd.none )

                Black ->
                    ( { model | selectedPieceBlack = piece }, Cmd.none )

        SpaceSelected index ->
            case model.winner of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    Tuple.mapFirst
                        (\m ->
                            { m | winner = checkWinCondition m.size m.board }
                        )
                    <|
                        case model.pieceToMove of
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

                            Nothing ->
                                updateNoPieceSelected index model


updateNoPieceSelected : Int -> Model -> ( Model, Cmd Msg )
updateNoPieceSelected index model =
    case boardGet index model.board of
        [] ->
            let
                piece =
                    case model.turn of
                        White ->
                            model.selectedPieceWhite

                        Black ->
                            model.selectedPieceBlack

                ( remStones, remCapstones, ( decStone, decCapstone ) ) =
                    case model.turn of
                        White ->
                            ( model.stoneCountWhite
                            , model.capstoneCountWhite
                            , ( \m -> { m | stoneCountWhite = m.stoneCountWhite - 1 }
                              , \m -> { m | capstoneCountWhite = m.capstoneCountWhite - 1 }
                              )
                            )

                        Black ->
                            ( model.stoneCountBlack
                            , model.capstoneCountBlack
                            , ( \m -> { m | stoneCountBlack = m.stoneCountBlack - 1 }
                              , \m -> { m | capstoneCountBlack = m.capstoneCountBlack - 1 }
                              )
                            )
            in
            case ( piece, remStones, remCapstones ) of
                ( Capstone, _, 0 ) ->
                    ( model, Cmd.none )

                ( Capstone, _, _ ) ->
                    ( { model
                        | board =
                            boardInsert index
                                ( piece, model.turn )
                                model.board
                        , turn =
                            case model.turn of
                                White ->
                                    Black

                                Black ->
                                    White
                        , pieceToMove = Nothing
                      }
                        |> decCapstone
                    , Cmd.none
                    )

                ( _, 0, _ ) ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | board =
                            boardInsert index
                                ( piece, model.turn )
                                model.board
                        , turn =
                            case model.turn of
                                White ->
                                    Black

                                Black ->
                                    White
                        , pieceToMove = Nothing
                      }
                        |> decStone
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


checkWinCondition : Int -> Board -> Maybe ( Player, List Int )
checkWinCondition size board =
    let
        xStart =
            List.range 0 (size - 1)
                |> List.concatMap
                    (\x ->
                        [ from2d size ( x, 0 )
                        ]
                    )

        xEnd =
            List.range 0 (size - 1)
                |> List.concatMap
                    (\x ->
                        [ from2d size ( x, size - 1 )
                        ]
                    )
    in
    case checkWinConditionHelper size board (Debug.log "start" xStart) (Debug.log "end" xEnd) of
        Just w ->
            Just w

        Nothing ->
            let
                yStart =
                    List.range 0 (size - 1)
                        |> List.concatMap
                            (\y ->
                                [ from2d size ( 0, y )
                                ]
                            )

                yEnd =
                    List.range 0 (size - 1)
                        |> List.concatMap
                            (\y ->
                                [ from2d size ( 0, size - 1 )
                                ]
                            )
            in
            checkWinConditionHelper size board yStart yEnd


checkWinConditionHelper : Int -> Board -> List Int -> List Int -> Maybe ( Player, List Int )
checkWinConditionHelper size board start end =
    case start of
        [] ->
            Nothing

        next :: rest ->
            case boardGet next board of
                [] ->
                    checkWinConditionHelper size board rest end

                ( piece, player ) :: _ ->
                    if piece == Stone || piece == Capstone then
                        case floodFill size board (Debug.log "ff next" next) (Debug.log "ff player" player) |> Debug.log "ff result" of
                            [] ->
                                checkWinConditionHelper size board rest end

                            indicies ->
                                if List.any (\e -> List.member e indicies) end then
                                    Just ( player, indicies )

                                else
                                    checkWinConditionHelper size board rest end

                    else
                        checkWinConditionHelper size board rest end


floodFill : Int -> Board -> Int -> Player -> List Int
floodFill size board start player =
    floodFillHelper size board start player [ start ]


floodFillHelper : Int -> Board -> Int -> Player -> List Int -> List Int
floodFillHelper size board start player collected =
    start
        |> Debug.log "ff start"
        |> adjacentIndices size
        |> Debug.log "ff adjacent"
        |> List.foldl
            (\idx coll ->
                if List.member idx coll then
                    coll

                else
                    case boardGet idx board of
                        [] ->
                            coll

                        ( Wall, _ ) :: _ ->
                            coll

                        ( _, plr ) :: _ ->
                            if plr == player then
                                floodFillHelper size board idx player (idx :: coll)

                            else
                                coll
            )
            collected



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Tak-Alike"
    , body =
        [ Html.h1 [] [ Html.text "Tak-Alike" ]
        , case model.winner of
            Nothing ->
                Html.text ""

            Just _ ->
                Html.button
                    [ Html.Events.onClick NewGame ]
                    [ Html.text "New game" ]
        , Html.h3 []
            [ Html.text <|
                case model.turn of
                    White ->
                        "White"

                    Black ->
                        "Black"
            ]
        , Html.p
            []
            [ Html.h4 [] [ Html.text "Piece to place" ]
            , let
                isSelected piece =
                    (case model.turn of
                        White ->
                            model.selectedPieceWhite

                        Black ->
                            model.selectedPieceBlack
                    )
                        |> (==) piece
                        |> boolToAttribute
                        |> Html.Attributes.attribute "aria-pressed"
              in
              Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "gap" "0.5rem"
                ]
                [ Html.button
                    [ Html.Events.onClick (SetSelectedPiece Stone)
                    , isSelected Stone
                    ]
                    [ Html.text "Stone" ]
                , Html.button
                    [ Html.Events.onClick (SetSelectedPiece Wall)
                    , isSelected Wall
                    ]
                    [ Html.text "Wall" ]
                , Html.button
                    [ Html.Events.onClick (SetSelectedPiece Capstone)
                    , isSelected Capstone
                    ]
                    [ Html.text "Capstone" ]
                ]
            , Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "gap" "0.5rem"
                ]
                [ Html.span []
                    [ Html.text
                        ("Stones left: "
                            ++ (String.fromInt <|
                                    case model.turn of
                                        White ->
                                            model.stoneCountWhite

                                        Black ->
                                            model.stoneCountBlack
                               )
                        )
                    ]
                , Html.span []
                    [ Html.text
                        ("Captones left: "
                            ++ (String.fromInt <|
                                    case model.turn of
                                        White ->
                                            model.capstoneCountWhite

                                        Black ->
                                            model.capstoneCountBlack
                               )
                        )
                    ]
                ]
            ]
        , model.board
            |> Dict.toList
            |> List.map (viewBoardSpace model.winner model.pieceToMove)
            |> Html.div
                [ Html.Attributes.style "display" "grid"
                , Html.Attributes.style "grid-template-columns" ("repeat(" ++ String.fromInt model.size ++ ", 10rem)")
                , Html.Attributes.style "grid-template-rows" ("repeat(" ++ String.fromInt model.size ++ ", 10rem)")
                , Html.Attributes.style "gap" "4px"
                ]
        ]
    }


boolToAttribute : Bool -> String
boolToAttribute bool =
    if bool then
        "true"

    else
        "false"


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


viewBoardSpace : Maybe ( Player, List Int ) -> Maybe Int -> ( Int, List ( Piece, Player ) ) -> Html Msg
viewBoardSpace maybeWinner pieceToMove ( index, pieceStack ) =
    Html.button
        [ Html.Events.onClick (SpaceSelected index)
        , Html.Attributes.style "border-style" "solid"
        , Html.Attributes.style "border-width" "10px"
        , Html.Attributes.style "border-color" <|
            case maybeWinner of
                Nothing ->
                    case pieceToMove of
                        Nothing ->
                            "gray"

                        Just idx ->
                            if idx == index then
                                "cornflowerblue"

                            else
                                "gray"

                Just ( _, indicies ) ->
                    if List.member index indicies then
                        "green"

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

            ( Stone, _ ) :: _ ->
                Html.text "STN"

            ( Wall, _ ) :: _ ->
                Html.text "WLL"

            ( Capstone, _ ) :: _ ->
                Html.text "CAP"
        ]
