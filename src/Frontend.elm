module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Lamdera
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , game =
            let
                baseGame =
                    initGame 3
            in
            { baseGame | winner = Just ( White, [] ) }
      }
    , Cmd.none
    )


initGame : Int -> Game
initGame size =
    let
        stoneCount : Int
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

        capstoneCount : Int
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
    , stackSizeSelected = 1
    , newGameSize = size
    }


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        GameMessage gameMsg ->
            updateGame gameMsg model.game
                |> Tuple.mapFirst (\game -> { model | game = game })


updateGame msg game =
    case msg of
        NewGame ->
            ( initGame game.newGameSize, Cmd.none )

        SetGameSize size ->
            ( { game | newGameSize = size }, Cmd.none )

        StackSizeSelected sizeStr ->
            case String.toInt sizeStr of
                Nothing ->
                    ( game, Cmd.none )

                Just size ->
                    ( { game | stackSizeSelected = size }, Cmd.none )

        SetSelectedPiece piece ->
            case game.turn of
                White ->
                    ( { game | selectedPieceWhite = piece }, Cmd.none )

                Black ->
                    ( { game | selectedPieceBlack = piece }, Cmd.none )

        SpaceSelected index ->
            case game.winner of
                Just _ ->
                    ( game, Cmd.none )

                Nothing ->
                    Tuple.mapFirst
                        (\m ->
                            { m | winner = checkWinCondition m.size m.board }
                        )
                    <|
                        case game.pieceToMove of
                            Just idx ->
                                if index == idx then
                                    ( { game | pieceToMove = Nothing }, Cmd.none )

                                else if List.member index (adjacentIndices game.size idx) then
                                    case boardGet idx game.board of
                                        [] ->
                                            ( game, Cmd.none )

                                        (( sourceTopPiece, player ) :: _) as sourceStack ->
                                            if player == game.turn then
                                                case boardGet index game.board of
                                                    [] ->
                                                        ( { game
                                                            | board =
                                                                game.board
                                                                    |> boardInsert index sourceStack
                                                                    |> boardRemove idx
                                                            , pieceToMove = Nothing
                                                            , turn =
                                                                case game.turn of
                                                                    White ->
                                                                        Black

                                                                    Black ->
                                                                        White
                                                          }
                                                        , Cmd.none
                                                        )

                                                    (( Stone, _ ) :: _) as destinationStack ->
                                                        ( { game
                                                            | board =
                                                                game.board
                                                                    |> boardInsert index (sourceStack ++ destinationStack)
                                                                    |> boardRemove idx
                                                            , pieceToMove = Nothing
                                                            , turn =
                                                                case game.turn of
                                                                    White ->
                                                                        Black

                                                                    Black ->
                                                                        White
                                                          }
                                                        , Cmd.none
                                                        )

                                                    ( Wall, plr ) :: destinationStack ->
                                                        if sourceTopPiece == Capstone then
                                                            ( { game
                                                                | board =
                                                                    game.board
                                                                        |> boardInsert index (sourceStack ++ ( Stone, plr ) :: destinationStack)
                                                                        |> boardRemove idx
                                                                , pieceToMove = Nothing
                                                                , turn =
                                                                    case game.turn of
                                                                        White ->
                                                                            Black

                                                                        Black ->
                                                                            White
                                                              }
                                                            , Cmd.none
                                                            )

                                                        else
                                                            ( { game | pieceToMove = Just index }, Cmd.none )

                                                    _ ->
                                                        ( { game | pieceToMove = Just index }, Cmd.none )

                                            else
                                                case boardGet index game.board of
                                                    [] ->
                                                        ( { game | pieceToMove = Nothing }, Cmd.none )

                                                    _ ->
                                                        ( { game | pieceToMove = Just index }, Cmd.none )

                                else
                                    case boardGet index game.board of
                                        [] ->
                                            ( { game | pieceToMove = Nothing }, Cmd.none )

                                        _ ->
                                            ( { game | pieceToMove = Just index }, Cmd.none )

                            Nothing ->
                                updateNoPieceSelected index game


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


updateNoPieceSelected : Int -> Game -> ( Game, Cmd FrontendMsg )
updateNoPieceSelected index game =
    case boardGet index game.board of
        [] ->
            let
                piece : Piece
                piece =
                    case game.turn of
                        White ->
                            game.selectedPieceWhite

                        Black ->
                            game.selectedPieceBlack

                ( remStones, remCapstones, ( decStone, decCapstone ) ) =
                    case game.turn of
                        White ->
                            ( game.stoneCountWhite
                            , game.capstoneCountWhite
                            , ( \m -> { m | stoneCountWhite = m.stoneCountWhite - 1 }
                              , \m -> { m | capstoneCountWhite = m.capstoneCountWhite - 1 }
                              )
                            )

                        Black ->
                            ( game.stoneCountBlack
                            , game.capstoneCountBlack
                            , ( \m -> { m | stoneCountBlack = m.stoneCountBlack - 1 }
                              , \m -> { m | capstoneCountBlack = m.capstoneCountBlack - 1 }
                              )
                            )
            in
            case ( piece, remStones, remCapstones ) of
                ( Capstone, _, 0 ) ->
                    ( game, Cmd.none )

                ( Capstone, _, _ ) ->
                    ( { game
                        | board =
                            boardInsert index
                                [ ( piece, game.turn ) ]
                                game.board
                        , turn =
                            case game.turn of
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
                    ( game, Cmd.none )

                _ ->
                    ( { game
                        | board =
                            boardInsert index
                                [ ( piece, game.turn ) ]
                                game.board
                        , turn =
                            case game.turn of
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
            ( { game
                | pieceToMove = Just index
                , stackSizeSelected = List.length (boardGet index game.board)
              }
            , Cmd.none
            )


boardGet : Int -> Board -> List ( Piece, Player )
boardGet index board =
    board
        |> Dict.get index
        |> Maybe.withDefault []


boardInsert : Int -> List ( Piece, Player ) -> Board -> Board
boardInsert index pp board =
    Dict.insert index pp board


boardRemove : Int -> Board -> Board
boardRemove index board =
    Dict.insert index [] board


checkWinCondition : Int -> Board -> Maybe ( Player, List Int )
checkWinCondition size board =
    let
        xStart : List Int
        xStart =
            List.range 0 (size - 1)
                |> List.map
                    (\x ->
                        from2d size ( x, 0 )
                    )

        xEnd : List Int
        xEnd =
            List.range 0 (size - 1)
                |> List.map
                    (\x ->
                        from2d size ( x, size - 1 )
                    )
    in
    case checkWinConditionHelper size board xStart xEnd of
        Just w ->
            Just w

        Nothing ->
            let
                yStart : List Int
                yStart =
                    List.range 0 (size - 1)
                        |> List.map
                            (\y ->
                                from2d size ( 0, y )
                            )

                yEnd : List Int
                yEnd =
                    List.range 0 (size - 1)
                        |> List.map
                            (\y ->
                                from2d size ( size - 1, y )
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
                        case floodFill size board next player of
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
        |> adjacentIndices size
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


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Tak-Alike"
    , body =
        viewGame model.game
            |> List.map (Html.map GameMessage)
    }


viewGame : Game -> List (Html GameMsg)
viewGame game =
    [ Html.h1 [] [ Html.text "Tak-Alike" ]
    , case game.winner of
        Nothing ->
            Html.text ""

        Just _ ->
            Html.div []
                [ Html.button
                    [ Html.Events.onClick NewGame ]
                    [ Html.text "New game" ]
                , Html.br [] []
                , Html.br [] []
                , Html.label
                    [ Css.sizeSelecttion ]
                    (Html.span [] [ Html.text "Board size:" ]
                        :: List.map
                            (\size ->
                                Html.button
                                    [ Html.Events.onClick (SetGameSize size)
                                    , buttonPressed (game.newGameSize == size)
                                    ]
                                    [ Html.text (String.fromInt size) ]
                            )
                            [ 3, 4, 5, 6, 8 ]
                    )
                ]
    , Html.h3 []
        [ Html.text <|
            "Turn: "
                ++ (case game.turn of
                        White ->
                            "White"

                        Black ->
                            "Black"
                   )
        ]
    , Html.p
        []
        [ Html.h4 [] [ Html.text "Piece to place" ]
        , let
            isSelected : Piece -> Html.Attribute msg
            isSelected piece =
                (case game.turn of
                    White ->
                        game.selectedPieceWhite

                    Black ->
                        game.selectedPieceBlack
                )
                    |> (==) piece
                    |> buttonPressed
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
        , Html.br [] []
        , Html.div
            [ Css.stoneCounts
            ]
            [ Html.span []
                [ Html.text
                    ("Stones left: "
                        ++ (String.fromInt <|
                                case game.turn of
                                    White ->
                                        game.stoneCountWhite

                                    Black ->
                                        game.stoneCountBlack
                           )
                    )
                ]
            , Html.span []
                [ Html.text
                    ("Captones left: "
                        ++ (String.fromInt <|
                                case game.turn of
                                    White ->
                                        game.capstoneCountWhite

                                    Black ->
                                        game.capstoneCountBlack
                           )
                    )
                ]
            ]
        ]
    , Html.div
        [ Css.gameplayArea ]
        [ game.board
            |> Dict.toList
            |> List.map (viewBoardSpace game.winner game.pieceToMove)
            |> Html.div
                [ Css.board
                , Html.Attributes.style "grid-template-columns" ("repeat(" ++ String.fromInt game.size ++ ", 10rem)")
                , Html.Attributes.style "grid-template-rows" ("repeat(" ++ String.fromInt game.size ++ ", 10rem)")
                ]
        , case game.pieceToMove of
            Nothing ->
                Html.text ""

            Just index ->
                let
                    selectedStack : List ( Piece, Player )
                    selectedStack =
                        boardGet index game.board

                    stackSize : Int
                    stackSize =
                        List.length selectedStack
                in
                if stackSize == 0 then
                    Html.text ""

                else
                    Html.div
                        [ Css.selectedStack ]
                        [ Html.h3 [] [ Html.text "Selected stack" ]
                        , Html.div
                            [ Css.selectedStackDisplay
                            , Html.Attributes.style "grid-template-rows" ("repeat(" ++ String.fromInt stackSize ++ ", 1fr)")
                            ]
                            ((if stackSize == 1 then
                                Html.text ""

                              else
                                Html.input
                                    [ Css.stackSelectionInput
                                    , Html.Attributes.style "grid-column" "2"
                                    , Html.Attributes.style "grid-row" ("1 / " ++ String.fromInt (min game.size stackSize + 1))
                                    , Html.Attributes.type_ "range"
                                    , Html.Attributes.step "1"
                                    , Html.Attributes.min "1"
                                    , Html.Attributes.max (String.fromInt stackSize)
                                    , Html.Attributes.value (String.fromInt game.stackSizeSelected)
                                    , Html.Events.onInput StackSizeSelected
                                    ]
                                    []
                             )
                                :: (selectedStack
                                        |> List.indexedMap
                                            (\idx ( piece, player ) ->
                                                Html.div
                                                    [ Html.Attributes.style "background-color" <|
                                                        case player of
                                                            White ->
                                                                "rgb(245, 245, 245)"

                                                            Black ->
                                                                "rgb(10, 10, 10)"
                                                    , Html.Attributes.style "color" <|
                                                        case player of
                                                            White ->
                                                                "rgb(10, 10, 10)"

                                                            Black ->
                                                                "rgb(245, 245, 245)"
                                                    , Css.stackPiece
                                                    , Html.Attributes.style "grid-column" "1"
                                                    , Html.Attributes.style "grid-row" (String.fromInt (idx + 1))
                                                    ]
                                                    [ Html.text <|
                                                        case piece of
                                                            Stone ->
                                                                "STONE"

                                                            Wall ->
                                                                "WALL"

                                                            Capstone ->
                                                                "CAPSTONE"
                                                    ]
                                            )
                                   )
                            )
                        ]
        ]
    ]


boolToAttribute : Bool -> String
boolToAttribute bool =
    if bool then
        "true"

    else
        "false"


viewBoardSpace : Maybe ( Player, List Int ) -> Maybe Int -> ( Int, List ( Piece, Player ) ) -> Html GameMsg
viewBoardSpace maybeWinner pieceToMove ( index, pieceStack ) =
    Html.button
        [ Html.Events.onClick (SpaceSelected index)
        , Css.boardSpace
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
        ]
        [ let
            bgStyle : List (Html.Attribute msg)
            bgStyle =
                [ Html.Attributes.style "border-color" <|
                    case maybeWinner of
                        Nothing ->
                            case pieceToMove of
                                Nothing ->
                                    "red"

                                Just idx ->
                                    if idx == index then
                                        "cornflowerblue"

                                    else
                                        "red"

                        Just ( _, indicies ) ->
                            if List.member index indicies then
                                "green"

                            else
                                "red"
                , Css.boardSpaceBackground
                ]
          in
          Html.div
            [ Css.boardSpaceGrid
            ]
            [ Html.div
                ([ Html.Attributes.style "grid-column" "1"
                 , Html.Attributes.style "grid-row" "1"
                 , Html.Attributes.style "transform" "translate(-50%, -50%)"
                 ]
                    ++ bgStyle
                )
                []
            , Html.div
                ([ Html.Attributes.style "grid-column" "2"
                 , Html.Attributes.style "grid-row" "1"
                 , Html.Attributes.style "transform" "translate(0%, -50%)"
                 ]
                    ++ bgStyle
                )
                []
            , Html.div
                ([ Html.Attributes.style "grid-column" "1"
                 , Html.Attributes.style "grid-row" "2"
                 , Html.Attributes.style "transform" "translate(-50%, 0%)"
                 ]
                    ++ bgStyle
                )
                []
            , Html.div
                ([ Html.Attributes.style "grid-column" "2"
                 , Html.Attributes.style "grid-row" "2"
                 ]
                    ++ bgStyle
                )
                []
            , Html.div
                [ Html.Attributes.style "grid-column" "1 / 3"
                , Html.Attributes.style "grid-row" "1 / 3"
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "z-index" "1"
                ]
                [ Html.div
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "justify-content" "center"
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
                    [ Html.text <|
                        case pieceStack of
                            [] ->
                                ""

                            ( Stone, _ ) :: _ ->
                                "STONE"

                            ( Wall, _ ) :: _ ->
                                "WALL"

                            ( Capstone, _ ) :: _ ->
                                "CAPSTONE"
                    ]
                ]
            ]
        ]


buttonPressed : Bool -> Html.Attribute msg
buttonPressed bool =
    bool
        |> boolToAttribute
        |> Html.Attributes.attribute "aria-pressed"



-- HELPERS


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
        |> List.filter (\( x_, y_ ) -> x_ >= 0 && x_ < size && y_ >= 0 && y_ < size)
        |> List.map (from2d size)
