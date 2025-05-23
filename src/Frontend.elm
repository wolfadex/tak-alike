module Frontend exposing (..)

import AppUrl
import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Css
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Lamdera
import Time
import Types exposing (..)
import Url exposing (Url)


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


init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        page =
            urlToPage url
    in
    ( { key = key
      , page = page
      }
    , case page of
        GamePage (Loading code) ->
            Lamdera.sendToBackend (TB_JoinMatch code)

        Admin_Page dashboard ->
            case dashboard.matches of
                Loading _ ->
                    Lamdera.sendToBackend (TB_JoinMatch dashboard.password)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none
    )


urlToPage : Url -> Page
urlToPage url =
    case (AppUrl.fromUrl url).path of
        [] ->
            MenuPage { code = "", loading = Untouched }

        [ "game", code ] ->
            GamePage (Loading code)

        [ "admin" ] ->
            Admin_Page
                { matches = Failure "Invalid passowrd"
                , password = ""
                , matchSort = Nothing
                , privacyFilter = Nothing
                , gameStateFilter = Nothing
                }

        [ "admin", password ] ->
            Admin_Page
                { matches = Loading "Gathering matches"
                , password = password
                , matchSort = Nothing
                , privacyFilter = Nothing
                , gameStateFilter = Nothing
                }

        _ ->
            MenuPage { code = "", loading = Untouched }


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged url ->
            let
                page =
                    urlToPage url
            in
            case page of
                MenuPage _ ->
                    ( { model | page = page }, Cmd.none )

                GamePage (Loading code) ->
                    case model.page of
                        GamePage (Success gameModel) ->
                            if gameModel.code == code then
                                ( model, Cmd.none )

                            else
                                ( { model | page = page }, Cmd.none )

                        _ ->
                            ( { model | page = page }, Cmd.none )

                _ ->
                    ( { model | page = page }, Cmd.none )

        MenuMessage menuMsg ->
            case model.page of
                MenuPage menuModel ->
                    updateMenu menuMsg menuModel
                        |> Tuple.mapFirst (\menu -> { model | page = MenuPage menu })

                _ ->
                    ( model, Cmd.none )

        GameMessage gameMsg ->
            case model.page of
                GamePage (Success gameModel) ->
                    updateGame gameMsg gameModel
                        |> Tuple.mapFirst (\game -> { model | page = GamePage (Success { gameModel | game = game }) })

                _ ->
                    ( model, Cmd.none )

        Admin_Message adminMsg ->
            case model.page of
                Admin_Page dashboard ->
                    updateAdmin adminMsg dashboard
                        |> Tuple.mapFirst (\admin -> { model | page = Admin_Page admin })

                _ ->
                    ( model, Cmd.none )


updateAdmin : Admin_Msg -> Admin_Model -> ( Admin_Model, Cmd FrontendMsg )
updateAdmin msg model =
    case msg of
        Admin_RefreshMatches ->
            ( { model | matches = Loading "" }, Lamdera.sendToBackend TB_Admin_GatherMatches )

        Admin_PasswordChanged password ->
            ( { model | password = password }, Cmd.none )

        Admin_Authenticate ->
            ( { model | matches = Loading "Authenticaing..." }
            , Lamdera.sendToBackend (TB_JoinMatch model.password)
            )

        Admin_DeleteMatch code ->
            ( model, Lamdera.sendToBackend (TB_Admin_DelteMatch code) )

        Admin_SetMatchSort matchSort ->
            ( { model
                | matchSort = matchSort
                , matches =
                    case model.matches of
                        Success matches ->
                            case matchSort of
                                Nothing ->
                                    model.matches

                                Just CreateAsc ->
                                    Success (List.sortBy (Tuple.second >> .createdAt >> Time.posixToMillis) matches)

                                Just CreateDesc ->
                                    Success (List.sortBy (Tuple.second >> .createdAt >> Time.posixToMillis) matches |> List.reverse)

                                Just UpdateAsc ->
                                    Success (List.sortBy (Tuple.second >> .updatedAt >> Time.posixToMillis) matches)

                                Just UpdateDesc ->
                                    Success (List.sortBy (Tuple.second >> .updatedAt >> Time.posixToMillis) matches |> List.reverse)

                        _ ->
                            model.matches
              }
            , Cmd.none
            )

        Admin_SetPrivacyFilter privacyFilter ->
            ( { model | privacyFilter = privacyFilter }
            , Cmd.none
            )

        Admin_SetGameStateFilter gameStateFilter ->
            ( { model | gameStateFilter = gameStateFilter }, Cmd.none )


updateMenu : MenuMsg -> Menu -> ( Menu, Cmd FrontendMsg )
updateMenu msg menu =
    case msg of
        JoinPublicMatch ->
            ( { menu | loading = Loading "Joining public match..." }, Lamdera.sendToBackend TB_JoinPublicMatch )

        HostPrivateMatch ->
            ( { menu | loading = Loading "Creating private match..." }, Lamdera.sendToBackend TB_HostPrivateMatch )

        ChangePrivateMatchCode code ->
            ( { menu | code = code }, Cmd.none )

        JoinPrivateMatch ->
            ( { menu | loading = Loading "Joining private match..." }, Lamdera.sendToBackend (TB_JoinMatch menu.code) )


updateGame : GameMsg -> GameModel -> ( Game, Cmd FrontendMsg )
updateGame msg gameModel =
    case msg of
        CreateNewGame size ->
            ( PlayingGame (initGame size)
            , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
            )

        SetGameSize size ->
            case gameModel.game of
                PlayingGame _ ->
                    ( gameModel.game, Cmd.none )

                NewGame newGame ->
                    ( NewGame { newGame | size = size }
                    , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
                    )

                CompletedGame completedGame ->
                    ( CompletedGame { completedGame | size = size }
                    , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
                    )

        StackSizeSelected sizeStr ->
            case String.toInt sizeStr of
                Nothing ->
                    ( gameModel.game, Cmd.none )

                Just size ->
                    case gameModel.game of
                        PlayingGame playingGame ->
                            ( PlayingGame { playingGame | stackSizeSelected = size }
                            , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
                            )

                        _ ->
                            ( gameModel.game, Cmd.none )

        SetSelectedPiece piece ->
            case gameModel.game of
                PlayingGame playingGame ->
                    case playingGame.turn of
                        White ->
                            ( PlayingGame { playingGame | selectedPieceWhite = piece }
                            , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
                            )

                        Black ->
                            ( PlayingGame { playingGame | selectedPieceBlack = piece }
                            , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
                            )

                _ ->
                    ( gameModel.game, Cmd.none )

        SpaceSelected index ->
            case gameModel.game of
                NewGame _ ->
                    ( gameModel.game, Cmd.none )

                CompletedGame ({ state } as completedGame) ->
                    ( CompletedGame { completedGame | state = { state | pieceToMove = Just index } }, Cmd.none )

                PlayingGame playingGame ->
                    Tuple.mapFirst
                        (\m ->
                            case checkWinCondition m.size m.board of
                                Nothing ->
                                    PlayingGame m

                                Just winner ->
                                    CompletedGame { winner = winner, state = m, size = m.size }
                        )
                    <|
                        case playingGame.pieceToMove of
                            Just idx ->
                                if index == idx then
                                    ( { playingGame | pieceToMove = Nothing }, Lamdera.sendToBackend (TB_GameMessage gameModel.code msg) )

                                else if List.member index (adjacentIndices playingGame.size idx) then
                                    case boardGet idx playingGame.board of
                                        [] ->
                                            ( playingGame, Cmd.none )

                                        (( sourceTopPiece, player ) :: _) as sourceStack ->
                                            if playingGame.turn == gameModel.self && player == playingGame.turn then
                                                case boardGet index playingGame.board of
                                                    [] ->
                                                        ( { playingGame
                                                            | board =
                                                                playingGame.board
                                                                    |> boardInsert index sourceStack
                                                                    |> boardRemove idx
                                                            , pieceToMove = Nothing
                                                            , turn =
                                                                case playingGame.turn of
                                                                    White ->
                                                                        Black

                                                                    Black ->
                                                                        White
                                                          }
                                                        , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
                                                        )

                                                    (( Stone, _ ) :: _) as destinationStack ->
                                                        ( { playingGame
                                                            | board =
                                                                playingGame.board
                                                                    |> boardInsert index (sourceStack ++ destinationStack)
                                                                    |> boardRemove idx
                                                            , pieceToMove = Nothing
                                                            , turn =
                                                                case playingGame.turn of
                                                                    White ->
                                                                        Black

                                                                    Black ->
                                                                        White
                                                          }
                                                        , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
                                                        )

                                                    ( Wall, plr ) :: destinationStack ->
                                                        if sourceTopPiece == Capstone then
                                                            ( { playingGame
                                                                | board =
                                                                    playingGame.board
                                                                        |> boardInsert index (sourceStack ++ ( Stone, plr ) :: destinationStack)
                                                                        |> boardRemove idx
                                                                , pieceToMove = Nothing
                                                                , turn =
                                                                    case playingGame.turn of
                                                                        White ->
                                                                            Black

                                                                        Black ->
                                                                            White
                                                              }
                                                            , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
                                                            )

                                                        else
                                                            ( { playingGame | pieceToMove = Just index }, Lamdera.sendToBackend (TB_GameMessage gameModel.code msg) )

                                                    _ ->
                                                        ( { playingGame | pieceToMove = Just index }, Lamdera.sendToBackend (TB_GameMessage gameModel.code msg) )

                                            else
                                                case boardGet index playingGame.board of
                                                    [] ->
                                                        ( { playingGame | pieceToMove = Nothing }, Lamdera.sendToBackend (TB_GameMessage gameModel.code msg) )

                                                    _ ->
                                                        ( { playingGame | pieceToMove = Just index }, Lamdera.sendToBackend (TB_GameMessage gameModel.code msg) )

                                else
                                    case boardGet index playingGame.board of
                                        [] ->
                                            ( { playingGame | pieceToMove = Nothing }, Lamdera.sendToBackend (TB_GameMessage gameModel.code msg) )

                                        _ ->
                                            ( { playingGame | pieceToMove = Just index }, Lamdera.sendToBackend (TB_GameMessage gameModel.code msg) )

                            Nothing ->
                                updateNoPieceSelected msg gameModel index playingGame


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TF_MatchNotFound ->
            case model.page of
                MenuPage menu ->
                    ( { model | page = MenuPage { menu | loading = Failure "Match not found" } }, Cmd.none )

                GamePage _ ->
                    ( { model | page = MenuPage { code = "", loading = Failure "Match not found" } }, Cmd.none )

                Admin_Page _ ->
                    ( model, Cmd.none )

        TF_MatchJoined code player opponent game ->
            ( { model
                | page =
                    GamePage
                        (Success
                            { game = game
                            , code = code
                            , self = player
                            , opponent = opponent
                            }
                        )
              }
            , Browser.Navigation.pushUrl model.key ("/game/" ++ code)
            )

        TF_SetOpponentConnectionStatus opponent ->
            case model.page of
                GamePage (Success gameModel) ->
                    ( { model | page = GamePage (Success { gameModel | opponent = opponent }) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TF_SetGameSize size ->
            case model.page of
                MenuPage _ ->
                    ( model, Cmd.none )

                GamePage (Success gameModel) ->
                    case gameModel.game of
                        NewGame newGame ->
                            ( { model | page = GamePage (Success { gameModel | game = NewGame { newGame | size = size } }) }, Cmd.none )

                        CompletedGame completedGame ->
                            ( { model | page = GamePage (Success { gameModel | game = CompletedGame { completedGame | size = size } }) }, Cmd.none )

                        PlayingGame _ ->
                            ( model, Cmd.none )

                GamePage _ ->
                    ( model, Cmd.none )

                Admin_Page _ ->
                    ( model, Cmd.none )

        TF_Admin_ShowAdminDashboard matches ->
            ( { model
                | page =
                    Admin_Page
                        { matches = Success matches
                        , password = ""
                        , matchSort = Nothing
                        , privacyFilter = Nothing
                        , gameStateFilter = Nothing
                        }
              }
            , Cmd.none
            )

        TF_Admin_Matches matches ->
            case model.page of
                Admin_Page dashboard ->
                    ( { model | page = Admin_Page { dashboard | matches = Success matches } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateNoPieceSelected : GameMsg -> GameModel -> Int -> GameState -> ( GameState, Cmd FrontendMsg )
updateNoPieceSelected msg gameModel index game =
    case boardGet index game.board of
        [] ->
            if game.turn == gameModel.self then
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
                        , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
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
                        , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
                        )

            else
                ( game, Cmd.none )

        stack ->
            ( { game
                | pieceToMove = Just index
                , stackSizeSelected = List.length stack
              }
            , Lamdera.sendToBackend (TB_GameMessage gameModel.code msg)
            )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Tak-Alike"
    , body =
        case model.page of
            MenuPage menu ->
                viewMenu menu
                    |> List.map (Html.map MenuMessage)

            GamePage game ->
                viewGame game
                    |> List.map (Html.map GameMessage)

            Admin_Page dashboard ->
                viewAdmin dashboard
                    |> List.map (Html.map Admin_Message)
    }


viewMenu : Menu -> List (Html MenuMsg)
viewMenu menu =
    [ Html.div [ Css.menu ]
        [ Html.h1 [] [ Html.text "Tak-Alike" ]
        , Html.main_ []
            [ Html.button
                [ Html.Events.onClick JoinPublicMatch ]
                [ Html.text "Join a public match" ]
            , Html.button
                [ Html.Events.onClick HostPrivateMatch ]
                [ Html.text "Host a private match" ]
            , Html.form [ Css.menuJoinOption, Html.Events.onSubmit JoinPrivateMatch ]
                [ Html.input
                    [ Html.Attributes.placeholder "Code"
                    , Html.Attributes.value menu.code
                    , Html.Events.onInput ChangePrivateMatchCode
                    ]
                    []
                , Html.button
                    [ Html.Attributes.type_ "submit" ]
                    [ Html.text "Join a private match" ]
                ]
            , case menu.loading of
                Untouched ->
                    Html.text ""

                Loading message ->
                    Html.text message

                Success _ ->
                    Html.text ""

                Failure error ->
                    Html.text error
            , Html.p
                []
                [ Html.span [] [ Html.b [] [ Html.text "Goal" ] ]
                , Html.br [] []
                , Html.text "Create a path of your stones and/or capstones between 2 opposite edges."
                , Html.br [] []
                , Html.br [] []
                , Html.span [] [ Html.b [] [ Html.text "Gameplay" ] ]
                , Html.br [] []
                , Html.text "On your turn, take 1 of 2 actions"
                , Html.br [] []
                , Html.text "• Places a stone, wall, or capstone on any empty space"
                , Html.br [] []
                , Html.text "• Move a stack of pieces to an adjacent space"
                , Html.p
                    [ Html.Attributes.style "padding-left" "1rem"
                    , Html.Attributes.style "margin" "0"
                    ]
                    [ Html.text "• You can't move on top of a wall, except with a capstone. This turns the wall into a stone" ]
                , Html.p
                    [ Html.Attributes.style "padding-left" "1rem"
                    , Html.Attributes.style "margin" "0"
                    ]
                    [ Html.text "• You can't move more pieces in a stack than the size of the board. I.e. if the board is of size 3, and you have a stack of 5 pieces, you may only move 1-3 of the top pieces." ]
                ]
            ]
        ]
    ]


viewGame : AsyncResult String GameModel -> List (Html GameMsg)
viewGame gameRes =
    case gameRes of
        Untouched ->
            [ Html.text "Loading..." ]

        Loading _ ->
            [ Html.text "Loading..." ]

        Failure error ->
            [ Html.text error ]

        Success { game, opponent, self } ->
            case game of
                NewGame newGame ->
                    viewGameCommon
                        [ viewNewGame newGame.size
                        , viewOpponent opponent
                        ]

                CompletedGame completedGame ->
                    viewGameCommon
                        (viewNewGame completedGame.size
                            :: viewOpponent opponent
                            :: viewGameState self (Just completedGame.winner) completedGame.state
                        )

                PlayingGame playingGame ->
                    viewGameCommon
                        (viewOpponent opponent :: viewGameState self Nothing playingGame)


viewGameCommon : List (Html GameMsg) -> List (Html GameMsg)
viewGameCommon children =
    [ Html.div
        [ Css.gameCommon
        ]
        (Html.h1 [] [ Html.text "Tak-Alike" ] :: children)
    ]


viewOpponent : ConnectionStatus -> Html GameMsg
viewOpponent opponent =
    case opponent of
        AwaitingOpponent ->
            Html.h2 [] [ Html.span [] [ Html.text "⏳ Awaiting an opponent" ] ]

        OpponentDisconnected ->
            Html.h2 [] [ Html.span [] [ Html.text "⚠️ Opponent has left the game" ] ]

        OpponentConnected ->
            Html.text ""


viewNewGame : Int -> Html GameMsg
viewNewGame size =
    Html.div
        [ Css.newGame
        ]
        [ Html.label
            [ Css.sizeSelecttion ]
            (Html.span [] [ Html.text "Board size:" ]
                :: List.map
                    (\s ->
                        Html.button
                            [ Html.Events.onClick (SetGameSize s)
                            , buttonPressed (size == s)
                            ]
                            [ Html.text (String.fromInt s) ]
                    )
                    [ 3, 4, 5, 6, 8 ]
            )
        , Html.button
            [ Html.Events.onClick (CreateNewGame size) ]
            [ Html.text "Start game" ]
        ]


viewGameState : Player -> Maybe ( Player, List Int ) -> GameState -> List (Html GameMsg)
viewGameState self winner game =
    [ case winner of
        Nothing ->
            Html.text ""

        Just ( winningPlayer, _ ) ->
            Html.h2 [ Html.Attributes.style "align-self" "center" ]
                [ Html.text <|
                    if winningPlayer == self then
                        "You win 🎉"

                    else
                        "They won!"
                ]
    , case winner of
        Nothing ->
            Html.span
                [ Css.turnNotice
                , Html.Attributes.style "color" <|
                    if game.turn == self then
                        "deeppink"

                    else
                        "inherit"
                ]
                [ Html.text <|
                    if game.turn == self then
                        "Your turn"

                    else
                        "Their turn"
                ]

        Just _ ->
            Html.text ""
    , case winner of
        Nothing ->
            Html.div
                [ Css.gameControls
                ]
                [ Html.div
                    [ Css.gameControlsActions
                    ]
                    [ Html.span []
                        [ Html.text
                            "Piece to place"
                        ]
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
                        [ Css.gameControlsButtons
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
                    ]
                , Html.div
                    [ Css.stoneCounts
                    , Html.Attributes.style "align-self" "flex-end"
                    ]
                    [ Html.span []
                        [ Html.text
                            ("Stones/Walls left: "
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

        Just _ ->
            Html.text ""
    , Html.div
        [ Css.gameplayArea ]
        [ game.board
            |> Dict.toList
            |> List.map (viewBoardSpace winner game.pieceToMove)
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


viewAdmin : Admin_Model -> List (Html Admin_Msg)
viewAdmin model =
    [ Html.h1 [] [ Html.text "Tak-Alike ADMIN" ]
    , Html.button [ Html.Events.onClick Admin_RefreshMatches ]
        [ Html.text "Refresh matches" ]
    , case model.matches of
        Untouched ->
            Html.button
                []
                [ Html.text "Load matches" ]

        Loading message ->
            Html.text message

        Failure error ->
            Html.form [ Html.Events.onSubmit Admin_Authenticate ]
                [ Html.text error
                , Html.input
                    [ Html.Attributes.value model.password
                    , Html.Attributes.placeholder "Admin password"
                    , Html.Events.onInput Admin_PasswordChanged
                    , Html.Attributes.type_ "password"
                    ]
                    []
                ]

        Success matches ->
            Html.table
                []
                [ Html.thead
                    []
                    [ Html.tr []
                        [ Html.th []
                            [ Html.button
                                [ Css.columnWithAction
                                , Html.Events.onClick
                                    (Admin_SetPrivacyFilter
                                        (case model.privacyFilter of
                                            Nothing ->
                                                Just OnlyPublic

                                            Just OnlyPublic ->
                                                Just OnlyPrivate

                                            Just OnlyPrivate ->
                                                Nothing
                                        )
                                    )
                                ]
                                [ Html.span [] [ Html.text "Privacy" ]
                                , Html.text <|
                                    case model.privacyFilter of
                                        Nothing ->
                                            "_"

                                        Just OnlyPublic ->
                                            "✅"

                                        Just OnlyPrivate ->
                                            "🤫"
                                ]
                            ]
                        , Html.th
                            []
                            [ Html.button
                                [ Css.columnWithAction
                                , Html.Events.onClick
                                    (Admin_SetGameStateFilter
                                        (case model.gameStateFilter of
                                            Nothing ->
                                                Just OnlyNewGames

                                            Just OnlyNewGames ->
                                                Just OnlyPlayingGames

                                            Just OnlyPlayingGames ->
                                                Just OnlyCompltedGames

                                            Just OnlyCompltedGames ->
                                                Nothing
                                        )
                                    )
                                ]
                                [ Html.span [] [ Html.text "Game state" ]
                                , Html.text <|
                                    case model.gameStateFilter of
                                        Nothing ->
                                            "_"

                                        Just OnlyNewGames ->
                                            "🆕"

                                        Just OnlyPlayingGames ->
                                            "🧩"

                                        Just OnlyCompltedGames ->
                                            "🏆"
                                ]
                            ]
                        , Html.th
                            []
                            [ Html.text "White status" ]
                        , Html.th
                            []
                            [ Html.text "Black status" ]
                        , Html.th
                            []
                            [ Html.button
                                [ Css.columnWithAction
                                , Html.Events.onClick
                                    (Admin_SetMatchSort
                                        (Just <|
                                            case model.matchSort of
                                                Just CreateAsc ->
                                                    CreateDesc

                                                _ ->
                                                    CreateAsc
                                        )
                                    )
                                ]
                                [ Html.span [] [ Html.text "Created at" ]
                                , Html.text <|
                                    case model.matchSort of
                                        Just CreateAsc ->
                                            "⬆️"

                                        Just CreateDesc ->
                                            "⬇️"

                                        _ ->
                                            "↕️"
                                ]
                            ]
                        , Html.th
                            []
                            [ Html.button
                                [ Css.columnWithAction
                                , Html.Events.onClick
                                    (Admin_SetMatchSort
                                        (Just <|
                                            case model.matchSort of
                                                Just UpdateAsc ->
                                                    UpdateDesc

                                                _ ->
                                                    UpdateAsc
                                        )
                                    )
                                ]
                                [ Html.span [] [ Html.text "Updated at" ]
                                , Html.text <|
                                    case model.matchSort of
                                        Just UpdateAsc ->
                                            "⬆️"

                                        Just UpdateDesc ->
                                            "⬇️"

                                        _ ->
                                            "↕️"
                                ]
                            ]
                        , Html.th
                            []
                            []
                        ]
                    ]
                , matches
                    |> List.filter
                        (\( _, match ) ->
                            let
                                visiblePrivacy =
                                    case model.privacyFilter of
                                        Nothing ->
                                            True

                                        Just OnlyPublic ->
                                            match.privacy == Public

                                        Just OnlyPrivate ->
                                            match.privacy == Private

                                visibleGameState =
                                    case model.gameStateFilter of
                                        Nothing ->
                                            True

                                        Just OnlyNewGames ->
                                            case match.game of
                                                NewGame _ ->
                                                    True

                                                _ ->
                                                    False

                                        Just OnlyPlayingGames ->
                                            case match.game of
                                                PlayingGame _ ->
                                                    True

                                                _ ->
                                                    False

                                        Just OnlyCompltedGames ->
                                            case match.game of
                                                CompletedGame _ ->
                                                    True

                                                _ ->
                                                    False
                            in
                            visiblePrivacy && visibleGameState
                        )
                    |> List.map
                        (\( code, match ) ->
                            Html.tr []
                                [ Html.td []
                                    [ Html.text <|
                                        case match.privacy of
                                            Public ->
                                                "Public"

                                            Private ->
                                                "Private"
                                    ]
                                , Html.td []
                                    [ Html.text <|
                                        case match.game of
                                            NewGame _ ->
                                                "New game"

                                            PlayingGame gameState ->
                                                "Playing - "
                                                    ++ (case gameState.turn of
                                                            White ->
                                                                "White's"

                                                            Black ->
                                                                "Black's"
                                                       )
                                                    ++ " turn"

                                            CompletedGame { winner } ->
                                                "Completed - "
                                                    ++ (case winner of
                                                            ( White, _ ) ->
                                                                "White"

                                                            ( Black, _ ) ->
                                                                "Black"
                                                       )
                                                    ++ " won"
                                    ]
                                , Html.td []
                                    [ Html.text <|
                                        case match.white of
                                            Admin_WaitingFor ->
                                                "Waiting for"

                                            Admin_Connected ->
                                                "Connected"

                                            Admin_Disconnected ->
                                                "Disconnected"
                                    ]
                                , Html.td []
                                    [ Html.text <|
                                        case match.black of
                                            Admin_WaitingFor ->
                                                "Waiting for"

                                            Admin_Connected ->
                                                "Connected"

                                            Admin_Disconnected ->
                                                "Disconnected"
                                    ]
                                , Html.td []
                                    [ Html.text <|
                                        formatDateTime match.createdAt
                                    ]
                                , Html.td []
                                    [ Html.text <|
                                        formatDateTime match.updatedAt
                                    ]
                                , Html.td []
                                    [ Html.button
                                        [ Html.Events.onClick (Admin_DeleteMatch code) ]
                                        [ Html.text "Delete match" ]
                                    ]
                                ]
                        )
                    |> Html.tbody []
                ]
    ]


formatDateTime : Time.Posix -> String
formatDateTime time =
    let
        year =
            time
                |> Time.toYear Time.utc
                |> String.fromInt

        month =
            time
                |> Time.toMonth Time.utc
                |> monthToInt
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            time
                |> Time.toDay Time.utc
                |> String.fromInt
                |> String.padLeft 2 '0'

        hour =
            time
                |> Time.toHour Time.utc
                |> String.fromInt
                |> String.padLeft 2 '0'

        min =
            time
                |> Time.toMinute Time.utc
                |> String.fromInt
                |> String.padLeft 2 '0'

        sec =
            time
                |> Time.toSecond Time.utc
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    year ++ "-" ++ month ++ "-" ++ day ++ "T" ++ hour ++ ":" ++ min ++ ":" ++ sec


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
