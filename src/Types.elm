module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera
import Random
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , page : Page
    }


type Page
    = MenuPage Menu
    | GamePage (AsyncResult String GameModel)


type alias Menu =
    { code : String
    , loading : AsyncResult String String
    }


type AsyncResult e a
    = Untouched
    | Loading String
    | Success a
    | Failure e


type alias GameModel =
    { self : Player
    , opponent : ConnectionStatus
    , code : String
    , game : Game
    }


type Game
    = NewGame { size : Int }
    | PlayingGame GameState
    | CompletedGame { winner : ( Player, List Int ), size : Int, state : GameState }


type alias GameState =
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
    , stackSizeSelected : Int
    }


initGame : Int -> GameState
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
    , stackSizeSelected = 1
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


type alias BackendModel =
    { matches : Dict String Match
    , seed : Random.Seed
    }


type Privacy
    = Public
    | Private


type alias Match =
    { privacy : Privacy
    , game : Game
    , white : Client
    , black : Client
    }


type Client
    = WaitingFor
    | Connected { device : Lamdera.SessionId }
    | Disconnected { device : Lamdera.SessionId }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | MenuMessage MenuMsg
    | GameMessage GameMsg


type MenuMsg
    = JoinPublicMatch
    | HostPrivateMatch
    | ChangePrivateMatchCode String
    | JoinPrivateMatch


type GameMsg
    = CreateNewGame Int
    | SetGameSize Int
    | SpaceSelected Int
    | SetSelectedPiece Piece
    | StackSizeSelected String


type ToBackend
    = TB_JoinPublicMatch
    | TB_HostPrivateMatch
    | TB_JoinMatch String
    | TB_GameMessage String GameMsg


type BackendMsg
    = SeedGenerated Random.Seed
    | DeviceConnected Lamdera.SessionId Lamdera.ClientId
    | DeviceDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = TF_MatchNotFound
    | TF_MatchJoined String Player ConnectionStatus Game
    | TF_SetGameSize Int
    | TF_SetOpponentConnectionStatus ConnectionStatus


type ConnectionStatus
    = AwaitingOpponent
    | OpponentConnected
    | OpponentDisconnected



-- COMMON CODE


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
