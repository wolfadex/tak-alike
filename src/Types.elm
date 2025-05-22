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
    , code : String
    , game : Game
    }


type alias Game =
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
    , stackSizeSelected : Int

    --
    , newGameSize : Int
    }


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
    { publicMatches : Dict String Match
    , privateMatches : Dict String Match
    , seed : Random.Seed
    }


type alias Match =
    { game : Game
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
    = NewGame
    | SetGameSize Int
    | SpaceSelected Int
    | SetSelectedPiece Piece
    | StackSizeSelected String


type ToBackend
    = NoOpToBackend
    | TB_JoinPublicMatch
    | TB_HostPrivateMatch
    | TB_JoinPrivateMatch String
    | TB_JoinMatch String
    | TB_GameMessage String GameMsg


type BackendMsg
    = NoOpBackendMsg
    | SeedGenerated Random.Seed


type ToFrontend
    = NoOpToFrontend
    | TF_PrivateMatchNotFound
    | TF_MatchJoined String Player Game
