module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Random
import Time
import Url


type AsyncResult e a
    = Untouched
    | Loading String
    | Success a
    | Failure e


type alias Menu =
    { code : String
    , loading : AsyncResult String String
    }


type Player
    = Black
    | White


type ConnectionStatus
    = AwaitingOpponent
    | OpponentConnected
    | OpponentDisconnected


type Piece
    = Stone
    | Wall
    | Capstone


type alias Board =
    Dict.Dict Int (List ( Piece, Player ))


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


type Game
    = NewGame
        { size : Int
        }
    | PlayingGame GameState
    | CompletedGame
        { winner : ( Player, List Int )
        , size : Int
        , state : GameState
        }


type alias GameModel =
    { self : Player
    , opponent : ConnectionStatus
    , code : String
    , game : Game
    }


type Privacy
    = Public
    | Private


type Admin_Client
    = Admin_WaitingFor
    | Admin_Connected
    | Admin_Disconnected


type alias Admin_Match =
    { privacy : Privacy
    , game : Game
    , white : Admin_Client
    , black : Admin_Client
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


type MatchSort
    = CreateAsc
    | CreateDesc
    | UpdateAsc
    | UpdateDesc


type PrivacyFilter
    = OnlyPublic
    | OnlyPrivate


type GameStateFilter
    = OnlyNewGames
    | OnlyPlayingGames
    | OnlyCompltedGames


type alias Admin_Model =
    { matches : AsyncResult String (List ( String, Admin_Match ))
    , password : String
    , matchSort : Maybe MatchSort
    , privacyFilter : Maybe PrivacyFilter
    , gameStateFilter : Maybe GameStateFilter
    }


type Page
    = MenuPage Menu
    | GamePage (AsyncResult String GameModel)
    | Admin_Page Admin_Model


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    }


type Client
    = WaitingFor
    | Connected
        { device : Lamdera.SessionId
        }
    | Disconnected
        { device : Lamdera.SessionId
        }


type alias Match =
    { privacy : Privacy
    , game : Game
    , white : Client
    , black : Client
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


type alias BackendModel =
    { matches : Dict.Dict String Match
    , adminClient : Maybe Lamdera.ClientId
    , seed : Random.Seed
    }


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


type Admin_Msg
    = Admin_RefreshMatches
    | Admin_PasswordChanged String
    | Admin_Authenticate
    | Admin_DeleteMatch String
    | Admin_SetMatchSort (Maybe MatchSort)
    | Admin_SetPrivacyFilter (Maybe PrivacyFilter)
    | Admin_SetGameStateFilter (Maybe GameStateFilter)


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | MenuMessage MenuMsg
    | GameMessage GameMsg
    | Admin_Message Admin_Msg


type ToBackend
    = TB_JoinPublicMatch
    | TB_HostPrivateMatch
    | TB_JoinMatch String
    | TB_GameMessage String GameMsg
    | TB_Admin_GatherMatches
    | TB_Admin_DelteMatch String


type BackendMsg
    = SeedGenerated Random.Seed
    | DeviceConnected Lamdera.SessionId Lamdera.ClientId
    | DeviceDisconnected Lamdera.SessionId Lamdera.ClientId
    | ToBackendWithTime Time.Posix Lamdera.SessionId Lamdera.ClientId ToBackend


type ToFrontend
    = TF_MatchNotFound
    | TF_MatchJoined String Player ConnectionStatus Game
    | TF_SetGameSize Int
    | TF_SetOpponentConnectionStatus ConnectionStatus
    | TF_Admin_ShowAdminDashboard (List ( String, Admin_Match ))
    | TF_Admin_Matches (List ( String, Admin_Match ))
