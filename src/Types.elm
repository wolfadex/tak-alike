module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
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
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | GameMessage GameMsg


type GameMsg
    = NewGame
    | SetGameSize Int
    | SpaceSelected Int
    | SetSelectedPiece Piece
    | StackSizeSelected String


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
