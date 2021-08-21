module Storage exposing
    ( Address
    , BtnOptions(..)
    , ContactDialogState
    , NavBarDisplay(..)
    , NavItem
    , SendState(..)
    , fromJson
    , init
    , toJson
    )

import Browser.Navigation as Nav
import Char exposing (isDigit)
import Email as Email
import Html exposing (option)
import Json.Decode as Json exposing (Value, bool, field, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E exposing (bool, int, list, object, string)
import PhoneNumber
import PhoneNumber.Countries exposing (countryUS)
import Ports exposing (disableScrolling, load, save)



-- STORAGE


type alias ContactDialogState =
    { name : String
    , nameError : Bool
    , email : Maybe String
    , emailError : Bool
    , phone : Maybe String
    , phoneError : Bool
    , message : Maybe String
    , messageError : Bool
    , currentPage : Int
    , send : SendState
    , showContactUs : Bool
    }


type SendState
    = Waiting
    | Send
    | SendOk
    | SendError


type alias Address =
    { street : String
    , city : String
    , mapsLink : String
    , phone : String
    , phoneLink : String
    , email : String
    , emailLink : String
    }


type NavBarDisplay
    = Enter
    | Show
    | Hide


type alias NavItem =
    { name : String
    , link : String
    , hovered : Bool
    , onClick : BtnOptions
    }


type BtnOptions
    = Url String
    | SetContactUs Bool



-- Converting to JSON


toJson : ContactDialogState -> Value
toJson state =
    let
        nullable : Maybe String -> E.Value
        nullable a =
            case a of
                Nothing ->
                    E.null

                Just str ->
                    E.string str
    in
    E.object
        [ ( "contactDialogState"
          , E.object
                [ ( "name", E.string state.name )
                , ( "nameError", E.bool state.nameError )
                , ( "email", nullable state.email )
                , ( "emailError", E.bool state.emailError )
                , ( "phone", nullable state.phone )
                , ( "phoneError", E.bool state.phoneError )
                , ( "message", nullable state.message )
                , ( "messageError", E.bool state.messageError )
                , ( "currentPage", E.int state.currentPage )
                , ( "send"
                  , E.string <|
                        case state.send of
                            Waiting ->
                                "Waiting"

                            Send ->
                                "Error"

                            SendOk ->
                                "Ok"

                            SendError ->
                                "Error"
                  )
                ]
          )
        ]


decoder : Json.Decoder ContactDialogState
decoder =
    Json.field "contactDialogState" <|
        (Json.succeed ContactDialogState
            |> required "name" Json.string
            |> required "nameError" Json.bool
            |> required "email" (Json.nullable Json.string)
            |> required "emailError" Json.bool
            |> required "phone" (Json.nullable Json.string)
            |> required "phoneError" Json.bool
            |> required "message" (Json.nullable Json.string)
            |> required "messageError" Json.bool
            |> optional "currentPage" Json.int 0
            |> required "send"
                (Json.string
                    |> Json.andThen
                        (\s ->
                            case s of
                                "Waiting" ->
                                    Json.succeed Waiting

                                "Send" ->
                                    Json.succeed Send

                                "Ok" ->
                                    Json.succeed SendOk

                                "Error" ->
                                    Json.succeed SendError

                                _ ->
                                    Json.fail "Invalid Send State"
                        )
                )
            |> hardcoded False
        )



-- Converting from JSON


fromJson : Value -> ContactDialogState
fromJson json =
    json
        |> Json.decodeValue decoder
        |> Result.withDefault init


init : ContactDialogState
init =
    ContactDialogState
        ""
        False
        Nothing
        False
        Nothing
        False
        Nothing
        False
        0
        Waiting
        False



-- Updating storage
