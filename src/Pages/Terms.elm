module Pages.Terms exposing (Model, Msg, page)

import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Gen.Params.Terms exposing (Params)
import Html exposing (br)
import Html.Attributes exposing (class, id)
import Http exposing (Error(..))
import Json.Decode as Json
import Json.Encode as Encode
import Page
import Pages.Home_ exposing (AnimationState, When(..), updateElement)
import Palette exposing (FontSize(..), black, fontSize, gciBlue, gciBlueLight, maxWidth, warning, white)
import Ports exposing (recvScroll)
import Request
import Shared exposing (FormResponse, acol, ael, contactUs, footer, navbar, reset)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage exposing (NavBarDisplay(..), SendState(..))
import Task
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { localShared : Shared.Model
    , legal : String
    , simpleBtnHoverTracker : List SimpleBtn
    , animationTracker : Dict String AnimationState
    }


type alias SimpleBtn =
    { id : Int
    , name : String
    , link : String
    , hovered : Bool
    , message : Maybe Msg
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { localShared = reset shared
      , legal = ""
      , simpleBtnHoverTracker =
            [ SimpleBtn 0 "ignored" "/" False Nothing
            , SimpleBtn 1 "Intellectual Property" "/ip" False Nothing
            , SimpleBtn 2 "What We Do" "/#whatwedo" False Nothing
            , SimpleBtn 3 "Contact Us" "" False (Just OpenContactUs)
            , SimpleBtn 4 "Technical Papers" "/papers" False Nothing
            ]
      , animationTracker =
            Dict.fromList
                [ ( "bottomButtons", AnimationState (PercentOfViewport 40) False )
                ]
      }
    , Http.get { url = "/misc/terms.txt", expect = Http.expectString GotTerms } |> Effect.fromCmd
    )



-- UPDATE


type Msg
    = Scrolled Int
    | ModifyLocalShared Shared.Model
    | WindowResized Int Int
    | OpenContactUs
    | GotTerms (Result Http.Error String)
    | SimpleBtnHover Int
    | SimpleBtnUnHover Int
    | GotElement String (Result Browser.Dom.Error Browser.Dom.Element)
    | Submited (Result Http.Error FormResponse)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Scrolled distance ->
            let
                modifyNavbarDisplay state =
                    model.localShared
                        |> (\l ->
                                { l
                                    | navbarDisplay = state
                                    , scrolledDistance = distance
                                    , showMobileNav =
                                        if state == Hide then
                                            False

                                        else
                                            l.showMobileNav
                                }
                           )
            in
            ( if abs (distance - model.localShared.scrolledDistance) > 3 then
                if distance > model.localShared.scrolledDistance && distance > 300 then
                    { model | localShared = modifyNavbarDisplay Hide }

                else
                    { model | localShared = modifyNavbarDisplay Enter }

              else
                model
            , Effect.batch
                (List.map animationTrackerToCmd (List.filter (\( _, v ) -> v.shouldAnimate == False) (Dict.toList model.animationTracker)))
            )

        SimpleBtnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setHovered id) model.simpleBtnHoverTracker }, Effect.none )

        SimpleBtnUnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setUnHovered id) model.simpleBtnHoverTracker }, Effect.none )

        ModifyLocalShared newSharedState ->
            let
                nullable : Maybe String -> Encode.Value
                nullable a =
                    case a of
                        Nothing ->
                            Encode.null

                        Just str ->
                            Encode.string str
            in
            ( { model | localShared = newSharedState }
            , if not (newSharedState.contactDialogState == model.localShared.contactDialogState) then
                Effect.batch
                    (if newSharedState.contactDialogState.send == Send then
                        [ Shared.UpdateModel newSharedState |> Effect.fromShared
                        , newSharedState.contactDialogState |> Storage.toJson |> Ports.save |> Effect.fromCmd
                        , Http.post
                            { url = "https://formspree.io/f/xdoygpvp"
                            , body =
                                Http.jsonBody <|
                                    Encode.object
                                        [ ( "name", Encode.string newSharedState.contactDialogState.name )
                                        , ( "email", nullable newSharedState.contactDialogState.email )
                                        , ( "telephone", nullable newSharedState.contactDialogState.phone )
                                        , ( "message", nullable newSharedState.contactDialogState.message )
                                        ]
                            , expect = Http.expectJson Submited (Json.map2 FormResponse (Json.field "next" Json.string) (Json.field "ok" Json.bool))
                            }
                            |> Effect.fromCmd
                        ]

                     else
                        [ Shared.UpdateModel newSharedState |> Effect.fromShared
                        , newSharedState.contactDialogState |> Storage.toJson |> Ports.save |> Effect.fromCmd
                        ]
                    )

              else
                Shared.UpdateModel newSharedState |> Effect.fromShared
            )

        Submited response ->
            let
                newSharedState =
                    model.localShared
                        |> (\local ->
                                { local
                                    | contactDialogState =
                                        local.contactDialogState
                                            |> (\state ->
                                                    { state
                                                        | send =
                                                            case response of
                                                                Ok _ ->
                                                                    SendOk

                                                                Err _ ->
                                                                    SendError
                                                    }
                                               )
                                }
                           )
            in
            ( { model | localShared = newSharedState }
            , Effect.batch
                [ Shared.UpdateModel newSharedState |> Effect.fromShared
                , newSharedState.contactDialogState |> Storage.toJson |> Ports.save |> Effect.fromCmd
                ]
            )

        OpenContactUs ->
            let
                withOpen state =
                    { state | contactDialogState = state.contactDialogState |> (\c -> { c | showContactUs = True }) }
            in
            ( { model | localShared = withOpen model.localShared }, Shared.UpdateModel (withOpen model.localShared) |> Effect.fromShared )

        WindowResized w h ->
            let
                newModel share =
                    { share | device = classifyDevice { width = w, height = h }, width = w, height = h }
            in
            ( { model | localShared = newModel model.localShared }, Shared.UpdateModel (newModel model.localShared) |> Effect.fromShared )

        GotTerms result ->
            case result of
                Ok legal ->
                    ( { model | legal = legal }, Effect.none )

                Err _ ->
                    ( { model | legal = "Failed to load Terms. Please try reloading the page." }, Effect.none )

        GotElement id element ->
            case element of
                Ok e ->
                    ( { model | animationTracker = Dict.fromList (List.map (updateElement id e) (Dict.toList model.animationTracker)) }, Effect.none )

                Err _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ recvScroll Scrolled
        , Browser.Events.onResize WindowResized
        ]



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        device =
            shared.device.class

        h =
            shared.height

        w =
            shared.width
    in
    { title = "GCI - Reverse Engineering IC for Obsolescence and High Temperatures"
    , attributes =
        [ inFront (navbar model.localShared ModifyLocalShared)
        , inFront
            (if shared.contactDialogState.showContactUs then
                contactUs model.localShared ModifyLocalShared

             else
                none
            )
        , clip
        ]
    , element =
        column [ width fill, Region.mainContent ]
            [ column [ width (fill |> maximum (min w maxWidth)), centerX, spacing 25 ]
                [ column
                    [ Region.heading 1
                    , centerX
                    , centerY
                    , width (px (min 800 (toFloat w * 0.8 |> round)))
                    ]
                    [ el [ height (px 100) ] none
                    , paragraph [ Font.extraLight, centerX, centerY, Font.extraLight, fontSize device Xlg, padding 10, Font.center ] [ text "Terms and Conditions" ]
                    , paragraph [ fontSize device Xsm, centerX, Font.family [ Font.serif ], Font.justify ] (List.concat (List.intersperse [ html <| br [] [], html <| br [] [] ] (model.legal |> String.split "\n" |> List.map (\t -> [ text t ]))))
                    ]
                , bottomButtons shared (List.filter (\b -> b.id > 0) model.simpleBtnHoverTracker) (shouldAnimate "bottomButtons" model)
                ]
            , footer model.localShared ModifyLocalShared
            ]
    }



-- Helper Functions


bottomButtons : Shared.Model -> List SimpleBtn -> Bool -> Element Msg
bottomButtons shared btns animateSelf =
    let
        h =
            shared.height

        w =
            shared.width

        device =
            shared.device.class

        isPhone =
            device == Phone

        isTablet =
            device == Tablet

        isMobile =
            isPhone || isTablet

        btn item =
            let
                attr =
                    [ centerX
                    , Border.width 5
                    , paddingXY 20 10
                    , Border.rounded 10
                    , fontSize device Md
                    , Font.color gciBlue
                    , Font.bold
                    , htmlAttribute <| class "background_transition"
                    , Border.color gciBlue
                    , Font.center
                    , inFront
                        (el
                            [ htmlAttribute <|
                                class
                                    (if item.hovered then
                                        "point_enter_down"

                                     else
                                        "point_leave_up"
                                    )
                            , centerX
                            , centerY
                            , paddingXY 22 10
                            , fontSize device Md
                            , Font.color white
                            , Border.rounded 5
                            , Font.bold
                            , Background.color gciBlue
                            , if isMobile then
                                width fill

                              else
                                pointer
                            ]
                            (text item.name)
                        )
                    , Events.onMouseEnter (SimpleBtnHover item.id)
                    , Events.onMouseLeave (SimpleBtnUnHover item.id)
                    , if isMobile then
                        width fill

                      else
                        pointer
                    , htmlAttribute <| class "gciBtn"
                    ]
            in
            case item.message of
                Just m ->
                    el
                        (Events.onClick m :: attr)
                        (text item.name)

                Nothing ->
                    link [ centerX, width fill ] { url = item.link, label = el attr (text item.name) }
    in
    acol
        (if animateSelf then
            Animation.fromTo
                { duration = 500
                , options = []
                }
                [ P.opacity 0, P.y 100 ]
                [ P.opacity 100, P.y 0 ]

         else
            Animation.empty
        )
        [ width fill
        , padding 50
        , spacing 10
        , transparent False

        {- (not animateSelf) -}
        , htmlAttribute <| id "bottomButtons"
        ]
        [ paragraph [ Region.heading 4, fontSize device Lg, Font.center, Font.extraLight ] [ text "Want to know more about us?" ]
        , (if isMobile then
            column

           else
            row
          )
            [ centerX, spacing 10 ]
            (List.map btn btns)
        ]


shouldAnimate : String -> Model -> Bool
shouldAnimate id model =
    case Dict.get id model.animationTracker of
        Just state ->
            state.shouldAnimate

        Nothing ->
            False


setUnHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setUnHovered _ _ data =
    { data | hovered = False }


setHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setHovered id i data =
    if id == i then
        { data | hovered = True }

    else
        { data | hovered = False }


animationTrackerToCmd : ( String, AnimationState ) -> Effect Msg
animationTrackerToCmd ( k, _ ) =
    Task.attempt (GotElement k) (Browser.Dom.getElement k) |> Effect.fromCmd
