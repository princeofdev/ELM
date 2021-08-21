module Pages.Papers exposing (Model, Msg, page)

import Browser.Dom exposing (Viewport)
import Browser.Events
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font exposing (letterSpacing)
import Element.Input as Input
import Element.Region as Region
import Gen.Params.Papers exposing (Params)
import Html exposing (br, div, iframe)
import Html.Attributes exposing (attribute, class, id, property, src, style)
import Http exposing (Error(..))
import Json.Decode as Json
import Json.Encode as Encode
import Page
import Pages.Home_ exposing (AnimationState, When(..), onScreenItemtoCmd, updateElement)
import Palette exposing (FontSize(..), black, fontSize, gciBlue, gciBlueLight, maxWidth, warning, white)
import Ports exposing (disableScrolling, recvScroll)
import Process
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
    { showVimeo : Bool
    , simpleBtnHoverTracker : List SimpleBtn
    , animationTracker : Dict String AnimationState
    , papers : List Paper
    , papersPerRow : Int
    , localShared : Shared.Model
    , hideAirlock : Bool
    }


type alias Paper =
    { id : Int
    , active : Bool
    , link : String
    , author : String
    , date : String
    , summary : String
    , image : String
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
    let
        isMobile =
            device == Phone || device == Tablet

        device =
            shared.device.class
    in
    ( { showVimeo = False
      , simpleBtnHoverTracker =
            [ SimpleBtn 0 "Play" "#" False (Just OpenVimeo)
            , SimpleBtn 1 "Intellectual Property" "/ip" False Nothing
            , SimpleBtn 2 "What We Do" "/#whatwedo" False Nothing
            , SimpleBtn 3 "Contact Us" "" False (Just OpenContactUs)
            , SimpleBtn 4 "Technical Papers" "/papers" False Nothing
            ]
      , animationTracker =
            Dict.fromList
                [ ( "mainText", AnimationState (PercentOfViewport 40) False )
                , ( "papers"
                  , AnimationState
                        (PercentOfViewport
                            (if isMobile then
                                5

                             else
                                10
                            )
                        )
                        False
                  )
                , ( "bottomButtons", AnimationState (PercentOfViewport 40) False )
                ]
      , papers =
            [ Paper 0 False "/download/Integrated_Circuit_(IC)_Die_Extraction_And_Reassembly.pdf" "Erick Spory" "2020" "" "/img/Air_Force_Research_Laboratory.jpg"
            , Paper 1 False "/download/LRU,_CCA,_&_IC_Microcircuit_Obsolescence_Solutions_without_System_Redesign.pdf" "Erick Spory" "2018" "" "/img/DMSMS2018.jpg"
            , Paper 2 False "/download/Successful_FPGA_Obsolescence_Form,_Fit,_and_Function_Solution_Using_a_MCM_and_DER_to_Implement_Original_Logic_Design.pdf" "Erick Spory" "2018" "" "/img/imaps.jpg"
            , Paper 3 False "/download/Die_Extraction_And_Reassembly_Process.pdf" "Erick Spory" "2017" "" "/img/AFLCMC.jpg"
            , Paper 4 False "/download/A_How-To_Guide_on_Addressing_and_Resolving_IC_Obsolescence.pdf" "Charlie Beebout" "2017" "" "/img/DMSMS2017.jpg"
            , Paper 5 False "/download/NSWC_Crane_&_GCI:_A_DMSMS_Case_Study.pdf" "Erick Spory" "2016" "" "/img/navsea.jpg"
            , Paper 6 False "/download/Increased_High-Temperature_Reliability_and_Package_Hardening_of_Commercial_Integrated_Circuits.pdf" "Erick Spory" "2015" "" "/img/imaps.jpg"
            , Paper 7 False "/download/Increasing_High-Temperature_Reliability_of_Plastic_ICs_Using_DEER.pdf" "Erick Spory" "2015" "" "/img/smta.jpg"
            , Paper 8 False "/download/Frequently_Asked_Questions.pdf" "" "" "FAQ" "/img/logo.jpg"
            ]
      , papersPerRow = 3
      , localShared = reset shared
      , hideAirlock = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = OpenVimeo
    | CloseVimeo
    | SimpleBtnHover Int
    | SimpleBtnUnHover Int
    | Scrolled Int
    | GotElement String (Result Browser.Dom.Error Browser.Dom.Element)
    | OpenContactUs
    | PaperActive Int
    | PaperDeactive Int
    | ModifyLocalShared Shared.Model
    | WindowResized Int Int
    | HideAirlock ()
    | Submited (Result Http.Error FormResponse)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenVimeo ->
            ( { model | showVimeo = True }, disableScrolling True |> Effect.fromCmd )

        CloseVimeo ->
            ( { model | showVimeo = False }, disableScrolling False |> Effect.fromCmd )

        SimpleBtnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setHovered id) model.simpleBtnHoverTracker }, Effect.none )

        SimpleBtnUnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setUnHovered id) model.simpleBtnHoverTracker }, Effect.none )

        GotElement id element ->
            case element of
                Ok e ->
                    ( { model | animationTracker = Dict.fromList (List.map (updateElement id e) (Dict.toList model.animationTracker)) }, Effect.none )

                Err _ ->
                    ( model, Effect.none )

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
                (List.map animationTrackerToCmd (List.filter (\( _, v ) -> v.shouldAnimate == False) (Dict.toList model.animationTracker))
                    ++ (if shouldAnimate "papers" model && not model.hideAirlock then
                            [ Task.perform HideAirlock (Process.sleep 1000) |> Effect.fromCmd ]

                        else
                            [ Effect.none ]
                       )
                )
            )

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

        PaperActive id ->
            ( { model
                | papers =
                    List.map
                        (\p ->
                            if p.id == id then
                                { p | active = True }

                            else
                                { p | active = False }
                        )
                        model.papers
              }
            , Effect.none
            )

        PaperDeactive _ ->
            ( { model | papers = List.map (\p -> { p | active = False }) model.papers }, Effect.none )

        HideAirlock _ ->
            ( { model | hideAirlock = True }, Effect.none )



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
        ]
    , element =
        column [ width fill, Region.mainContent, clip ]
            [ column [ width (fill |> maximum maxWidth), centerX, spacing 50 ]
                [ head shared model
                , column
                    [ paddingXY
                        (if isPhone then
                            10

                         else
                            100
                        )
                        0
                    , width (fill |> maximum maxWidth)
                    , spacing 50
                    ]
                    [ mainText shared (shouldAnimate "mainText" model) ]
                , papers shared model (shouldAnimate "papers" model)
                , bottomButtons shared (List.filter (\b -> b.id > 0) model.simpleBtnHoverTracker) (shouldAnimate "bottomButtons" model)
                ]
            , footer model.localShared ModifyLocalShared
            ]
    }


head : Shared.Model -> Model -> Element Msg
head shared model =
    let
        simpleBtns =
            model.simpleBtnHoverTracker

        h =
            shared.height

        w =
            shared.width

        device =
            shared.device.class

        isPhone =
            device == Phone
    in
    image
        [ width fill
        , height (px h)
        , clip
        , inFront (el [ width fill, height fill, Background.color (rgba 0 0 0 0.25) ] none)
        , inFront
            (paragraph
                [ fontSize device XXlg
                , Font.color white
                , Font.extraBold
                , alignBottom
                , padding
                    (if isPhone then
                        3

                     else
                        min 150 (toFloat w * 0.1) |> floor
                    )
                ]
                (List.intersperse (html <| br [] []) [ text "Technical", text "Papers and", text "Presentations." ])
            )
        ]
        { src = "/img/papers.jpg", description = "Picture of GCI's head quarters" }


mainText : Shared.Model -> Bool -> Element Msg
mainText shared animateSelf =
    let
        device =
            shared.device.class

        isPhone =
            device == Phone

        w =
            shared.width
    in
    column
        [ width fill
        , spacing 25
        , padding 25
        , width fill
        , inFront
            (el [ htmlAttribute <| class "clip_top", width fill, height fill, Background.color gciBlue ]
                none
            )
        , inFront
            (el
                [ htmlAttribute <|
                    class
                        (if animateSelf then
                            "animate_clip_bottom"

                         else
                            "clip_bottom"
                        )
                , width fill
                , height fill
                , Background.color gciBlue
                ]
                none
            )
        , inFront
            (el
                [ htmlAttribute <|
                    class
                        (if animateSelf then
                            "animate_clip_cover"

                         else
                            "clip_cover"
                        )
                , width fill
                , height fill
                , Background.color white
                ]
                none
            )
        ]
        [ paragraph [ Font.extraLight, Region.heading 1, fontSize device Lg ] [ text "GCI Engages with the Technical Community." ]
        , paragraph [ spacing 10, fontSize device Sm, Font.light, htmlAttribute <| id "mainText", width fill ]
            [ text "GCI regularly presents at technical conferences and routinely publishes technical papers."
            ]
        ]


papers : Shared.Model -> Model -> Bool -> Element Msg
papers shared model animateSelf =
    let
        papersPerRow =
            model.papersPerRow

        hideAirlock =
            model.hideAirlock

        device =
            shared.device.class

        w =
            shared.width

        isPhone =
            device == Phone

        cardWidth =
            300

        cardHeight =
            500

        cardSpacing =
            50

        name s =
            s |> String.replace "/download/" "" |> String.replace "_" " "

        zoom =
            if animateSelf then
                Animation.steps
                    { startAt = [ P.scaleXY 0.9 0.9 ]
                    , options = [ Animation.easeInOutQuad ]
                    }
                    [ Animation.step 1000 [ P.scaleXY 0.9 0.9 ]
                    , Animation.step 500 [ P.scaleXY 1 1 ]
                    ]

            else
                Animation.steps
                    { startAt = [ P.scaleXY 0.9 0.9 ]
                    , options = [ Animation.easeInOutQuad ]
                    }
                    [ Animation.step 10 [ P.scaleXY 0.9 0.9 ]
                    , Animation.step 10 [ P.scaleXY 0.9 0.9 ]
                    ]

        paper p =
            ael
                zoom
                [ width fill ]
                (column
                    [ width (px cardWidth)
                    , height (px cardHeight)
                    , centerX
                    , clip
                    , Background.color white
                    , Events.onClick (PaperActive p.id)
                    , Events.onMouseEnter (PaperActive p.id)
                    , Events.onMouseLeave (PaperDeactive p.id)
                    , htmlAttribute <|
                        class
                            (if animateSelf then
                                "animate_float"

                             else
                                ""
                            )
                    ]
                    [ image [ htmlAttribute <| class "animateTransform", width fill, height (px (toFloat cardHeight * (1.0 / 2.0) |> round)) ] { src = p.image, description = name p.link |> String.split "." |> List.head |> Maybe.withDefault (name p.link) }
                    , el
                        ([ htmlAttribute <| class "animateTransform"
                         , height (px (toFloat cardHeight * (1.0 / 2.0) |> round))
                         , width fill
                         , Background.color white
                         , Border.shadow { blur = 3, color = rgba 0 0 0 0.2, offset = ( 0, 0 ), size = 1 }
                         ]
                            ++ (if p.active then
                                    [ moveUp cardHeight
                                    ]

                                else
                                    []
                               )
                        )
                        (column [ centerX, centerY, spacing 20, padding 5 ]
                            [ paragraph [ centerX, fontSize device Sm, Font.light, Font.center ] [ text (name p.link |> String.split "." |> List.head |> Maybe.withDefault (name p.link)) ]
                            , el [ centerX, fontSize device Xsm, Font.color (rgb 0.2 0.2 0.3) ] (text p.date)
                            ]
                        )
                    , column
                        ([ width fill, htmlAttribute <| class "animateTransform", height (px cardHeight), Background.color white ]
                            ++ (if p.active then
                                    [ moveUp cardHeight ]

                                else
                                    []
                               )
                        )
                        [ paragraph [ fontSize device Xsm, padding 20, Font.center, centerY ]
                            [ el [ Font.center, fontSize device Sm, Font.light ] (text p.summary)
                            , html <| br [] []
                            , html <| br [] []
                            , if String.isEmpty p.author then
                                none

                              else
                                el [] (text ("Author : " ++ p.author))
                            , html <| br [] []
                            , if String.isEmpty p.date then
                                none

                              else
                                el [] (text ("Published on: " ++ p.date))
                            , html <| br [] []
                            , html <| br [] []
                            ]
                        , downloadAs [ Font.color gciBlue, centerY, mouseOver [ Font.color gciBlueLight ], centerX, Font.bold ] { url = p.link, filename = name p.link, label = el [] (text "Download") }
                        ]
                    ]
                )

        airlock =
            Animation.fromTo
                { duration = 3000
                , options = []
                }
                [ P.x 0 ]
                [ P.x (toFloat w) ]

        airlock2 =
            Animation.fromTo
                { duration = 3000
                , options = []
                }
                [ P.x 0 ]
                [ P.x -(toFloat w) ]

        shadowSettings =
            if animateSelf then
                { blur = 10, color = rgba 0 0 0 0.3, offset = ( -5, 5 ), size = 5 }

            else
                { blur = 0, color = rgba 0 0 0 0.3, offset = ( 0, 0 ), size = 0 }
    in
    column
        [ width fill
        , htmlAttribute <| class "circuit_board"
        , htmlAttribute <| id "papers"
        , spacing 50
        , padding 50
        , inFront
            (if hideAirlock then
                none

             else
                row [ width fill, height fill, htmlAttribute <| class "ignore_pointer" ]
                    [ ael
                        (if animateSelf then
                            airlock2

                         else
                            Animation.empty
                        )
                        [ height fill, width fill, Background.color white, Border.shadow shadowSettings ]
                        none
                    , ael
                        (if animateSelf then
                            airlock

                         else
                            Animation.empty
                        )
                        [ height fill, width fill, Background.color white, Border.shadow shadowSettings ]
                        none
                    ]
            )
        , Border.innerShadow { blur = 10, color = rgba 0 0 0 0.3, offset = ( -5, 5 ), size = 5 }
        ]
        [ el
            [ centerX
            , htmlAttribute <|
                class
                    (if animateSelf then
                        "animate_float"

                     else
                        ""
                    )
            ]
            (acol
                zoom
                [ spacing 3
                , if isPhone then
                    paddingXY 20 20

                  else
                    padding 55
                , Background.color white
                ]
                [ el [ Font.bold, fontSize device Xsm, Font.center, centerX, Font.color (rgb 0.2 0.2 0.3) ] (text "Global Circuit Innovations")
                , paragraph [ Font.extraLight, Font.letterSpacing 5, Font.center, centerX, fontSize device Xlg ] [ text "Technical Papers" ]
                ]
            )
        , el [ width (px (min (toFloat w * 0.8 |> round) (papersPerRow * cardWidth + (papersPerRow * cardSpacing)))), centerX ]
            (wrappedRow [ centerX, spacing cardSpacing ] (List.map paper model.papers))
        ]


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
        [ width fill, padding 50, spacing 10, transparent (not animateSelf), htmlAttribute <| id "bottomButtons" ]
        [ paragraph [ Region.heading 4, fontSize device Lg, Font.center, Font.extraLight ] [ text "Want to know more about us?" ]
        , (if isMobile then
            column

           else
            row
          )
            [ centerX, spacing 10 ]
            (List.map btn btns)
        ]


setHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setHovered id i data =
    if id == i then
        { data | hovered = True }

    else
        { data | hovered = False }


setUnHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setUnHovered _ _ data =
    { data | hovered = False }


animationTrackerToCmd : ( String, AnimationState ) -> Effect Msg
animationTrackerToCmd ( k, _ ) =
    Task.attempt (GotElement k) (Browser.Dom.getElement k) |> Effect.fromCmd


shouldAnimate : String -> Model -> Bool
shouldAnimate id model =
    case Dict.get id model.animationTracker of
        Just state ->
            state.shouldAnimate

        Nothing ->
            False
