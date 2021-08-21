module Pages.Ip exposing (Model, Msg, page)

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
import Gen.Params.Ip exposing (Params)
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
    , patents : List Patent
    , trademarks : List Trademark
    , patentsPerRow : Int
    , localShared : Shared.Model
    , hideAirlock : Bool
    }


type alias Patent =
    { id : Int
    , active : Bool
    , number : String
    , date : String
    , inventor : String
    , name : String
    , image : String
    , link : String
    }


type alias Trademark =
    { id : Int
    , active : Bool
    , name : String
    , number : String
    , class : String
    , registered : String
    , firstUse : String
    , link : String
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
                , ( "patents"
                  , AnimationState
                        (PercentOfViewport
                            (if isMobile then
                                2

                             else
                                5
                            )
                        )
                        False
                  )
                , ( "bottomButtons", AnimationState (PercentOfViewport 40) False )
                ]
      , patents =
            [ Patent 0 False "9,966,319" "8/21/11" "Erick M. Spory" "Environmental Hardened Integrated Circuit Method and Apparatus" "/img/9966319.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=9966319.PN.&OS=PN/9966319&RS=PN/9966319"
            , Patent 1 False "8,466,371" "6/18/13" "Erick M. Spory" "Printed circuit board interconnecting structure with compliant cantilever interposers" "/img/8466371.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=8466371.PN.&OS=PN/8466371&RS=PN/8466371"
            , Patent 2 False "9,935,028" "11/23/13" "Erick M. Spory" "Method and Apparatus for Printing Integrated Circuit Bond Connections" "/img/9935028.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=9935028.PN.&OS=PN/9935028&RS=PN/9935028"
            , Patent 3 False "9,711,480" "11/28/11" "Erick M. Spory" "Environmental Hardened Packaged Integrated Circuit" "/img/9711480.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=9711480.PN.&OS=PN/9711480&RS=PN/9711480"
            , Patent 4 False "9,824,948" "11/20/13" "Erick M. Spory" "Integrated Circuit with Printed Bond Connections" "/img/9824948.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=9824948.PN.&OS=PN/9824948&RS=PN/9824948"
            , Patent 5 False "9,870,968" "1/28/16" "Erick M. Spory\nTimothy M. Barry" "Repackaged Integrated Circuit and Assembly Method" "/img/9870968.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=9870968.PN.&OS=PN/9870968&RS=PN/9870968"
            , Patent 6 False "10,128,161" "6/19/17" "Erick M. Spory" "3D Printed Hermetic Package Assembly and Method" "/img/10128161.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10128161.PN.&OS=PN/10128161&RS=PN/10128161"
            , Patent 7 False "10,177,056" "1/28/16" "Erick M. Spory" "Repackaged Integrated Circuit Assembly Method" "/img/10177056.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10177056.PN.&OS=PN/10177056&RS=PN/10177056"
            , Patent 8 False "10,654,259" "1/28/16" "Erick M. Spory" "Conductive Diamond Application Method and System" "/img/10654259.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10,654,259.PN.&OS=PN/10,654,259&RS=PN/10,654,259"
            , Patent 9 False "10,109,606" "1/28/16" "Erick M. Spory" "Remapped Packaged Extracted Die" "/img/10109606.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10109606.PN.&OS=PN/10109606&RS=PN/10109606"
            , Patent 10 False "10,177,054" "1/28/16" "Erick M. Spory\nTimothy M. Barry" "Method for Remapping a Packaged Extracted Die" "/img/10177054.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10177054.PN.&OS=PN/10177054&RS=PN/10177054"
            , Patent 11 False "10,147,660" "1/28/16" "Erick M. Spory" "Repackaged Integrated Circuit with 3D Printed Bond Connections" "/img/10147660.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10147660.PN.&OS=PN/10147660&RS=PN/10147660"
            , Patent 12 False "10,002,846" "1/28/16" "Erick M. Spory" "3D Printed Bond Connection Method for Repackaged Integrated Circuit" "/img/10002846.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10002846.PN.&OS=PN/10002846&RS=PN/10002846"
            , Patent 13 False "10,460,326" "1/28/16" "Erick M. Spory" "IDD Signature" "/img/10460326.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10460326.PN.&OS=PN/10460326&RS=PN/10460326"
            , Patent 14 False "10,431,510" "10/3/17" "Erick M. Spory" "Hermetic Lid Seal Method and Apparatus" "/img/10431510.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10431510.PN.&OS=PN/10431510&RS=PN/10431510"
            , Patent 15 False "10,115,645" "10/27/17" "Erick M. Spory" "Repackaged Reconditioned Die Method and Assembly" "/img/10115645.png" "https://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=10115645.PN.&OS=PN/10115645&RS=PN/10115645"
            ]
      , trademarks =
            [ Trademark 0 False "DER" "5,278,571" "CLASS 37: Repair or Maintenance of Integrated Circuits Manufacturing Machines and Systems" "8/29/17" "6/15/10" "/download/5278571.pdf"
            , Trademark 1 False "DEER" "5,400,012" "CLASS 9: Semiconductor Devices" "2/13/18" "11/1/13" "/download/5400012.pdf"
            , Trademark 2 False "DER" "5,215,549" "CLASS 9: Electronic Chips for the Manufacture of Integrated Circuits" "5/30/17" "11/1/13" "/download/5400012.pdf"
            ]
      , patentsPerRow = 3
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
    | PatentActive Int
    | PatentDeactive Int
    | TrademarkActive Int
    | TrademarkDeactive Int
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
                    ++ (if shouldAnimate "patents" model && not model.hideAirlock then
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

        PatentActive id ->
            ( { model
                | patents =
                    List.map
                        (\l ->
                            if l.id == id then
                                { l | active = True }

                            else
                                { l | active = False }
                        )
                        model.patents
              }
            , Effect.none
            )

        PatentDeactive _ ->
            ( { model | patents = List.map (\l -> { l | active = False }) model.patents }, Effect.none )

        TrademarkActive id ->
            ( { model
                | trademarks =
                    List.map
                        (\l ->
                            if l.id == id then
                                { l | active = True }

                            else
                                { l | active = False }
                        )
                        model.trademarks
              }
            , Effect.none
            )

        TrademarkDeactive _ ->
            ( { model | trademarks = List.map (\l -> { l | active = False }) model.trademarks }, Effect.none )

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
                    , spacing 100
                    ]
                    [ mainText shared (shouldAnimate "mainText" model) ]
                , patents shared model (shouldAnimate "patents" model)
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
        , inFront (el [ width fill, height fill, Background.color (rgba 0 0 0 0.4) ] none)
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
                (List.intersperse (html <| br [] []) [ text "Our", text "Patents and", text "Trademarks." ])
            )
        ]
        { src = "/img/patents.jpg", description = "Picture of GCI's head quarters" }


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
        [ paragraph [ Font.extraLight, Region.heading 1, fontSize device Xlg ] [ text "16 issued patents and counting." ]
        , paragraph [ spacing 10, fontSize device Sm, Font.light, htmlAttribute <| id "mainText", width fill ]
            [ text "GCI’s innovations are recognized with 16 patents, 3 trademarks, and additional patents currently in process.  These technological advancements set GCI apart from its competitors."
            , html <| br [] []
            , html <| br [] []
            , text "We continue to push the boundaries."
            ]
        ]


patents : Shared.Model -> Model -> Bool -> Element Msg
patents shared model animateSelf =
    let
        patentsPerRow =
            model.patentsPerRow

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

        trademark l =
            ael
                zoom
                [ width fill ]
                (column
                    [ width (px cardWidth)
                    , height (px cardHeight)
                    , centerX
                    , clip
                    , Background.color white
                    , Events.onClick (TrademarkActive l.id)
                    , Events.onMouseEnter (TrademarkActive l.id)
                    , Events.onMouseLeave (TrademarkDeactive l.id)
                    , htmlAttribute <|
                        class
                            (if animateSelf then
                                "animate_float"

                             else
                                ""
                            )
                    ]
                    [ image [ htmlAttribute <| class "animateTransform", width fill, height (px (toFloat cardHeight * (2.0 / 3.0) |> round)) ] { src = "/img/uspto.jpg", description = "USPTO logo" }
                    , el
                        ([ htmlAttribute <| class "animateTransform"
                         , height (px (toFloat cardHeight * (1.0 / 3.0) |> round))
                         , width fill
                         , Background.color white
                         , Border.shadow { blur = 3, color = rgba 0 0 0 0.2, offset = ( 0, 0 ), size = 1 }
                         ]
                            ++ (if l.active then
                                    [ moveUp cardHeight
                                    ]

                                else
                                    []
                               )
                        )
                        (column [ centerX, centerY, spacing 20 ]
                            [ el [ centerX, fontSize device Md, Font.light, Font.center ] (text l.name)
                            , el [ centerX, fontSize device Xsm, Font.color (rgb 0.2 0.2 0.3) ] (text ("#" ++ l.number ++ " - " ++ l.registered))
                            ]
                        )
                    , column
                        ([ width fill, htmlAttribute <| class "animateTransform", height (px cardHeight), Background.color white ]
                            ++ (if l.active then
                                    [ moveUp cardHeight ]

                                else
                                    []
                               )
                        )
                        [ paragraph [ fontSize device Xsm, padding 20, Font.center, centerY ]
                            [ el [ Font.center, fontSize device Sm, Font.light ] (text l.class)
                            , html <| br [] []
                            , html <| br [] []
                            , el [] (text ("Reg. No.: " ++ l.number))
                            , html <| br [] []
                            , el [] (text ("First Use: " ++ l.firstUse))
                            , html <| br [] []
                            , html <| br [] []
                            ]
                        , download [ Font.color gciBlue, centerY, mouseOver [ Font.color gciBlueLight ], centerX, Font.bold ] { url = l.link, label = el [] (text "Read More") }
                        ]
                    ]
                )

        patent l =
            ael
                zoom
                [ width fill ]
                (column
                    [ width (px cardWidth)
                    , height (px cardHeight)
                    , centerX
                    , clip
                    , Background.color white
                    , Events.onClick (PatentActive l.id)
                    , Events.onMouseEnter (PatentActive l.id)
                    , Events.onMouseLeave (PatentDeactive l.id)
                    , htmlAttribute <|
                        class
                            (if animateSelf then
                                "animate_float"

                             else
                                ""
                            )
                    ]
                    [ image [ htmlAttribute <| class "animateTransform", width fill, height (px (toFloat cardHeight * (2.0 / 3.0) |> round)) ] { src = l.image, description = l.name }
                    , el
                        ([ htmlAttribute <| class "animateTransform"
                         , height (px (toFloat cardHeight * (1.0 / 3.0) |> round))
                         , width fill
                         , Background.color white
                         , Border.shadow { blur = 3, color = rgba 0 0 0 0.2, offset = ( 0, 0 ), size = 1 }
                         ]
                            ++ (if l.active then
                                    [ moveUp cardHeight
                                    ]

                                else
                                    []
                               )
                        )
                        (column [ centerX, centerY, spacing 20 ]
                            [ el [ centerX, fontSize device Md, Font.light, Font.center ] (text l.inventor)
                            , el [ centerX, fontSize device Xsm, Font.color (rgb 0.2 0.2 0.3) ] (text ("#" ++ l.number ++ " - " ++ l.date))
                            ]
                        )
                    , column
                        ([ width fill, htmlAttribute <| class "animateTransform", height (px cardHeight), Background.color white ]
                            ++ (if l.active then
                                    [ moveUp cardHeight ]

                                else
                                    []
                               )
                        )
                        [ paragraph [ fontSize device Xsm, padding 20, Font.center, centerY ]
                            [ el [ Font.center, fontSize device Sm, Font.light ] (text l.name)
                            , html <| br [] []
                            , html <| br [] []
                            , el [] (text ("Patent number: " ++ l.number))
                            , html <| br [] []
                            , el [] (text ("Issued on: " ++ l.date))
                            , html <| br [] []
                            , html <| br [] []
                            ]
                        , newTabLink [ Font.color gciBlue, centerY, mouseOver [ Font.color gciBlueLight ], centerX, Font.bold ] { url = l.link, label = el [] (text "Read More") }
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
        , htmlAttribute <| id "patents"
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
                , el [ Font.extraLight, Font.letterSpacing 5, Font.center, centerX, fontSize device Xlg ] (text "Patents")
                ]
            )
        , el [ width (px (min (toFloat w * 0.8 |> round) (patentsPerRow * cardWidth + (patentsPerRow * cardSpacing)))), centerX ]
            (wrappedRow [ centerX, spacing cardSpacing ] (List.map patent model.patents))
        , el
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
                , el [ Font.extraLight, Font.letterSpacing 5, Font.center, centerX, fontSize device Xlg ] (text "Trademarks")
                ]
            )
        , el [ width (px (min (toFloat w * 0.8 |> round) (patentsPerRow * cardWidth + (patentsPerRow * cardSpacing)))), centerX ]
            (wrappedRow [ centerX, spacing cardSpacing ] (List.map trademark model.trademarks))
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
