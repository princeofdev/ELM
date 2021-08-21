module Pages.Obsolescence exposing (Model, Msg, page)

import Browser.Dom exposing (Viewport)
import Browser.Events
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region exposing (description)
import Gen.Params.Obsolescence exposing (Params)
import Html exposing (br, div, iframe)
import Html.Attributes exposing (attribute, class, id, property, src, style)
import Http exposing (Error(..))
import Json.Decode as Json
import Json.Encode as Encode
import Page
import Pages.Home_ exposing (AnimationState, When(..), onScreenItemtoCmd, updateElement)
import Palette exposing (FontSize(..), black, fontSize, gciBlue, maxWidth, warning, white)
import Ports exposing (disableScrolling, recvScroll)
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
    , subTexts : List SubText
    , localShared : Shared.Model
    }


type alias SubText =
    { id : Int
    , title : String
    , image : String
    , description : String
    , text : String
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
                , ( "bottomButtons", AnimationState (PercentOfViewport 40) False )
                , ( "1", AnimationState (PercentOfViewport 20) False )
                , ( "2", AnimationState (PercentOfViewport 40) False )
                , ( "3", AnimationState (PercentOfViewport 40) False )
                ]
      , subTexts =
            [ SubText 1 "GCI is a solutions provider." "/img/Power_Monitor_A1A1A5.jpg" "GCI CCA solution for Power Monitor, Maverick Missile Test Box" "GCI designs, develops and manufactures form, fit, and function drop-in replacement electronics that can be seamlessly integrated into a larger electronics system as required. The replacement electronic components work identically to the original obsolete components."
            , SubText 2 "Supporting Legacy Systems." "/img/black_circuit.jpg" "" "GCI’s engineering team has decades of experience designing electronics. Our proprietary and proven technologies provide the building blocks to engineer custom electronic solutions based upon the customer needs and requirements.\nThese drop-in replacement solutions for obsolete CCAs and microcircuits improve DoD system readiness, solving the DMSMS issues associated with lifecycle sustainment."
            , SubText 3 "Combating Counterfeits." "/img/fpga.jpg" "XILINX XC4013 FPGA Open Cavity" "GCI only uses components from authorized, franchised distributors with full traceability. This removes any possibility of counterfeit parts entering the supply chain with GCI’s solutions. Memories and FPGAs, particularly the obsolete families, are some of the commonly identified counterfeits for military customers as reported through GIDEP.\nGCI’s strict adherence to franchised suppliers eliminates this risk."
            ]
      , localShared = reset shared
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
    | WindowResized Int Int
    | ModifyLocalShared Shared.Model
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

        subtext item =
            let
                img =
                    el
                        [ width fill
                        , clip
                        , centerY
                        , Border.rounded 10
                        , inFront
                            (el
                                [ width fill
                                , height fill
                                , Border.innerShadow { blur = 18, color = rgba 0 0 0 0.3, offset = ( 1, 8 ), size = 8 }
                                ]
                                none
                            )
                        ]
                        (if item.description == "" then
                            image
                                [ centerX
                                , centerY
                                , width fill
                                ]
                                { src = item.image, description = item.title }

                         else
                            el [ inFront (paragraph [ fontSize device Xsm, Font.center, Font.light, padding 10, width fill, alignBottom, Background.color (rgba 1 1 1 0.85) ] [ text item.description ]) ]
                                (image
                                    [ centerX
                                    , centerY
                                    , width fill
                                    ]
                                    { src = item.image, description = item.title }
                                )
                        )

                content =
                    paragraph [ width fill, fontSize device Sm, Font.light ] (List.concat (List.intersperse [ html <| br [] [], html <| br [] [] ] (item.text |> String.split "\n" |> List.map (\t -> [ text t ]))))
            in
            acol
                (if shouldAnimate (String.fromInt item.id) model then
                    Animation.fromTo
                        { duration = 500
                        , options = []
                        }
                        [ P.opacity 0, P.y 100 ]
                        [ P.opacity 100, P.y 0 ]

                 else
                    Animation.empty
                )
                [ width fill, height fill, spacing 20, htmlAttribute <| id (String.fromInt item.id), transparent (not (shouldAnimate (String.fromInt item.id) model)) ]
                [ paragraph [ Region.heading 3, Font.extraLight, fontSize device Lg ] [ text item.title ]
                , (if isMobile then
                    column

                   else
                    row
                  )
                    [ width fill, spacing 20 ]
                    (if modBy 2 item.id == 0 || isMobile then
                        [ img, content ]

                     else
                        [ content, img ]
                    )
                ]
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
        , inFront
            (if model.showVimeo then
                vimeo shared

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
                    , centerX
                    , width (fill |> maximum (toFloat maxWidth * 0.7 |> round))
                    , spacing 100
                    ]
                    (mainText shared (shouldAnimate "mainText" model)
                        :: List.map subtext model.subTexts
                    )
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

        scaleByHeight =
            w // h <= 16 // 9

        playBtn item =
            el
                [ width shrink
                , height fill
                , centerX
                , transparent model.showVimeo
                ]
                (row
                    [ Border.rounded 1000
                    , width
                        (px
                            (if item.hovered then
                                300

                             else
                                120
                            )
                        )
                    , height (px 120)
                    , centerX
                    , centerY
                    , Border.shadow { blur = 20, color = rgba 0 0 0 0.7, offset = ( 0, 0 ), size = 1 }
                    , Background.color gciBlue
                    , Font.color white
                    , fontSize device Xlg
                    , htmlAttribute <| class "backgroundStretch"
                    , Events.onMouseEnter (SimpleBtnHover 0)
                    , Events.onMouseLeave (SimpleBtnUnHover 0)
                    ]
                    (if item.hovered then
                        [ ael
                            (Animation.fromTo
                                { duration = 300
                                , options = []
                                }
                                [ P.opacity 0, P.x 10 ]
                                [ P.opacity 100, P.x 0 ]
                            )
                            [ Font.bold, centerX, padding 10 ]
                            (text "Play")
                        , ael
                            (Animation.fromTo
                                { duration = 300
                                , options = []
                                }
                                [ P.x -100 ]
                                [ P.x 0 ]
                            )
                            [ Font.family [ Font.typeface "icons" ], centerX ]
                            (text "\u{E801}")
                        ]

                     else
                        [ ael
                            (Animation.fromTo
                                { duration = 300
                                , options = []
                                }
                                [ P.x 50 ]
                                [ P.x 0 ]
                            )
                            [ Font.family [ Font.typeface "icons" ], centerX ]
                            (text "\u{E801}")
                        ]
                    )
                )
    in
    Input.button
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
                (List.intersperse (html <| br [] []) [ text "We Solve", text "Electronic", text "Obsolescence." ])
            )
        , inFront (row [ centerX, centerY ] (List.map playBtn (List.filter (\a -> a.id == 0) simpleBtns)))
        ]
        { onPress = Just OpenVimeo
        , label =
            image
                [ centerX
                , if scaleByHeight then
                    height (px h)

                  else
                    width (px w)
                ]
                { src = "/img/bourbon_street_video.jpg", description = "Click or tap to play Global circuit innovation's company video" }
        }


vimeo : Shared.Model -> Element Msg
vimeo shared =
    let
        w =
            shared.width

        h =
            shared.height

        videoWidth =
            let
                scale =
                    if isPhone then
                        0.95

                    else
                        0.75
            in
            if h > ((9 * (toFloat w * scale)) / 16 |> round) then
                toFloat w * scale |> floor

            else
                (16 * (toFloat h * 0.9)) / 9 |> round

        device =
            shared.device.class

        isPhone =
            device == Phone
    in
    el
        [ width fill
        , height fill
        , htmlAttribute <| class "point_enter_down_long"
        , behindContent
            (link [ width fill, height fill ]
                { url = "/obsolescence#mainText"
                , label =
                    el
                        [ width fill
                        , height fill
                        , Background.gradient
                            { angle = degrees 165
                            , steps = [ rgba255 87 83 78 0.7, rgba255 17 24 39 0.9 ]
                            }
                        , Events.onClick CloseVimeo
                        ]
                        none
                }
            )
        ]
        (column
            [ width (px videoWidth)
            , centerX
            , centerY
            , Border.rounded 10
            , clip
            , Border.shadow { blur = 20, color = rgba 0 0 0 0.5, offset = ( 0, 0 ), size = 1 }
            ]
            [ el [ width fill, height fill, centerX, centerY ]
                (html <|
                    div
                        [ style "padding" "56.25% 0 0 0"
                        , style "position" "relative"
                        ]
                        [ iframe
                            [ style "position" "absolute"
                            , style "top" "0"
                            , style "left" "0"
                            , style "width" "100%"
                            , style "height" "100%"
                            , attribute "frameborder" "0"
                            , attribute "allow" "autoplay; fullscreen; picture-in-picture"
                            , property "allowfullscreen" (Encode.bool True)
                            , src "https://player.vimeo.com/video/322836491?autoplay=1&color=1d376c&title=0&byline=0&portrait=0"
                            ]
                            []
                        ]
                )
            ]
        )


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
        , htmlAttribute <| id "mainText"
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
        [ paragraph [ Font.extraLight, Region.heading 1, fontSize device Xlg ] [ text "Obsolescence is a big deal." ]
        , paragraph [ spacing 10, fontSize device Sm, Font.light ]
            [ text "Electronics obsolescence is common in sectors such as Defense, where equipment has long lead times and needs to be supported for many decades. It is not unusual that 70–80% of the electronic components are obsolete before the system has been deployed."
            ]
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
