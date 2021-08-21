module Pages.Whoweare exposing (Model, Msg, page)

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
import Gen.Params.Whoweare exposing (Params)
import Html exposing (br, div, iframe)
import Html.Attributes exposing (attribute, class, id, property, src, style)
import Http exposing (Error(..))
import Json.Decode as Json
import Json.Encode as Encode
import Page
import Pages.Home_ exposing (AnimationState, When(..), onScreenItemtoCmd, updateElement)
import Palette exposing (FontSize(..), black, fontSize, gciBlue, maxWidth, warning, white)
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
    , leadership : List Leadership
    , leadersPerRow : Int
    , localShared : Shared.Model
    , hideAirlock : Bool
    , values : List Value
    }


type alias Leadership =
    { id : Int
    , active : Bool
    , name : String
    , job : String
    , email : String
    , phone : String
    , image : String
    , story : String
    }


type alias SimpleBtn =
    { id : Int
    , name : String
    , link : String
    , hovered : Bool
    , message : Maybe Msg
    }


type alias Value =
    { active : Bool
    , title : String
    , detailed : String
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
                , ( "leadership"
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
                , ( "core", AnimationState (PercentOfViewport 50) False )
                , ( "bottomButtons", AnimationState (PercentOfViewport 40) False )
                ]
      , leadership =
            [ Leadership 0 False "Erick Spory" "CEO and CTO" "erick.spory@gci-global.com" "(719) 573 - 6777 x104" "/img/erick.webp" "Erick M. Spory spent 22 years at Atmel Corporation in Colorado Springs, CO, ultimately becoming a Senior Principle Failure Analysis Engineer. During his time at Atmel he worked closely with process, design, device, and packaging engineers to resolve yield issues.\nHe is currently the President and CTO of Global Circuit Innovations, which he co-founded in 2006. Mr. Spory holds 15 patents and has 4 other patents pending. These involve Environmentally Hardened Integrated Circuits, Methods for Printing Integrated Circuit Bond Connections, Repackaged Integrated Circuit and Assembly Methods, Extracted Die and Reassembly and Counterfeit Mitigation.\nMr. Spory has published numerous papers for a variety of publications, including the International Symposium for Testing and Failure Analysis and International Microelectronics Packaging and Assembly Society (IMAPS). He has presented at over twelve conferences including the Component Obsolescence Group (COG), International Microelectronics Assembly & Packaging (IMAPS) Components for Military and Space Electronics, Surface Mount Technology Association SMTA) Electronics in Harsh Environments, Afnor Obsolescence Organization, and multiple times at the Diminishing Manufacturing Supply and Material Shortages (DMSMS).\nMr. Spory has a B.S. in Materials Science Engineering with a Chemical Engineering Minor from Cornell University. Mr. Spory also has a M.S. in Electrical Engineering with a Microelectronics emphasis from the University of Colorado, Colorado Springs (UCCS) and is a current Candidate for a Ph.D. in Electrical Engineering from the University of Colorado."
            , Leadership 1 False "Jeff Murphy" "VP Operations" "jeff.murphy@gci-global.com" "(719) 573 - 6777 x103" "/img/jeff.webp" "For over 25 years Mr. Murphy has led high performing manufacturing organizations. From 1991 through 2008, he held various manufacturing management roles in the semiconductor industry working for Cypress Semiconductor and Atmel Corporation.\nPrior to co-founding GCI, Mr. Murphy led the manufacturing organization for Atmel in Colorado Springs, which was one of the highest volume and lowest cost semiconductor fabrication plants in the United States. His teams have advanced company financial metrics through improved yields, cost reduction/avoidance efforts, and productivity growth utilizing Lean methods. He places significant value on a collaborative approach with customers that ensures expectations are surpassed on a consistent basis.\nMr. Murphy received his BBA in Finance and Management from the University of Texas at Austin and holds an MBA from Colorado Christian University."
            , Leadership 2 False "Dustin Morgan" "Business Development" "dustin .morgan@gci-global.com" "(719) 573 - 6777 x107" "/img/dustin.webp" "Dustin Morgan joined GCI in 2016 as the Director of Marketing. Ms. Morgan has over 20 years of experience in both marketing and program management. She previously worked for Central Bancorp in Colorado Springs where she was tasked with the design, creation, and management of a new business venture, Johnny Martin's Car Central. Ms. Morgan served as the Executive Director in this role. Ms. Morgan currently manages DoD contract programs and is Director of Marketing and Business Development.\nMs. Morgan holds a B.A. in Communications from the University of Colorado, Boulder and a M.A. in International Development from the Josef Korbel School of International Studies at the University of Denver."
            , Leadership 3 False "Tim Barry" "Director of Engineering" "tim.barry@gci-global.com" "(719) 573 - 6777 x101" "/img/tim.webp" "Tim Barry began his career in Colorado Springs at Honeywell as a semiconductor manufacturing process engineer. Subsequently, he worked at Atmel Corp for 19 years in a variety of technical roles including Yield Staff engineer, Sustaining Process and Equipment engineering manager, and Process Engineering manager for a new facility start-up. Mr. Barry’s Wafer Fabrication Engineering Management responsibilities included overseeing a department containing over 100 technical personnel in a diversified product and technology fabrication facility.\nLater he became the Manufacturing Operations and Integration Engineering Manager for Abound Solar in Longmont Colorado. There, he directed an operations team consisting of 13 engineers refining and implementing processes and equipment necessary to support a CdTe PV manufacturing facility.\nCurrently, Mr. Barry is the Director of Engineering at Global Circuit Innovations. In his role he develops systems and structure for an emerging company engaged in specialty application semiconductors and engineering services. He provides technical consulting and hands on engineering for key clients in order to solve custom integrated circuit challenges.\nMr. Barry received his Bachelor of Science in Chemical Engineering at Arizona State University, and his Masters in Business Administration (MBA) from the University of Phoenix."
            , Leadership 4 False "Brian Mena" "Vice President of Finance" "brian.mena@gci-global.com" "(719) 573 - 6777 x102" "/img/brian.jpg" "Mr. Mena has over 20+ years of corporate strategic financial experience. He is a results oriented financial professional with a specialty in the development and design of strategic financial planning that supports decision management, critical financial transformation, performance improvement activities, analytics, reporting, and internal control. This entails providing hands-on vision development for end-to-end support, from strategy to execution, that delivers the improvements needed to transform the business to the next level of financial management.\nMr. Mena has worked in various business environments from start-up ventures to distress industries whereby he has directly managed business lines over $1.5B in revenues.\nMr. Mena graduated from the University of Colorado (Boulder & Denver) earning a bachelors and dual masters, M.B.A. & and Masters of Finance."
            , Leadership 5 False "Charlie Beebout" "Program Manager" "charlie.beebout@gci-global.com" "(719) 573 - 6777 x110" "/img/charlie.webp" "Mr. Beebout was Director of Engineering at Atmel Corporation for 17 years managing ASICs, FPGAs, EEPROMs, EPROMs, SRAMs, Digital Imaging, Wireless Communication and Cryptographic Security products. In this capacity, he had extensive achievements in integrated circuit management positions designing, debugging and manufacturing thousands of IC products for diverse Department of Defense (DoD) applications including satellites, missiles, and aircraft. His expertise also extended to other industrial and consumer utilizations.\nMr. Beebout currently serves as Program Manager for Global Circuit Innovations and focuses primarily on Circuit Card Assembly and integrated circuit obsolescence solutions as well as environmental hardening of COTS devices for the DoD.\nMr. Beebout has presented at Diminishing Manufacturing Sources and Material Shortages (DMSMS), Air Worthiness and Sustainment (AA&S), and the International Microelectronics Packaging and Assembly Society (IMAPS) technical conferences.\nMr. Beebout currently has a patent application pending entitled “Environmental Hardening to Extend Operating Lifetimes of Integrated Circuits at Elevated Temperatures” with the USPTO.\nMr. Beebout received his BSEE degree from Iowa State University."
            , Leadership 7 False "Patrick Jenkins" "Program Manager" "patrick.jenkins@gci-global.com" "(719) 573 - 6777 x111" "/img/patrick.webp" "Mr. Jenkins managed technical and operations departments at Intel, Inmos, Atmel, Vitesse, OrganicID, dpiX, and Unipixel over a career of more than 30 years. He has been involved in the development and high-volume manufacturing of ICs in numerous technologies (Microprocessor, DRAM, SRAM, non-volatile/programmable memory, GaAs digital logic) as well as printed ICs, glass flat-panel, and flexible plastic (roll-to-roll) electronics.\nMr. Jenkins currently serves as a Program Manager for Global Circuit Innovations (GCI) and focuses primarily on integrated circuit obsolescence solutions for the DoD. The particular emphasis is on solutions for obsolete FPGA/CPLD integrated circuits.\nMr. Jenkins has one patent issued. US Patent 7858513 issued 2010 “Fabrication of Self-aligned via Holes in Polymer Thin Films”.\nMr. Jenkins received his BSChE degree from Caltech."
            ]
      , values =
            [ Value False "Performance" "Executional excellence in electronics solutions is our foundation."
            , Value False "Passion" "Passion is at the heart of our company. We continue to advance, innovate, and improve."
            , Value False "Integrity" "We are honest, transparent, ethical, and fair. Customers trust us to adhere to our word."
            , Value False "Diversity" "It takes people with different ideas, strengths, interests, and cultural backgrounds to make our company succeed."
            , Value False "Accountability" "We measure ourselves against the highest standards of integrity and fiscal responsibility."
            ]
      , leadersPerRow = 3
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
    | LeaderActive Int
    | LeaderDeactive Int
    | ValueActive Int
    | ValueDeactive Int
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
                    ++ (if shouldAnimate "leadership" model && not model.hideAirlock then
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

        LeaderActive id ->
            ( { model
                | leadership =
                    List.map
                        (\l ->
                            if l.id == id then
                                { l | active = True }

                            else
                                { l | active = False }
                        )
                        model.leadership
              }
            , Effect.none
            )

        LeaderDeactive _ ->
            ( { model | leadership = List.map (\l -> { l | active = False }) model.leadership }, Effect.none )

        ValueActive id ->
            ( { model
                | values =
                    List.indexedMap
                        (\i v ->
                            if i == id then
                                { v | active = True }

                            else
                                { v | active = False }
                        )
                        model.values
              }
            , Effect.none
            )

        ValueDeactive _ ->
            ( { model | values = List.map (\v -> { v | active = False }) model.values }, Effect.none )

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
                    [ mainText shared (shouldAnimate "mainText" model)
                    , if not isPhone then
                        core shared model.values (shouldAnimate "core" model)

                      else
                        none
                    ]
                , leadership shared model (shouldAnimate "leadership" model)
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
                (List.intersperse (html <| br [] []) [ text "Global", text "Circuit", text "Innovations" ])
            )
        ]
        { src = "/img/building.jpg", description = "Picture of GCI's head quarters" }


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
        [ paragraph [ Font.extraLight, Region.heading 1, fontSize device Lg ] [ text "Where Innovation and Talent Integrate." ]
        , paragraph [ spacing 10, fontSize device Sm, Font.light, htmlAttribute <| id "mainText", width fill ]
            [ text "Founded in 2006 in Colorado Springs, CO, GCI is an established Design & Manufacturing Engineering Solutions house for DoD electronic obsolescence and electronics in extreme environments. GCI’s team leverages decades of experience to generate innovative solutions for the most challenging electronics requirements."
            ]
        ]


core shared values animateSelf =
    let
        device =
            shared.device.class

        value i item =
            el
                [ clip
                , width (fill |> minimum 250)
                , height (px 250)
                , Background.color (rgba255 29 55 108 (1.0 - (toFloat (i + 1) * 0.1)))
                , Events.onClick (ValueActive i)
                , Events.onMouseEnter (ValueActive i)
                , Events.onMouseLeave (ValueDeactive i)
                ]
                (column
                    ([ htmlAttribute <| class "animateTransform", fontSize device Sm, width fill, height fill ]
                        ++ (if item.active then
                                [ moveUp 250 ]

                            else
                                []
                           )
                    )
                    [ el [ width fill, height (px 250) ] (el [ centerX, centerY ] (text item.title))
                    , el [ width fill, height (px 250) ] (paragraph [ width (px 250), centerX, centerY, Font.center ] [ text item.detailed ])
                    ]
                )
    in
    column
        [ width fill
        , transparent (not animateSelf)
        , htmlAttribute <|
            class
                (if animateSelf then
                    "point_enter_left_long"

                 else
                    "point_idle"
                )
        , htmlAttribute <| id "core"
        ]
        [ el [ width fill, padding 20, fontSize device Md, Font.bold, Font.center, Region.heading 2, Background.color gciBlue, Font.color white ] (text "Our Core Values")
        , wrappedRow [ width fill, Font.bold, Font.color white ]
            (List.indexedMap value values)
        ]


leadership : Shared.Model -> Model -> Bool -> Element Msg
leadership shared model animateSelf =
    let
        leaders =
            model.leadership

        leadersPerRow =
            model.leadersPerRow

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

        leader l =
            ael
                zoom
                [ width fill ]
                (column
                    [ width (px cardWidth)
                    , height (px cardHeight)
                    , centerX
                    , clip
                    , Background.color white
                    , Events.onClick (LeaderActive l.id)
                    , Events.onMouseEnter (LeaderActive l.id)
                    , Events.onMouseLeave (LeaderDeactive l.id)
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
                            [ el [ centerX, fontSize device Md, Font.light ] (text l.name)
                            , el [ centerX, fontSize device Xsm, Font.color (rgb 0.2 0.2 0.3) ] (text l.job)
                            ]
                        )
                    , el
                        ([ width fill, htmlAttribute <| class "animateTransform", height (px cardHeight), Background.color white ]
                            ++ (if l.active then
                                    [ moveUp cardHeight ]

                                else
                                    []
                               )
                        )
                        (paragraph [ Font.alignLeft, fontSize device Xsm, padding 20, height (px cardHeight), scrollbarY ]
                            (List.concat
                                (List.intersperse [ html <| br [] [], html <| br [] [] ]
                                    (l.story
                                        |> String.split "\n"
                                        |> List.reverse
                                        |> List.indexedMap
                                            (\i t ->
                                                if i == 0 then
                                                    [ el [ Font.italic ] (text t) ]

                                                else
                                                    [ text t ]
                                            )
                                        |> List.reverse
                                    )
                                )
                            )
                        )
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

        nullLeader id =
            Leadership id False "" "" "" "" "" ""

        shadowSettings =
            if animateSelf then
                { blur = 10, color = rgba 0 0 0 0.3, offset = ( -5, 5 ), size = 5 }

            else
                { blur = 0, color = rgba 0 0 0 0.3, offset = ( 0, 0 ), size = 0 }
    in
    column
        [ width fill
        , htmlAttribute <| class "circuit_board"
        , htmlAttribute <| id "leadership"
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
                , el [ Font.extraLight, Font.letterSpacing 5, Font.center, centerX, fontSize device Xlg ] (text "Leadership")
                ]
            )

        -- if width will wrap someone to the bottom, then move Erick up rather than leave someone alone on the bottom
        , if modBy (min (toFloat w * 0.8 |> round) (leadersPerRow * cardWidth + (leadersPerRow * cardSpacing)) // cardWidth) (List.length leaders) == 1 then
            column [ spacing cardSpacing, width (px (min (toFloat w * 0.8 |> round) (leadersPerRow * cardWidth + (leadersPerRow * cardSpacing)))), centerX ]
                [ List.head leaders |> Maybe.withDefault (nullLeader 0) |> leader
                , wrappedRow [ centerX, spacing cardSpacing ] (List.tail leaders |> Maybe.withDefault [ nullLeader 1 ] |> List.map leader)
                ]

          else
            el [ width (px (min (toFloat w * 0.8 |> round) (leadersPerRow * cardWidth + (leadersPerRow * cardSpacing)))), centerX ]
                (wrappedRow [ centerX, spacing cardSpacing ] (List.map leader leaders))
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
