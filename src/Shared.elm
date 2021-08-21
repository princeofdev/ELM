module Shared exposing
    ( Flags
    , FormResponse
    , Model
    , Msg(..)
    , acol
    , ael
    , arow
    , contactUs
    , footer
    , init
    , navbar
    , reset
    , setPhoneCursor
    , subscriptions
    , update
    )

import Browser.Events
import Browser.Navigation as Nav
import Char exposing (isDigit)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (innerShadow, rounded, shadow)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Email as Email
import Html exposing (a, br, div, iframe, span, video, wbr)
import Html.Attributes exposing (alt, attribute, autoplay, class, classList, id, loop, property, src, style)
import Html.Events
import Json.Decode as Json
import Json.Encode as Encode
import Palette exposing (FontSize(..), black, fontSize, gciBlue, gciBlueLight, maxWidth, warning, white)
import PhoneNumber
import PhoneNumber.Countries exposing (countryUS)
import Ports exposing (disableScrolling, google, recvScroll, setCursor, showNav)
import Process
import Request exposing (Request)
import Set exposing (Set)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage as Storage
    exposing
        ( Address
        , BtnOptions(..)
        , ContactDialogState
        , NavBarDisplay(..)
        , NavItem
        , SendState(..)
        )
import Task
import Time


type alias Flags =
    { width : Int
    , height : Int
    , year : Int
    , storage : Json.Value
    }


type alias Model =
    { scrolledDistance : Int
    , navbarDisplay : NavBarDisplay
    , address : Address
    , socialMedia : List SocialMediaItem
    , certifications : List CertificationItem
    , currentYear : Int
    , device : Device
    , width : Int
    , height : Int
    , contactDialogState : ContactDialogState
    , showMobileNav : Bool
    , navHoverTracker : List NavItem
    , user : Maybe String
    }


type alias FormResponse =
    { next : String
    , ok : Bool
    }


type alias CertificationItem =
    { src : String
    , description : String
    }



-- expects social media images to be a font. I reccommend using fontello to build the font


type alias SocialMediaItem =
    { char : String
    , hoverColor : Element.Color
    , link : String
    }


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    let
        store =
            Storage.fromJson flags.storage
    in
    ( { scrolledDistance = 0
      , navbarDisplay = Show
      , navHoverTracker =
            [ NavItem "WHO WE ARE" "#" False (Url "/whoweare")
            , NavItem "WHAT WE DO" "#" False (Url "/#whatwedo")
            , NavItem "NEWSROOM" "#" False (Url "/newsroom")
            , NavItem "CONTACT US" "#" False (SetContactUs True)
            ]
      , address =
            Address
                "4815 List Drive, Suite 109"
                "Colorado Springs, CO 80919"
                "https://www.google.com/maps/place/Global+Circuit+Innovations/@38.9010033,-104.853527,17z/data=!3m1!4b1!4m5!3m4!1s0x871346a0f04367d1:0x112099ed55c03e4b!8m2!3d38.9010033!4d-104.8513383?hl=en"
                "+1 (719) 573 - 6777"
                "tel:+17195736777"
                "info@gci-global.com"
                "mailto:info@gci-global.com"
      , socialMedia =
            [ SocialMediaItem "\u{F09A}" (rgb255 59 89 152) "https://www.facebook.com/globalcircuitinnovations"
            , SocialMediaItem "\u{F099}" (rgb255 29 161 242) "https://twitter.com/dieextraction"
            , SocialMediaItem "\u{F30C}" (rgb255 0 119 181) "https://www.linkedin.com/company/4804252"
            ]
      , certifications =
            [ CertificationItem "/img/platinum_certified-v2_white.svg" "AS9100:2016 - ISO 9001:2015 Certified"
            , CertificationItem "/img/ANAB-certified_white.svg" "AS9100:2016 - ISO 9001:2015 Certified"
            ]
      , currentYear = flags.year
      , device = classifyDevice { width = flags.width, height = flags.height }
      , width = flags.width
      , height = flags.height
      , contactDialogState = store
      , showMobileNav = False
      , user = Nothing
      }
    , Cmd.none
    )


type Msg
    = UpdateModel Model
    | Google String


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        UpdateModel newModel ->
            let
                oldPhone =
                    model.contactDialogState.phone

                newPhone =
                    newModel.contactDialogState.phone

                newSafeModel =
                    { newModel | user = if userWantsSignOut then Nothing else model.user }

                userWantsSignOut =
                    case model.user of
                        Just _ ->
                            case newModel.user of
                                Just _ ->
                                    False
                                Nothing  ->
                                    True
                        _ ->
                            False

                signOutUser =
                    if userWantsSignOut then
                        Ports.signOut ()
                    else
                        Cmd.none
                

            in
            if not (oldPhone == newPhone) then
                ( newSafeModel
                , Cmd.batch [ setPhoneCursor (Maybe.withDefault "" oldPhone) (Maybe.withDefault "" newPhone), signOutUser ]
                )

            else if newModel.contactDialogState.showContactUs && not model.contactDialogState.showContactUs then
                ( newSafeModel, Cmd.batch [ disableScrolling True, signOutUser ] )

            else if not newModel.contactDialogState.showContactUs && model.contactDialogState.showContactUs then
                ( newSafeModel, Cmd.batch [ disableScrolling False, signOutUser ] )

            else
                ( newSafeModel, Cmd.batch [ Cmd.none, signOutUser ] )

        Google idToken ->
            ( { model | user = Just idToken }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    google Google



-- hook in for elm-simple-animation


animatedUi : (List (Attribute msg) -> children -> Element msg) -> Animation -> List (Attribute msg) -> children -> Element msg
animatedUi =
    Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }


ael : Animation -> List (Element.Attribute msg) -> Element msg -> Element msg
ael =
    animatedUi Element.el


arow : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
arow =
    animatedUi Element.row


acol : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
acol =
    animatedUi Element.column



-- Helper Functions


currentYear : Task.Task x Int
currentYear =
    Task.map2 Time.toYear Time.here Time.now


navbar : Model -> (Model -> b) -> Element b
navbar shared message =
    let
        animationTracker =
            shared.navHoverTracker

        display =
            shared.navbarDisplay

        device =
            shared.device.class

        isDesktop =
            device == Desktop

        isBigDesktop =
            device == BigDesktop

        navBtn ( id, item ) =
            case item.onClick of
                Url s ->
                    link [] { url = s, label = navbarBtn ( id, item ) False }

                _ ->
                    navbarBtn ( id, item ) True

        mobileNavBtn item =
            let
                attr =
                    [ fontSize device Md
                    , Border.color gciBlue
                    , Border.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
                    , htmlAttribute <| class "letterSpacing"
                    , centerX
                    , padding 5
                    ]
            in
            case item.onClick of
                Url s ->
                    link attr { url = s, label = text item.name }

                SetContactUs b ->
                    Input.button attr
                        { onPress = Just (message (setContactUs shared True))
                        , label = text item.name
                        }

        navbarBtn ( id, item ) shouldOnClick =
            row
                [ height (px 80)
                , pointer
                , paddingXY 80 0
                , inFront
                    (row
                        [ htmlAttribute <|
                            class
                                (if item.hovered then
                                    "wipe_point_active"

                                 else
                                    "wipe_point"
                                )
                        , width fill
                        , height fill
                        , Background.color white
                        ]
                        [ el [ centerX, centerY, Font.color black ] (text item.name) ]
                    )
                , behindContent
                    (row
                        [ width fill
                        , height fill
                        , Background.color gciBlue
                        , innerShadow { offset = ( 0, 0 ), size = 0.15, blur = 8, color = rgb255 13 25 48 }
                        ]
                        [ el [ centerX, centerY, Font.color white ] (text item.name) ]
                    )
                , if shouldOnClick then
                    Events.onClick (message (setContactUs shared True))

                  else
                    pointer
                , Events.onMouseEnter (message (navBtnHover shared id))
                , Events.onMouseLeave (message (navBtnUnHover shared id))
                ]
                []

        spacer =
            column
                [ width fill
                , height fill
                , Background.color (rgb 1 1 1)
                ]
                []

        logo =
            link [ height fill, Background.color white ]
                { url = "/"
                , label =
                    el
                        [ height fill
                        , pointer
                        ]
                        (image
                            [ height (px 50)
                            , paddingXY
                                (if device == Phone then
                                    10

                                 else
                                    24
                                )
                                0
                            , centerX
                            , centerY
                            ]
                            { src =
                                if shared.width < 350 then
                                    "/img/logo_sans_text.svg"

                                else
                                    "/img/logo_sans_ring.svg"
                            , description = "Global Circuit Innovations"
                            }
                        )
                }
    in
    arow
        (case display of
            Show ->
                Animation.empty

            Hide ->
                Animation.fromTo
                    { duration = 300
                    , options = [ Animation.easeIn ]
                    }
                    [ P.y 0 ]
                    [ P.y -100 ]

            Enter ->
                Animation.fromTo
                    { duration = 300
                    , options = [ Animation.easeIn ]
                    }
                    [ P.y -100 ]
                    [ P.y 0 ]
        )
        (( shared.showMobileNav
         , [ width fill
           , height shrink
           , Font.family [ Font.sansSerif ]
           , fontSize device Xsm
           , Region.navigation
           , Background.color white
           , shadow { offset = ( 0, 0 ), size = 0.15, blur = 5, color = black }
           ]
         )
            |> (\( m, a ) ->
                    if not (isDesktop || isBigDesktop) then
                        behindContent
                            (el
                                [ width fill
                                , height fill
                                , below
                                    (ael
                                        (if shared.showMobileNav then
                                            Animation.fromTo
                                                { duration = 500
                                                , options = []
                                                }
                                                [ P.y -300 ]
                                                [ P.y 0 ]

                                         else
                                            Animation.fromTo
                                                { duration = 500
                                                , options = []
                                                }
                                                [ P.y 0 ]
                                                [ P.y -300 ]
                                        )
                                        [ Background.color white
                                        , width fill
                                        , shadow { offset = ( 0, 2 ), size = 0.15, blur = 3, color = black }
                                        ]
                                        (column
                                            [ width fill
                                            , centerX
                                            , Font.light
                                            , width shrink
                                            , padding 20
                                            , spacing 20
                                            ]
                                            (List.map mobileNavBtn animationTracker)
                                        )
                                    )
                                ]
                                none
                            )
                            :: a

                    else
                        a
               )
        )
        [ case device of
            Desktop ->
                column [ width (fill |> maximum maxWidth), centerX ]
                    [ row [ width fill, spaceEvenly ]
                        (List.concat
                            [ [ logo, spacer ]
                            , List.map navBtn (List.indexedMap Tuple.pair animationTracker)
                            ]
                        )
                    ]

            BigDesktop ->
                column [ width (fill |> maximum maxWidth), centerX ]
                    [ row [ width fill, spaceEvenly ]
                        (List.concat
                            [ [ logo, spacer ]
                            , List.map navBtn (List.indexedMap Tuple.pair animationTracker)
                            ]
                        )
                    ]

            _ ->
                row [ width fill, height (px 80), Background.color white ]
                    [ el [ height fill ] logo
                    , if device == Tablet then
                        Input.button [ height fill, alignRight, centerY ]
                            { onPress = Just (message (setContactUs shared True))
                            , label =
                                image [ height (px 50) ]
                                    { src =
                                        if shared.contactDialogState.showContactUs then
                                            "/img/email-open.svg"

                                        else
                                            "/img/email.svg"
                                    , description = "contact button"
                                    }
                            }

                      else
                        none
                    , Input.button [ alignRight ]
                        { onPress = Just (message (toggleMobileNav shared))
                        , label =
                            html <|
                                div [ classList [ ( "hamburger", True ), ( "hamburger--collapse", True ), ( "is-active", shared.showMobileNav ) ] ]
                                    [ div [ class "hamburger-box" ]
                                        [ div [ class "hamburger-inner" ] []
                                        ]
                                    ]
                        }
                    ]
        ]


contactUs : Model -> (Model -> b) -> Element b
contactUs shared message =
    let
        state =
            shared.contactDialogState

        address =
            shared.address

        device =
            shared.device.class

        isDesktop =
            device == Desktop

        isBigDesktop =
            device == BigDesktop

        isPhone =
            device == Phone

        w =
            shared.width

        h =
            shared.height

        break =
            html <| br [] []

        contactDialog =
            column [ width fill, height fill ]
                [ case state.currentPage of
                    0 ->
                        column
                            [ width fill, height (px 150), Font.light, htmlAttribute <| class "backgroundGrow" ]
                            [ row [ width fill, alignTop, padding 10 ]
                                [ el [ fontSize device Md, centerX ] (text "Welcome to GCI!")
                                ]
                            , if state.nameError then
                                el [ fontSize device Sm, centerX, Font.color warning ] (text "Please tell us who you are.")

                              else
                                el [ fontSize device Sm, centerX, padding 10 ] (text "Can we get a name?")
                            , Input.text
                                [ rounded 100
                                , width (px (min 400 w))
                                , centerX
                                , centerY
                                , onEnter (message (contactUsNext shared))
                                , Border.color
                                    (if state.nameError then
                                        warning

                                     else
                                        gciBlue
                                    )
                                , Border.width 2
                                , Font.center
                                ]
                                { onChange = \s -> message (contactName shared s)
                                , text = state.name
                                , placeholder = Just (Input.placeholder [ Font.center ] (text "First & Last"))
                                , label = Input.labelHidden "Name"
                                }
                            ]

                    1 ->
                        column
                            [ width fill, height (px 300), padding 30, Font.light, spacing 25, htmlAttribute <| class "backgroundGrow" ]
                            [ row [ width fill, alignTop ]
                                [ paragraph [ fontSize device Md, centerX, Font.center ]
                                    [ case String.trim state.name of
                                        "" ->
                                            text "Thanks for reaching out!"

                                        n ->
                                            case List.head (String.split " " n) of
                                                Just first_name ->
                                                    case String.uncons first_name of
                                                        Just ( first, tail ) ->
                                                            text ("Thanks for reaching out " ++ (String.toUpper (String.fromChar first) ++ tail) ++ "!")

                                                        Nothing ->
                                                            text "Thanks for reaching out!"

                                                Nothing ->
                                                    text "Thanks for reaching out!"
                                    ]
                                ]
                            , if state.emailError then
                                el [ fontSize device Sm, centerX, Font.color warning ] (text "That email seems wrong.")

                              else if state.phoneError then
                                el [ fontSize device Sm, centerX, Font.color warning ] (text "That phone number seems wrong")

                              else
                                el [ fontSize device Sm, centerX ] (text "How can we contact you?")
                            , Input.email
                                [ rounded 100
                                , width (px (min w 400))
                                , centerX
                                , Font.center
                                , onEnter (message (contactUsNext shared))
                                , Border.color
                                    (if state.emailError then
                                        warning

                                     else
                                        gciBlue
                                    )
                                , Border.width 2
                                ]
                                { onChange = \s -> message (contactEmail shared s)
                                , text = Maybe.withDefault "" state.email
                                , placeholder = Just (Input.placeholder [ Font.center ] (text "name@example.com"))
                                , label = Input.labelHidden "Email"
                                }
                            , Input.text
                                [ rounded 100
                                , width (px (min w 400))
                                , centerX
                                , Font.center
                                , onEnter (message (contactUsNext shared))
                                , htmlAttribute <| id "phoneInput"
                                , Border.color
                                    (if state.phoneError then
                                        warning

                                     else
                                        gciBlue
                                    )
                                , Border.width 2
                                ]
                                { onChange = \s -> message (contactPhone shared s)
                                , text = Maybe.withDefault "" state.phone
                                , placeholder = Just (Input.placeholder [ Font.center ] (text "(123) 456 - 7890"))
                                , label = Input.labelHidden "Phone Number"
                                }
                            ]

                    2 ->
                        column
                            [ width fill
                            , padding 30
                            , height
                                (px
                                    (if isPhone then
                                        310

                                     else
                                        min (toFloat h * 0.87 |> floor) 530
                                    )
                                )
                            , Font.light
                            , htmlAttribute <| class "backgroundGrow"
                            , htmlAttribute <| class "gciScroll"
                            ]
                            [ row [ width fill, alignTop, padding 10 ]
                                [ if state.messageError then
                                    el [ fontSize device Md, centerX, Font.color warning ] (text "Use your words please!")

                                  else
                                    el [ fontSize device Md, centerX ] (text "What can we do for you?")
                                ]
                            , Input.multiline
                                [ rounded 20
                                , width (px (min w 500))
                                , height
                                    (px
                                        (if isPhone then
                                            200

                                         else
                                            min (toFloat h * 0.72 |> floor) 415
                                        )
                                    )
                                , alignTop
                                , centerX
                                , Border.color
                                    (if state.messageError then
                                        warning

                                     else
                                        gciBlue
                                    )
                                , Border.width 2
                                ]
                                { onChange = \s -> message (contactMsg shared s)
                                , text = Maybe.withDefault "" state.message
                                , placeholder =
                                    Just
                                        (Input.placeholder []
                                            (paragraph []
                                                [ text "Type message here."
                                                ]
                                            )
                                        )
                                , spellcheck = True
                                , label = Input.labelHidden "Message"
                                }
                            ]

                    3 ->
                        case state.send of
                            SendOk ->
                                column
                                    [ width fill, height (px 100), Font.light, htmlAttribute <| class "backgroundGrow" ]
                                    [ row [ width fill, alignTop, centerY, padding 10 ]
                                        [ el [ fontSize device Md, centerX, centerY ] (text "Sent!")
                                        ]
                                    , el [ fontSize device Sm, centerX ] (paragraph [ Font.center ] [ text "We will reach back out to ", html <| br [] [], text (Maybe.withDefault "you" state.email ++ " soon!") ])
                                    ]

                            Waiting ->
                                image
                                    [ width (px 120)
                                    , height (px 120)
                                    , centerX
                                    , centerY
                                    , inFront (image [ width (px 80), height (px 80), centerX, centerY ] { src = "/img/logo_sans_text.svg", description = "logo" })
                                    ]
                                    { src = "/img/loading.svg", description = "Loading..." }

                            SendError ->
                                link [ height (px 100), htmlAttribute <| class "backgroundGrow", fontSize device Sm, centerX, mouseOver [ Font.color gciBlue ], padding 25 ] { url = shared.address.emailLink, label = paragraph [ Font.center ] [ el [ Font.color warning, fontSize device Md ] (text "Send Failed!"), html <| br [] [], text "Check that your email and phone entries are valid.", html <| br [] [], html <| br [] [], text "If that doesn't work please email us at:", html <| br [] [], text shared.address.email, html <| br [] [], text "Our appologies for the inconvenience." ] }

                            Send ->
                                image
                                    [ width (px 120)
                                    , height (px 120)
                                    , centerX
                                    , centerY
                                    , inFront (image [ width (px 80), height (px 80), centerX, centerY ] { src = "/img/logo_sans_text.svg", description = "logo" })
                                    ]
                                    { src = "/img/loading.svg", description = "Loading..." }

                    _ ->
                        row [] []
                , row [ width fill, padding 15, alignBottom ]
                    (if state.currentPage < 3 || state.send == SendError then
                        [ Input.button
                            [ alignBottom
                            , alignLeft
                            , paddingXY 30 10
                            , rounded 100
                            , Font.color gciBlue
                            , Border.color gciBlue
                            , Border.width 2
                            , mouseOver [ Border.color gciBlueLight, Font.color gciBlueLight ]
                            ]
                            { onPress = Just (message (contactUsBack shared)), label = text "Back" }
                        , if state.send == SendError && state.currentPage == 3 then
                            none

                          else
                            Input.button
                                [ alignBottom
                                , alignRight
                                , paddingXY 30 10
                                , rounded 100
                                , Background.color gciBlue
                                , Font.bold
                                , Font.color white
                                , Border.color gciBlue
                                , mouseOver [ Border.color gciBlueLight, Background.color gciBlueLight ]
                                , Border.width 2
                                ]
                                (if state.currentPage == 2 then
                                    { onPress = Just (message (setStateToSend shared)), label = text "Send!" }

                                 else
                                    { onPress = Just (message (contactUsNext shared)), label = text "Next" }
                                )
                        ]

                     else if state.send == Waiting || state.send == Send then
                        [ none ]

                     else
                        [ Input.button
                            [ alignBottom
                            , paddingXY 100 10
                            , rounded 100
                            , centerX
                            , Background.color gciBlue
                            , Font.bold
                            , Font.color white
                            , Border.color gciBlue
                            , mouseOver [ Border.color gciBlueLight, Background.color gciBlueLight ]
                            , Border.width 2
                            ]
                            { onPress = Just (message (setContactUs shared False)), label = text "Close" }
                        ]
                    )
                , paragraph
                    [ alignLeft
                    , Font.center
                    , centerY
                    , centerX
                    , padding 10
                    , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                    ]
                    [ newTabLink [] { url = address.mapsLink, label = text address.street }
                    , break
                    , newTabLink [] { url = address.mapsLink, label = text address.city }
                    , break
                    , link [ paddingXY 10 0 ] { url = address.phoneLink, label = text address.phone }
                    , text "|"
                    , link [ paddingXY 10 0 ] { url = address.emailLink, label = text address.email }
                    ]
                ]
    in
    el
        [ width fill
        , height fill
        , htmlAttribute <| class "point_enter_down_long"
        , behindContent
            (el
                [ width fill
                , height fill
                , Background.gradient
                    { angle = degrees 165
                    , steps = [ rgba255 87 83 78 0.7, rgba255 17 24 39 0.9 ]
                    }
                , Events.onClick (message (setContactUs shared False))
                ]
                none
            )
        ]
        (column
            [ Background.color white
            , width (px (min 600 w))
            , height (px (min h 600))
            , centerX
            , if isPhone then
                alignTop

              else
                centerY
            , Border.shadow { blur = 20, color = rgb 0.25 0.25 0.3, offset = ( 0, 0 ), size = 1 }
            , rounded 25
            , clip
            , inFront
                (row
                    [ padding
                        (if isPhone then
                            0

                         else
                            5
                        )
                    , alignRight
                    ]
                    [ Input.button
                        [ alignRight
                        , Font.family [ Font.typeface "icons" ]
                        , fontSize device Md
                        , pointer
                        , Font.color
                            (if isDesktop || isBigDesktop then
                                white

                             else
                                warning
                            )
                        , mouseOver [ Font.color warning ]
                        ]
                        { onPress = Just (message (setContactUs shared False)), label = text "\u{E800}" }
                    ]
                )
            ]
            [ -- el [height (fillPortion 3), width fill, clip]
              {- }
                 (html <| iframe
                 [ src "https://www.google.com/maps/embed?pb=!1m14!1m8!1m3!1d24841.18006570458!2d-104.8844136!3d38.897742!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x8713502b631d3ad1%3A0x1ba83899ee826bda!2s4815%20List%20Dr%2C%20Colorado%20Springs%2C%20CO%2080919%2C%20USA!5e0!3m2!1sen!2sca!4v1622055792062!5m2!1sen!2sca"
                 , style "border" "0"
                 , style "width" "100%"
                 , style "height" "100%"
                 , attribute "loading" "lazy"
                 , property "allowfullscreen" (Encode.string "")
                 ]
                 []
                 )
              -}
              image
                [ width fill
                , height (fillPortion 3)
                , clip
                ]
                { src = "/img/building2.jpg", description = "Picutre of GCI's building" }
            , el [ width fill, height (fillPortion 5) ] contactDialog
            ]
        )


onEnter : msg -> Element.Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Json.field "key" Json.string
                |> Json.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.succeed msg

                        else
                            Json.fail "Not the enter key"
                    )
            )
        )


footer : Model -> (Model -> b) -> Element b
footer shared message =
    let
        certifications =
            shared.certifications

        address =
            shared.address

        navbtns =
            shared.navHoverTracker

        socials =
            shared.socialMedia

        year =
            shared.currentYear

        device =
            shared.device.class

        isPhone =
            device == Phone

        isDesktop =
            device == Desktop

        isBigDesktop =
            device == BigDesktop

        w =
            shared.width

        footerNavBtn item =
            let
                attr =
                    [ mouseOver [ Font.color gciBlue ]
                    , pointer
                    , centerX
                    , padding 10
                    ]
            in
            case item.onClick of
                Url s ->
                    link attr { url = s, label = text item.name }

                SetContactUs b ->
                    Input.button attr
                        { onPress =
                            Just (message (setContactUs shared True))
                        , label = text item.name
                        }

        footerSocialBtn item =
            newTabLink
                [ Font.family [ Font.typeface "icons" ]
                , mouseOver [ Font.color item.hoverColor ]
                , pointer
                , padding 10
                ]
                { url = item.link, label = text item.char }

        spacer =
            el [ paddingXY 28 10 ] (text "|")

        footerCertification item =
            el
                (if shared.width < 1000 then
                    [ width fill ]

                 else
                    [ width shrink, centerX ]
                )
                (image [ height (px 150), centerX ] { src = item.src, description = item.description })
    in
    el
        [ height fill
        , width fill
        , Region.footer
        , Background.color (rgb255 70 70 72)
        , Border.color gciBlue
        , Border.widthEach { top = 8, bottom = 0, left = 0, right = 0 }
        ]
        (column
            [ Font.color white, centerX, width (fill |> minimum shared.width), clip ]
            [ wrappedRow [ padding 20, spacing 40, centerX, width (minimum shared.width fill) ]
                (List.map footerCertification certifications)
            , if isDesktop || isBigDesktop then
                row [ Font.bold, fontSize device Xsm, centerX ]
                    (List.map footerNavBtn navbtns ++ spacer :: List.map footerSocialBtn socials)

              else
                column [ width fill, Font.bold, fontSize device Sm, spacing 10 ]
                    [ wrappedRow [ width (minimum w fill) ] (List.map (\btn -> el [ width fill ] (footerNavBtn btn)) navbtns)
                    , row [ centerX, fontSize device Md ] (List.map footerSocialBtn socials)
                    ]
            , column [ width fill, Border.widthEach { top = 1, bottom = 1, left = 0, right = 0 }, padding 10 ]
                [ wrappedRow [ centerX, width shrink, fontSize device Xsm ]
                    [ newTabLink [ width fill ] { url = address.mapsLink, label = el [ padding 10, centerX ] (text address.street) }
                    , newTabLink [ width fill ] { url = address.mapsLink, label = el [ padding 10, centerX ] (text address.city) }
                    , el [ width fill ] (link [ padding 10, centerX ] { label = text address.phone, url = address.phoneLink })
                    , el [ width fill ] (link [ padding 10, centerX ] { label = text address.email, url = address.emailLink })
                    ]
                , wrappedRow [ centerX, fontSize device Xsm, spacing 20, padding 10 ]
                    [ el [ fontSize device Xsm ] (text "Cage: 7DGP6")
                    , el [ fontSize device Xsm ] (text "Duns: 80126549")
                    , ( case shared.user of
                       Nothing ->
                            el [ fontSize device Xsm, inFront (el [ centerX, alignBottom, transparent True, mouseOver [ transparent False ], htmlAttribute <| class "g-signin2", htmlAttribute <| attribute "data-onsuccess" "onSignIn" ] none) ] (text "Login")
                       Just _ ->
                            Input.button [ fontSize device Xsm, mouseOver [Font.color gciBlue] ] {label = (text "Sign Out"), onPress = Just (message (clearUser shared)) }
                     )
                    ]
                ]
            , column [ fontSize device Xsm, paddingXY 200 20, centerX, spacing 10 ]
                [ (if isPhone then
                    column

                   else
                    wrappedRow
                  )
                    [ spacing 15, width fill ]
                    [ el [ padding 2, width fill ] (el [ centerX ] (text ("©" ++ String.fromInt year ++ " Global Circuit Innovations, Inc.")))
                    , el [ padding 2, width fill, mouseOver [ Font.color gciBlue ] ] (download [ centerX ] { url = "/sitemap.xml", label = text "Sitemap" })
                    , el [ padding 2, width fill, mouseOver [ Font.color gciBlue ] ] (link [ centerX ] { url = "/terms", label = text "Terms and Conditions" })
                    , el [ padding 2, width fill, mouseOver [ Font.color gciBlue ] ]
                        (download [ mouseOver [ Font.color gciBlue ], pointer, centerX ]
                            { url = "/download/press.zip"
                            , label = text "Press Materials"
                            }
                        )
                    ]
                , newTabLink
                    [ padding 5
                    , centerX
                    , mouseOver [ Font.color (rgb255 144 0 255) ]
                    ]
                    { url = "https://regaltechsupport.com"
                    , label =
                        row
                            [ spacing 5
                            , paddingXY 5 5
                            ]
                            [ image [ width (px 20) ] { src = "https://regaltechsupport.com/img/favicon.ico", description = "Regal Tech Support, LLC Logo" }
                            , text "Website made by Regal Tech Support"
                            ]
                    }
                ]
            ]
        )


classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice window =
    -- Tested in this ellie:
    -- https://ellie-app.com/68QM7wLW8b9a1
    { class =
        let
            width =
                window.width

            longSide =
                max window.width window.height

            shortSide =
                min window.width window.height
        in
        if width <= 600 then
            Phone

        else if width > 600 && width <= 1200 then
            Tablet

        else if width > 1200 && width <= 1920 then
            Desktop

        else
            BigDesktop
    , orientation =
        if window.width < window.height then
            Portrait

        else
            Landscape
    }


setPhoneCursor : String -> String -> Cmd msg
setPhoneCursor oldPhone newPhone =
    let
        parse val =
            String.toList (String.filter isDigit (String.replace "+1" "" val))

        index a =
            a |> Tuple.first

        one a =
            a |> Tuple.second |> Tuple.first

        two a =
            a |> Tuple.second |> Tuple.second
    in
    -- creates List (index, (oldDigit, newDigit)) and filters for first change returning that number
    -- the first difference is what matters, so we just take the head and return the modified index
    case
        List.head
            (List.filterMap
                (\a ->
                    if not (one a == two a) then
                        Just (index a)

                    else
                        Nothing
                )
                (List.indexedMap Tuple.pair (List.map2 Tuple.pair (parse oldPhone) (parse newPhone)))
            )
    of
        Just i ->
            if String.length oldPhone > String.length newPhone then
                setCursor
                    (case i of
                        0 ->
                            4

                        1 ->
                            5

                        2 ->
                            6

                        3 ->
                            10

                        4 ->
                            11

                        5 ->
                            12

                        n ->
                            n + 10
                    )

            else
                setCursor
                    (case i of
                        0 ->
                            5

                        1 ->
                            6

                        2 ->
                            10

                        3 ->
                            11

                        4 ->
                            12

                        n ->
                            n + 11
                    )

        Nothing ->
            Cmd.none


setHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setHovered id i data =
    if id == i then
        { data | hovered = True }

    else
        { data | hovered = False }


validUSNumber : String -> Bool
validUSNumber number =
    if number == "" then
        True

    else
        PhoneNumber.valid
            { defaultCountry = countryUS
            , otherCountries = []
            , types = PhoneNumber.anyType
            }
            number


prettyPhoneNumber : String -> String
prettyPhoneNumber number =
    let
        clean =
            String.filter isDigit (String.replace "+1" "" number)
    in
    case String.length clean of
        0 ->
            "+1 ("

        1 ->
            "+1 (" ++ clean

        2 ->
            "+1 (" ++ clean

        3 ->
            "+1 (" ++ clean ++ ")  "

        4 ->
            "+1 (" ++ String.left 3 clean ++ ")  " ++ String.right 1 clean

        5 ->
            "+1 (" ++ String.left 3 clean ++ ")  " ++ String.right 2 clean

        _ ->
            "+1 (" ++ String.left 3 clean ++ ")  " ++ String.slice 3 6 clean ++ " - " ++ String.slice 6 10 clean


setContactUs : Model -> Bool -> Model
setContactUs model b =
    if not b && (model.contactDialogState.send == SendError || model.contactDialogState.send == SendOk) then
        { model | contactDialogState = model.contactDialogState |> (\c -> { c | showContactUs = b, name = "", email = Nothing, phone = Nothing, message = Nothing, currentPage = 0, send = Waiting }) }

    else
        { model | contactDialogState = model.contactDialogState |> (\c -> { c | showContactUs = b }) }


toggleMobileNav : Model -> Model
toggleMobileNav model =
    { model | showMobileNav = not model.showMobileNav }


contactUsBack : Model -> Model
contactUsBack model =
    if model.contactDialogState.currentPage == 0 then
        { model | navHoverTracker = List.map (\b -> { b | hovered = False }) model.navHoverTracker, contactDialogState = model.contactDialogState |> (\c -> { c | showContactUs = False }) }

    else
        { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage - 1 }) }


contactEmail : Model -> String -> Model
contactEmail model newEmail =
    if model.contactDialogState.emailError then
        { model
            | contactDialogState =
                model.contactDialogState
                    |> (\s ->
                            { s
                                | email = Just (String.trim newEmail)
                                , emailError = not (Email.isValid newEmail)
                            }
                       )
        }

    else
        { model | contactDialogState = model.contactDialogState |> (\s -> { s | email = Just (String.trim newEmail) }) }


contactMsg : Model -> String -> Model
contactMsg model newMessage =
    if model.contactDialogState.messageError then
        { model | contactDialogState = model.contactDialogState |> (\s -> { s | message = Just newMessage, messageError = newMessage == "" }) }

    else
        { model | contactDialogState = model.contactDialogState |> (\s -> { s | message = Just newMessage }) }


contactName : Model -> String -> Model
contactName model newName =
    if model.contactDialogState.nameError then
        { model | contactDialogState = model.contactDialogState |> (\s -> { s | name = newName, nameError = newName == "" }) }

    else
        { model | contactDialogState = model.contactDialogState |> (\s -> { s | name = newName }) }


contactUsNext : Model -> Model
contactUsNext model =
    case model.contactDialogState.currentPage of
        0 ->
            if not (String.trim model.contactDialogState.name == "") then
                { model
                    | contactDialogState =
                        model.contactDialogState
                            |> (\s ->
                                    { s
                                        | currentPage = s.currentPage + 1
                                        , nameError = False
                                        , name = String.trim s.name
                                    }
                               )
                }

            else
                { model | contactDialogState = model.contactDialogState |> (\s -> { s | nameError = True, name = s.name }) }

        1 ->
            if Email.isValid (Maybe.withDefault "" model.contactDialogState.email) && validUSNumber (String.right 10 (String.filter isDigit (Maybe.withDefault "" model.contactDialogState.phone))) then
                { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, phoneError = False, emailError = False }) }

            else if not (Email.isValid (Maybe.withDefault "" model.contactDialogState.email)) then
                { model | contactDialogState = model.contactDialogState |> (\s -> { s | emailError = True, phoneError = False }) }

            else if not (validUSNumber (String.right 10 (String.filter isDigit (Maybe.withDefault "" model.contactDialogState.phone)))) then
                { model | contactDialogState = model.contactDialogState |> (\s -> { s | phoneError = True }) }

            else
                { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, phoneError = False, emailError = False }) }

        _ ->
            { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1 }) }


setStateToSend : Model -> Model
setStateToSend model =
    case model.contactDialogState.currentPage of
        2 ->
            if Maybe.withDefault "" model.contactDialogState.message == "" then
                { model | contactDialogState = model.contactDialogState |> (\s -> { s | messageError = True }) }

            else
                { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, messageError = False, send = Send }) }

        _ ->
            { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1 }) }


navBtnHover : Model -> Int -> Model
navBtnHover model id =
    { model | navHoverTracker = List.indexedMap (setHovered id) model.navHoverTracker }


navBtnUnHover : Model -> Int -> Model
navBtnUnHover model id =
    { model | navHoverTracker = List.map (\i -> { i | hovered = False }) model.navHoverTracker }


contactPhone : Model -> String -> Model
contactPhone model newPhone =
    if model.contactDialogState.phoneError then
        { model
            | contactDialogState =
                model.contactDialogState
                    |> (\s ->
                            { s
                                | phone =
                                    Just
                                        (if newPhone == "+1 ( " then
                                            ""

                                         else if String.length newPhone < String.length (Maybe.withDefault newPhone s.phone) then
                                            newPhone

                                         else
                                            prettyPhoneNumber newPhone
                                        )
                                , phoneError = not (validUSNumber (String.right 10 (String.filter isDigit (prettyPhoneNumber newPhone))))
                            }
                       )
        }

    else
        { model
            | contactDialogState =
                model.contactDialogState
                    |> (\s ->
                            { s
                                | phone =
                                    Just
                                        (if newPhone == "+1 ( " then
                                            ""

                                         else if String.length newPhone < String.length (Maybe.withDefault newPhone s.phone) then
                                            newPhone

                                         else
                                            prettyPhoneNumber newPhone
                                        )
                            }
                       )
        }


reset : Model -> Model
reset model =
    { model | navbarDisplay = Enter, showMobileNav = False, scrolledDistance = 0 }

clearUser : Model -> Model
clearUser model =
    { model | user = Nothing }
