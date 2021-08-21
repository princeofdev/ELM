module Pages.Newsroom exposing (Model, Msg, page)

import Browser.Dom exposing (Viewport)
import Browser.Events
import Browser.Navigation as Nav
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), DatePicker, defaultSettings)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font exposing (center)
import Element.Input as Input
import Element.Region as Region
import File exposing (File)
import File.Select as Select
import Gen.Params.Newsroom exposing (Params)
import Html exposing (br)
import Html.Attributes exposing (class, classList, id)
import Http exposing (Error(..), expectJson)
import Json.Decode as Json exposing (Decoder)
import Json.Encode as Encode
import Page
import Pages.Home_ exposing (AnimationState, When(..), onScreenItemtoCmd, updateElement)
import Palette exposing (FontSize(..), black, fontSize, gciBlue, gciBlueLight, green500, maxWidth, warning, white)
import Ports exposing (google, idLoaded, recvScroll)
import Process
import Request
import Shared exposing (FormResponse, acol, ael, contactUs, footer, navbar, reset)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage exposing (NavBarDisplay(..), SendState(..))
import Swiper exposing (SwipingState)
import Task
import View exposing (View)


serverUrl =
    ""


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    let
        linkedPost =
            req.url.query |> Maybe.andThen String.toInt
    in
    Page.advanced
        { init = init shared linkedPost
        , update = update shared linkedPost
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { localShared : Shared.Model
    , posts : List Posts
    , postRecvError : Bool
    , animationTracker : Dict String AnimationState
    , loadingState : LoadingState
    , postIndex : Int
    , swipingState : SwipingState
    , thumbnails : List String
    , today : Maybe Date
    , alertCopy : Alert
    , confirmDelete : Maybe Int
    }


type LoadingState
    = StartLoading
    | RecvPosts
    | RecvImg
    | LoadingFailed
    | LoadingDone


type alias Post =
    { id : Int
    , title : String
    , editTitle : Maybe String
    , images : List String
    , editImages : List String
    , content : String
    , editContent : Maybe String
    , posttime : String
    , editPosttime : Maybe DatePicker
    , date : Maybe Date
    , viewNum : Int
    , state : PostState
    }


type PostState
    = Idle
    | Editing


type alias Posts =
    { posts : List Post
    , show : Bool
    }


type Alert
    = Good String
    | Bad String
    | None


init : Shared.Model -> Maybe Int -> ( Model, Effect Msg )
init shared linkedPost =
    ( { localShared = reset shared
      , posts = []
      , postRecvError = False
      , animationTracker =
            Dict.fromList []
      , loadingState = StartLoading
      , postIndex = 0
      , swipingState = Swiper.initialSwipingState
      , thumbnails = []
      , today = Nothing
      , alertCopy = None
      , confirmDelete = Nothing
      }
    , Effect.batch
        [ Http.post
            { url =
                serverUrl
                    ++ (case linkedPost of
                            Just id ->
                                "/newsroom/posts?linked_post=" ++ String.fromInt id

                            Nothing ->
                                "/newsroom/posts?i=0&range=3"
                       )
            , body = Http.emptyBody
            , expect =
                Http.expectJson GotPosts
                    (Json.list
                        (Json.map5 (\id title images content posttime -> Post id title Nothing images [] content Nothing posttime Nothing Nothing 0 Idle)
                            (Json.field "id" Json.int)
                            (Json.field "title" Json.string)
                            (Json.field "images" (Json.list Json.string))
                            (Json.field "content" Json.string)
                            (Json.field "posttime" Json.string)
                        )
                    )
            }
            |> Effect.fromCmd
        , Date.today |> Task.perform GotDate |> Effect.fromCmd
        ]
    )



-- UPDATE


type Msg
    = Scrolled Int
    | ModifyLocalShared Shared.Model
    | WindowResized Int Int
    | OpenContactUs
    | Submited (Result Http.Error FormResponse)
    | GotPosts (Result Http.Error (List Post))
    | GotElement String (Result Browser.Dom.Error Browser.Dom.Element)
    | IdLoaded String
    | IdFailed String
    | MoveLeft Int
    | MoveRight Int
    | ImageSwiped Int Swiper.SwipeEvent
    | Google String
    | GetImages
    | GotImages (Result Http.Error (List String))
    | Edit Int
    | Add Int String
    | Subtract Int String
    | TitleChanged Int String
    | ContentChanged Int String
    | Cancel
    | SetDatePicker Int DatePicker.Msg
    | GetUpload
    | GotUpload File
    | PublishPost Int
    | Reload (Result Http.Error ())
    | New
    | GotDate Date
    | Delete Int
    | CopyText String
    | AlertCopy Bool
    | ClearCopy ()
    | CancelDelete
    | AskDelete Int


update : Shared.Model -> Maybe Int -> Msg -> Model -> ( Model, Effect Msg )
update shared linkedPost msg model =
    let
        getThumbnails =
            Http.request
                { method = "POST"
                , headers = [ Http.header "idToken" (Maybe.withDefault "" model.localShared.user) ]
                , url = serverUrl ++ "/newsroom/getimages"
                , body = Http.emptyBody
                , expect =
                    Http.expectJson GotImages
                        (Json.list Json.string)
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Effect.fromCmd
    in
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
                    { model | localShared = modifyNavbarDisplay Hide, animationTracker = Dict.update "spinner" (Maybe.map (\_ -> AnimationState (PercentOfViewport 1) False)) model.animationTracker }

                else
                    { model | localShared = modifyNavbarDisplay Enter }

              else
                model
            , Effect.batch
                ((if shouldAnimate "spinner" model && not (model.loadingState == LoadingDone) then
                    Http.post
                        { url =
                            serverUrl
                                ++ "/newsroom/posts?i="
                                ++ String.fromInt
                                    (case linkedPost of
                                        Just id ->
                                            if model.postIndex == 1 then
                                                0

                                            else
                                                model.postIndex

                                        Nothing ->
                                            model.postIndex
                                    )
                                ++ "&range=3"
                        , body = Http.emptyBody
                        , expect =
                            Http.expectJson GotPosts
                                (Json.list
                                    (Json.map5 (\id title images content posttime -> Post id title Nothing images [] content Nothing posttime Nothing Nothing 0 Idle)
                                        (Json.field "id" Json.int)
                                        (Json.field "title" Json.string)
                                        (Json.field "images" (Json.list Json.string))
                                        (Json.field "content" Json.string)
                                        (Json.field "posttime" Json.string)
                                    )
                                )
                        }
                        |> Effect.fromCmd

                  else
                    Effect.none
                 )
                    :: List.map animationTrackerToCmd (List.filter (\( _, v ) -> v.shouldAnimate == False) (Dict.toList model.animationTracker))
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

        GotPosts response ->
            let
                postFilter newPosts =
                    List.filter (\newPost -> not (List.member newPost (List.foldl (\a b -> b ++ a.posts) [] model.posts))) newPosts
            in
            case response of
                Ok newPosts ->
                    if List.isEmpty newPosts then
                        ( { model | loadingState = LoadingDone }, Effect.none )

                    else
                        postFilter newPosts
                            |> List.head
                            |> Maybe.map .images
                            |> Maybe.andThen List.head
                            |> (\maybeId ->
                                    case maybeId of
                                        Just id ->
                                            -- If the lead post of the section has at least one image we want to wait until at least that post has its image loaded
                                            ( { model
                                                | posts = model.posts ++ [ Posts (postFilter newPosts) False ]
                                                , postIndex = model.postIndex + List.length (postFilter newPosts)
                                                , loadingState = RecvPosts
                                              }
                                            , Ports.waitForId id |> Effect.fromCmd
                                            )

                                        Nothing ->
                                            -- head post has no image, so we are just going to show posts immediately.
                                            ( { model
                                                | posts = model.posts ++ [ Posts (postFilter newPosts) True ]
                                                , loadingState = RecvImg
                                                , postIndex = model.postIndex + List.length (postFilter newPosts)
                                                , animationTracker =
                                                    if Dict.isEmpty model.animationTracker then
                                                        Dict.fromList
                                                            (( "spinner", AnimationState (PercentOfViewport 1) False )
                                                                :: List.indexedMap
                                                                    (\i p ->
                                                                        ( String.fromInt p.id
                                                                        , AnimationState (PercentOfViewport 20)
                                                                            (if i == 0 then
                                                                                True

                                                                             else
                                                                                False
                                                                            )
                                                                        )
                                                                    )
                                                                    ((model.posts ++ [ Posts (postFilter newPosts) False ]) |> List.foldl (\a b -> b ++ a.posts) [])
                                                            )

                                                    else
                                                        Dict.union model.animationTracker
                                                            ((model.posts ++ [ Posts (postFilter newPosts) False ])
                                                                |> List.filter (\p -> not p.show)
                                                                |> List.foldl (\a b -> b ++ a.posts) []
                                                                |> List.indexedMap
                                                                    (\i p ->
                                                                        ( String.fromInt p.id
                                                                        , AnimationState (PercentOfViewport 20)
                                                                            (if i == 0 then
                                                                                True

                                                                             else
                                                                                False
                                                                            )
                                                                        )
                                                                    )
                                                                |> Dict.fromList
                                                            )
                                              }
                                            , Effect.none
                                            )
                               )

                Err _ ->
                    ( { model | postRecvError = True, loadingState = LoadingFailed }, Effect.none )

        IdLoaded _ ->
            ( { model
                | posts = model.posts |> List.map (\p -> { p | show = True })
                , loadingState = RecvImg
                , animationTracker =
                    if Dict.isEmpty model.animationTracker then
                        Dict.fromList
                            (( "spinner", AnimationState (PercentOfViewport 1) False )
                                :: List.indexedMap
                                    (\i p ->
                                        ( String.fromInt p.id
                                        , AnimationState (PercentOfViewport 20)
                                            (if i == 0 then
                                                True

                                             else
                                                False
                                            )
                                        )
                                    )
                                    (model.posts |> List.foldl (\a b -> b ++ a.posts) [])
                            )

                    else
                        Dict.union model.animationTracker
                            (model.posts
                                |> List.filter (\p -> not p.show)
                                |> List.foldl (\a b -> b ++ a.posts) []
                                |> List.indexedMap
                                    (\i p ->
                                        ( String.fromInt p.id
                                        , AnimationState (PercentOfViewport 20)
                                            (if i == 0 then
                                                True

                                             else
                                                False
                                            )
                                        )
                                    )
                                |> Dict.fromList
                            )
              }
            , Effect.none
            )

        IdFailed id ->
            ( model, Ports.waitForId id |> Effect.fromCmd )

        GotElement id element ->
            case element of
                Ok e ->
                    ( { model | animationTracker = Dict.fromList (List.map (updateElement id e) (Dict.toList model.animationTracker)) }, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        MoveLeft id ->
            ( { model
                | posts =
                    List.map
                        (\ps ->
                            { ps
                                | posts =
                                    List.map
                                        (\p ->
                                            if not (p.viewNum == 0) && p.id == id then
                                                { p | viewNum = p.viewNum - 1 }

                                            else
                                                p
                                        )
                                        ps.posts
                            }
                        )
                        model.posts
              }
            , Effect.none
            )

        MoveRight id ->
            ( { model
                | posts =
                    List.map
                        (\ps ->
                            { ps
                                | posts =
                                    List.map
                                        (\p ->
                                            if not (p.viewNum > (List.length p.images - 2)) && p.id == id then
                                                { p | viewNum = p.viewNum + 1 }

                                            else
                                                p
                                        )
                                        ps.posts
                            }
                        )
                        model.posts
              }
            , Effect.none
            )

        ImageSwiped id event ->
            let
                test fn =
                    Tuple.second (fn event model.swipingState)
            in
            if test Swiper.hasSwipedLeft then
                ( { model
                    | posts =
                        List.map
                            (\ps ->
                                { ps
                                    | posts =
                                        List.map
                                            (\p ->
                                                if not (p.viewNum == 0) && p.id == id then
                                                    { p | viewNum = p.viewNum - 1 }

                                                else
                                                    p
                                            )
                                            ps.posts
                                }
                            )
                            model.posts
                    , swipingState = Tuple.first (Swiper.hasSwipedLeft event model.swipingState)
                  }
                , Effect.none
                )

            else if test Swiper.hasSwipedRight then
                ( { model
                    | posts =
                        List.map
                            (\ps ->
                                { ps
                                    | posts =
                                        List.map
                                            (\p ->
                                                if not (p.viewNum > (List.length p.images - 2)) && p.id == id then
                                                    { p | viewNum = p.viewNum + 1 }

                                                else
                                                    p
                                            )
                                            ps.posts
                                }
                            )
                            model.posts
                    , swipingState = Tuple.first (Swiper.hasSwipedRight event model.swipingState)
                  }
                , Effect.none
                )

            else
                ( { model | swipingState = Tuple.first (Swiper.hasSwipedDown event model.swipingState) }, Effect.none )

        Google idToken ->
            let
                newSharedState =
                    model.localShared |> (\l -> { l | user = Just idToken })
            in
            ( { model | localShared = newSharedState }
            , Shared.UpdateModel newSharedState |> Effect.fromShared
            )

        GetImages ->
            ( model
            , getThumbnails
            )

        GotImages result ->
            case result of
                Ok thumbnails ->
                    ( { model | thumbnails = thumbnails }, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        Edit id ->
            let
                ( datePicker, datePickerFx ) =
                    DatePicker.init
            in
            ( { model
                | posts =
                    List.map
                        (\ps ->
                            { ps
                                | posts =
                                    List.map
                                        (\p ->
                                            if p.id == id then
                                                { p
                                                    | state = Editing
                                                    , editTitle = Just p.title
                                                    , editPosttime =
                                                        Just
                                                            (case String.split "T" p.posttime |> List.head |> Maybe.withDefault "" |> Date.fromIsoString of
                                                                Ok date ->
                                                                    DatePicker.initFromDate date

                                                                Err _ ->
                                                                    datePicker
                                                            )
                                                    , date =
                                                        case String.split "T" p.posttime |> List.head |> Maybe.withDefault "" |> Date.fromIsoString of
                                                            Ok date ->
                                                                Just date

                                                            Err _ ->
                                                                Nothing
                                                    , editContent = Just p.content
                                                    , editImages = p.images
                                                }

                                            else
                                                { p | state = Idle }
                                        )
                                        ps.posts
                            }
                        )
                        model.posts
              }
            , getThumbnails
            )

        Cancel ->
            ( { model
                | posts =
                    List.map
                        (\ps ->
                            { ps
                                | posts =
                                    List.map
                                        (\p -> { p | state = Idle, editPosttime = Nothing, date = Nothing })
                                        ps.posts
                            }
                        )
                        model.posts
                        -- remove any "new" posts
                        |> List.filter (\ps -> List.any (\p -> p.id > 0) ps.posts)
              }
            , getThumbnails
            )

        Add id img ->
            ( { model | posts = List.map (\ps -> { ps | posts = List.map (\p -> { p | editImages = p.editImages ++ [ img ] }) ps.posts }) model.posts }, Effect.none )

        Subtract id img ->
            ( { model | posts = List.map (\ps -> { ps | posts = List.map (\p -> { p | editImages = List.filter (\pi -> not (pi == img)) p.editImages }) ps.posts }) model.posts }, Effect.none )

        TitleChanged id newTitle ->
            ( { model | posts = List.map (\ps -> { ps | posts = List.map (\p -> { p | editTitle = Just newTitle }) ps.posts }) model.posts }, Effect.none )

        ContentChanged id newContent ->
            ( { model | posts = List.map (\ps -> { ps | posts = List.map (\p -> { p | editContent = Just newContent }) ps.posts }) model.posts }, Effect.none )

        SetDatePicker id subMsg ->
            let
                picker =
                    model.posts |> List.foldl (\a b -> b ++ a.posts) [] |> List.filter (\p -> p.id == id) |> List.head
            in
            case picker of
                Just picky ->
                    case picky.editPosttime of
                        Just pik ->
                            let
                                ( newDatePicker, dateEvent ) =
                                    DatePicker.update settings subMsg pik

                                date =
                                    case dateEvent of
                                        Picked newDate ->
                                            Just newDate

                                        _ ->
                                            Nothing
                            in
                            ( { model
                                | posts =
                                    List.map
                                        (\ps ->
                                            { ps
                                                | posts =
                                                    List.map
                                                        (\p ->
                                                            if p.id == id then
                                                                { p | date = date, editPosttime = Just newDatePicker }

                                                            else
                                                                p
                                                        )
                                                        ps.posts
                                            }
                                        )
                                        model.posts
                              }
                            , Effect.none
                            )

                        Nothing ->
                            ( model, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        GetUpload ->
            ( model, Select.file [ "image/png", "image/jpg", "image/gif" ] GotUpload |> Effect.fromCmd )

        GotUpload file ->
            ( model
            , Http.request
                { method = "POST"
                , headers = [ Http.header "idToken" (Maybe.withDefault "" model.localShared.user), Http.header "File-Name" (File.name file) ]
                , url = serverUrl ++ "/newsroom/upload/image"
                , body = Http.fileBody file
                , expect =
                    Http.expectJson GotImages
                        (Json.list Json.string)
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Effect.fromCmd
            )

        PublishPost id ->
            let
                publishing =
                    model.posts |> List.foldl (\a b -> b ++ a.posts) [] |> List.filter (\p -> p.id == id) |> List.head

                nullable : Maybe String -> Encode.Value
                nullable a =
                    case a of
                        Nothing ->
                            Encode.null

                        Just str ->
                            Encode.string str
            in
            ( { model
                | posts =
                    List.map
                        (\ps ->
                            { ps
                                | posts =
                                    List.map
                                        (\p -> { p | state = Idle, editPosttime = Nothing, date = Nothing })
                                        ps.posts
                                        |> List.filter (\p -> not (p.id < 0))
                            }
                        )
                        model.posts
              }
            , case publishing of
                Just editedPost ->
                    Http.request
                        { method = "POST"
                        , headers = [ Http.header "idToken" (Maybe.withDefault "" model.localShared.user) ]
                        , url = serverUrl ++ "/newsroom/upload/post"
                        , body =
                            Http.jsonBody <|
                                Encode.object
                                    [ ( "id", Encode.int editedPost.id )
                                    , ( "title", Encode.string (Maybe.withDefault "" editedPost.editTitle) )
                                    , ( "content", Encode.string (Maybe.withDefault "" editedPost.editContent) )
                                    , ( "posttime", nullable (editedPost.date |> Maybe.map (\date -> Date.toIsoString date ++ "T13:00:00")) )
                                    , ( "images", Encode.list Encode.string editedPost.editImages )
                                    ]
                        , expect = Http.expectWhatever Reload
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                        |> Effect.fromCmd

                Nothing ->
                    Effect.none
            )

        Reload _ ->
            ( model, Nav.reload |> Effect.fromCmd )

        New ->
            let
                ( datePicker, datePickerFx ) =
                    DatePicker.init
            in
            ( { model
                | posts = Posts [ Post -1 "" (Just "") [] [] "" (Just "") "" (Maybe.map DatePicker.initFromDate model.today) model.today 0 Editing ] True :: model.posts
                , animationTracker = Dict.insert "-1" (AnimationState Middle True) model.animationTracker
              }
            , getThumbnails
            )

        GotDate date ->
            ( { model | today = Just date }, Effect.none )

        Delete id ->
            ( model
            , Http.request
                { method = "POST"
                , headers = [ Http.header "idToken" (Maybe.withDefault "" model.localShared.user) ]
                , url = serverUrl ++ "/newsroom/delete/post?post_id=" ++ String.fromInt id
                , body = Http.emptyBody
                , expect = Http.expectWhatever Reload
                , timeout = Nothing
                , tracker = Nothing
                }
                |> Effect.fromCmd
            )

        CopyText s ->
            ( model, Ports.copyText s |> Effect.fromCmd )

        AlertCopy b ->
            ( { model
                | alertCopy =
                    if b then
                        Good ""

                    else
                        Bad ""
              }
            , Task.perform ClearCopy (Process.sleep 2000) |> Effect.fromCmd
            )

        ClearCopy _ ->
            ( { model | alertCopy = None }, Effect.none )

        CancelDelete ->
            ( { model | confirmDelete = Nothing }, Effect.none )

        AskDelete id ->
            ( { model | confirmDelete = Just id }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ recvScroll Scrolled
        , Browser.Events.onResize WindowResized
        , idLoaded IdLoaded
        , Ports.idFailed IdFailed
        , google Google
        , Ports.successfulCopy (\c -> AlertCopy c)
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

        isPhone =
            shared.device.class == Phone

        postWidth =
            min 800
                (toFloat w
                    * (if isPhone then
                        0.9

                       else
                        0.8
                      )
                    |> round
                )

        post item =
            let
                editImg =
                    el
                        [ width fill
                        , clip
                        , centerY
                        , htmlAttribute <| id (String.fromInt item.id)
                        , Border.rounded 10
                        , Background.gradient { angle = degrees 45, steps = [ rgb255 17 24 39, rgb255 87 83 78 ] }
                        , height (shrink |> minimum 90)
                        ]
                        (el
                            [ width (px postWidth)
                            , inFront
                                (column [ fontSize device Md, Font.bold, Font.color white, centerY, centerX, spacing 20 ]
                                    [ el [ centerX ] (text "Selected:")
                                    , el [ width (px postWidth), scrollbarX, height (px 150), clipY ] (row [ height (px 100), spacing 20 ] (List.map (\i -> el [ width (px 100), height (px 100), inFront (Input.button [ width fill, height fill ] { label = el [ fontSize device Sm, width fill, height fill, transparent True, htmlAttribute <| class "animateTransformFast", mouseOver [ transparent False ], below (el [ Background.color (rgb255 87 83 78), width fill, Font.center, padding 8, Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 8, bottomRight = 8 }, clip ] (text (i |> String.split "." |> List.head |> Maybe.withDefault ""))) ] (image [ width fill, height fill, Background.color (rgba255 254 202 202 0.8) ] { src = "/img/down.svg", description = "" }), onPress = Just (Subtract item.id i) }) ] (image [ width fill, height (px 100) ] { src = serverUrl ++ "/newsroom/thumbnail/" ++ i, description = i })) item.editImages))
                                    , el [ centerX ] (text "Options:")
                                    , el [ width (px postWidth), scrollbarX, height (px 150), clipY ] (row [ height (px 100), spacing 20 ] (List.map (\i -> el [ width (px 100), height (px 100), inFront (Input.button [ width fill, height fill ] { label = el [ fontSize device Sm, width fill, height fill, transparent True, htmlAttribute <| class "animateTransformFast", mouseOver [ transparent False ], below (el [ Background.color (rgb255 87 83 78), width fill, Font.center, padding 8, Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 8, bottomRight = 8 }, clip ] (text (i |> String.split "." |> List.head |> Maybe.withDefault ""))) ] (image [ width fill, height fill, Background.color (rgba255 217 249 157 0.8) ] { src = "/img/up.svg", description = "" }), onPress = Just (Add item.id i) }) ] (image [ width fill, height (px 100) ] { src = serverUrl ++ "/newsroom/thumbnail/" ++ i, description = i })) (List.filter (\t -> not (List.member t item.editImages)) model.thumbnails)))
                                    , Input.button [ centerX ] { label = el [ Background.color white, Font.color gciBlue, paddingXY 40 10, mouseOver [ Font.color gciBlueLight ], Border.rounded 5 ] (text "Upload"), onPress = Just GetUpload }
                                    ]
                                )
                            ]
                            (image [ transparent True, width fill, height (shrink |> minimum 600) ] { src = serverUrl ++ "/newsroom/images/" ++ (item.images |> List.head |> Maybe.withDefault ""), description = "" })
                        )

                editContent =
                    let
                        date picker =
                            el []
                                (html <|
                                    Html.map (\message -> SetDatePicker item.id message)
                                        (DatePicker.view
                                            item.date
                                            settings
                                            picker
                                        )
                                )

                        save =
                            Input.button [] { label = el [ Background.color (rgb255 77 124 15), Font.color white, paddingXY 20 5, mouseOver [ Background.color (rgb255 101 163 13) ], Border.rounded 5 ] (text "Publish This Post"), onPress = Just (PublishPost item.id) }

                        delete =
                            if item.id > 0 then
                                Input.button [] { label = el [ Background.color warning, Font.color white, paddingXY 20 5, mouseOver [ Background.color (rgb255 224 71 71) ], Border.rounded 5 ] (text "Delete"), onPress = Just (AskDelete item.id) }

                            else
                                none

                        cancel =
                            Input.button [] { label = el [ Background.color gciBlue, Font.color white, paddingXY 20 5, mouseOver [ Background.color gciBlueLight ], Border.rounded 5 ] (text "Cancel"), onPress = Just Cancel }
                    in
                    column [ width fill, spacing 10 ]
                        [ Input.multiline [ Region.heading 3, fontSize device Sm, Border.color gciBlue, Border.rounded 5 ] { onChange = TitleChanged item.id, text = Maybe.withDefault "" item.editTitle, placeholder = Just (Input.placeholder [] (paragraph [] [ text "Title" ])), label = Input.labelHidden "", spellcheck = True }
                        , wrappedRow [ spacing 20 ]
                            [ case item.editPosttime of
                                Just picker ->
                                    date picker

                                Nothing ->
                                    none
                            , save
                            , cancel
                            , delete
                            ]
                        , Input.multiline [ Font.light, fontSize device Sm, Border.color gciBlue, Border.rounded 5, height (shrink |> minimum 150) ] { onChange = ContentChanged item.id, text = Maybe.withDefault "" item.editContent, placeholder = Just (Input.placeholder [] (paragraph [] [ text "What do you want to talk about today?" ])), label = Input.labelHidden "", spellcheck = True }
                        ]

                img =
                    el
                        ([ width fill
                         , clip
                         , centerY
                         , htmlAttribute <| id (String.fromInt item.id)
                         , Border.rounded 10
                         , Background.gradient { angle = degrees 45, steps = [ rgb255 161 161 170, rgb255 82 82 91, gciBlue, gciBlue ] }
                         , height (shrink |> minimum 100)
                         , inFront
                            (el
                                ([ width fill
                                 , height fill
                                 , Border.innerShadow { blur = 18, color = rgba 0 0 0 0.3, offset = ( 1, 8 ), size = 8 }
                                 ]
                                    ++ List.map (\a -> htmlAttribute <| a) (Swiper.onSwipeEvents (ImageSwiped item.id))
                                )
                                none
                            )
                         ]
                            ++ (if List.length item.images > 1 then
                                    [ inFront
                                        (ael
                                            (if item.viewNum == (List.length item.images - 1) then
                                                Animation.fromTo
                                                    { duration = 200
                                                    , options = []
                                                    }
                                                    [ P.opacity 100, P.y 0 ]
                                                    [ P.opacity 0, P.y 10 ]

                                             else
                                                Animation.fromTo
                                                    { duration = 200
                                                    , options = []
                                                    }
                                                    [ P.opacity 0, P.y 10 ]
                                                    [ P.opacity 100, P.y 0 ]
                                            )
                                            [ centerY, Background.color white, padding 5, alignRight, Border.roundEach { topLeft = 5, topRight = 0, bottomLeft = 5, bottomRight = 0 } ]
                                            (Input.button [ centerY ]
                                                { onPress = Just (MoveRight item.id)
                                                , label = image [ width (px 30), height (px 30), centerY, centerX, mouseOver [ moveRight 5 ] ] { src = "/img/right.svg", description = "right button" }
                                                }
                                            )
                                        )
                                    , inFront
                                        (ael
                                            (if item.viewNum == 0 then
                                                Animation.fromTo
                                                    { duration = 200
                                                    , options = []
                                                    }
                                                    [ P.opacity 100, P.y 0 ]
                                                    [ P.opacity 0, P.y 10 ]

                                             else
                                                Animation.fromTo
                                                    { duration = 200
                                                    , options = []
                                                    }
                                                    [ P.opacity 0, P.y 10 ]
                                                    [ P.opacity 100, P.y 0 ]
                                            )
                                            [ centerY, Background.color white, padding 5, Border.roundEach { topLeft = 0, topRight = 5, bottomLeft = 0, bottomRight = 5 } ]
                                            (Input.button [ centerY ]
                                                { onPress = Just (MoveLeft item.id)
                                                , label = image [ width (px 30), height (px 30), centerY, centerX, mouseOver [ moveLeft 5 ] ] { src = "/img/left.svg", description = "right button" }
                                                }
                                            )
                                        )
                                    ]

                                else
                                    []
                               )
                        )
                        (el
                            [ moveLeft ((item.viewNum * postWidth) |> toFloat)
                            , htmlAttribute <| class "animateTransform"
                            ]
                            (List.foldr
                                (\a b ->
                                    image
                                        [ centerY
                                        , width (px postWidth)
                                        , onRight b
                                        , htmlAttribute <| id a
                                        ]
                                        { src = serverUrl ++ "/newsroom/images/" ++ a, description = "" }
                                )
                                none
                                item.images
                            )
                        )

                content =
                    let
                        date =
                            paragraph [ fontSize device Xsm, Font.color (rgb 0.1 0.1 0.13) ] [ item.posttime |> String.split "T" |> List.head |> Maybe.withDefault "" |> String.split "-" |> prettyDate |> text ]

                        edit =
                            Input.button [] { label = el [ Background.color gciBlue, Font.color white, paddingXY 20 5, mouseOver [ Background.color gciBlueLight ], Border.rounded 5 ] (text "Edit"), onPress = Just (Edit item.id) }

                        share =
                            Input.button [] { label = el [ fontSize device Xsm, Font.color (rgb 0.1 0.1 0.13), paddingXY 20 5, mouseOver [ Font.color (rgb 0.5 0.5 0.53) ] ] (text "Share"), onPress = Just (CopyText ("https://gci-global.com/newsroom?" ++ String.fromInt item.id)) }
                    in
                    column [ width fill, spacing 10 ]
                        [ paragraph [ Region.heading 3, Font.extraLight, fontSize device Lg ] [ text item.title ]
                        , case model.localShared.user of
                            Just _ ->
                                wrappedRow [ spacing 20 ]
                                    [ date, share, edit ]

                            Nothing ->
                                wrappedRow [ spacing 10 ]
                                    [ date, share ]
                        , paragraph [ width fill, fontSize device Sm, Font.light ] (List.concat (List.intersperse [ html <| br [] [] ] (item.content |> String.split "\n" |> List.map (\t -> [ text t ]))))
                        ]
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
                [ column
                    [ width fill, spacing 20 ]
                    (case item.state of
                        Editing ->
                            [ editImg, editContent ]

                        Idle ->
                            [ img, content ]
                    )
                ]

        loadingSpinner =
            column
                [ centerX
                , height
                    (if List.any (\p -> p.show) model.posts then
                        shrink

                     else
                        px h
                    )
                , htmlAttribute <| id "spinner"
                ]
                [ image
                    [ width (px 120)
                    , height
                        (px
                            (if model.loadingState == LoadingFailed || model.loadingState == LoadingDone then
                                0

                             else
                                120
                            )
                        )
                    , clip
                    , centerX
                    , inFront (image [ width (px 80), height (px 80), centerX, centerY ] { src = "/img/logo_sans_text.svg", description = "logo" })
                    ]
                    { src = "/img/loading.svg", description = "Loading..." }
                , paragraph [ centerX, Font.center, padding 10 ]
                    [ text
                        (case model.loadingState of
                            StartLoading ->
                                "Loading..."

                            RecvPosts ->
                                "Finishing up..."

                            RecvImg ->
                                ""

                            LoadingFailed ->
                                "Failed. Please check your internet connection and try again."

                            LoadingDone ->
                                "Showing all posts."
                        )
                    ]
                ]

        posts =
            column [ width (px postWidth), centerX ]
                (List.map
                    (\postList ->
                        column
                            (if postList.show then
                                [ width fill, spacing 100, paddingEach { top = 0, bottom = 100, left = 0, right = 0 } ]

                             else
                                [ height (px 0), clip ]
                            )
                            (List.map (\p -> post p) postList.posts)
                    )
                    model.posts
                )
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
        , inFront (copyalert device model.alertCopy)
        , inFront
            (case model.confirmDelete of
                Just id ->
                    deleteConfirm model id

                Nothing ->
                    none
            )
        , clip
        ]
    , element =
        column [ width fill, Region.mainContent ]
            [ column [ width (fill |> maximum (min w maxWidth)), centerX, spacing 25 ]
                [ column
                    [ centerX
                    , width (fill |> maximum (toFloat maxWidth * 0.5 |> round))
                    , spacing 50
                    ]
                    [ el [ height (px 50) ] none
                    , case model.localShared.user of
                        Nothing ->
                            el [ Region.heading 1, Font.extraLight, Font.extraLight, fontSize device Xlg, centerX ] (text "Newsroom")

                        Just _ ->
                            row [ spacing 20, centerX ]
                                (if model.posts |> List.foldl (\a b -> b ++ a.posts) [] |> List.any (\p -> p.id == -1 || p.state == Editing) then
                                    [ el [ Region.heading 1, Font.extraLight, Font.extraLight, fontSize device Xlg, centerX ] (text "Newsroom")
                                    ]

                                 else
                                    [ el [ Region.heading 1, Font.extraLight, Font.extraLight, fontSize device Xlg, centerX ] (text "Newsroom")
                                    , Input.button [] { label = el [ Background.color gciBlue, Font.color white, paddingXY 20 5, mouseOver [ Background.color gciBlueLight ], Border.rounded 5 ] (text "New"), onPress = Just New }
                                    ]
                                )
                    , posts
                    , loadingSpinner
                    ]
                ]
            , footer model.localShared ModifyLocalShared
            ]
    }


shouldAnimate : String -> Model -> Bool
shouldAnimate id model =
    case Dict.get id model.animationTracker of
        Just state ->
            state.shouldAnimate

        Nothing ->
            False


animationTrackerToCmd : ( String, AnimationState ) -> Effect Msg
animationTrackerToCmd ( k, _ ) =
    Task.attempt (GotElement k) (Browser.Dom.getElement k) |> Effect.fromCmd


prettyDate : List String -> String
prettyDate list =
    let
        year =
            List.head list |> Maybe.withDefault ""

        month =
            List.tail list |> Maybe.withDefault [ "", "" ] |> List.head |> Maybe.withDefault ""

        day =
            List.tail list |> Maybe.withDefault [ "", "" ] |> List.tail |> Maybe.withDefault [ "" ] |> List.head |> Maybe.withDefault ""
    in
    (case month of
        "01" ->
            "January"

        "02" ->
            "Febuary"

        "03" ->
            "March"

        "04" ->
            "April"

        "05" ->
            "May"

        "06" ->
            "June"

        "07" ->
            "July"

        "08" ->
            "August"

        "09" ->
            "September"

        "10" ->
            "October"

        "11" ->
            "November"

        "12" ->
            "December"

        _ ->
            ""
    )
        ++ " "
        ++ day
        ++ ", "
        ++ year


settings : DatePicker.Settings
settings =
    { defaultSettings | dateFormatter = \date -> prettyDate (Date.toIsoString date |> String.split "-") }


copyalert : DeviceClass -> Alert -> Element Msg
copyalert device state =
    case state of
        Good _ ->
            el [ htmlAttribute <| class "slide_up_and_out", width fill, paddingXY 0 25, alignBottom, Font.color white, fontSize device Lg, Font.center, Background.color green500 ] (paragraph [] [ text "Link Copied to Clipboard!" ])

        Bad _ ->
            el [ htmlAttribute <| class "slide_up_and_out", width fill, paddingXY 0 25, alignBottom, Font.color white, fontSize device Lg, Font.center, Background.color warning ] (text "Copy Failed.")

        None ->
            Element.none


deleteConfirm : Model -> Int -> Element Msg
deleteConfirm model id =
    let
        device =
            model.localShared.device.class

        w =
            model.localShared.width

        h =
            model.localShared.height

        isPhone =
            device == Phone

        isBigDesktop =
            device == BigDesktop

        isDesktop =
            device == Desktop
    in
    el
        [ width fill
        , height fill
        , behindContent
            (el
                [ width fill
                , height fill
                , Background.gradient
                    { angle = degrees 165
                    , steps = [ rgba255 87 83 78 0.7, rgba255 17 24 39 0.9 ]
                    }
                , Events.onClick CancelDelete
                ]
                none
            )
        ]
        (column
            [ Background.color white
            , width (px (min 600 w))
            , height (px (min h 600))
            , centerX
            , centerY
            , padding 25
            , spacing 50
            , Border.shadow { blur = 20, color = rgb 0.25 0.25 0.3, offset = ( 0, 0 ), size = 1 }
            , Border.rounded 25
            , clip
            ]
            [ el [ centerX, Font.color warning, Font.extraBold, fontSize device Xlg ] (text "DELETE?")
            , paragraph [ Font.center, width (fill |> maximum 400), centerX, fontSize device Md ]
                [ text
                    (case model.posts |> List.foldl (\a b -> b ++ a.posts) [] |> List.filter (\p -> p.id == id) |> List.head |> Maybe.map .title of
                        Just title ->
                            title

                        Nothing ->
                            "Error: Failed to get post title. You probably don't want to delete this, the result is unpredictable."
                    )
                ]
            , Input.button [ centerX, Font.color white, Font.extraBold, fontSize device Xlg, Background.color warning, Border.rounded 6, padding 20, mouseOver [ Background.color (rgb255 224 71 71) ] ] { label = text "DELETE.", onPress = Just (Delete id) }
            , Input.button [ centerX, Font.color white, Font.extraBold, fontSize device Xlg, Background.color gciBlue, Border.rounded 6, padding 20, mouseOver [ Background.color gciBlueLight ] ] { label = text "Cancel!", onPress = Just CancelDelete }
            ]
        )
