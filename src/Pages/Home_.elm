module Pages.Home_ exposing (Model, Msg, page)

import Auth
import Colors.Alpha exposing (blue)
import Effect exposing (Effect)
import Route exposing (Route)
import Element exposing (el, link, row, text)
import Element.Font as Font
import Layouts
import Page exposing (Page)
import Route.Path
import Shared
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (toLayout user)


toLayout : Auth.User -> Model -> Layouts.Layout Msg
toLayout user model =
    Layouts.Navigation
        { title = "Home"
        , user = user
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ExampleMsgReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ExampleMsgReplaceMe ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Home_"
    , attributes = []
    , element = el [ Element.centerX, Element.centerY ] <|
        row []
            [ text "Nothing to see here (yet), go to "
            , link []
                { url = Route.Path.toString Route.Path.Game
                , label = el [ Font.bold, Font.underline, Font.color (blue 1) ] (text "My Games")
                }
            ]
    }
