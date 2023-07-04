module Pages.SignIn exposing (Model, Msg, page)

import Dict
import Colors.Alpha exposing (lightblue)
import Effect exposing (Effect)
import Element exposing (Element, column, el)
import Element.Font as Font
import Element.Input as Input
import Route exposing (Route)
import Html.Events
import Page exposing (Page)
import Shared
import View exposing (View)

import Auth.Authentication


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update route
        , subscriptions = subscriptions
        , view = view shared
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
    = SigningIn
    | SigningOut


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        SigningIn ->
            ( model
            , Effect.signIn (Dict.get "from" route.query)
            )

        SigningOut ->
            ( model
            , Effect.signOut
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Pages.SignIn"
    , attributes = []
    , element = el
        [ Element.centerX
        , Element.centerY
        ]
        (viewPage model)
    }

viewPage : Model -> Element Msg
viewPage model =
    column []
        [ Input.button []
            { onPress = Just SigningIn
            , label = el
                [ Font.bold
                , Font.size 72
                , Font.color (lightblue 1)
                ]
                (Element.text "Sign In")
            }
        ]
