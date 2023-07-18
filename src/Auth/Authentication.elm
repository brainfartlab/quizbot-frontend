module Auth.Authentication exposing
  ( Msg(..)
  , Model
  , init
  , update
  , handleAuthResult
  , tryGetUserProfile
  , isLoggedIn
  )

import Auth.Auth0 as Auth0
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)

type alias Model =
  { state : Auth0.AuthenticationState
  , lastError : Maybe Auth0.AuthenticationError
  , authorize : Auth0.Options -> Cmd Msg
  , logOut : () -> Cmd Msg
  }

init : (Auth0.Options -> Cmd Msg) -> (() -> Cmd Msg) -> Maybe Auth0.LoggedInUser -> Model
init authorize logOut initialData =
  { state =
      case initialData of
        Just user ->
          Auth0.LoggedIn user

        Nothing ->
          Auth0.LoggedOut

  , lastError = Nothing
  , authorize = authorize
  , logOut = logOut
  }

type Msg
  = AuthenticationResult Auth0.AuthenticationResult
  | LogIn RedirectPath
  | LogOut

type alias RedirectPath
    = Maybe String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AuthenticationResult result ->
      let ( newState, error ) =
            case result of 
              Ok user ->
                ( Auth0.LoggedIn user, Nothing )

              Err err ->
                ( model.state, Just err )

      in
        ( { model | state = newState, lastError = error }, Cmd.none )

    LogIn (Just redirectPath) ->
      let
          options : Auth0.Options
          options =
              { redirectPath = redirectPath
              }
      in
      ( model, model.authorize options )

    LogIn Nothing ->
      ( model, model.authorize Auth0.defaultOpts)

    LogOut ->
      ( { model | state = Auth0.LoggedOut }, model.logOut () )

userDecoder : Json.Decode.Decoder Auth0.LoggedInUser
userDecoder =
    Json.Decode.succeed Auth0.LoggedInUser
        |> required "profile" profileDecoder
        |> required "token" Json.Decode.string


profileDecoder : Json.Decode.Decoder Auth0.UserProfile
profileDecoder =
    Json.Decode.succeed Auth0.UserProfile
        |> required "email" Json.Decode.string
        -- |> optional "email_verified" (Json.Decode.map Just Json.Decode.bool) Nothing
        |> required "picture" Json.Decode.string

rawDecoder : Json.Decode.Decoder Auth0.RawAuthenticationResult
rawDecoder =
    Json.Decode.succeed Auth0.RawAuthenticationResult
        |> optional "err" (Json.Decode.map Just errDecoder) Nothing
        |> optional "ok" (Json.Decode.map Just userDecoder) Nothing

errDecoder : Json.Decode.Decoder Auth0.AuthenticationError
errDecoder =
    Json.Decode.succeed Auth0.AuthenticationError
        |> optional "name" (Json.Decode.map Just Json.Decode.string) Nothing
        |> optional "code" (Json.Decode.map Just Json.Decode.string) Nothing
        |> required "description" Json.Decode.string
        |> optional "statusCode" (Json.Decode.map Just Json.Decode.int) Nothing

handleAuthResult : Auth0.RawAuthenticationResult -> Msg
handleAuthResult =
  Auth0.mapResult >> AuthenticationResult

tryGetUserProfile : Model -> Maybe Auth0.UserProfile
tryGetUserProfile model =
  case model.state of
    Auth0.LoggedIn user ->
      Just user.profile

    Auth0.LoggedOut ->
      Nothing

isLoggedIn : Model -> Bool
isLoggedIn model =
  case model.state of
    Auth0.LoggedIn _ ->
      True

    Auth0.LoggedOut ->
      False
