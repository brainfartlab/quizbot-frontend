module Auth.Auth0 exposing
  ( AuthenticationState (..)
  , AuthenticationError
  , AuthenticationResult
  , RawAuthenticationResult
  , Options
  , defaultOpts
  , LoggedInUser
  , UserProfile
  , Token
  , mapResult
  , userDecoder
  )

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)

type alias LoggedInUser =
  { profile : UserProfile
  , token : Token
  }

type AuthenticationState
  = LoggedOut
  | LoggedIn LoggedInUser

type alias Options =
  { redirectPath : String
  }

type alias UserProfile =
  { email : String
  , email_verified : Maybe Bool
  , picture : String
  }

type alias Token =
  String

type alias AuthenticationError =
  { name : Maybe String
  , code : Maybe String
  , description : String
  , statusCode : Maybe Int
  }

type alias AuthenticationResult =
  Result AuthenticationError LoggedInUser

type alias RawAuthenticationResult =
  { err : Maybe AuthenticationError
  , ok : Maybe LoggedInUser
  }

mapResult : Json.Decode.Value -> AuthenticationResult
mapResult result =
    let
        raw : Result Json.Decode.Error RawAuthenticationResult
        raw =
            Json.Decode.decodeValue rawDecoder result
    in
    case raw of
        Ok rawResult ->
            case (rawResult.err, rawResult.ok) of
                ( Just msg, _ ) ->
                  Err msg

                ( Nothing, Nothing) ->
                  Err { name = Nothing, code = Nothing, statusCode = Nothing, description = "No information was received from the authentication provider" }

                ( Nothing, Just user ) ->
                  Ok user

        Err _ ->
              Err { name = Nothing, code = Nothing, statusCode = Nothing, description = "No information was received from the authentication provider" }

defaultOpts : Options
defaultOpts =
  { redirectPath = "/"
  }


-- DECODERS


userDecoder : Json.Decode.Decoder LoggedInUser
userDecoder =
    Json.Decode.succeed LoggedInUser
        |> required "profile" profileDecoder
        |> required "token" Json.Decode.string


profileDecoder : Json.Decode.Decoder UserProfile
profileDecoder =
    Json.Decode.succeed UserProfile
        |> required "email" Json.Decode.string
        |> optional "email_verified" (Json.Decode.map Just Json.Decode.bool) Nothing
        |> required "picture" Json.Decode.string


rawDecoder : Json.Decode.Decoder RawAuthenticationResult
rawDecoder =
    Json.Decode.succeed RawAuthenticationResult
        |> optional "err" (Json.Decode.map Just errDecoder) Nothing
        |> optional "ok" (Json.Decode.map Just userDecoder) Nothing


errDecoder : Json.Decode.Decoder AuthenticationError
errDecoder =
    Json.Decode.succeed AuthenticationError
        |> optional "name" (Json.Decode.map Just Json.Decode.string) Nothing
        |> optional "code" (Json.Decode.map Just Json.Decode.string) Nothing
        |> required "description" Json.Decode.string
        |> optional "statusCode" (Json.Decode.map Just Json.Decode.int) Nothing
