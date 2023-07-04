module Api exposing
    ( Data(..), Error(..), ServiceError
    , handleHttpResponse
    , toUserFriendlyMessage
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing (set)
import Json.Decode.Pipeline exposing (optional, required, resolve)

type Data value
    = Loading
    | Success value
    | Failure Error


type Error
    = UnAuthorized
    | Feedback (List ServiceError)


type alias ServiceError =
    { message : String
    , field : Maybe String
    }


handleHttpResponse : Decoder a -> Http.Response String -> Result Error a
handleHttpResponse aDecoder response =
    case response of
        Http.BadUrl_ _ ->
            Err (Feedback
                    [
                        { message = "Unexpected URL format"
                        , field = Nothing
                        }
                    ]
                )

        Http.Timeout_ ->
            Err (Feedback
                    [
                        { message = "Request timed out"
                        , field = Nothing
                        }
                    ]
                )

        Http.NetworkError_ ->
            Err (Feedback
                    [
                        { message = "Unable to connect"
                        , field = Nothing
                        }
                    ]
                )

        Http.BadStatus_ { statusCode } body ->
            case statusCode of
                401 ->
                    Err UnAuthorized

                _ ->
                    Err (Feedback
                            [
                                { message = "Something unexpected happened"
                                , field = Nothing
                                }
                            ]
                        )

        Http.GoodStatus_ _ body ->
            case Decode.decodeString aDecoder body of
                Ok data ->
                    Ok data

                Err _ ->
                    Err (Feedback
                            [
                                { message = "Something unexpected happened"
                                , field = Nothing
                                }
                            ]
                        )


toUserFriendlyMessage : Http.Error -> String
toUserFriendlyMessage httpError =
    case httpError of
        Http.BadUrl _ ->
            -- The URL is malformed, probably caused by a typo
            "This page requested a bad URL"

        Http.Timeout ->
            -- Happens after
            "Request took too long to respond"

        Http.NetworkError ->
            -- Happens if the user is offline or the API isn't online
            "Could not connect to the API"

        Http.BadStatus code ->
            -- Connected to the API, but something went wrong
            case code of
                404 ->
                    "Item not found"

                401 ->
                    "Item not found"

                _ ->
                    "API returned an error code: " ++ String.fromInt code

        Http.BadBody _ ->
            -- Our JSON decoder didn't match what the API sent
            "Unexpected response from API"

-- DECODERS


errorsDecoder : Decoder Error
errorsDecoder =
    let
        toDecoder : List ServiceError -> Decoder Error
        toDecoder serviceErrors =
            Decode.succeed (Feedback serviceErrors)
    in
    Decode.succeed toDecoder
        |> required "errors" (Decode.list errorDecoder)
        |> resolve

errorDecoder : Decoder ServiceError
errorDecoder =
    Decode.succeed ServiceError
        |> required "message" Decode.string
        |> optional "field" (Decode.map Just Decode.string) Nothing
