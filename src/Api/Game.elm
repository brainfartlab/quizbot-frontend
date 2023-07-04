module Api.Game exposing
    ( Game
    , list, get, post
    )

import Api exposing (Error, handleHttpResponse)
import Effect exposing (Effect)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing (set)
import Json.Decode.Pipeline exposing (optional, required, resolve)
import Json.Encode as Encode
import Set exposing (Set)
import Time


type alias Game =
    { id : String
    , keywords : Set String
    , questionsCount : Int
    , questionsLimit : Int
    , creationTime : Time.Posix
    }


-- METHODS


list :
    { onResponse : Result Error (List Game) -> msg
    , backendUri : String
    , token : String
    }
    -> Effect msg
list options =
    let
        url : String
        url = options.backendUri ++ "/quiz/v1/games"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "GET"
                , headers =
                    [ authHeader options.token ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse options.onResponse (handleHttpResponse gamesDecoder)
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


get :
    { onResponse : Result Error Game -> msg
    , backendUri : String
    , token : String
    , id : String
    }
    -> Effect msg
get options =
    let
        url : String
        url = options.backendUri ++ "/quiz/v1/games/" ++ options.id

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "GET"
                , headers =
                    [ authHeader options.token ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse options.onResponse (handleHttpResponse gameDecoder)
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


post:
    { onResponse : Result Error Game -> msg
    , backendUri : String
    , token : String
    , keywords : Set String
    }
    -> Effect msg
post options =
    let
        url : String
        url = options.backendUri ++ "/quiz/v1/games"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "POST"
                , headers =
                    [ authHeader options.token
                    ]
                , body = Http.jsonBody ( newGameEncoder { keywords = options.keywords } )
                , expect = Http.expectStringResponse options.onResponse (handleHttpResponse gameDecoder)
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


authHeader : String -> Http.Header
authHeader token =
    Http.header "Authorization" ("Bearer " ++ token)


-- DECODERS


gameDecoder : Decoder Game
gameDecoder =
    let
        toDecoder : String -> Set String -> Int -> Int -> Int -> Decoder Game
        toDecoder id keywords questionsCount questionsLimit creationTime =
            Decode.succeed (Game id keywords questionsCount questionsLimit (Time.millisToPosix creationTime))
    in
    Decode.succeed toDecoder
        |> required "id" Decode.string
        |> required "keywords" (set Decode.string)
        |> required "questions_count" Decode.int
        |> required "questions_limit" Decode.int
        |> required "creation_time" Decode.int
        |> resolve

gamesDecoder : Decoder (List Game)
gamesDecoder =
    Decode.field "games" (Decode.list gameDecoder)

newGameEncoder : { keywords : Set String } -> Encode.Value
newGameEncoder { keywords } =
    Encode.object
        [ ( "keywords", Encode.list Encode.string <| Set.toList keywords)
        ]
