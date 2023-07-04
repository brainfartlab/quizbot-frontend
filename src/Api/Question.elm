module Api.Question exposing
    ( Question, Option, Feedback, QuestionSummary(..)
    , list, get, ask, answer
    )

import Effect exposing (Effect)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required, resolve)
import Json.Encode

import Api exposing (Error, handleHttpResponse)
import Api.Game exposing (Game)


-- MODELS


type alias Question =
    { prompt : String
    , options : List Option
    }

type alias Option
    = String

type alias QuestionStatus =
    { prompt : String
    , is_answered : Bool
    }

type QuestionSummary
    = Answered
        { prompt : String
        , solution : String
        , result : Bool
        }
    | Pending
        { prompt : String
        }

type alias Feedback =
    { result : Bool
    , solution : Int
    , clarification : String
    }


-- METHODS


list :
    { onResponse : Result Error (List QuestionSummary) -> msg
    , backendUri : String
    , token : String
    , gameId : String
    }
    -> Effect msg
list options =
    let
        url : String
        url = options.backendUri ++ "/quiz/v1/games/" ++ options.gameId ++ "/questions"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "GET"
                , headers =
                    [ authHeader options.token ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse options.onResponse (handleHttpResponse questionsSummaryDecoder)
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd

get :
    { onResponse : Result Error QuestionStatus -> msg
    , backendUri : String
    , token : String
    , gameId : String
    , id : String
    }
    -> Effect msg
get options =
    let
        url : String
        url = options.backendUri ++ "/quiz/v1/games/" ++ options.gameId ++ "/questions/" ++ options.id

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "GET"
                , headers =
                    [ authHeader options.token ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse options.onResponse (handleHttpResponse questionStatusDecoder)
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


ask:
    { onResponse : Result Error Question -> msg
    , backendUri : String
    , token : String
    , gameId : String
    }
    -> Effect msg
ask options =
    let
        url : String
        url = options.backendUri ++ "/quiz/v1/games/" ++ options.gameId ++ "/questions/ask"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "POST"
                , headers =
                    [ authHeader options.token
                    ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse options.onResponse (handleHttpResponse questionDecoder)
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


answer:
    { onResponse : Result Error Feedback -> msg
    , backendUri : String
    , token : String
    , gameId : String
    , choice : Int
    }
    -> Effect msg
answer options =
    let
        url : String
        url = options.backendUri ++ "/quiz/v1/games/" ++ options.gameId ++ "/questions/answer"

        cmd : Cmd msg
        cmd =
            Http.request
                { url = url
                , method = "POST"
                , headers =
                    [ authHeader options.token
                    ]
                , body = Http.jsonBody ( answerEncoder { choice = options.choice } )
                , expect = Http.expectStringResponse options.onResponse (handleHttpResponse feedbackDecoder)
                , timeout = Just 10000
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


authHeader : String -> Http.Header
authHeader token =
    Http.header "Authorization" ("Bearer " ++ token)


-- DECODERS


questionDecoder : Decode.Decoder Question
questionDecoder =
     Decode.succeed Question
        |> required "prompt" Decode.string
        |> required "options" ( Decode.list Decode.string)

questionSummaryDecoder : Decode.Decoder QuestionSummary
questionSummaryDecoder =
    let
        toDecoder : String -> Maybe String -> Maybe Bool -> Decoder QuestionSummary
        toDecoder prompt maybeSolution maybeResult =
            case maybeResult of
                Nothing ->
                    Decode.succeed
                        ( Pending
                            { prompt = prompt
                            }
                        )

                Just result ->
                    Decode.succeed
                        ( Answered
                            { prompt = prompt
                            , solution = Maybe.withDefault "" maybeSolution
                            , result = result
                            }
                        )
    in
     Decode.succeed toDecoder
        |> required "prompt" Decode.string
        |> required "solution" (Decode.nullable Decode.string)
        |> required "result" (Decode.nullable Decode.bool)
        |> resolve

questionsSummaryDecoder : Decode.Decoder (List QuestionSummary)
questionsSummaryDecoder =
     Decode.field "questions" ( Decode.list questionSummaryDecoder)

questionStatusDecoder : Decode.Decoder QuestionStatus
questionStatusDecoder =
     Decode.succeed QuestionStatus
        |> required "prompt" Decode.string
        |> required "is_answered" Decode.bool

questionsDecoder : Decode.Decoder (List QuestionStatus)
questionsDecoder =
     Decode.field "questions" ( Decode.list questionStatusDecoder)

feedbackDecoder : Decode.Decoder Feedback
feedbackDecoder =
     Decode.succeed Feedback
        |> required "result" Decode.bool
        |> required "solution" Decode.int
        |> required "clarification" Decode.string

answerEncoder : { choice : Int } -> Json.Encode.Value
answerEncoder { choice } =
    Json.Encode.object
        [ ( "choice", Json.Encode.int choice )
        ]
