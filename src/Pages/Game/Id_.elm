module Pages.Game.Id_ exposing (Model, Msg, page)

import Auth
import Colors.Alpha exposing (
    black, lightgrey, lightblue, white, crimson, salmon, lawngreen)
import Effect exposing (Effect)
import Element exposing (Element, el, height, row, column, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Route exposing (Route)
import Http
import Layouts
import Page exposing (Page)
import Shared
import View exposing (View)

import Api exposing (Error, ServiceError)
import Api.Game exposing (Game)
import Api.Question exposing (Option, Question, QuestionSummary(..), Feedback)


page : Auth.User -> Shared.Model -> Route { id : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared route
        , update = update user shared
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (toLayout user route.params.id)


toLayout : Auth.User -> String -> Model -> Layouts.Layout
toLayout user id model =
    Layouts.Navigation
        { navigation =
            { title = ("Game " ++ id)
            , user = user
            }
        }


-- INIT


type Model
    = Loading
    | Loaded
        { gameData : Api.Data Game
        }
    | LoadedQuestion
        { game : Game
        , questionData : Api.Data Question
        , choice : Choice
        }
    | LoadedFeedback
        { game : Game
        , question : Question
        , choice : Choice
        , feedbackData : Api.Data Feedback
        }
    | GameOver
        { game : Game
        , summariesData : Api.Data (List QuestionSummary)
        }


type Choice
    = Choice Int
    | NoChoice


init : Auth.User -> Shared.Model -> Route { id : String } -> () -> ( Model, Effect Msg )
init user shared route () =
    ( Loading
    , Api.Game.get
        { onResponse = ApiGameResponded
        , backendUri = shared.backendUri
        , token = user.token
        , id = route.params.id
        }
    )



-- UPDATE


type Msg
    = ApiGameResponded (Result Error Game)
    | ApiQuestionAskResponded
        { game : Game
        } (Result Error Question)
    | ApiQuestionAnswerResponded
        { game : Game
        , question : Question
        , choice : Choice
        } (Result Error Feedback)
    | ApiQuestionListResponded
        { game : Game
        } (Result Error (List QuestionSummary))
    | MakeChoice Choice
    | SubmitChoice
        { game : Game
        , question : Question
        } Choice
    | NextQuestion
        { game : Game
        }
    | FinishGame
        { game : Game
        }


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        ApiGameResponded (Ok game) ->
            ( Loaded
                { gameData = Api.Success game
                }
            , if isCompleted game then
                Api.Question.list
                    { onResponse = ApiQuestionListResponded { game = game }
                    , backendUri = shared.backendUri
                    , token = user.token
                    , gameId = game.id
                    }
              else
                Api.Question.ask
                    { onResponse = ApiQuestionAskResponded { game = game }
                    , backendUri = shared.backendUri
                    , token = user.token
                    , gameId = game.id
                    }
            )

        ApiGameResponded (Err error) ->
            case error of
                Api.UnAuthorized ->
                    ( model
                    , Effect.signOut
                    )

                errors ->
                    ( Loaded { gameData = Api.Failure errors }
                    , Effect.none
                    )

        ApiQuestionAskResponded { game } (Ok question) ->
            ( LoadedQuestion
                { game = game
                , questionData = Api.Success question
                , choice = NoChoice
                }
            , Effect.none
            )

        ApiQuestionAskResponded { game } (Err error) ->
            case error of
                Api.UnAuthorized ->
                    ( model
                    , Effect.signOut
                    )

                errors ->
                    ( LoadedQuestion
                        { game = game
                        , questionData = Api.Failure errors
                        , choice = NoChoice
                        }
                    , Effect.none
                    )

        ApiQuestionAnswerResponded { game, question, choice } (Ok feedback) ->
            ( LoadedFeedback
                { game = { game | questionsCount = game.questionsCount + 1 }
                , question = question
                , choice = choice
                , feedbackData = Api.Success feedback
                }
            , Effect.none
            )

        ApiQuestionAnswerResponded { game, question, choice } (Err error) ->
            case error of
                Api.UnAuthorized ->
                    ( model
                    , Effect.signOut
                    )

                errors ->
                    ( LoadedFeedback
                        { game = game
                        , question = question
                        , choice = choice
                        , feedbackData = Api.Failure errors
                        }
                    , Effect.none
                    )

        ApiQuestionListResponded { game } (Ok summaries) ->
            ( GameOver
                { game = game
                , summariesData = Api.Success summaries
                }
            , Effect.none
            )

        ApiQuestionListResponded { game } (Err error) ->
            case error of
                Api.UnAuthorized ->
                    ( model
                    , Effect.signOut
                    )

                errors ->
                    ( GameOver
                        { game = game
                        , summariesData = Api.Failure errors
                        }
                    , Effect.none
                    )

        MakeChoice choice ->
            case model of
                LoadedQuestion options ->
                    ( LoadedQuestion { options | choice = choice }
                    , Effect.none
                    )

                _ ->
                    ( model
                    , Effect.none
                    )

        SubmitChoice { game, question } choice ->
            case choice of
                Choice index ->
                    ( LoadedFeedback
                        { game = game
                        , question = question
                        , choice = choice
                        , feedbackData = Api.Loading
                        }
                    , Api.Question.answer
                        { onResponse = ApiQuestionAnswerResponded
                            { game = game
                            , question = question
                            , choice = choice
                            }
                        , backendUri = shared.backendUri
                        , token = user.token
                        , gameId = game.id
                        , choice = index
                        }
                    )

                NoChoice ->
                    ( model
                    , Effect.none
                    )

        NextQuestion { game } ->
            ( LoadedQuestion
                { game = game
                , questionData = Api.Loading
                , choice = NoChoice
                }
            , Api.Question.ask
                { onResponse = ApiQuestionAskResponded { game = game }
                , backendUri = shared.backendUri
                , token = user.token
                , gameId = game.id
                }
            )

        FinishGame { game } ->
            ( GameOver
                { game = game
                , summariesData = Api.Loading
                }
            , Api.Question.list
                { onResponse = ApiQuestionListResponded { game = game }
                , backendUri = shared.backendUri
                , token = user.token
                , gameId = game.id
                }
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Game.Id_"
    , attributes = [ width Element.fill ]
    , element = el [ width (Element.px 1000), Element.centerX ]
        ( case model of
            Loading ->
                viewLoadingScreen

            Loaded { gameData } ->
                viewLoadedGame gameData

            LoadedQuestion { game, questionData, choice } ->
                viewLoadedQuestion { game = game } questionData choice

            LoadedFeedback { game, question, choice, feedbackData } ->
                viewLoadedFeedback
                    { game = game
                    , question = question
                    , choice = choice
                    } feedbackData

            GameOver { game, summariesData } ->
                viewGameOver
                    { game = game
                    } summariesData
        )
    }


viewLoadingScreen : Element Msg
viewLoadingScreen =
    el [] (Element.text "Loading...")


viewLoadedGame : Api.Data Game -> Element Msg
viewLoadedGame gameData =
    column
        [ width Element.fill
        ]
        [ case gameData of
            Api.Loading ->
                el [] (Element.text "Loading game...")

            Api.Success game ->
                viewGame game

            Api.Failure errors ->
                el [] (Element.text "Showing game errors...")
        ]


viewGame : Game -> Element Msg
viewGame game =
    let
        shortHash : String
        shortHash =
            String.slice 0 8 game.id
    in
    el
        [ Element.paddingXY 0 10
        , Element.centerX
        ]
        (Element.text shortHash)


viewLoadedQuestion : { game : Game } -> Api.Data Question -> Choice -> Element Msg
viewLoadedQuestion { game } questionData choice =
    column
        [ width Element.fill
        ]
        [ viewGame game
        , case questionData of
            Api.Loading ->
                el [] (Element.text "Loading question...")

            Api.Success question ->
                viewQuestion game question choice True

            Api.Failure errors ->
                el [] (Element.text "Showing question errors...")
        ]


viewLoadedFeedback : { game : Game, question : Question, choice : Choice } -> Api.Data Feedback -> Element Msg
viewLoadedFeedback { game, question, choice } feedbackData =
    column
        [ width Element.fill
        ]
        [ viewGame game
        , viewQuestion game question choice False
        , case feedbackData of
            Api.Loading ->
                el [] (Element.text "Loading feedback...")

            Api.Success feedback ->
                viewFeedback { game = game } feedback

            Api.Failure errors ->
                el [] (Element.text "Showing question errors...")
        ]


viewGameOver : { game : Game } -> Api.Data (List QuestionSummary) -> Element Msg
viewGameOver { game } summariesData =
    let
        baseView : List (Element Msg)
        baseView =
            [ viewGame game
            ]

        summaryView : List (Element Msg)
        summaryView =
            [ case summariesData of
                Api.Loading ->
                    el [] (Element.text "Loading summaries...")

                Api.Success summaries ->
                    column
                        [ Element.spacingXY 0 10
                        ]
                        (List.map viewQuestionSummary summaries)

                Api.Failure errors ->
                    el [] (Element.text "Showing summary errors...")
            ]

        combinedView : List (Element Msg)
        combinedView = List.append baseView summaryView
    in
    column
        [ width Element.fill
        ]
        combinedView


viewQuestionSummary : QuestionSummary -> Element Msg
viewQuestionSummary summary =
    case summary of
        Answered { prompt, solution, result } ->
            el
                [ Element.padding 10
                , Border.rounded 10
                , if result then
                    Background.color (lawngreen 1)
                  else
                    Background.color (crimson 1)
                , width Element.fill
                ] <|
                    column
                        [ Element.spacingXY 0 10
                        ]
                        [ Element.paragraph []
                            [ Element.text prompt
                            ]
                        , el
                            [ Font.size 14
                            ]
                            (Element.text ("Solution: " ++ solution))
                        ]
        Pending { prompt } ->
            el
                [ Element.padding 10
                , Border.rounded 10
                , Background.color (lightgrey 1)
                , width Element.fill
                ]
                (Element.text prompt)


viewQuestion : Game -> Question -> Choice -> Bool -> Element Msg
viewQuestion game question choice allowSubmit =
    column
        [ width Element.fill
        , Element.centerX
        ]
        [ Element.paragraph
            [ Region.heading 2
            , Font.bold
            , Element.paddingXY 0 20
            ]
            [ Element.text question.prompt
            ]
        , viewOptions question.options choice
        , if allowSubmit then
            viewSubmit { game = game, question = question } choice
          else
            el [] (Element.text "")
        ]


viewOptions : List Option -> Choice -> Element Msg
viewOptions options choice =
    Input.radio
        [ width Element.fill
        , Element.spacing 10
        , Element.padding 10
        ]
        { onChange = MakeChoice
        , selected =
            case choice of
                Choice _ ->
                    Just choice

                NoChoice ->
                    Nothing
        , label = Input.labelAbove [ Font.size 14 ] (Element.text "Pick your choice")
        , options =
            (List.indexedMap viewOption options)
        }


viewOption : Int -> String -> Input.Option Choice Msg
viewOption zeroBasedIndex option =
    let
        index : Int
        index = zeroBasedIndex + 1
    in
    Input.optionWith (Choice index) <| radioOption (String.fromInt index ++ ") " ++ option)


radioOption : String -> Input.OptionState -> Element msg
radioOption label state =
    row [ Element.paddingXY 10 20
        , Border.rounded 10
        , Background.color <|
            case state of
                Input.Idle ->
                    (lightgrey 1)

                Input.Focused ->
                    (lightgrey 1)

                Input.Selected ->
                    (lightblue 1)
        , width Element.fill
        ]
        [ Element.text label
        ]


viewSubmit : { game : Game, question : Question } -> Choice -> Element Msg
viewSubmit { game, question } choice =
    el
        [ Element.centerX
        ] <|
        Input.button
            [ Element.padding 10
            , Background.color (salmon 1)
            , Border.rounded 5
            ]
            { onPress = Just (SubmitChoice { game = game, question = question } choice)
            , label = Element.text "Choose"
            }


viewFeedback : { game : Game } -> Feedback -> Element Msg
viewFeedback { game } feedback =
    column
        [ Element.spacingXY 0 10
        , width Element.fill
        ]
        [ if feedback.result then
            el
                [ Font.color (lawngreen 1)
                , Font.size 72
                , Font.bold
                , Element.centerX
                , Element.padding 20
                ]
                (Element.text "Correct!")
          else
            el
                [ Font.color (crimson 1)
                , Font.size 72
                , Font.bold
                , Element.centerX
                , Element.padding 20
                ]
                (Element.text "Wrong!")
        , if not feedback.result then
            column
                []
                [ Element.text ("The answer is: " ++ String.fromInt feedback.solution)
                , Element.paragraph []
                    [ Element.text feedback.clarification
                    ]
                ]
          else
            column
                []
                [ Element.paragraph []
                    [ Element.text feedback.clarification
                    ]
                ]
        , el
            [ Element.centerX
            ] <|
            ( if game.questionsCount == game.questionsLimit then
                Input.button
                    [ Element.padding 10
                    , Background.color (salmon 1)
                    , Border.rounded 5
                    ]
                    { onPress = Just (FinishGame { game = game })
                    , label = Element.text "Finish"
                    }
            else
                Input.button
                    [ Element.padding 10
                    , Background.color (salmon 1)
                    , Border.rounded 5
                    ]
                    { onPress = Just (NextQuestion { game = game })
                    , label = Element.text "Next"
                    }
            )
        ]


-- HELPERS


isCompleted : Game -> Bool
isCompleted game =
    game.questionsCount == game.questionsLimit
