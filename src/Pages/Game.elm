module Pages.Game exposing (Model, Msg, page)

import Auth
import Colors.Alpha exposing (
    black, lightgrey, lightblue, white, crimson, salmon)
import Effect exposing (Effect)
import Element exposing (Element, el, column, row, px, width, height)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Events
import Json.Decode
import Route exposing (Route)
import Route.Path
import Html.Events
import Http
import Layouts
import Page exposing (Page)
import Set exposing (Set)
import Shared
import Time
import View exposing (View)

import Api exposing (Error, ServiceError)
import Api.Game exposing (Game)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared
        , update = update user shared
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (toLayout user)


toLayout : Auth.User -> Model -> Layouts.Layout Msg
toLayout user model =
    Layouts.Navigation
        { title = "Games"
        , user = user
        }


-- INIT


type alias Model =
    { games : Api.Data (List Game)
    , keywordField : String
    , keywords : Set Keyword
    , errors : List ServiceError
    , timeZone : Time.Zone
    }

type alias Keyword
    = String

type Field
    = Keywords
    | QuestionsLimit


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { games = Api.Loading
      , keywordField = ""
      , keywords = Set.empty
      , errors = []
      , timeZone = shared.timeZone
      }
    , Api.Game.list
        { onResponse = GameApiResponded
        , backendUri = shared.backendUri
        , token = user.token
        }
    )



-- UPDATE


type Msg
    = GameApiResponded (Result Error (List Game))
    | KeywordUpdated String
    | AddKeyword Keyword
    | RemoveKeyword Keyword
    | StartNewGame
    | DisabledStartNewGame
    | GameCreated (Result Error Game)


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        GameApiResponded (Ok listOfGames) ->
            ( { model | games = Api.Success listOfGames }
            , Effect.none
            )

        GameApiResponded (Err error) ->
            case error of
                Api.UnAuthorized ->
                    ( model
                    , Effect.signOut
                    )

                Api.Feedback errors ->
                    ( { model | errors = errors }
                    , Effect.none
                    )

        KeywordUpdated keyword ->
            ( { model | keywordField = keyword }
            , Effect.none
            )

        RemoveKeyword keyword ->
            let
                keywords : Set Keyword
                keywords = Set.remove keyword model.keywords
            in
            ( { model | keywords = keywords, keywordField = "" }
            , Effect.none
            )

        AddKeyword keyword ->
            let
                keywords : Set Keyword
                keywords =
                    if Set.size model.keywords < 5 then
                        Set.insert keyword model.keywords
                    else
                        model.keywords
            in
            ( { model | keywords = keywords, keywordField = "" }
            , Effect.none
            )

        StartNewGame ->
            ( model
            , Api.Game.post
                { onResponse = GameCreated
                , backendUri = shared.backendUri
                , token = user.token
                , keywords = model.keywords
                }
            )

        DisabledStartNewGame ->
            ( model
            , Effect.none
            )

        GameCreated (Ok game) ->
            case model.games of
                Api.Success games ->
                    ( { model | games  = Api.Success (game :: games) }
                    , Effect.none
                    )

                _ ->
                    ( model
                    , Effect.none
                    )

        GameCreated (Err error) ->
            case error of
                Api.UnAuthorized ->
                    ( model
                    , Effect.signOut
                    )

                Api.Feedback errors ->
                    ( { model | errors = errors }
                    , Effect.none
                    )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Game"
    , attributes = [ width Element.fill ]
    , element =
        column
            [ width Element.fill
            ]
            [ row [ Element.centerX, width Element.fill ]
                [ viewStartNewGame model
                , viewGameList model
                ]
            ]
    }

viewStartNewGame : Model -> Element Msg
viewStartNewGame model =
    column
        [ Element.width <| Element.fillPortion 1
        , Element.spacingXY 20 0
        , Element.padding 30
        , Element.alignTop
        , Element.alignLeft
        ]
        [ el
            [ Region.heading 2
            , Font.color (lightblue 1)
            , Font.size 36
            , Font.bold
            , Element.paddingXY 10 50
            ]
            (Element.text "New Game")
        , row
            [ Element.alignTop
            , Element.spacingXY 20 5
            , Element.paddingXY 0 10
            ]
            [ Input.text
                [ onEnter (AddKeyword model.keywordField)
                ]
                { onChange = KeywordUpdated
                , text = model.keywordField
                , placeholder = Just <| Input.placeholder [] <| Element.text "keyword"
                , label = Input.labelAbove
                    [ Font.size 12
                    , Element.padding 5
                    ]
                    (Element.text "Type a keyword and press 'ENTER' to add, you can have between 1 and 5 keywords")
                }
            , if Set.isEmpty model.keywords then
                Input.button
                    [ Background.color (lightgrey 1)
                    ]
                    { onPress = Just DisabledStartNewGame
                    , label = Element.text "Go!"
                    }
              else
                Input.button
                    [ Background.color (lightblue 1)
                    ]
                    { onPress = Just StartNewGame
                    , label = Element.text "Go!"
                    }
            ]
        , viewKeywords model.keywords True
        ]

viewKeywords : Set Keyword -> Bool -> Element Msg
viewKeywords keywords removable =
    Element.wrappedRow
        [ Element.spacing 10
        , Element.padding 10
        ]
        <| List.map (viewKeyword removable)
        <| Set.toList keywords

viewKeyword : Bool -> Keyword -> Element Msg
viewKeyword removable keyword =
    el
        [ Element.padding 5
        , Background.color (lightgrey 1)
        , Font.size 12
        ] <|
            row [ Element.spacingXY 5 0]
                [ Element.text keyword
                , if removable then
                    Input.button [ Font.bold ]
                        { onPress = Just (RemoveKeyword keyword)
                        , label = Element.text "x"
                        }
                  else
                      Element.text ""
                ]

viewGameList : Model -> Element Msg
viewGameList model =
    column
        [ width <| Element.fillPortion 3
        , height Element.fill
        , Element.alignTop
        , Element.alignLeft
        , Element.padding 30
        ]
        [ el
            [ Region.heading 2
            , Font.color (lightblue 1)
            , Font.size 36
            , Font.bold
            , Element.paddingXY 10 50
            ]
            (Element.text "History")
        , case model.games of
            Api.Loading ->
                el []
                    (Element.text "Loading your games...")

            Api.Success games ->
                Element.column
                    [ Element.spacing 20
                    , width Element.fill
                    ]
                    (List.map (viewGameLink model.timeZone) games)

            Api.Failure error ->
                el []
                    (Element.text "Uhoh")
        ]

viewGameLink : Time.Zone -> Game -> Element Msg
viewGameLink timeZone game =
    let
        gameDetailRoute : Route.Path.Path
        gameDetailRoute =
            Route.Path.Game_Id_
                { id = game.id
                }

        creationDate : String
        creationDate =
            Shared.toDatetime game.creationTime timeZone
    in
    Element.column
        [ width Element.fill
        ]
        [ el [ Font.size 12, Element.paddingXY 0 10 ] (Element.text creationDate)
        , Element.link
            [ Element.padding 10
            , Border.rounded 10
            , if isCompleted game then
                Background.color (lightblue 1)
              else
                Background.color (salmon 1)
            , width Element.fill
            ]
            { url = Route.Path.toString gameDetailRoute
            , label = viewGame timeZone game
            }
        ]

viewGame : Time.Zone -> Game -> Element Msg
viewGame timeZone game =
    let
        shortHash : String
        shortHash =
            String.slice 0 8 game.id
    in
        row [ Element.spaceEvenly, width Element.fill, Element.centerY ]
            [ column []
                [ el [ Font.color (white 1), Font.size 24, Element.padding 5 ] (Element.text ("#" ++ shortHash))
                , viewKeywords game.keywords False
                ]
            , el
                [ Font.size 36
                , Font.bold
                , Font.color (white 1)
                ]
                ( case game.status of
                    Api.Game.PENDING ->
                        Element.text "CREATING"

                    _ ->
                        Element.text (String.fromInt game.questionsAnswered ++ " / " ++ String.fromInt game.questionsLimit)
                )
            ]

onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg
                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )

-- ERROR

findFieldError : String -> Model -> Maybe ServiceError
findFieldError field model =
    let
        hasMatchingField : ServiceError -> Bool
        hasMatchingField error =
            error.field == Just field
    in
    model.errors
        |> List.filter hasMatchingField
        |> List.head

findFormError : Model -> Maybe ServiceError
findFormError model =
    let
        doesntHaveField : ServiceError -> Bool
        doesntHaveField error =
            error.field == Nothing
    in
    model.errors
        |> List.filter doesntHaveField
        |> List.head


-- HELPERS


isPending : Game -> Bool
isPending game =
    game.status == Api.Game.PENDING


isCompleted : Game -> Bool
isCompleted game =
    game.questionsAnswered == game.questionsLimit
