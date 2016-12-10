module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Show =
    { title : String
    , description : String
    , rating : Maybe Int
    }


type alias ShowRecord =
    { id : String
    , last_modified : Int
    , entity : Show
    }


type alias Model =
    { shows : List Show
    , formData : Show
    }


type Msg
    = NoOp
    | FormUpdateTitle String
    | FormUpdateDescription String
    | FormUpdateRating String
    | FormSubmit


dummyShows : List Show
dummyShows =
    [ { title = "plop1"
      , description = "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
      , rating = Nothing
      }
    , { title = "plop2"
      , description = "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua"
      , rating = Just 4
      }
    ]


init : ( Model, Cmd Msg )
init =
    ( { shows = dummyShows
      , formData = initFormData
      }
    , Cmd.none
    )


initFormData : Show
initFormData =
    Show "" "" Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ formData } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FormUpdateTitle title ->
            ( { model
                | formData = { formData | title = title }
              }
            , Cmd.none
            )

        FormUpdateDescription description ->
            ( { model
                | formData = { formData | description = description }
              }
            , Cmd.none
            )

        FormUpdateRating rating ->
            ( { model
                | formData = { formData | rating = (String.toInt rating |> Result.toMaybe) }
              }
            , Cmd.none
            )

        FormSubmit ->
            ( { model
                | shows = List.append model.shows [ formData ]
                , formData = initFormData
              }
            , Cmd.none
            )


ratingStars : Maybe Int -> String
ratingStars rating =
    case rating of
        Nothing ->
            "☆☆☆☆☆"

        Just n ->
            (String.repeat n "★") ++ (String.repeat (5 - n) "☆")


showForm : Show -> Html Msg
showForm show =
    let
        ratingString =
            case show.rating of
                Nothing ->
                    ""

                Just n ->
                    toString n
    in
        Html.form [ Events.onSubmit FormSubmit ]
            [ Html.h2 [] [ Html.text "Add a show" ]
            , Html.p []
                [ Html.input
                    [ Attributes.type_ "text"
                    , Events.onInput FormUpdateTitle
                    , Attributes.value show.title
                    ]
                    []
                ]
            , Html.p []
                [ Html.textarea
                    [ Events.onInput FormUpdateDescription
                    , Attributes.value show.description
                    ]
                    []
                ]
            , Html.p []
                [ Html.input
                    [ Attributes.type_ "number"
                    , Attributes.min "1"
                    , Attributes.max "5"
                    , Attributes.value ratingString
                    , Events.onInput FormUpdateRating
                    ]
                    []
                ]
            , Html.p [] [ Html.button [] [ Html.text "Add show" ] ]
            ]


showView : Show -> Html msg
showView show =
    Html.li []
        [ Html.h3 [] [ Html.text <| (ratingStars show.rating) ++ " " ++ show.title ]
        , Html.p [] [ Html.text show.description ]
        ]


view : Model -> Html Msg
view model =
    let
        orderedShows =
            List.sortBy .title model.shows
    in
        Html.div []
            [ Html.h1 [] [ Html.text "My shows" ]
            , Html.ul [] (List.map showView orderedShows)
            , showForm model.formData
            ]
