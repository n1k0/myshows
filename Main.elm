module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Json
import Validate exposing (..)


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
    , formErrors : List String
    }


type Msg
    = NoOp
    | RateShow String Int
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
      , formErrors = []
      }
    , Cmd.none
    )


initFormData : Show
initFormData =
    Show "" "" Nothing


ifShowExists : List Show -> error -> Validator error String
ifShowExists shows =
    ifInvalid (\title -> List.any (\show -> show.title == title) shows)


validateShow : List Show -> Show -> List String
validateShow shows =
    Validate.all
        [ .title >> ifBlank "Please enter a title."
        , .title >> (ifShowExists shows) "This show is already listed."
        ]


rateShow : String -> Int -> List Show -> List Show
rateShow title rating shows =
    List.map
        (\show ->
            if show.title == title then
                { show | rating = Just rating }
            else
                show
        )
        shows


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ shows, formData } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RateShow title rating ->
            ( { model | shows = rateShow title rating shows }, Cmd.none )

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
            let
                errors =
                    (validateShow shows) formData
            in
                if List.length errors > 0 then
                    ( { model | formErrors = errors }, Cmd.none )
                else
                    ( { model
                        | shows = formData :: shows
                        , formData = initFormData
                        , formErrors = []
                      }
                    , Cmd.none
                    )


onClick_ : msg -> Attribute msg
onClick_ msg =
    -- Custom onClick implementation with preventDefault enabled
    Events.onWithOptions
        "click"
        { preventDefault = True, stopPropagation = True }
        (Json.succeed msg)


starLink : Show -> Int -> Html Msg
starLink show rank =
    let
        showRating =
            Maybe.withDefault 0 show.rating

        star =
            if rank > showRating then
                "☆"
            else
                "★"
    in
        Html.a [ Attributes.href "", onClick_ (RateShow show.title rank) ]
            [ Html.text star ]


ratingStars : Show -> Html Msg
ratingStars show =
    Html.div [] (List.range 1 5 |> List.map (\rank -> starLink show rank))


showView : Show -> Html Msg
showView show =
    Html.div [ Attributes.class "panel panel-default" ]
        [ Html.div [ Attributes.class "panel-heading" ]
            [ Html.div [ Attributes.class "row" ]
                [ Html.strong [ Attributes.class "col-sm-6" ] [ Html.text show.title ]
                , Html.span [ Attributes.class "col-sm-6 text-right" ]
                    [ ratingStars show ]
                ]
            ]
        , Html.div [ Attributes.class "panel-body" ] [ Html.text show.description ]
        ]


formErrors : List String -> Html msg
formErrors errors =
    Html.ul [ Attributes.class "error" ]
        (List.map (\e -> Html.li [] [ Html.text e ]) errors)


formRow : String -> List (Html Msg) -> Html Msg
formRow label children =
    let
        htmlLabel =
            Html.label [] [ Html.text label ]
    in
        Html.div
            [ Attributes.class "form-group" ]
            [ htmlLabel, Html.div [] children ]


showForm : List String -> Show -> Html Msg
showForm errors show =
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
            , formErrors errors
            , formRow "Title"
                [ Html.input
                    [ Events.onInput FormUpdateTitle
                    , Attributes.value show.title
                    , Attributes.type_ "text"
                    , Attributes.class "form-control"
                    , Attributes.placeholder "Show title"
                    ]
                    []
                ]
            , formRow "Description"
                [ Html.textarea
                    [ Events.onInput FormUpdateDescription
                    , Attributes.value show.description
                    , Attributes.class "form-control"
                    , Attributes.placeholder "Description"
                    ]
                    []
                ]
            , formRow "Rating"
                [ Html.input
                    [ Events.onInput FormUpdateRating
                    , Attributes.value ratingString
                    , Attributes.type_ "number"
                    , Attributes.class "form-control"
                    , Attributes.min "1"
                    , Attributes.max "5"
                    , Attributes.placeholder "Rating"
                    ]
                    []
                ]
            , Html.p []
                [ Html.button [ Attributes.class "btn btn-primary" ]
                    [ Html.text "Add show" ]
                ]
            ]


view : Model -> Html Msg
view model =
    let
        orderedShows =
            List.sortBy .title model.shows
    in
        Html.div [ Attributes.class "container" ]
            [ Html.div [ Attributes.class "row" ]
                [ Html.div [ Attributes.class "col-sm-7" ]
                    [ Html.h1 [] [ Html.text "My shows" ]
                    , Html.div [] (List.map showView orderedShows)
                    ]
                , Html.div [ Attributes.class "col-sm-5" ] [ showForm model.formErrors model.formData ]
                ]
            ]
