module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Json
import Validate exposing (..)


-- TODO
-- - sort
-- - search


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
    , description : Maybe String
    , rating : Maybe Int
    }


type alias Model =
    { shows : List Show
    , formData : Show
    , formErrors : List String
    , formEdit : Maybe String
    }


type Msg
    = NoOp
    | RateShow String Int
    | EditShow Show
    | MarkUnseen String
    | FormUpdateTitle String
    | FormUpdateDescription String
    | FormUpdateRating String
    | FormSubmit


dummyShows : List Show
dummyShows =
    [ { title = "plop1"
      , description = Nothing
      , rating = Nothing
      }
    , { title = "plop2"
      , description = Just "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua"
      , rating = Just 4
      }
    ]


init : ( Model, Cmd Msg )
init =
    ( { shows = dummyShows
      , formData = initFormData
      , formErrors = []
      , formEdit = Nothing
      }
    , Cmd.none
    )


initFormData : Show
initFormData =
    Show "" Nothing Nothing


ifShowExists : Model -> error -> Validator error String
ifShowExists { shows, formEdit } =
    ifInvalid
        (\title ->
            -- Do not check for uniqueness if a show is being edited
            case formEdit of
                Nothing ->
                    List.any (\show -> show.title == title) shows

                Just _ ->
                    False
        )


validateShow : Model -> Show -> List String
validateShow model =
    Validate.all
        [ .title >> ifBlank "Please enter a title."
        , .title >> (ifShowExists model) "This show is already listed."
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


markUnseen : String -> List Show -> List Show
markUnseen title shows =
    List.map
        (\show ->
            if show.title == title then
                { show | rating = Nothing }
            else
                show
        )
        shows


processForm : Model -> Model
processForm ({ formData, formEdit, shows } as model) =
    let
        updatedShows =
            case formEdit of
                Nothing ->
                    formData :: shows

                Just edited ->
                    List.map
                        (\show ->
                            if show.title == edited then
                                formData
                            else
                                show
                        )
                        shows
    in
        { model
            | shows = updatedShows
            , formData = initFormData
            , formErrors = []
            , formEdit = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ shows, formData } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EditShow show ->
            ( { model | formData = show, formEdit = Just show.title }, Cmd.none )

        RateShow title rating ->
            ( { model | shows = rateShow title rating shows }, Cmd.none )

        MarkUnseen title ->
            ( { model | shows = markUnseen title shows }, Cmd.none )

        FormUpdateTitle title ->
            ( { model
                | formData = { formData | title = title }
              }
            , Cmd.none
            )

        FormUpdateDescription description ->
            ( { model
                | formData = { formData | description = Just description }
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
                    (validateShow model) formData
            in
                if List.length errors > 0 then
                    ( { model | formErrors = errors }, Cmd.none )
                else
                    ( processForm model, Cmd.none )


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
        Html.a [ Attr.href "", onClick_ (RateShow show.title rank) ]
            [ Html.text star ]


ratingStars : Show -> Html Msg
ratingStars show =
    Html.span [ Attr.style [ ( "font-size", "1.35em" ) ] ]
        (List.range 1 5 |> List.map (\rank -> starLink show rank))


icon : String -> Html Msg
icon type__ =
    Html.i [ Attr.class <| "glyphicon glyphicon-" ++ type__ ] []


seenView : Show -> Html Msg
seenView { rating, title } =
    let
        seen =
            case rating of
                Nothing ->
                    False

                Just _ ->
                    True

        iconLink =
            if seen then
                Html.a [ Attr.href "", onClick_ (MarkUnseen title), Attr.title "Mark as unseen" ]
                    [ icon "eye-open" ]
            else
                icon "eye-close"
    in
        Html.div
            [ Attr.class "badge"
            , Attr.style
                [ ( "color", "#fff" )
                , ( "background-color"
                  , if seen then
                        "#3aa63a"
                    else
                        "#aaa"
                  )
                ]
            ]
            [ iconLink ]


showView : Show -> Html Msg
showView show =
    Html.div [ Attr.class "panel panel-default" ]
        [ Html.div [ Attr.class "panel-heading" ]
            [ Html.div [ Attr.class "row" ]
                [ Html.strong [ Attr.class "col-sm-6" ]
                    [ Html.text show.title ]
                , Html.span [ Attr.class "col-sm-6 text-right" ]
                    [ ratingStars show
                    , Html.text " "
                    , seenView show
                    ]
                ]
            ]
        , Html.div [ Attr.class "panel-body" ]
            [ Html.text <| Maybe.withDefault "No description available." show.description ]
        , Html.div [ Attr.class "panel-footer" ]
            [ Html.button
                [ Attr.class "btn btn-xs btn-primary", Events.onClick (EditShow show) ]
                [ Html.text "Edit" ]
            ]
        ]


formErrorsView : List String -> Html msg
formErrorsView errors =
    if List.length errors > 0 then
        Html.div [ Attr.class "alert alert-danger" ]
            [ Html.ul [ Attr.class "error" ]
                (List.map (\e -> Html.li [] [ Html.text e ]) errors)
            ]
    else
        Html.text ""


formRow : String -> List (Html Msg) -> Html Msg
formRow label children =
    let
        htmlLabel =
            Html.label [] [ Html.text label ]
    in
        Html.div
            [ Attr.class "form-group" ]
            [ htmlLabel, Html.div [] children ]


showForm : Model -> Html Msg
showForm ({ formErrors, formEdit, formData } as model) =
    let
        ratingString =
            case formData.rating of
                Nothing ->
                    ""

                Just n ->
                    toString n
    in
        Html.form [ Events.onSubmit FormSubmit ]
            [ Html.h2 [] [ Html.text "Add a show" ]
            , formErrorsView formErrors
            , formRow "Title"
                [ Html.input
                    [ Events.onInput FormUpdateTitle
                    , Attr.value formData.title
                    , Attr.type_ "text"
                    , Attr.class "form-control"
                    , Attr.placeholder "Show title"
                    ]
                    []
                ]
            , formRow "Description"
                [ Html.textarea
                    [ Events.onInput FormUpdateDescription
                    , Attr.value <| Maybe.withDefault "" formData.description
                    , Attr.class "form-control"
                    , Attr.placeholder "Description"
                    ]
                    []
                ]
            , formRow "Rating"
                [ Html.input
                    [ Events.onInput FormUpdateRating
                    , Attr.value ratingString
                    , Attr.type_ "number"
                    , Attr.class "form-control"
                    , Attr.min "1"
                    , Attr.max "5"
                    , Attr.placeholder "Rating"
                    ]
                    []
                ]
            , Html.p []
                [ Html.button [ Attr.class "btn btn-primary" ]
                    [ Html.text <| Maybe.withDefault "Add show" formEdit
                    ]
                ]
            ]


view : Model -> Html Msg
view model =
    let
        orderedShows =
            List.sortBy .title model.shows
    in
        Html.div [ Attr.class "container" ]
            [ Html.div [ Attr.class "row" ]
                [ Html.div [ Attr.class "col-sm-7" ]
                    [ Html.h1 [] [ Html.text "My shows" ]
                    , Html.div [] (List.map showView orderedShows)
                    ]
                , Html.div [ Attr.class "col-sm-5" ]
                    [ showForm model ]
                ]
            ]
