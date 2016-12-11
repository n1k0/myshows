module Main exposing (main)

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Set
import Json.Decode as Decode
import Json.Encode as Encode
import Validate exposing (..)
import Store


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Store.load onLoaded


onLoaded : Decode.Value -> Msg
onLoaded json =
    case (Decode.decodeValue decodeShows json) of
        Ok shows ->
            LoadShows shows

        Err err ->
            -- XXX: Better error notification
            Debug.log (toString err) NoOp


maxStars : Int
maxStars =
    5


type alias Genre =
    String


type alias Show =
    { title : String
    , description : Maybe String
    , rating : Maybe Int
    , genres : List Genre
    }


type alias Model =
    { shows : List Show
    , currentSort : OrderBy
    , currentGenre : Maybe Genre
    , allGenres : Set.Set Genre
    , formData : Show
    , formErrors : List String
    , formEdit : Maybe String
    }


type OrderBy
    = TitleAsc
    | RatingAsc
    | RatingDesc


type Msg
    = NoOp
    | LoadShows (List Show)
    | RateShow String Int
    | EditShow Show
    | SetSort OrderBy
    | RefineGenre Genre
    | ClearGenre
    | MarkUnseen String
    | FormUpdateTitle String
    | FormUpdateDescription String
    | FormUpdateGenres String
    | FormUpdateRating String
    | FormSubmit


dummyShows : List Show
dummyShows =
    [ { title = "Breaking Bad"
      , description = Just """
        Breaking Bad follows protagonist Walter White, a chemistry teacher who
        lives in New Mexico with his wife and teenage son who has cerebral palsy.
        White is diagnosed with Stage III cancer and given a prognosis of two
        years left to live. With a new sense of fearlessness based on his
        medical prognosis, and a desire to secure his family's financial
        security, White chooses to enter a dangerous world of drugs and crime
        and ascends to power in this world. The series explores how a fatal
        diagnosis such as White's releases a typical man from the daily concerns
        and constraints of normal society and follows his transformation from
        mild family man to a kingpin of the drug trade."""
      , rating = Nothing
      , genres = [ "drama", "crime", "thriller" ]
      }
    , { title = "Better Call Saul"
      , description = Just """
        Better Call Saul is the prequel to the award-winning series Breaking
        Bad, set six years before Saul Goodman became Walter White's lawyer.
        When we meet him, the man who will become Saul Goodman is known as
        Jimmy McGill, a small-time lawyer searching for his destiny, and,
        more immediately, hustling to make ends meet. Working alongside, and
        often against, Jimmy is "fixer" Mike Ehrmantraut, a beloved character
        introduced in Breaking Bad. The series will track Jimmy's
        transformation into Saul Goodman, the man who puts "criminal" in
        "criminal lawyer"."""
      , rating = Nothing
      , genres = [ "drama", "crime" ]
      }
    ]


init : ( Model, Cmd Msg )
init =
    ( { shows = []
      , currentSort = TitleAsc
      , currentGenre = Nothing
      , allGenres = extractAllGenres []
      , formData = initFormData
      , formErrors = []
      , formEdit = Nothing
      }
    , Cmd.none
    )


initFormData : Show
initFormData =
    Show "" Nothing Nothing []


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


updateShow : String -> (Show -> Show) -> List Show -> List Show
updateShow title updateShow shows =
    List.map
        (\show ->
            if show.title == title then
                updateShow show
            else
                show
        )
        shows


rateShow : String -> Int -> List Show -> List Show
rateShow title rating shows =
    updateShow title (\show -> { show | rating = Just rating }) shows


markUnseen : String -> List Show -> List Show
markUnseen title shows =
    updateShow title (\show -> { show | rating = Nothing }) shows


processForm : Model -> Model
processForm ({ formData, formEdit, shows } as model) =
    let
        updatedShows =
            case formEdit of
                Nothing ->
                    formData :: shows

                Just edited ->
                    updateShow edited (\_ -> formData) shows
    in
        { model
            | shows = updatedShows
            , allGenres = extractAllGenres updatedShows
            , formData = initFormData
            , formErrors = []
            , formEdit = Nothing
        }


sortShows : OrderBy -> List Show -> List Show
sortShows order shows =
    case order of
        TitleAsc ->
            List.sortBy (.title >> String.toLower) shows

        RatingAsc ->
            List.sortBy (\show -> Maybe.withDefault 0 show.rating) shows

        RatingDesc ->
            List.reverse <| sortShows RatingAsc shows


filterGenre : Maybe Genre -> List Show -> List Show
filterGenre genre shows =
    case genre of
        Nothing ->
            shows

        Just currentGenre ->
            List.filter
                (\show -> List.member currentGenre show.genres)
                shows


extractAllGenres : List Show -> Set.Set Genre
extractAllGenres shows =
    Set.fromList <| List.concat <| List.map .genres shows


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ shows, formData } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoadShows shows ->
            ( { model | shows = shows, allGenres = extractAllGenres shows }, Cmd.none )

        EditShow show ->
            ( { model | formData = show, formEdit = Just show.title }, Cmd.none )

        RateShow title rating ->
            let
                updatedModel =
                    { model | shows = rateShow title rating shows }
            in
                ( updatedModel, encodeShows updatedModel.shows |> Store.save )

        MarkUnseen title ->
            let
                updatedModel =
                    { model | shows = markUnseen title shows }
            in
                ( updatedModel, encodeShows updatedModel.shows |> Store.save )

        SetSort order ->
            ( { model | currentSort = order }, Cmd.none )

        RefineGenre genre ->
            ( { model | currentGenre = Just genre }, Cmd.none )

        ClearGenre ->
            ( { model | currentGenre = Nothing }, Cmd.none )

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

        FormUpdateGenres genresString ->
            ( { model
                | formData =
                    { formData
                        | genres =
                            List.map (String.trim << String.toLower) <|
                                String.split "," genresString
                    }
              }
            , Cmd.none
            )

        FormUpdateRating rating ->
            ( { model
                | formData =
                    { formData
                        | rating = (String.toInt rating |> Result.toMaybe)
                    }
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
                    let
                        updatedModel =
                            processForm model
                    in
                        ( updatedModel, encodeShows updatedModel.shows |> Store.save )


onClick_ : msg -> Attribute msg
onClick_ msg =
    -- Custom onClick implementation with preventDefault enabled
    Events.onWithOptions
        "click"
        { preventDefault = True, stopPropagation = True }
        (Decode.succeed msg)


starLink : Show -> Int -> Html Msg
starLink show rank =
    let
        showRating =
            Maybe.withDefault 0 show.rating

        star =
            if rank > showRating then
                icon "star-empty"
            else
                icon "star"
    in
        Html.a [ Attr.href "", onClick_ (RateShow show.title rank) ]
            [ star ]


ratingStars : Show -> Html Msg
ratingStars show =
    Html.span [] (List.range 1 maxStars |> List.map (\rank -> starLink show rank))


icon : String -> Html Msg
icon kind =
    Html.i [ Attr.class <| "glyphicon glyphicon-" ++ kind ] []


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


genreLabel : String -> Html Msg
genreLabel genre =
    Html.span [ Attr.class "badge", Attr.style [ ( "margin", "0 .2em" ) ] ]
        [ Html.text genre ]


showView : Show -> Html Msg
showView show =
    Html.div [ Attr.class "panel panel-default" ]
        [ Html.div [ Attr.class "panel-heading" ]
            [ Html.div [ Attr.class "row" ]
                [ Html.strong [ Attr.class "col-sm-4" ] [ Html.text show.title ]
                , Html.div [ Attr.class "col-sm-4 text-center" ] (List.map genreLabel show.genres)
                , Html.div [ Attr.class "col-sm-4 text-right" ]
                    [ ratingStars show
                    , Html.text " "
                    , seenView show
                    , Html.text " "
                    , Html.button
                        [ Attr.class "btn btn-xs btn-primary", Events.onClick (EditShow show) ]
                        [ icon "pencil" ]
                    ]
                ]
            ]
        , Html.div [ Attr.class "panel-body" ]
            [ Html.text <| Maybe.withDefault "No description available." show.description ]
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

        buttonLabel =
            case formEdit of
                Nothing ->
                    "Add show"

                Just title ->
                    "Update " ++ title
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
            , formRow "Genres"
                [ Html.input
                    [ Events.onInput FormUpdateGenres
                    , Attr.value <| String.join ", " formData.genres
                    , Attr.type_ "text"
                    , Attr.class "form-control"
                    , Attr.placeholder "Drama, Action"
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
                    , Attr.max (toString maxStars)
                    , Attr.placeholder "Rating"
                    ]
                    []
                ]
            , Html.p []
                [ Html.button [ Attr.class "btn btn-primary" ]
                    [ Html.text <| buttonLabel ]
                ]
            ]


sortLink : OrderBy -> OrderBy -> Html Msg
sortLink order current =
    if current == order then
        Html.ins [] [ Html.text (toString order) ]
    else
        Html.a [ Attr.href "", onClick_ (SetSort order) ]
            [ Html.text (toString order) ]


sortLinks : Model -> Html Msg
sortLinks model =
    Html.p []
        [ Html.text "Sort by"
        , Html.text " "
        , sortLink TitleAsc model.currentSort
        , Html.text ", "
        , sortLink RatingAsc model.currentSort
        , Html.text ", "
        , sortLink RatingDesc model.currentSort
        ]


genreLinks : Model -> Html Msg
genreLinks { allGenres } =
    let
        genreLink : Genre -> Html Msg
        genreLink genre =
            Html.a
                [ Attr.class "badge"
                , Attr.href ""
                , Attr.style [ ( "margin", "0 .2em" ) ]
                , onClick_ (RefineGenre genre)
                ]
                [ Html.text genre ]
    in
        Html.p []
            [ Html.text "Refine genre: "
            , Html.text " "
            , Html.span [] (List.map genreLink (Set.toList <| allGenres))
            , Html.text " "
            , Html.a [ Attr.href "", onClick_ ClearGenre ] [ Html.text "Clear" ]
            ]


maybeEncode : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybeEncode encode thing =
    case thing of
        Nothing ->
            Encode.null

        Just value ->
            encode value


encodeShow : Show -> Encode.Value
encodeShow show =
    Encode.object
        [ ( "title", Encode.string show.title )
        , ( "description", maybeEncode Encode.string show.description )
        , ( "genres", Encode.list (List.map Encode.string show.genres) )
        , ( "rating", maybeEncode Encode.int show.rating )
        ]


encodeShows : List Show -> Encode.Value
encodeShows shows =
    Encode.list (List.map encodeShow shows)


decodeShow : Decode.Decoder Show
decodeShow =
    Decode.map4 Show
        (Decode.field "title" Decode.string)
        (Decode.maybe <| Decode.field "description" Decode.string)
        (Decode.maybe <| Decode.field "rating" Decode.int)
        (Decode.field "genres" <| Decode.list Decode.string)


decodeShows : Decode.Decoder (List Show)
decodeShows =
    Decode.list decodeShow


listView : Model -> Html Msg
listView model =
    if List.length model.shows == 0 then
        Html.div []
            [ Html.div [ Attr.class "alert alert-info" ]
                [ Html.text "No shows." ]
            , Html.p [ Attr.class "text-center" ]
                [ Html.button
                    [ Attr.class "btn btn-primary"
                    , onClick_ (LoadShows dummyShows)
                    ]
                    [ Html.text "Load sample shows" ]
                ]
            ]
    else
        let
            processedShows =
                model.shows
                    |> sortShows model.currentSort
                    |> filterGenre model.currentGenre
        in
            Html.div []
                [ sortLinks model
                , genreLinks model
                , Html.div [] (List.map showView processedShows)
                ]


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "container" ]
        [ Html.div [ Attr.class "row" ]
            [ Html.div [ Attr.class "col-sm-7" ]
                [ Html.h1 [] [ Html.text "My shows" ]
                , listView model
                ]
            , Html.div [ Attr.class "col-sm-5" ]
                [ showForm model ]
            ]
        ]
