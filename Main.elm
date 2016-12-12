module Main exposing (main)

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Set
import Json.Decode as Decode
import Json.Encode as Encode
import Validate exposing (..)
import Store


-- TODO
-- - ? Add dates: added_on, rated_on (seen)
-- - kinto integration


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
    | DeleteShow Show
    | SetSort OrderBy
    | RefineGenre Genre
    | ClearGenre
    | FormUpdateTitle String
    | FormUpdateDescription String
    | FormUpdateGenres String
    | FormUpdateRating String
    | FormSubmit


dummyShows : List Show
dummyShows =
    [ { title = "Breaking Bad"
      , description =
            String.join
                " "
                [ "Breaking Bad follows protagonist Walter White, a chemistry"
                , "teacher who lives in New Mexico and has been diagnosed with"
                , "lung cancer. He badly needs to find money for fighting it..."
                ]
                |> Just
      , rating = Nothing
      , genres = [ "drama", "crime", "thriller" ]
      }
    , { title = "Better Call Saul"
      , description =
            String.join
                " "
                [ "Better Call Saul is the prequel to the award-winning series"
                , "Breaking Bad. The series will track Jimmy's transformation"
                , """into Saul Goodman, the man who puts "criminal" in"""
                , """"criminal lawyer"."""
                ]
                |> Just
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
    shows
        |> List.map
            (\show ->
                if show.title == title then
                    updateShow show
                else
                    show
            )


deleteShow : Show -> List Show -> List Show
deleteShow { title } shows =
    shows |> List.filter (\show -> title /= show.title)


rateShow : String -> Int -> List Show -> List Show
rateShow title rating shows =
    shows |> updateShow title (\show -> { show | rating = Just rating })


processForm : Model -> Model
processForm ({ formData, formEdit, shows } as model) =
    let
        -- Ensure entered genres are unique for this show
        processedFormData =
            { formData | genres = formData.genres |> Set.fromList |> Set.toList }

        updatedShows =
            case formEdit of
                Nothing ->
                    processedFormData :: shows

                Just edited ->
                    updateShow edited (always processedFormData) shows
    in
        -- Notes:
        -- We clear current genre filter to ensure added show is listed
        { model
            | shows = updatedShows
            , currentGenre = Nothing
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
            shows |> List.filter (\show -> List.member currentGenre show.genres)


extractAllGenres : List Show -> Set.Set Genre
extractAllGenres shows =
    List.map .genres shows
        |> List.concat
        |> Set.fromList


saveShows : List Show -> Cmd Msg
saveShows shows =
    encodeShows shows |> Store.save


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ shows, formData } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoadShows shows ->
            ( { model | shows = shows, allGenres = extractAllGenres shows }, Cmd.none )

        EditShow show ->
            ( { model | formData = show, formEdit = Just show.title }, Cmd.none )

        DeleteShow show ->
            let
                updatedShows =
                    deleteShow show shows
            in
                ( { model
                    | shows = updatedShows
                    , allGenres = extractAllGenres updatedShows
                  }
                , saveShows updatedShows
                )

        RateShow title rating ->
            let
                updatedModel =
                    { model | shows = rateShow title rating shows }
            in
                ( updatedModel, saveShows updatedModel.shows )

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
                            String.split "," genresString
                                |> List.map (String.trim << String.toLower)
                    }
              }
            , Cmd.none
            )

        FormUpdateRating rating ->
            ( { model
                | formData =
                    { formData
                        | rating = String.toInt rating |> Result.toMaybe
                    }
              }
            , Cmd.none
            )

        FormSubmit ->
            let
                errors =
                    formData |> validateShow model
            in
                if List.length errors > 0 then
                    ( { model | formErrors = errors }, Cmd.none )
                else
                    let
                        updatedModel =
                            processForm model
                    in
                        ( updatedModel, saveShows updatedModel.shows )


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
        Html.a
            [ Attr.href ""
            , Attr.style [ ( "color", "lightyellow" ), ( "font-size", "1.2em" ) ]
            , onClick_ <| RateShow show.title rank
            ]
            [ star ]


ratingStars : Show -> Html Msg
ratingStars show =
    Html.span []
        (List.range 1 maxStars |> List.map (\rank -> starLink show rank))


icon : String -> Html Msg
icon kind =
    Html.i [ Attr.class <| "glyphicon glyphicon-" ++ kind ] []


maybeAsBool : Maybe a -> Bool
maybeAsBool x =
    case x of
        Nothing ->
            False

        Just _ ->
            True


seenView : Show -> Html Msg
seenView { rating, title } =
    let
        seen =
            maybeAsBool rating
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
            [ if seen then
                icon "eye-open"
              else
                icon "eye-close"
            ]


genreLabel : String -> Html Msg
genreLabel genre =
    Html.a
        [ Attr.href ""
        , onClick_ <| RefineGenre genre
        , Attr.class "badge"
        , Attr.style [ ( "margin", "0 .2em" ) ]
        ]
        [ Html.text genre ]


showView : Show -> Html Msg
showView show =
    Html.div [ Attr.class "panel panel-default" ]
        [ Html.div [ Attr.class "panel-heading" ]
            [ Html.div [ Attr.class "row" ]
                [ Html.strong [ Attr.class "col-sm-4" ]
                    [ seenView show
                    , Html.text " "
                    , Html.text show.title
                    ]
                , Html.div [ Attr.class "col-sm-4 text-center" ] (List.map genreLabel show.genres)
                , Html.div [ Attr.class "col-sm-4 text-right" ]
                    [ ratingStars show
                    , Html.text " "
                    , Html.text " "
                    , Html.button
                        [ Attr.class "btn btn-xs btn-danger", Events.onClick (DeleteShow show) ]
                        [ icon "remove" ]
                    , Html.text " "
                    , Html.button
                        [ Attr.class "btn btn-xs btn-info", Events.onClick (EditShow show) ]
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
                    , Attr.rows 3
                    ]
                    []
                ]
            , formRow "Genres"
                [ Html.input
                    [ Events.onInput FormUpdateGenres
                    , Attr.value <| String.join ", " formData.genres
                    , Attr.type_ "text"
                    , Attr.class "form-control"
                    , Attr.placeholder "Comma separated, eg.: drama, action"
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
        Html.a [ Attr.href "", onClick_ <| SetSort order ]
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


genreLink : Maybe Genre -> Genre -> Html Msg
genreLink currentGenre genre =
    let
        bgColor =
            case currentGenre of
                Nothing ->
                    "#555"

                Just current ->
                    if current == genre then
                        "#999"
                    else
                        "#555"
    in
        Html.a
            [ Attr.class "badge"
            , Attr.href ""
            , Attr.style [ ( "margin", "0 .2em" ), ( "background-color", bgColor ) ]
            , onClick_ <| RefineGenre genre
            ]
            [ Html.text genre ]


genreLinks : Model -> Html Msg
genreLinks { allGenres, currentGenre } =
    Html.p []
        [ Html.text "Refine genre: "
        , Html.text " "
        , Html.span [] (List.map (genreLink currentGenre) (Set.toList <| allGenres))
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
                    , onClick_ <| LoadShows dummyShows
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
