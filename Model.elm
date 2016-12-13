module Model
    exposing
        ( FormMsg(..)
        , Model
        , Msg(..)
        , Genre
        , OrderBy(..)
        , Show
        , dummyShows
        , filterGenre
        , init
        , onStoreLoaded
        , update
        , sortShows
        , subscriptions
        )

import Set
import Json.Decode as Decode
import Json.Encode as Encode
import Validate exposing (..)
import Store


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
    , currentOrderBy : OrderBy
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


type FormMsg
    = UpdateTitle String
    | UpdateDescription String
    | UpdateGenres String
    | UpdateRating String


type Msg
    = NoOp
    | LoadShows (List Show)
    | RateShow String Int
    | EditShow Show
    | DeleteShow Show
    | SetOrderBy OrderBy
    | RefineGenre Genre
    | ClearGenre
    | FormEvent FormMsg
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


subscriptions : Model -> Sub Msg
subscriptions =
    always <| Store.load onStoreLoaded


onStoreLoaded : Decode.Value -> Msg
onStoreLoaded json =
    case (Decode.decodeValue decodeShows json) of
        Ok shows ->
            LoadShows shows

        Err err ->
            -- XXX: Better error notification
            Debug.log (toString err) NoOp


init : ( Model, Cmd Msg )
init =
    ( { shows = []
      , currentOrderBy = TitleAsc
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


stringToGenres : String -> List Genre
stringToGenres genresString =
    String.split "," genresString |> List.map (String.trim << String.toLower)


updateForm : FormMsg -> Show -> Show
updateForm formMsg formData =
    case formMsg of
        UpdateTitle title ->
            { formData | title = title }

        UpdateDescription description ->
            { formData
                | description =
                    if description == "" then
                        Nothing
                    else
                        Just description
            }

        UpdateGenres genresString ->
            { formData | genres = stringToGenres genresString }

        UpdateRating rating ->
            { formData | rating = String.toInt rating |> Result.toMaybe }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ shows, formData } as model) =
    case msg of
        NoOp ->
            model ! []

        LoadShows shows ->
            { model | shows = shows, allGenres = extractAllGenres shows } ! []

        EditShow show ->
            { model | formData = show, formEdit = Just show.title } ! []

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
                updatedModel ! [ saveShows updatedModel.shows ]

        SetOrderBy orderBy ->
            { model | currentOrderBy = orderBy } ! []

        RefineGenre genre ->
            { model | currentGenre = Just genre } ! []

        ClearGenre ->
            { model | currentGenre = Nothing } ! []

        FormSubmit ->
            let
                errors =
                    formData |> validateShow model
            in
                if List.length errors > 0 then
                    { model | formErrors = errors } ! []
                else
                    let
                        updatedModel =
                            processForm model
                    in
                        updatedModel ! [ saveShows updatedModel.shows ]

        FormEvent formMsg ->
            { model | formData = updateForm formMsg formData } ! []


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
    Encode.list <| List.map encodeShow shows


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
