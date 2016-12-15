module Model
    exposing
        ( AuthToken
        , Flags
        , FormMsg(..)
        , Model
        , Msg(..)
        , Genre
        , OrderBy(..)
        , Show
        , dummyShows
        , filterGenre
        , init
        , update
        , sortShows
        )

import Set
import Json.Decode as Decode
import Json.Encode as Encode
import Validate exposing (..)
import Kinto
import Navigation
import Http exposing (encodeUri)
import Ports


type alias Flags =
    { authToken : Maybe AuthToken
    }


type alias AuthToken =
    String


type alias Genre =
    String


type alias Show =
    { title : String
    , description : Maybe String
    , rating : Maybe Int
    , genres : List Genre
    }


type alias Backup =
    { shows : List Show }


type alias Model =
    { appUrl : String
    , authUrl : String
    , authToken : Maybe AuthToken
    , shows : List Show
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
    | UrlChange Navigation.Location
    | Logout
    | BackupSaved (Result Kinto.Error Backup)
    | BackupReceived (Result Kinto.Error Backup)
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


kintoServerUrl : String
kintoServerUrl =
    "https://kinto.dev.mozaws.net/v1/"


authHashPattern : String
authHashPattern =
    "#auth="


authRedirectUrl : String -> String
authRedirectUrl appUrl =
    encodeUri <| appUrl ++ authHashPattern


getAuthUrl : String -> String
getAuthUrl serverRoot =
    serverRoot ++ "portier/login"


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        appUrl =
            extractAppUrl location

        authUrl =
            getAuthUrl kintoServerUrl

        authToken =
            case flags.authToken of
                Just token ->
                    Just token

                Nothing ->
                    extractAuthToken location

        commands =
            case authToken of
                Just token ->
                    [ Ports.saveAuthToken <| Just token
                    , fetchBackup <| Just token
                    , Navigation.newUrl "#"
                    ]

                Nothing ->
                    [ fetchBackup authToken ]
    in
        { appUrl = appUrl
        , authUrl = authUrl
        , authToken = authToken
        , shows = []
        , currentOrderBy = TitleAsc
        , currentGenre = Nothing
        , allGenres = extractAllGenres []
        , formData = initFormData
        , formErrors = []
        , formEdit = Nothing
        }
            ! commands


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


extractAppUrl : Navigation.Location -> String
extractAppUrl { origin, pathname } =
    origin ++ pathname


extractAuthToken : Navigation.Location -> Maybe AuthToken
extractAuthToken { hash } =
    case (String.split authHashPattern hash) of
        [ _, authToken ] ->
            Just authToken

        _ ->
            Nothing


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
update msg ({ authToken, shows, formData } as model) =
    case msg of
        NoOp ->
            model ! []

        UrlChange location ->
            model ! []

        Logout ->
            { model
                | authToken = Nothing
                , shows = []
                , currentOrderBy = TitleAsc
                , currentGenre = Nothing
                , allGenres = extractAllGenres []
                , formData = initFormData
                , formErrors = []
                , formEdit = Nothing
                , shows = []
            }
                ! [ Ports.saveAuthToken Nothing ]

        LoadShows shows ->
            { model | shows = shows, allGenres = extractAllGenres shows } ! []

        BackupReceived (Ok backup) ->
            { model | shows = backup.shows, allGenres = extractAllGenres backup.shows } ! []

        BackupReceived (Err error) ->
            -- TODO handle error
            model ! []

        BackupSaved (Ok backup) ->
            model ! []

        BackupSaved (Err error) ->
            model ! []

        EditShow show ->
            { model | formData = show, formEdit = Just show.title } ! []

        DeleteShow show ->
            let
                updatedShows =
                    deleteShow show shows
            in
                { model
                    | shows = updatedShows
                    , allGenres = extractAllGenres updatedShows
                }
                    ! [ saveBackup authToken updatedShows ]

        RateShow title rating ->
            let
                updatedModel =
                    { model | shows = rateShow title rating shows }
            in
                updatedModel ! [ saveBackup authToken updatedModel.shows ]

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
                        updatedModel ! [ saveBackup authToken updatedModel.shows ]

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


encodeBackup : List Show -> Encode.Value
encodeBackup shows =
    Encode.object [ ( "shows", encodeShows shows ) ]


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


decodeBackup : Decode.Decoder Backup
decodeBackup =
    Decode.map Backup <|
        Decode.field "shows" decodeShows


client : String -> Kinto.Client
client token =
    Kinto.client kintoServerUrl <| Kinto.Custom "Portier" token


backupResource : Kinto.Resource Backup
backupResource =
    Kinto.collectionResource "default" decodeBackup


saveBackup : Maybe AuthToken -> List Show -> Cmd Msg
saveBackup authToken shows =
    case authToken of
        Just token ->
            client token
                |> Kinto.replace backupResource "myshows" (encodeBackup shows)
                |> Kinto.send BackupSaved

        Nothing ->
            Cmd.none


fetchBackup : Maybe AuthToken -> Cmd Msg
fetchBackup authToken =
    case authToken of
        Just token ->
            client token
                |> Kinto.get backupResource "myshows"
                |> Kinto.send BackupReceived

        Nothing ->
            Cmd.none
