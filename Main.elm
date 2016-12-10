module Main exposing (..)

import Html exposing (..)


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
    { shows : List Show }


type Msg
    = NoOp


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
    ( { shows = dummyShows }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


showView : Show -> Html msg
showView show =
    li []
        [ h3 [] [ text show.title ]
        , p [] [ text show.description ]
        , p [] [ text <| "Rated " ++ (toString show.rating) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "My shows" ]
        , ul [] (List.map showView model.shows)
        ]
