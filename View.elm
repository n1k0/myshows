module View exposing (view)

import Set
import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Model exposing (..)


maxStars : Int
maxStars =
    5


maybeAsBool : Maybe a -> Bool
maybeAsBool x =
    case x of
        Nothing ->
            False

        Just _ ->
            True


htmlSpace : Html Msg
htmlSpace =
    Html.text " "


icon : String -> Html Msg
icon kind =
    Html.i [ Attr.class <| "glyphicon glyphicon-" ++ kind ] []


{-| Custom onClick implementation with preventDefault enabled
-}
onClick_ : msg -> Attribute msg
onClick_ msg =
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
            , Attr.style [ ( "font-size", "1.2em" ) ]
            , onClick_ <| RateShow show.title rank
            ]
            [ star ]


ratingStars : Show -> Html Msg
ratingStars show =
    Html.span []
        (List.range 1 maxStars |> List.map (\rank -> starLink show rank))


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
        [ Html.div
            [ Attr.class "panel-heading"
            , Attr.style [ ( "padding", "10px 15px 7px 15px" ) ]
            ]
            [ Html.div [ Attr.class "row" ]
                [ Html.strong [ Attr.class "col-sm-6" ]
                    [ if maybeAsBool show.rating then
                        icon "eye-open"
                      else
                        icon "eye-close"
                    , Html.span [ Attr.style [ ( "margin-left", ".5em" ) ] ]
                        [ Html.text show.title ]
                    ]
                , Html.div [ Attr.class "col-sm-6 text-right" ] <|
                    List.intersperse htmlSpace
                        [ ratingStars show
                        , Html.a
                            [ Attr.href ""
                            , Attr.style [ ( "margin", "0 .5em" ) ]
                            , onClick_ <| DeleteShow show
                            ]
                            [ icon "trash" ]
                        , Html.a
                            [ Attr.href ""
                            , onClick_ <| EditShow show
                            ]
                            [ icon "pencil" ]
                        ]
                ]
            ]
        , Html.div [ Attr.class "panel-body" ]
            [ Html.text <| Maybe.withDefault "No description available." show.description ]
        , if List.length show.genres > 0 then
            Html.div
                [ Attr.class "panel-footer text-center"
                , Attr.style [ ( "padding", "7px 15px 4px 15px" ) ]
                ]
                (List.map genreLabel show.genres)
          else
            Html.text ""
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

        label =
            case formEdit of
                Nothing ->
                    "Add show"

                Just title ->
                    "Update " ++ title
    in
        Html.form [ Events.onSubmit <| FormSubmit ]
            [ Html.h2 [] [ Html.text label ]
            , formErrorsView formErrors
            , formRow "Title"
                [ Html.input
                    [ Events.onInput <| FormEvent << UpdateTitle
                    , Attr.value formData.title
                    , Attr.type_ "text"
                    , Attr.class "form-control"
                    , Attr.placeholder "Show title"
                    ]
                    []
                ]
            , formRow "Description"
                [ Html.textarea
                    [ Events.onInput <| FormEvent << UpdateDescription
                    , Attr.value <| Maybe.withDefault "" formData.description
                    , Attr.class "form-control"
                    , Attr.placeholder "Description"
                    , Attr.rows 3
                    ]
                    []
                ]
            , formRow "Genres"
                [ Html.input
                    [ Events.onInput <| FormEvent << UpdateGenres
                    , Attr.value <| String.join ", " formData.genres
                    , Attr.type_ "text"
                    , Attr.class "form-control"
                    , Attr.placeholder "Comma separated, eg.: drama, action"
                    ]
                    []
                ]
            , formRow "Rating"
                [ Html.input
                    [ Events.onInput <| FormEvent << UpdateRating
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
                    [ Html.text <| label ]
                ]
            ]


sortLink : OrderBy -> OrderBy -> Html Msg
sortLink orderBy current =
    if current == orderBy then
        Html.ins [] [ Html.text (toString orderBy) ]
    else
        Html.a [ Attr.href "", onClick_ <| SetOrderBy orderBy ]
            [ Html.text (toString orderBy) ]


sortLinks : Model -> Html Msg
sortLinks model =
    Html.p [] <|
        List.intersperse htmlSpace
            [ Html.text "Sort by"
            , sortLink TitleAsc model.currentOrderBy
            , Html.text "|"
            , sortLink RatingAsc model.currentOrderBy
            , Html.text "|"
            , sortLink RatingDesc model.currentOrderBy
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
    Html.p [] <|
        List.intersperse
            htmlSpace
            [ Html.text "Refine genre: "
            , Html.span [] <| List.map (genreLink currentGenre) (Set.toList <| allGenres)
            , Html.a [ Attr.href "", onClick_ ClearGenre ] [ Html.text "Clear" ]
            ]


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
                    |> sortShows model.currentOrderBy
                    |> filterGenre model.currentGenre
        in
            Html.div []
                [ sortLinks model
                , genreLinks model
                , Html.div [] <| List.map showView processedShows
                ]


authView : Model -> Html Msg
authView { authToken, authUrl } =
    case authToken of
        Just _ ->
            Html.text "Authenticated with FxA"

        Nothing ->
            Html.a
                [ Attr.href authUrl
                , Attr.class "btn btn-primary"
                ]
                [ Html.text "Sign in with your Firefox Account" ]


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "container" ]
        [ Html.div [ Attr.class "row" ]
            [ Html.h1 [ Attr.class "col-sm-6" ] [ Html.text "My shows" ]
            , Html.div [ Attr.class "col-sm-6 text-right" ]
                [ authView model ]
            ]
        , Html.div [ Attr.class "row" ]
            [ Html.div [ Attr.class "col-sm-7" ]
                [ listView model ]
            , Html.div [ Attr.class "col-sm-5" ]
                [ showForm model ]
            ]
        ]
