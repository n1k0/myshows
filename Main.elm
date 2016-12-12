module Main exposing (main)

import Html exposing (Html)
import Model exposing (Model, Msg, init, update, subscriptions)
import View exposing (view)


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
