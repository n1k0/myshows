module Main exposing (main)

import Model exposing (Model, Msg(..), init, update)
import View exposing (view)
import Navigation


-- TODO
-- - ? Add dates: added_on, rated_on (seen)
-- - kinto integration


main : Program Model.Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
