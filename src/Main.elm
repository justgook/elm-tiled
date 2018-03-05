module Main exposing (..)

import Html
import Http
import Tiled.Decode as Tiled


type Message
    = LevelLoaded (Result Http.Error Tiled.Level)


load : String -> Cmd Message
load url =
    Http.send LevelLoaded <| Http.get url Tiled.decode


main : Program Never Tiled.Level Message
main =
    Html.program
        { init = ( Tiled.init, load "./some_tiled_level.json" )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


view : Tiled.Level -> Html.Html Message
view model =
    Html.text (toString model)


update : Message -> Tiled.Level -> ( Tiled.Level, Cmd msg )
update msg model =
    case msg of
        LevelLoaded (Ok level) ->
            ( level, Cmd.none )

        LevelLoaded (Err _) ->
            ( model, Cmd.none )
