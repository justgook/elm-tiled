module Level exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import Mock
import Test exposing (..)
import Tiled.Decode as Tiled


-- exposing (decodeWith)


suite : Test
suite =
    describe "The Tiled.Decode module"
        [ describe "Tiled.Decode.decode"
            -- Nest as many descriptions as you like.
            [ test "decode simple level" <|
                \_ ->
                    case decodeString Tiled.decode Mock.bareMinimum of
                        Ok data ->
                            Expect.pass

                        Err err ->
                            Expect.fail err
            , test "geting props" <|
                \_ ->
                    case decodeString Tiled.decode Mock.bareMinimum of
                        Ok { properties } ->
                            let
                                _ =
                                    Debug.log "properties" properties
                            in
                            Expect.notEqual properties Dict.empty

                        Err err ->
                            Expect.fail err

            -- , describe "Layer Decoding"
            --     [ test "decode TileLayer" <|
            --         \_ ->
            --             case decodeString Tiled.decode Mock.bareMinimum of
            --                 Ok data ->
            --                     if
            --                         List.any
            --                             (\layer ->
            --                                 case layer of
            --                                     TileLayer _ ->
            --                                         True
            --                                     _ ->
            --                                         False
            --                             )
            --                             data.layers
            --                     then
            --                         Expect.pass
            --                     else
            --                         Expect.fail "No TileLayer found"
            --                 Err err ->
            --                     Expect.fail err
            --     ]
            ]
        ]
