module Level exposing (..)

import Dict
import Expect exposing (Expectation)
import Json.Decode exposing (decodeString)
import Mock
import Test exposing (..)
import Tiled.Decode as Tiled


suite : Test
suite =
    describe "The Tiled.Decode module"
        [ describe "Tiled.Decode.decode"
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
                            Expect.notEqual properties Dict.empty

                        Err err ->
                            Expect.fail err
            ]
        , describe "Tiled.Decode.decodeTiles"
            [ test "old vs new" <|
                \_ ->
                    decodeString Tiled.decodeTiles Mock.tilesDataOld
                        |> Expect.equal (decodeString Tiled.decodeTiles Mock.tilesDataNew)
            ]
        ]
