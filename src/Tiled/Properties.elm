module Tiled.Properties exposing
    ( Properties, Property(..)
    , decode, encode
    )

{-|

@docs Properties, Property
@docs decode, encode

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


{-| -}
type alias Properties =
    Dict String Property


{-| Custom properties values
-}
type Property
    = Bool Bool
    | Int Int
    | Float Float
    | String String
    | Color String
    | File String
    | Object Int


{-| -}
decode : Decoder Properties
decode =
    Decode.list
        (Decode.succeed identity
            |> required "type" Decode.string
            |> Decode.andThen
                (\kind ->
                    Decode.succeed (\a b -> ( a, b ))
                        |> required "name" Decode.string
                        |> required "value" (decodeProperty kind)
                )
        )
        |> Decode.map Dict.fromList


{-| -}
encode : Properties -> Encode.Value
encode props =
    props
        |> Dict.toList
        |> Encode.list
            (\( key, value ) ->
                Encode.object
                    (( "name", Encode.string key )
                        :: (case value of
                                Bool v ->
                                    [ ( "type", Encode.string "bool" ), ( "value", Encode.bool v ) ]

                                Int v ->
                                    [ ( "type", Encode.string "int" ), ( "value", Encode.int v ) ]

                                Float v ->
                                    [ ( "type", Encode.string "float" ), ( "value", Encode.float v ) ]

                                String v ->
                                    [ ( "type", Encode.string "string" ), ( "value", Encode.string v ) ]

                                Color v ->
                                    [ ( "type", Encode.string "color" ), ( "value", Encode.string v ) ]

                                File v ->
                                    [ ( "type", Encode.string "file" ), ( "value", Encode.string v ) ]

                                Object v ->
                                    [ ( "type", Encode.string "object" ), ( "value", Encode.int v ) ]
                           )
                    )
            )


{-| -}
decodeProperty : String -> Decoder Property
decodeProperty typeString =
    case typeString of
        "bool" ->
            Decode.map Bool Decode.bool

        "int" ->
            Decode.map Int Decode.int

        "float" ->
            Decode.map Float Decode.float

        "string" ->
            Decode.map String Decode.string

        "color" ->
            Decode.map Color Decode.string

        "file" ->
            Decode.map File Decode.string

        "object" ->
            Decode.map Object Decode.int

        _ ->
            Decode.fail <| "I can't decode the property type " ++ typeString
