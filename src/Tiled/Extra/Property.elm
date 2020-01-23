module Tiled.Extra.Property exposing (Property(..), convert, get, at, values, toList)

{-| Nested way to represent custom properties.

@docs Property, convert, get, at, values, toList

-}

import Dict exposing (Dict)
import Tiled.Properties


{-| Custom nested properties
-}
type Property
    = Bool Bool
    | Int Int
    | Float Float
    | String String
    | Color String
    | File String
    | Object Int
    | Group (Dict String Property)


{-| Convert [`Tiled.Properties`](Tiled-Properties#Properties) to [`Tiled.Extra.Property`](Tiled-Extra-Property#Property) all properties that have `name.subName` or `name[subName]` is grouped as `Group` with nested properties of `subName`.

Example:

    {
      "prop1.a": 1,
      "prop1.b": 2,
      "prop1[c]": 3,
      "prop2.a": 4
    }

become:

    {
      "prop1": {
        "a": 1,
        "b": 2,
        "c": 3
      },
      "prop2": {
        "a": 4
      }
    }

-}
convert : Tiled.Properties.Properties -> Property
convert props =
    props
        |> Dict.toList
        |> digIn Dict.empty


{-| Get deep property:

    Property.get "a.b.c.d" prop

-}
get : String -> Property -> Maybe Property
get k prop =
    at (keyToPath k) prop


{-| Same as [`get`](Tiled-Extra-Property#get) but path is List of keys

    Property.get [ "a", "b", "c", "d" ] prop

-}
at : List String -> Property -> Maybe Property
at path prop =
    case ( path, prop ) of
        ( k :: rest, Group dict ) ->
            Dict.get k dict
                |> Maybe.andThen (at rest)

        ( [], v ) ->
            Just v

        _ ->
            Nothing


{-| Convert `Group` to `List` of its values, or get list of single `Property`
-}
values : Property -> List Property
values property =
    case property of
        Group dict ->
            Dict.values dict

        p ->
            [ p ]


{-| Convert `Group` to List of key-value pairs
-}
toList : Property -> List ( String, Property )
toList property =
    case property of
        Group dict ->
            Dict.toList dict

        p ->
            [ ( "", p ) ]


digIn : Dict String Property -> List ( String, Tiled.Properties.Property ) -> Property
digIn acc list =
    case list of
        ( key, value ) :: rest ->
            digIn (updateCrumbsInt (keyToPath key) value acc) rest

        [] ->
            Group acc


keyToPath : String -> List String
keyToPath key =
    String.replace "[" "." key
        |> String.replace "]" "."
        |> String.replace ".." "."
        |> (\a -> applyIf (String.endsWith "." a) (String.dropRight 1) a)
        |> String.split "."


updateCrumbsInt : List String -> Tiled.Properties.Property -> Dict String Property -> Dict String Property
updateCrumbsInt path val props =
    case path of
        [] ->
            props

        x :: [] ->
            Dict.insert x (prop2Prop val) props

        x :: xs ->
            let
                oldProps =
                    case Dict.get x props of
                        Just (Group currentProps) ->
                            currentProps

                        _ ->
                            Dict.empty
            in
            Dict.insert x (Group (updateCrumbsInt xs val oldProps)) props


prop2Prop v_ =
    case v_ of
        Tiled.Properties.Bool v ->
            Bool v

        Tiled.Properties.Int v ->
            Int v

        Tiled.Properties.Float v ->
            Float v

        Tiled.Properties.String v ->
            String v

        Tiled.Properties.Color v ->
            Color v

        Tiled.Properties.File v ->
            File v

        Tiled.Properties.Object v ->
            Object v


{-| -}
applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f world =
    if bool then
        f world

    else
        world
