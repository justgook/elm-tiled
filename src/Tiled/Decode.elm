module Tiled.Decode
    exposing
        ( DefaultProps
        , EmbeddedTileData
        , Layer(..)
        , LevelWith
        , Object(..)
        , Options
        , Property(..)
        , Tileset(..)
        , decode
        , decodeWith
        , defaultOptions
        , init
        )

{-| Use the `decode` to get default Level or `decodeWith` to get custom decoding version


# Default Decoding

@docs init, decode


# Custom Decoding

@docs decodeWith, defaultOptions


# Definition

@docs DefaultProps, LevelWith, Property,EmbeddedTileData, Layer, Tileset,Options, Object

-}

import BinaryBase64
import Bitwise exposing (or, shiftLeftBy)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline as Pipeline exposing (decode, hardcoded, optional, required)


-- https://robots.thoughtbot.com/5-common-json-decoders#5---conditional-decoding-based-on-a-field
-- http://eeue56.github.io/json-to-elm/


{-| Props that is used for custom props
-}
type alias DefaultProps =
    Dict String Property


{-| Custom props values
-}
type Property
    = PropBool Bool
    | PropColor String
    | PropFloat Float
    | PropFile String
    | PropInt Int
    | PropString String


{-| Extendable structure
if You would like use custom post parsers looke @decodeWith
-}
type alias LevelWith layer tileset =
    { height : Float
    , infinite : Bool
    , layers : List layer
    , nextobjectid : Int
    , orientation : String
    , renderorder : String
    , tiledversion : String
    , tileheight : Float
    , tilesets : List tileset
    , tilewidth : Float
    , kind : String
    , version : Float
    , width : Float
    , properties : DefaultProps
    }


{-| -}
init : LevelWith layer tileset
init =
    { height = 0
    , infinite = False
    , layers = []
    , nextobjectid = 0
    , orientation = ""
    , renderorder = ""
    , tiledversion = ""
    , tileheight = 0
    , tilesets = []
    , tilewidth = 0
    , kind = ""
    , version = 0
    , width = 0
    , properties = Dict.empty
    }


{-| -}
type alias Options layer tileset =
    { postLayer : Layer -> Decoder layer
    , postTilest : Tileset -> Decoder tileset
    }


{-| -}
defaultOptions : { postLayer : layer -> Decoder layer, postTilest : tileset -> Decoder tileset }
defaultOptions =
    { postLayer = Decode.succeed
    , postTilest = Decode.succeed
    }


{-| -}
decode : Decoder (LevelWith Layer Tileset)
decode =
    decodeWith defaultOptions


{-| Custom decoder
-}
decodeWith : Options layer tileset -> Decoder (LevelWith layer tileset)
decodeWith opts =
    Pipeline.decode LevelWith
        |> required "height" Decode.float
        |> required "infinite" Decode.bool
        |> required "layers" (decodeLayer |> Decode.andThen opts.postLayer |> Decode.list)
        |> required "nextobjectid" Decode.int
        |> required "orientation" Decode.string
        |> required "renderorder" Decode.string
        |> required "tiledversion" Decode.string
        |> required "tileheight" Decode.float
        |> required "tilesets" (decodeTileset |> Decode.andThen opts.postTilest |> Decode.list)
        |> required "tilewidth" Decode.float
        |> required "type" Decode.string
        |> required "version" Decode.float
        |> required "width" Decode.float
        |> Pipeline.custom propertiesDecoder


{-| Decoding properties as Dict String Poperty
-}
propertiesDecoder : Decoder DefaultProps
propertiesDecoder =
    let
        combine : List (Decoder a) -> Decoder (List a)
        combine =
            List.foldr (Decode.map2 (::)) (Decode.succeed [])

        propertyDecoder : String -> Decoder Property
        propertyDecoder typeString =
            case typeString of
                "bool" ->
                    Decode.map PropBool Decode.bool

                "color" ->
                    Decode.map PropColor Decode.string

                "float" ->
                    Decode.map PropFloat Decode.float

                "file" ->
                    Decode.map PropFile Decode.string

                "int" ->
                    Decode.map PropInt Decode.int

                "string" ->
                    Decode.map PropString Decode.string

                _ ->
                    Decode.fail <| "I can't decode the type " ++ typeString

        decodeProperty : ( String, String ) -> Decoder ( String, Property )
        decodeProperty ( propName, propType ) =
            Decode.at [ "properties", propName ]
                (propertyDecoder propType)
                |> Decode.map ((,) propName)

        propertiesDecoder : Decoder (Dict String Property)
        propertiesDecoder =
            Decode.field "propertytypes" (Decode.keyValuePairs Decode.string)
                |> Decode.map (List.map decodeProperty)
                |> Decode.andThen combine
                |> Decode.map Dict.fromList
    in
    Decode.maybe propertiesDecoder
        |> Decode.map (Maybe.withDefault Dict.empty)


{-| -}
type Tileset
    = TilesetSource SourceTileData
    | TilesetEmbedded EmbeddedTileData
    | TilesetImageCollection ImageCollectionTileData


{-| -}
type alias ImageCollectionTileData =
    { columns : Int
    , firstgid : Int
    , margin : Int
    , name : String
    , spacing : Int
    , tilecount : Int
    , tileheight : Int
    , tiles : ImageCollectionTileDataTiles
    , tilewidth : Int
    }


{-| -}
type alias ImageCollectionTileDataTile =
    { image : String
    , imageheight : Int
    , imagewidth : Int
    }


{-| -}
type alias ImageCollectionTileDataTiles =
    Dict Int ImageCollectionTileDataTile


{-| -}
decodeImageCollectionTileData : Decode.Decoder Tileset
decodeImageCollectionTileData =
    Pipeline.decode ImageCollectionTileData
        |> Pipeline.required "columns" Decode.int
        |> Pipeline.required "firstgid" Decode.int
        |> Pipeline.required "margin" Decode.int
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "spacing" Decode.int
        |> Pipeline.required "tilecount" Decode.int
        |> Pipeline.required "tileheight" Decode.int
        |> Pipeline.required "tiles" decodeImageCollectionTileDataTiles
        |> Pipeline.required "tilewidth" Decode.int
        |> Decode.map TilesetImageCollection


{-| -}
decodeImageCollectionTileDataTiles : Decoder (Dict Int ImageCollectionTileDataTile)
decodeImageCollectionTileDataTiles =
    Decode.keyValuePairs decodeImageCollectionTileDataTile
        |> Decode.andThen
            (List.foldl
                (\( i, data ) acc ->
                    case String.toInt i of
                        Ok index ->
                            acc |> Decode.andThen (Dict.insert index data >> Decode.succeed)

                        Err a ->
                            Decode.fail a
                )
                (Decode.succeed Dict.empty)
            )


{-| -}
decodeImageCollectionTileDataTile : Decode.Decoder ImageCollectionTileDataTile
decodeImageCollectionTileDataTile =
    Decode.map3 ImageCollectionTileDataTile
        (field "image" Decode.string)
        (field "imageheight" Decode.int)
        (field "imagewidth" Decode.int)


{-| -}
decodeTileset : Decoder Tileset
decodeTileset =
    Decode.oneOf [ decodeEmbeddedTileset, decodeSourceTileset, decodeImageCollectionTileData ]


{-| -}
type alias SourceTileData =
    { firstgid : Int
    , source : String
    }


{-| -}
decodeSourceTileset : Decoder Tileset
decodeSourceTileset =
    Pipeline.decode SourceTileData
        |> required "firstgid" Decode.int
        |> required "source" Decode.string
        |> Decode.map TilesetSource


{-| -}
type alias EmbeddedTileData =
    { columns : Int
    , firstgid : Int
    , image : String
    , imageheight : Int
    , imagewidth : Int
    , margin : Int
    , name : String
    , spacing : Int
    , tilecount : Int
    , tileheight : Int
    , tilewidth : Int
    , transparentcolor : String
    , tiles : Dict Int TilesData
    , properties : DefaultProps
    }


{-| -}
decodeEmbeddedTileset : Decoder Tileset
decodeEmbeddedTileset =
    Pipeline.decode EmbeddedTileData
        |> required "columns" Decode.int
        |> required "firstgid" Decode.int
        |> required "image" Decode.string
        |> required "imageheight" Decode.int
        |> required "imagewidth" Decode.int
        |> required "margin" Decode.int
        |> required "name" Decode.string
        |> required "spacing" Decode.int
        |> required "tilecount" Decode.int
        |> required "tileheight" Decode.int
        |> required "tilewidth" Decode.int
        |> optional "transparentcolor" Decode.string "none"
        |> optional "tiles" decodeTiles Dict.empty
        |> Pipeline.custom propertiesDecoder
        |> Decode.map TilesetEmbedded


{-| -}
decodeTiles : Decoder (Dict Int TilesData)
decodeTiles =
    Decode.keyValuePairs decodeTilesData
        |> Decode.andThen
            (List.foldl
                (\( i, data ) acc ->
                    case String.toInt i of
                        Ok index ->
                            acc |> Decode.andThen (Dict.insert index data >> Decode.succeed)

                        Err a ->
                            Decode.fail a
                )
                (Decode.succeed Dict.empty)
            )


{-| -}
type alias TilesData =
    TilesDataPlain {}



-- | TilesDataImage


{-| -}
type alias TilesDataPlain a =
    { a
        | animation : Maybe (List SpriteAnimation)
        , objectgroup : Maybe TilesDataObjectgroup
    }


{-| -}
type alias TilesDataObjectgroup =
    { draworder : String
    , name : String
    , objects : List Object
    , opacity : Int
    , kind : String
    , visible : Bool
    , x : Int
    , y : Int
    }


{-| -}
decodeTilesData : Decoder TilesData
decodeTilesData =
    Decode.map2 (\a b -> { animation = a, objectgroup = b })
        (Decode.maybe (field "animation" (Decode.list decodeSpriteAnimation)))
        (Decode.maybe (field "objectgroup" decodeTilesDataObjectgroup))


{-| -}
decodeTilesDataNew : Decoder (TilesDataPlain { id : Int })
decodeTilesDataNew =
    Decode.map3 (\a b c -> { animation = a, objectgroup = b, id = c })
        (Decode.maybe (field "animation" (Decode.list decodeSpriteAnimation)))
        (Decode.maybe (field "objectgroup" decodeTilesDataObjectgroup))
        (field "id" Decode.int)


{-| -}
decodeTilesDataObjectgroup : Decoder TilesDataObjectgroup
decodeTilesDataObjectgroup =
    Pipeline.decode TilesDataObjectgroup
        |> Pipeline.required "draworder" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "objects" (Decode.list decodeObject)
        |> Pipeline.required "opacity" Decode.int
        |> Pipeline.required "type" Decode.string
        |> Pipeline.required "visible" Decode.bool
        |> Pipeline.required "x" Decode.int
        |> Pipeline.required "y" Decode.int


{-| -}
type alias SpriteAnimation =
    { duration : Int
    , tileid : Int
    }


{-| -}
decodeSpriteAnimation : Decoder SpriteAnimation
decodeSpriteAnimation =
    Decode.map2 SpriteAnimation
        (field "duration" Decode.int)
        (field "tileid" Decode.int)


{-| -}
type Layer
    = ImageLayer ImageLayerData
    | TileLayer TileLayerData
    | ObjectLayer ObjectLayerData


{-| -}
type alias ImageLayerData =
    { image : String
    , name : String
    , opacity : Float
    , kind : String
    , visible : Bool
    , x : Float
    , y : Float
    , transparentcolor : String
    , properties : DefaultProps
    }


{-| -}
type alias TileLayerData =
    { data : List Int
    , height : Float
    , name : String
    , opacity : Float
    , kind : String
    , visible : Bool
    , width : Float
    , x : Float
    , y : Float
    , properties : DefaultProps
    }


{-| -}
type alias ObjectLayerData =
    { draworder : String
    , name : String
    , objects : List Object
    , opacity : Float
    , kind : String
    , visible : Bool
    , x : Int
    , y : Int
    , properties : DefaultProps
    }


{-| -}
decodeLayer : Decoder Layer
decodeLayer =
    field "type" Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "tilelayer" ->
                        Decode.map TileLayer
                            decodeTileLayer

                    "imagelayer" ->
                        Decode.map ImageLayer
                            decodeImageLayer

                    "objectgroup" ->
                        Decode.map ObjectLayer
                            decodeObjectLayer

                    _ ->
                        Decode.fail ("Invalid layer type: " ++ string)
            )


{-| -}
decodeImageLayer : Decoder ImageLayerData
decodeImageLayer =
    Pipeline.decode ImageLayerData
        |> required "image" Decode.string
        |> required "name" Decode.string
        |> required "opacity" Decode.float
        |> required "type" Decode.string
        |> required "visible" Decode.bool
        |> required "x" Decode.float
        |> required "y" Decode.float
        |> optional "transparentcolor" Decode.string "none"
        |> Pipeline.custom propertiesDecoder


{-| -}
decodeTileLayer : Decoder TileLayerData
decodeTileLayer =
    Pipeline.decode (,)
        |> optional "encoding" Decode.string "none"
        |> optional "compression" Decode.string "none"
        |> Decode.andThen
            (\( encoding, compression ) ->
                Pipeline.decode TileLayerData
                    |> required "data" (decodeTileLayerData encoding compression)
                    |> required "height" Decode.float
                    |> required "name" Decode.string
                    |> required "opacity" Decode.float
                    |> required "type" Decode.string
                    |> required "visible" Decode.bool
                    |> required "width" Decode.float
                    |> required "x" Decode.float
                    |> required "y" Decode.float
                    |> Pipeline.custom propertiesDecoder
            )


decodeTileLayerData : String -> String -> Decoder (List Int)
decodeTileLayerData encoding compression =
    if compression /= "none" then
        Decode.fail "Tile layer compression not supported yet"
    else if encoding == "base64" then
        Decode.string
            |> Decode.andThen
                (\string ->
                    string
                        |> BinaryBase64.decode
                        |> Result.map (\data -> Decode.succeed (convertTilesData data))
                        |> resultExtract (\err -> Decode.fail err)
                )
    else
        Decode.list Decode.int


shiftValues : List Int
shiftValues =
    [ 0, 8, 16, 24 ]


convertTilesData : BinaryBase64.ByteString -> List Int
convertTilesData octets =
    let
        ( dword, rest ) =
            ( List.take 4 octets, List.drop 4 octets )

        value =
            zip shiftValues dword
                |> List.foldl
                    (\( shiftValues, octet ) acc ->
                        or (octet |> shiftLeftBy shiftValues) acc
                    )
                    0
    in
    if List.isEmpty rest then
        [ value ]
    else
        value :: convertTilesData rest


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)


resultExtract : (e -> a) -> Result e a -> a
resultExtract f x =
    -- http://package.elm-lang.org/packages/elm-community/result-extra/2.2.0/Result-Extra
    case x of
        Ok a ->
            a

        Err e ->
            f e


{-| -}
decodeObjectLayer : Decoder ObjectLayerData
decodeObjectLayer =
    Pipeline.decode ObjectLayerData
        |> required "draworder" Decode.string
        |> required "name" Decode.string
        |> required "objects" (Decode.list decodeObject)
        |> required "opacity" Decode.float
        |> required "type" Decode.string
        |> required "visible" Decode.bool
        |> required "x" Decode.int
        |> required "y" Decode.int
        |> Pipeline.custom propertiesDecoder


{-| -}
type Object
    = ObjectRectangle ObjectRectangleData
    | ObjectPoint ObjectPointData
    | ObjectPolygon ObjectPolygonData
    | ObjectEllipse ObjectRectangleData
    | ObjectPolyLine ObjectPolygonData
    | ObjectTile ObjectTileleData


when : Decoder a -> (a -> Bool) -> Decoder b -> Decoder b
when checkDecoder expected actualDecoder =
    -- http://package.elm-lang.org/packages/zwilias/json-decode-exploration/5.0.0/Json-Decode-Exploration#check
    checkDecoder
        |> Decode.andThen
            (\actual ->
                if expected actual then
                    actualDecoder
                else
                    Decode.fail <|
                        "Verification failed, expected '"
                            ++ toString expected
                            ++ "'."
            )


{-| -}
decodeObject : Decoder Object
decodeObject =
    let
        point =
            Decode.map ObjectPoint decodeObjectPoint
                |> when (field "point" Decode.bool) ((==) True)

        elipse =
            Decode.map ObjectEllipse decodeObjectRectangle
                |> when (field "ellipse" Decode.bool) ((==) True)

        polygon =
            Decode.map ObjectPolygon (decodeObjectPolygonData "polygon")

        polyline =
            Decode.map ObjectPolyLine (decodeObjectPolygonData "polyline")

        tile =
            Decode.map ObjectTile decodeObjectTile
                |> when (field "gid" Decode.int) ((<) 0)

        rectangle =
            Decode.map ObjectRectangle decodeObjectRectangle
    in
    Decode.oneOf
        [ point
        , elipse
        , tile
        , polygon
        , polyline
        , rectangle
        ]


{-| -}
type alias ObjectPolygonData =
    { height : Float
    , id : Int
    , name : String
    , polygon : List ObjectPolygonPoint
    , rotation : Float
    , kind : String
    , visible : Bool
    , width : Float
    , x : Float
    , y : Float
    , properties : DefaultProps
    }


{-| -}
type alias ObjectPolygonPoint =
    { x : Float
    , y : Float
    }


{-| -}
decodeObjectPolygonPoint : Decoder ObjectPolygonPoint
decodeObjectPolygonPoint =
    Decode.map2 ObjectPolygonPoint
        (field "x" Decode.float)
        (field "y" Decode.float)


{-| -}
decodeObjectPolygonData : String -> Decoder ObjectPolygonData
decodeObjectPolygonData key =
    Pipeline.decode ObjectPolygonData
        |> required "height" Decode.float
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required key (Decode.list decodeObjectPolygonPoint)
        |> required "rotation" Decode.float
        |> required "type" Decode.string
        |> required "visible" Decode.bool
        |> required "width" Decode.float
        |> required "x" Decode.float
        |> required "y" Decode.float
        |> Pipeline.custom propertiesDecoder


{-| -}
type alias ObjectRectangleData =
    { height : Float
    , id : Int
    , name : String
    , rotation : Float
    , kind : String
    , visible : Bool
    , width : Float
    , x : Float
    , y : Float
    , properties : DefaultProps
    }


{-| -}
decodeObjectRectangle : Decoder ObjectRectangleData
decodeObjectRectangle =
    Pipeline.decode ObjectRectangleData
        |> required "height" Decode.float
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "rotation" Decode.float
        |> required "type" Decode.string
        |> required "visible" Decode.bool
        |> required "width" Decode.float
        |> required "x" Decode.float
        |> required "y" Decode.float
        |> Pipeline.custom propertiesDecoder


{-| -}
type alias ObjectTileleData =
    { gid : Int
    , height : Float
    , id : Int
    , name : String
    , rotation : Float
    , kind : String
    , visible : Bool
    , width : Float
    , x : Float
    , y : Float
    , properties : DefaultProps
    }


{-| -}
decodeObjectTile : Decoder ObjectTileleData
decodeObjectTile =
    Pipeline.decode ObjectTileleData
        |> required "gid" Decode.int
        |> required "height" Decode.float
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "rotation" Decode.float
        |> required "type" Decode.string
        |> required "visible" Decode.bool
        |> required "width" Decode.float
        |> required "x" Decode.float
        |> required "y" Decode.float
        |> Pipeline.custom propertiesDecoder


{-| -}
type alias ObjectPointData =
    { id : Int
    , name : String
    , rotation : Float
    , kind : String
    , visible : Bool
    , x : Float
    , y : Float
    , properties : DefaultProps
    }


{-| -}
decodeObjectPoint : Decoder ObjectPointData
decodeObjectPoint =
    Pipeline.decode ObjectPointData
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "rotation" Decode.float
        |> required "type" Decode.string
        |> required "visible" Decode.bool
        |> required "x" Decode.float
        |> required "y" Decode.float
        |> Pipeline.custom propertiesDecoder
