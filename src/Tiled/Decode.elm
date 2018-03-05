module Tiled.Decode exposing (DefaultProps, Layer, LayerWith(..), Level, LevelWith, Object(..), Options, Properties(..), Tileset(..), decode, decodeWith, defaultOptions, init, initWith)

{-| Use the `decode` to get default Level or `decodeWith` to get custom decoding version


# Default Decoding

@docs init, decode


# Custom Decoding

@docs initWith, decodeWith, defaultOptions


# Definition

@docs Level, Layer, Tileset, DefaultProps, Object, Properties, Options, LayerWith, LevelWith

-}

import BinaryBase64
import Bitwise exposing (or, shiftLeftBy)
import Dict exposing (Dict)
import Json.Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


-- https://robots.thoughtbot.com/5-common-json-decoders#5---conditional-decoding-based-on-a-field
-- http://eeue56.github.io/json-to-elm/


{-| Default decode structure
-}
type alias Level =
    LevelWith DefaultProps Layer Tileset


{-| Props that is used for custom props
-}
type alias DefaultProps =
    Dict String Properties


{-| -}
type alias LevelWith customProperties layer tileset =
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
    , properties : customProperties
    }


{-| Crate Empty data level
-}
init : Level
init =
    initWith Dict.empty


{-| -}
initWith : props -> LevelWith props layer tileset
initWith props =
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
    , properties = props
    }


{-| -}
decodeWith : OptionsWith a b c d e f -> Json.Decode.Decoder (LevelWith f d e)
decodeWith opts =
    let
        layerOpts =
            { imageLayerCustomPropertiesDecode = opts.imageLayerCustomPropertiesDecode
            , imageLayerDefaultCustomProperties = opts.imageLayerDefaultCustomProperties
            , tileLayerCustomPropertiesDecode = opts.tileLayerCustomPropertiesDecode
            , tileLayerDefaultCustomProperties = opts.tileLayerDefaultCustomProperties
            , objectLayerCustomPropertiesDecode = opts.objectLayerCustomPropertiesDecode
            , objectLayerDefaultCustomProperties = opts.objectLayerDefaultCustomProperties
            }
    in
    Json.Decode.Pipeline.decode LevelWith
        |> required "height" Json.Decode.float
        |> required "infinite" Json.Decode.bool
        |> required "layers" (opts.decodeLayer layerOpts |> Json.Decode.list)
        |> required "nextobjectid" Json.Decode.int
        |> required "orientation" Json.Decode.string
        |> required "renderorder" Json.Decode.string
        |> required "tiledversion" Json.Decode.string
        |> required "tileheight" Json.Decode.float
        |> required "tilesets" (opts.decodeTileset |> Json.Decode.list)
        |> required "tilewidth" Json.Decode.float
        |> required "type" Json.Decode.string
        |> required "version" Json.Decode.float
        |> required "width" Json.Decode.float
        |> optional "properties" (opts.properties opts.defaultCustomProperties) opts.defaultCustomProperties


{-| -}
type Properties
    = PropBool Bool
    | PropColor String
    | PropFloat Float
    | PropFile String
    | PropInt Int
    | PropString String


{-| -}
type alias LayerOptionsWith a b c =
    { imageLayerCustomPropertiesDecode : a -> Json.Decode.Decoder a
    , imageLayerDefaultCustomProperties : a
    , tileLayerCustomPropertiesDecode : b -> Json.Decode.Decoder b
    , tileLayerDefaultCustomProperties : b
    , objectLayerCustomPropertiesDecode : c -> Json.Decode.Decoder c
    , objectLayerDefaultCustomProperties : c
    }


type alias LayerOptions =
    LayerOptionsWith DefaultProps DefaultProps DefaultProps


{-|

    type alias Options =
        { decodeLayer : LayerOptions -> Json.Decode.Decoder Layer
        , decodeTileset : Json.Decode.Decoder Tileset
        , defaultCustomProperties : DefaultProps
        , imageLayerCustomPropertiesDecode : DefaultProps -> Json.Decode.Decoder DefaultProps
        , imageLayerDefaultCustomProperties : DefaultProps
        , objectLayerCustomPropertiesDecode : DefaultProps -> Json.Decode.Decoder DefaultProps
        , objectLayerDefaultCustomProperties : DefaultProps
        , properties : DefaultProps -> Json.Decode.Decoder DefaultProps
        , tileLayerCustomPropertiesDecode : DefaultProps -> Json.Decode.Decoder DefaultProps
        , tileLayerDefaultCustomProperties : DefaultProps
        }

-}
type alias Options =
    OptionsWith DefaultProps DefaultProps DefaultProps Layer Tileset DefaultProps


{-| -}
type alias OptionsWith a b c d e f =
    { decodeLayer : LayerOptionsWith a b c -> Json.Decode.Decoder d
    , decodeTileset : Json.Decode.Decoder e
    , defaultCustomProperties : f
    , properties : f -> Json.Decode.Decoder f
    , imageLayerCustomPropertiesDecode : a -> Json.Decode.Decoder a
    , imageLayerDefaultCustomProperties : a
    , tileLayerCustomPropertiesDecode : b -> Json.Decode.Decoder b
    , tileLayerDefaultCustomProperties : b
    , objectLayerCustomPropertiesDecode : c -> Json.Decode.Decoder c
    , objectLayerDefaultCustomProperties : c
    }


{-| -}
defaultOptions : Options
defaultOptions =
    let
        properties =
            \_ -> decodeCustomProperties

        defaultCustomProperties =
            Dict.empty
    in
    { decodeLayer = \layer -> decodeLayerDefault layer
    , decodeTileset = decodeTilesetDefault
    , properties = properties
    , defaultCustomProperties = defaultCustomProperties
    , imageLayerCustomPropertiesDecode = properties
    , imageLayerDefaultCustomProperties = defaultCustomProperties
    , tileLayerCustomPropertiesDecode = properties
    , tileLayerDefaultCustomProperties = defaultCustomProperties
    , objectLayerCustomPropertiesDecode = properties
    , objectLayerDefaultCustomProperties = defaultCustomProperties
    }


{-| -}
decodeCustomProperties : Json.Decode.Decoder (Dict String Properties)
decodeCustomProperties =
    Json.Decode.list
        (Json.Decode.Pipeline.decode identity
            |> required "type" Json.Decode.string
            |> Json.Decode.andThen
                (\kind ->
                    Json.Decode.Pipeline.decode (,)
                        |> required "name" Json.Decode.string
                        |> required "value"
                            (case kind of
                                "bool" ->
                                    Json.Decode.map PropBool Json.Decode.bool

                                "color" ->
                                    Json.Decode.map PropColor Json.Decode.string

                                "float" ->
                                    Json.Decode.map PropFloat Json.Decode.float

                                "file" ->
                                    Json.Decode.map PropFile Json.Decode.string

                                "int" ->
                                    Json.Decode.map PropInt Json.Decode.int

                                _ ->
                                    Json.Decode.oneOf
                                        [ Json.Decode.map toString Json.Decode.float
                                        , Json.Decode.string
                                        ]
                                        |> Json.Decode.map PropString
                            )
                )
        )
        |> Json.Decode.map Dict.fromList


{-| -}
decode : Json.Decode.Decoder Level
decode =
    decodeWith defaultOptions


{-| -}
type Tileset
    = TilesetSource SourceTileData
    | TilesetEmbedded EmbeddedTileData


{-| -}
decodeTilesetDefault : Json.Decode.Decoder Tileset
decodeTilesetDefault =
    Json.Decode.oneOf [ decodeEmbeddedTileset, decodeSourceTileset ]


{-| -}
type alias SourceTileData =
    { firstgid : Int
    , source : String
    }


{-| -}
decodeSourceTileset : Json.Decode.Decoder Tileset
decodeSourceTileset =
    Json.Decode.Pipeline.decode SourceTileData
        |> required "firstgid" Json.Decode.int
        |> required "source" Json.Decode.string
        |> Json.Decode.map TilesetSource


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
    }


{-| -}
decodeEmbeddedTileset : Json.Decode.Decoder Tileset
decodeEmbeddedTileset =
    Json.Decode.Pipeline.decode EmbeddedTileData
        |> required "columns" Json.Decode.int
        |> required "firstgid" Json.Decode.int
        |> required "image" Json.Decode.string
        |> required "imageheight" Json.Decode.int
        |> required "imagewidth" Json.Decode.int
        |> required "margin" Json.Decode.int
        |> required "name" Json.Decode.string
        |> required "spacing" Json.Decode.int
        |> required "tilecount" Json.Decode.int
        |> required "tileheight" Json.Decode.int
        |> required "tilewidth" Json.Decode.int
        |> required "transparentcolor" Json.Decode.string
        |> optional "tiles" decodeTiles Dict.empty
        |> Json.Decode.map TilesetEmbedded


{-| -}
decodeTiles : Json.Decode.Decoder (Dict Int TilesData)
decodeTiles =
    let
        newFormat =
            Json.Decode.list decodeTilesDataNew
                |> Json.Decode.andThen
                    (List.foldl
                        (\data acc ->
                            acc |> Json.Decode.andThen (Dict.insert data.id { animation = data.animation, objectgroup = data.objectgroup } >> Json.Decode.succeed)
                        )
                        (Json.Decode.succeed Dict.empty)
                    )

        oldFormat =
            Json.Decode.keyValuePairs decodeTilesData
                |> Json.Decode.andThen
                    (List.foldl
                        (\( i, data ) acc ->
                            case String.toInt i of
                                Ok index ->
                                    acc |> Json.Decode.andThen (Dict.insert index data >> Json.Decode.succeed)

                                Err a ->
                                    Json.Decode.fail a
                        )
                        (Json.Decode.succeed Dict.empty)
                    )
    in
    Json.Decode.oneOf [ oldFormat, newFormat ]


{-| -}
type alias TilesData =
    TilesDataPlain {}


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
decodeTilesData : Json.Decode.Decoder { animation : Maybe (List SpriteAnimation), objectgroup : Maybe TilesDataObjectgroup }
decodeTilesData =
    Json.Decode.map2 (\a b -> { animation = a, objectgroup = b })
        (Json.Decode.maybe (field "animation" (Json.Decode.list decodeSpriteAnimation)))
        (Json.Decode.maybe (field "objectgroup" decodeTilesDataObjectgroup))


{-| -}
decodeTilesDataNew : Json.Decode.Decoder (TilesDataPlain { id : Int })
decodeTilesDataNew =
    Json.Decode.map3 (\a b c -> { animation = a, objectgroup = b, id = c })
        (Json.Decode.maybe (field "animation" (Json.Decode.list decodeSpriteAnimation)))
        (Json.Decode.maybe (field "objectgroup" decodeTilesDataObjectgroup))
        (field "id" Json.Decode.int)


{-| -}
decodeTilesDataObjectgroup : Json.Decode.Decoder TilesDataObjectgroup
decodeTilesDataObjectgroup =
    Json.Decode.Pipeline.decode TilesDataObjectgroup
        |> Json.Decode.Pipeline.required "draworder" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "objects" (Json.Decode.list decodeObject)
        |> Json.Decode.Pipeline.required "opacity" Json.Decode.int
        |> Json.Decode.Pipeline.required "type" Json.Decode.string
        |> Json.Decode.Pipeline.required "visible" Json.Decode.bool
        |> Json.Decode.Pipeline.required "x" Json.Decode.int
        |> Json.Decode.Pipeline.required "y" Json.Decode.int


{-| -}
type alias SpriteAnimation =
    { duration : Int
    , tileid : Int
    }


{-| -}
decodeSpriteAnimation : Json.Decode.Decoder SpriteAnimation
decodeSpriteAnimation =
    Json.Decode.map2 SpriteAnimation
        (field "duration" Json.Decode.int)
        (field "tileid" Json.Decode.int)


{-| -}
type alias Layer =
    LayerWith DefaultProps DefaultProps DefaultProps


{-| -}
type LayerWith imageLayerProps tileLayerProps objectLayerProps
    = ImageLayer (ImageLayerDataWith imageLayerProps)
    | TileLayer (TileLayerDataWith tileLayerProps)
    | ObjectLayer (ObjectLayerDataWith objectLayerProps)


{-| -}
type alias ImageLayerData =
    ImageLayerDataWith (Dict String String)


{-| -}
type alias ImageLayerDataWith customProperties =
    { image : String
    , name : String
    , opacity : Float
    , kind : String
    , visible : Bool
    , x : Float
    , y : Float
    , transparentcolor : String
    , properties : customProperties
    }


{-| -}
type alias TileLayerData =
    TileLayerDataWith (Dict String String)


{-| -}
type alias TileLayerDataWith customProperties =
    { data : List Int
    , height : Float
    , name : String
    , opacity : Float
    , kind : String
    , visible : Bool
    , width : Float
    , x : Float
    , y : Float
    , properties : customProperties
    }


{-| -}
type alias ObjectLayerData =
    ObjectLayerDataWith (Dict String String)


{-| -}
type alias ObjectLayerDataWith customProperties =
    { draworder : String
    , name : String
    , objects : List Object
    , opacity : Float
    , kind : String
    , visible : Bool
    , x : Int
    , y : Int
    , properties : customProperties
    }


{-| -}
decodeLayerDefault : LayerOptions -> Json.Decode.Decoder Layer
decodeLayerDefault { imageLayerCustomPropertiesDecode, imageLayerDefaultCustomProperties, tileLayerCustomPropertiesDecode, tileLayerDefaultCustomProperties, objectLayerCustomPropertiesDecode, objectLayerDefaultCustomProperties } =
    field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "tilelayer" ->
                        Json.Decode.map TileLayer
                            (decodeTileLayer
                                |> optional "properties" (tileLayerCustomPropertiesDecode tileLayerDefaultCustomProperties) tileLayerDefaultCustomProperties
                            )

                    "imagelayer" ->
                        Json.Decode.map ImageLayer
                            (decodeImageLayer
                                |> optional "properties" (imageLayerCustomPropertiesDecode imageLayerDefaultCustomProperties) imageLayerDefaultCustomProperties
                            )

                    "objectgroup" ->
                        Json.Decode.map ObjectLayer
                            (decodeObjectLayer
                                |> optional "properties" (objectLayerCustomPropertiesDecode objectLayerDefaultCustomProperties) objectLayerDefaultCustomProperties
                            )

                    _ ->
                        Json.Decode.fail ("Invalid layer type: " ++ string)
            )


{-| -}
decodeImageLayer : Json.Decode.Decoder (customProperties -> ImageLayerDataWith customProperties)
decodeImageLayer =
    Json.Decode.Pipeline.decode ImageLayerDataWith
        |> required "image" Json.Decode.string
        |> required "name" Json.Decode.string
        |> required "opacity" Json.Decode.float
        |> required "type" Json.Decode.string
        |> required "visible" Json.Decode.bool
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float
        |> optional "transparentcolor" Json.Decode.string "none"


{-| -}
decodeTileLayer : Json.Decode.Decoder (customProperties -> TileLayerDataWith customProperties)
decodeTileLayer =
    Json.Decode.Pipeline.decode (,)
        |> optional "encoding" Json.Decode.string "none"
        |> optional "compression" Json.Decode.string "none"
        |> Json.Decode.andThen
            (\( encoding, compression ) ->
                Json.Decode.Pipeline.decode TileLayerDataWith
                    |> required "data" (decodeTileLayerData encoding compression)
                    |> required "height" Json.Decode.float
                    |> required "name" Json.Decode.string
                    |> required "opacity" Json.Decode.float
                    |> required "type" Json.Decode.string
                    |> required "visible" Json.Decode.bool
                    |> required "width" Json.Decode.float
                    |> required "x" Json.Decode.float
                    |> required "y" Json.Decode.float
            )


{-| -}
decodeTileLayerData : String -> String -> Json.Decode.Decoder (List Int)
decodeTileLayerData encoding compression =
    if compression /= "none" then
        Json.Decode.fail "Tile layer compression not supported yet"
    else if encoding == "base64" then
        Json.Decode.string
            |> Json.Decode.andThen
                (\string ->
                    string
                        |> BinaryBase64.decode
                        |> Result.map (\data -> Json.Decode.succeed (convertTilesData data))
                        |> resultExtract (\err -> Json.Decode.fail err)
                )
    else
        Json.Decode.list Json.Decode.int


{-| -}
shiftValues : List Int
shiftValues =
    [ 0, 8, 16, 24 ]


{-| -}
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



-- http://package.elm-lang.org/packages/elm-community/result-extra/2.2.0/Result-Extra


{-| -}
resultExtract : (e -> a) -> Result e a -> a
resultExtract f x =
    case x of
        Ok a ->
            a

        Err e ->
            f e


{-| -}
decodeObjectLayer : Json.Decode.Decoder (customProperties -> ObjectLayerDataWith customProperties)
decodeObjectLayer =
    Json.Decode.Pipeline.decode ObjectLayerDataWith
        |> required "draworder" Json.Decode.string
        |> required "name" Json.Decode.string
        |> required "objects" (Json.Decode.list decodeObject)
        |> required "opacity" Json.Decode.float
        |> required "type" Json.Decode.string
        |> required "visible" Json.Decode.bool
        |> required "x" Json.Decode.int
        |> required "y" Json.Decode.int


{-| -}
type Object
    = ObjectRectangle ObjectRectangleData
    | ObjectPoint ObjectPointData
    | ObjectPolygon ObjectPolygonData
    | ObjectEllipse ObjectRectangleData
      -- | ObjectPolyLine
    | ObjectTile ObjectTileleData


{-| -}
decodeObject : Json.Decode.Decoder Object
decodeObject =
    -- maybe use Json.Decode.Pipeline.resolve
    Json.Decode.Pipeline.decode (,,,)
        |> optional "point" Json.Decode.bool False
        |> optional "ellipse" Json.Decode.bool False
        |> optional "polygon" (Json.Decode.list decodeObjectPolygonPoint) []
        |> optional "gid" Json.Decode.int 0
        |> Json.Decode.andThen
            (\( point, ellipse, polygon, gid ) ->
                case ( point, ellipse, polygon, gid ) of
                    ( False, False, [], 0 ) ->
                        Json.Decode.map ObjectRectangle decodeObjectRectangle

                    ( True, _, [], 0 ) ->
                        Json.Decode.map ObjectPoint decodeObjectPoint

                    ( _, True, [], 0 ) ->
                        Json.Decode.map ObjectEllipse decodeObjectRectangle

                    ( _, _, list, 0 ) ->
                        Json.Decode.map ObjectPolygon decodeObjectPolygonData

                    ( _, _, _, gid ) ->
                        Json.Decode.map ObjectTile decodeObjectTile
            )


{-| -}
type alias ObjectPolygonData =
    { height : Float
    , id : Int
    , name : String
    , polygon : List ObjectPolygonPoint
    , rotation : Int
    , kind : String
    , visible : Bool
    , width : Float
    , x : Float
    , y : Float
    }


{-| -}
type alias ObjectPolygonPoint =
    { x : Float
    , y : Float
    }


{-| -}
decodeObjectPolygonPoint : Json.Decode.Decoder ObjectPolygonPoint
decodeObjectPolygonPoint =
    Json.Decode.map2 ObjectPolygonPoint
        (field "x" Json.Decode.float)
        (field "y" Json.Decode.float)


{-| -}
decodeObjectPolygonData : Json.Decode.Decoder ObjectPolygonData
decodeObjectPolygonData =
    Json.Decode.Pipeline.decode ObjectPolygonData
        |> required "height" Json.Decode.float
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> required "polygon" (Json.Decode.list decodeObjectPolygonPoint)
        |> required "rotation" Json.Decode.int
        |> required "type" Json.Decode.string
        |> required "visible" Json.Decode.bool
        |> required "width" Json.Decode.float
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


{-| -}
type alias ObjectRectangleData =
    { height : Float
    , id : Int
    , name : String
    , rotation : Int
    , kind : String
    , visible : Bool
    , width : Float
    , x : Float
    , y : Float
    }


{-| -}
decodeObjectRectangle : Json.Decode.Decoder ObjectRectangleData
decodeObjectRectangle =
    Json.Decode.Pipeline.decode ObjectRectangleData
        |> required "height" Json.Decode.float
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> required "rotation" Json.Decode.int
        |> required "type" Json.Decode.string
        |> required "visible" Json.Decode.bool
        |> required "width" Json.Decode.float
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


{-| -}
type alias ObjectTileleData =
    { gid : Int
    , height : Float
    , id : Int
    , name : String
    , rotation : Int
    , kind : String
    , visible : Bool
    , width : Float
    , x : Float
    , y : Float
    }


{-| -}
decodeObjectTile : Json.Decode.Decoder ObjectTileleData
decodeObjectTile =
    Json.Decode.Pipeline.decode ObjectTileleData
        |> required "gid" Json.Decode.int
        |> required "height" Json.Decode.float
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> required "rotation" Json.Decode.int
        |> required "type" Json.Decode.string
        |> required "visible" Json.Decode.bool
        |> required "width" Json.Decode.float
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


{-| -}
type alias ObjectPointData =
    { id : Int
    , name : String
    , rotation : Int
    , kind : String
    , visible : Bool
    , x : Float
    , y : Float
    }


{-| -}
decodeObjectPoint : Json.Decode.Decoder ObjectPointData
decodeObjectPoint =
    Json.Decode.Pipeline.decode ObjectPointData
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> required "rotation" Json.Decode.int
        |> required "type" Json.Decode.string
        |> required "visible" Json.Decode.bool
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float
