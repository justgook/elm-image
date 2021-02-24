module Image.Internal.ImageData exposing
    ( EncodeOptions
    , Image(..)
    , Order(..)
    , PixelFormat(..)
    , bytesPerPixel
    , defaultOptions
    , dimensions
    , eval
    , forceColor
    , getInfo
    , map
    , toArray
    , toArray2d
    , toList
    , toList2d
    , toPalette
    , width
    )

import Array exposing (Array)
import Bitwise
import Dict
import Image.Internal.Array2D as Array2D exposing (Array2D)
import Image.Internal.Meta as Metadata exposing (BmpBitsPerPixel(..), FromDataColor(..), Header(..), PngColor(..))


type Image
    = ImageEval Header (Array2D Int)
    | Lazy Header (Header -> Image)


eval : Image -> Image
eval image =
    case image of
        Lazy meta fn ->
            fn meta

        _ ->
            image


toPalette : Image -> ( Array Int, Array Int )
toPalette image =
    image
        |> toArray
        |> Array.foldl
            (\px ( palette, indexes ) ->
                let
                    i =
                        Dict.get px palette
                in
                case i of
                    Just index ->
                        ( palette, Array.push index indexes )

                    Nothing ->
                        let
                            index =
                                Dict.size palette
                        in
                        ( Dict.insert px index palette, Array.push index indexes )
            )
            ( Dict.empty, Array.empty )
        |> Tuple.mapFirst
            (\p ->
                p
                    |> Dict.toList
                    |> List.sortBy Tuple.second
                    |> List.foldl (Tuple.first >> Array.push) Array.empty
            )


forceColor : Metadata.FromDataColor -> Image -> Image
forceColor color image =
    case image of
        ImageEval meta im ->
            ImageEval (toFromData color meta) im

        Lazy meta fn ->
            case fn meta of
                Lazy _ _ ->
                    image

                newData ->
                    forceColor color newData


toFromData : Metadata.FromDataColor -> Header -> Header
toFromData color meta =
    let
        dim =
            Metadata.dimensions meta
    in
    FromData
        { width = dim.width
        , height = dim.height
        , color = color
        }


type alias EncodeOptions =
    { format : PixelFormat
    , order : Order
    }


type PixelFormat
    = RGBA
    | RGB
    | LUMINANCE_ALPHA
    | ALPHA


{-| Pixel render order in image
-}
type Order
    = RightDown
    | RightUp
    | LeftDown
    | LeftUp


{-| -}
defaultOptions : EncodeOptions
defaultOptions =
    { format = RGBA
    , order = RightDown
    }


{-| -}
map : (Int -> Int) -> Image -> Image
map fn image =
    case image of
        ImageEval meta arr ->
            ImageEval meta (Array.map (Array.map fn) arr)

        Lazy meta fn_ ->
            case fn_ meta of
                Lazy _ _ ->
                    image

                newData ->
                    map fn newData


toList : Image -> List Int
toList image =
    case image of
        ImageEval _ arr ->
            Array.foldr (\line acc1 -> Array.foldr (\px acc2 -> px :: acc2) acc1 line) [] arr

        Lazy meta fn ->
            case fn meta of
                Lazy _ _ ->
                    []

                newData ->
                    toList newData


toList2d : Image -> List (List Int)
toList2d info =
    case info of
        ImageEval _ arr ->
            Array.foldr
                (\line acc1 ->
                    Array.foldr (\px acc2 -> px :: acc2) [] line
                        |> (\l -> l :: acc1)
                )
                []
                arr

        Lazy meta fn ->
            case fn meta of
                Lazy _ _ ->
                    []

                newData ->
                    toList2d newData


toArray : Image -> Array Int
toArray image =
    case image of
        ImageEval _ arr ->
            Array.foldr Array.append Array.empty arr

        Lazy meta fn ->
            case fn meta of
                Lazy _ _ ->
                    Array.empty

                newData ->
                    toArray newData


toArray2d : Image -> Array (Array Int)
toArray2d image =
    case image of
        ImageEval _ arr ->
            arr

        Lazy meta fn ->
            case fn meta of
                Lazy _ _ ->
                    Array.empty

                newData ->
                    toArray2d newData


getInfo : Image -> Header
getInfo image =
    case image of
        ImageEval meta _ ->
            meta

        Lazy meta _ ->
            meta


dimensions : Image -> { width : Int, height : Int }
dimensions image =
    getInfo image |> Metadata.dimensions


width : Image -> Int
width image =
    (dimensions image).width


bytesPerPixel : Header -> Int
bytesPerPixel meta =
    case meta of
        Png { color } ->
            case color of
                Greyscale _ ->
                    1

                GreyscaleAlpha _ ->
                    2

                TrueColour _ ->
                    3

                TrueColourAlpha _ ->
                    4

                IndexedColour _ ->
                    4

        Bmp { bitsPerPixel } ->
            case bitsPerPixel of
                BmpBitsPerPixel8 ->
                    1

                BmpBitsPerPixel16 ->
                    2

                BmpBitsPerPixel24 ->
                    3

                BmpBitsPerPixel32 ->
                    4

        FromData { color } ->
            case color of
                FromDataChannel1 _ ->
                    1

                FromDataChannel2 _ ->
                    2

                FromDataChannel3 _ ->
                    3

                FromDataChannel4 _ ->
                    4

        Gif _ ->
            1
