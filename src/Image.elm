module Image exposing
    ( Image
    , decode
    , encodeBmp, encodePng
    , fromList, fromList2d, fromArray, fromArray2d, fromBytes
    , toList, toList2d, toArray, toArray2d
    , Width, Height
    )

{-|

@docs Image


# Decoding

@docs decode


# Encoding

@docs encodeBmp, encodePng


# Construct

@docs fromList, fromList2d, fromArray, fromArray2d, fromBytes


# Destruct

@docs toList, toList2d, toArray, toArray2d


# Helper Types

@docs Width, Height

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode exposing (Decoder)
import Image.Internal.BMP as BMP
import Image.Internal.ImageData exposing (Image(..), defaultOptions)
import Image.Internal.PNG as PNG
import Maybe exposing (Maybe)


{-| Just syntax sugar for easier understanding
-}
type alias Width =
    Int


{-| Just syntax sugar for easier understanding
-}
type alias Height =
    Int


{-| Data Model representing Image data, that can be used to create image, or convert primitives to use image pixels as data
-}
type alias Image =
    Image.Internal.ImageData.Image


{-| Create [`Image`](#Image) of `List Int` where each Int is `0xRRGGBBAA`
-}
fromList : Width -> List Int -> Image
fromList =
    List defaultOptions


{-| Create [`Image`](#Image) of `List (List Int)` where each Int is `0xRRGGBBAA`
-}
fromList2d : List (List Int) -> Image
fromList2d =
    List2d defaultOptions


{-| Create [`Image`](#Image) of `Array Int` where each Int is `0xRRGGBBAA`
-}
fromArray : Width -> Array Int -> Image
fromArray =
    Array defaultOptions


{-| Create [`Image`](#Image) of `Array (Array Int)` where each Int is `0xRRGGBBAA`
-}
fromArray2d : Array (Array Int) -> Image
fromArray2d =
    Array2d defaultOptions


{-| Create [`Image`](#Image) of Bytes where each pixel is `unsignedInt32` - `0xRRGGBBAA`
-}
fromBytes : Decoder Image -> Bytes -> Image
fromBytes =
    Bytes defaultOptions


{-| Take [`Image`](#Image) of and converts it to `List Int` where each Int is `0xRRGGBBAA`
-}
toList : Image -> List Int
toList =
    Image.Internal.ImageData.toList


{-| Take [`Image`](#Image) of and converts it to matrix `List (List Int)` where each Int is `0xRRGGBBAA`
-}
toList2d : Image -> List (List Int)
toList2d =
    Image.Internal.ImageData.toList2d


{-| Take [`Image`](#Image) of and converts it to `Array Int` where each Int is `0xRRGGBBAA`
-}
toArray : Image -> Array Int
toArray =
    Image.Internal.ImageData.toArray


{-| Take [`Image`](#Image) of and converts it to matrix `Array (Array Int)` where each Int is `0xRRGGBBAA`
-}
toArray2d : Image -> Array (Array Int)
toArray2d =
    Image.Internal.ImageData.toArray2d


{-| Portable Network Graphics (PNG) is a raster-graphics file-format that supports lossless data compression. PNG was developed as an improved, non-patented replacement for Graphics Interchange Format (GIF).

PNG supports palette-based images (with palettes of 24-bit RGB or 32-bit RGBA colors), grayscale images (with or without alpha channel for transparency), and full-color non-palette-based RGB images (with or without alpha channel).

-}
encodePng : Image -> Bytes
encodePng =
    PNG.encode


{-| The BMP file format, also known as bitmap image file or device independent bitmap (DIB) file format or simply a bitmap, is a raster graphics image file format used to store bitmap digital images, independently of the display device (such as a graphics adapter), especially on Microsoft Windows and OS/2 operating systems.

**Note**: Using BMP 32bit is discouraged due to lack of proper support across browsers

-}
encodeBmp : Image -> Bytes
encodeBmp =
    BMP.encode


{-| Convert blob of image (`png` or `bmp`) into [`Image`](#Image)
-}
decode : Bytes -> Maybe { width : Width, height : Height, data : Image }
decode bytes =
    PNG.decode bytes
        |> or (BMP.decode bytes)


or : Maybe a -> Maybe a -> Maybe a
or ma mb =
    case ma of
        Nothing ->
            mb

        Just _ ->
            ma
