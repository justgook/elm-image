module Image.Advanced exposing
    ( source, Source(..)
    , map, get, put, eval, mirror
    , toPng32
    , toBmp24, toBmp32
    , toGIF89a
    )

{-|


# Image Info

@docs source, Source


# Manipulations

@docs map, get, put, eval, mirror


# Custom Encoding

@docs toPng32
@docs toBmp24, toBmp32
@docs toGIF89a

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Image.Internal.Array2d as Array2d
import Image.Internal.BMP as BMP
import Image.Internal.GIF as GIF
import Image.Internal.ImageData as ImageData exposing (Image(..), PixelFormat(..))
import Image.Internal.Meta as Meta exposing (BmpHeader, FromDataBitDepth(..), FromDataColor(..), PngHeader)
import Image.Internal.PNG as PNG
import Image.Internal.Pixel as Pixel exposing (toBit32)


{-| Possible image decoded type
-}
type Source
    = Png
    | Bmp
    | Gif
    | Code


{-| Get image type
-}
source : ImageData.Image -> Source
source image =
    case ImageData.getInfo image of
        Meta.Png _ ->
            Png

        Meta.Bmp _ ->
            Bmp

        Meta.Gif _ ->
            Gif

        Meta.FromData _ ->
            Code


{-| Apply a function on every pixel in an image.
-}
map : (Int -> Int) -> Image -> Image
map =
    ImageData.map


{-| When decoding images they are decoded in _lazy way_ (real decoding is postponed until data is needed)
this function evaluates postponed decode, useful if you need to encode multiple images from same source.
-}
eval : Image -> Image
eval =
    ImageData.eval


{-| Returns an `Image` representing the underlying pixel data for a specified portion of the `Image`.

    region =
        Image.get sx sy sw sh image

  - `sx` The x-axis coordinate of the top-left corner of the rectangle from which the `Image` will be extracted.
  - `sy` The y-axis coordinate of the top-left corner of the rectangle from which the `Image` will be extracted.
  - `sw` The width of the rectangle from which the `Image` will be extracted. Positive values are to the right, and negative to the left.
  - `sh` The height of the rectangle from which the `Image` will be extracted. Positive values are down, and negative are up.
  - `image` The source image to select pixels from.
  - `region` An `Image` containing the image data for the rectangle of the `image` specified.
    The coordinates of the rectangle's top-left corner are (`sx`, `sy`), while the coordinates of the bottom corner are (`sx` + `sw`, `sy` + `sh`).

-}
get : Int -> Int -> Int -> Int -> Image -> Image
get sx sy sw sh image =
    case toBit32 image of
        ImageRaw header array2d ->
            ImageRaw
                (Meta.FromData
                    { width = sw
                    , height = sh
                    , color = Meta.FromDataChannel4 Meta.FromDataBitDepth8
                    }
                )
                (Array2d.part sx sy sw sh array2d)

        Lazy _ _ ->
            case eval image of
                ImageRaw header array2d ->
                    get sx sy sw sh (ImageRaw header array2d)

                Lazy _ _ ->
                    image


{-| Paints data from the given `Image` onto the other `Image`.

    newImage =
        Image.put dx dy imageFrom imageTo

  - `imageData` An `Image` containing the array of pixel values.
  - `dx` Horizontal position (x coordinate) at which to place the image data in the destination `Image`.
  - `dy` Vertical position (y coordinate) at which to place the image data in the destination `Image`.

-}
put : Int -> Int -> Image -> Image -> Image
put dx dy from to =
    case ( toBit32 from, toBit32 to ) of
        ( ImageRaw _ dataFrom, ImageRaw headerTo dataTo ) ->
            let
                dim =
                    Meta.dimensions headerTo
            in
            ImageRaw
                (Meta.FromData
                    { width = dim.width
                    , height = dim.height
                    , color = Meta.FromDataChannel4 Meta.FromDataBitDepth8
                    }
                )
                (Array2d.apply dx dy dataFrom dataTo)

        _ ->
            to


{-| Mirror image horizontally or/and vertically

    newImage =
        Image.mirror x y image

-}
mirror : Bool -> Bool -> Image -> Image
mirror horizontally vertically image =
    case ( image, horizontally, vertically ) of
        ( ImageRaw meta data, True, True ) ->
            Array.foldr (\l acc -> Array.push (arrayReverse l) acc) Array.empty data
                |> ImageRaw meta

        ( ImageRaw meta data, True, False ) ->
            Array.map arrayReverse data
                |> ImageRaw meta

        ( ImageRaw meta data, False, True ) ->
            arrayReverse data
                |> ImageRaw meta

        ( _, False, False ) ->
            image

        ( Lazy meta fn, _, _ ) ->
            mirror horizontally vertically (fn meta)


arrayReverse : Array a -> Array a
arrayReverse =
    Array.foldr (\a acc -> Array.push a acc) Array.empty


{-| Encode image into True color with alpha PNG image
-}
toPng32 : Image -> Bytes
toPng32 =
    Pixel.toBit32 >> PNG.encode


{-| Encode image into BMP24
-}
toBmp24 : Image -> Bytes
toBmp24 =
    Pixel.toBit24
        >> ImageData.forceColor (FromDataChannel3 FromDataBitDepth8)
        >> BMP.encode


{-| Encode image into BMP32

**Note**: Using BMP 32bit is discouraged due to lack of proper support across browsers

-}
toBmp32 : Image -> Bytes
toBmp32 =
    Pixel.toBit32
        >> ImageData.forceColor (FromDataChannel4 FromDataBitDepth8)
        >> BMP.encode


{-| Encode image into GIF89a

**Note**:

1.  Gif supports only 256 colors in palette - if image have more that 256 colors, all colors that exceed 256 will become first color in palette
2.  Gif supports only fully transparent color, all colors that isn't fully transparent (alpha > 0) will be flatted to it color

-}
toGIF89a : Image -> Bytes
toGIF89a =
    Pixel.toBit32
        >> ImageData.forceColor (FromDataChannel4 FromDataBitDepth8)
        >> GIF.encode
