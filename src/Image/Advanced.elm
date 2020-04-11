module Image.Advanced exposing
    ( getType, ImageType
    , map, get, put, eval
    , toPng32
    , toBmp24, toBmp32
    , toGif
    )

{-|


# Image Info

@docs getType, ImageType


# Manipulations

@docs map, get, put, eval


# Custom Encoding

@docs toPng32
@docs toBmp24, toBmp32
@docs toGif

-}

import Bytes exposing (Bytes)
import Image.Internal.BMP as BMP
import Image.Internal.GIF as GIF
import Image.Internal.ImageData as ImageData exposing (Image, PixelFormat(..))
import Image.Internal.Meta exposing (BmpHeader, FromDataBitDepth(..), FromDataColor(..), Header(..), PngHeader)
import Image.Internal.PNG as PNG
import Image.Internal.Pixel as Pixel


{-| Possible image decoded type
-}
type ImageType
    = PNG
    | BMP
    | GIF
    | SCR


{-| Get image type
-}
getType : ImageData.Image -> ImageType
getType image =
    case ImageData.getInfo image of
        Png _ ->
            PNG

        Bmp _ ->
            BMP

        Gif _ ->
            GIF

        FromData _ ->
            SCR


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
    Debug.log "IMPLEMENT ME" image


{-| Paints data from the given `Image` onto the other `Image`.

    newImage =
        Image.put dx dy imageData image

  - `imageData` An `Image` containing the array of pixel values.
  - `dx` Horizontal position (x coordinate) at which to place the image data in the destination `Image`.
  - `dy` Vertical position (y coordinate) at which to place the image data in the destination `Image`.

-}
put : Int -> Int -> Image -> Image -> Image
put sx sy from to =
    Debug.log "IMPLEMENT ME" to


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
toGif : Image -> Bytes
toGif =
    Pixel.toBit32
        >> ImageData.forceColor (FromDataChannel4 FromDataBitDepth8)
        >> GIF.encode
