module Image.Internal.GIF exposing (..)

import Bytes exposing (Bytes, Endianness(..))
import Image.Internal.ImageData exposing (EncodeOptions, Image(..), Order(..), PixelFormat(..))



--https://www.w3.org/Graphics/GIF/spec-gif87.txt


decode : Bytes -> Maybe Image
decode bytes =
    Nothing
