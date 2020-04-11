module Image.Internal.GIF exposing (decode, encode)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D exposing (Decoder)
import Bytes.Encode as E
import Image.Internal.Decode as D
import Image.Internal.ImageData as ImageData exposing (Image(..), Order(..), PixelFormat(..))
import Image.Internal.Lzw as Lzw
import Image.Internal.Meta exposing (Header(..))



--https://www.w3.org/Graphics/GIF/spec-gif87.txt
--https://www.matthewflickinger.com/lab/whatsinagif/index.html


type alias GifImage =
    { header :
        { width : Int
        , height : Int
        }
    , palette : Array Int
    }


decode : Bytes -> Maybe Image
decode bytes =
    D.decode (mainDecoder <| Bytes.width bytes) bytes


encode : Image -> Bytes
encode image =
    [ encodeSignature
    , encodeScreenDescriptor image
    ]
        |> E.sequence
        |> E.encode


encodeScreenDescriptor : Image -> E.Encoder
encodeScreenDescriptor image =
    let
        { width, height } =
            ImageData.dimensions image

        packedByte =
            0x91

        backgroundColorIndex =
            0x00
    in
    E.sequence
        [ E.unsignedInt16 LE width
        , E.unsignedInt16 LE height
        , E.unsignedInt8 packedByte
        , E.unsignedInt8 backgroundColorIndex
        , E.unsignedInt8 0x00 -- pixel aspect ratio
        ]


encodeSignature : E.Encoder
encodeSignature =
    E.sequence
        [ E.unsignedInt8 0x47
        , E.unsignedInt8 0x49
        , E.unsignedInt8 0x46
        , E.unsignedInt8 0x38
        , E.unsignedInt8 0x39
        , E.unsignedInt8 0x61
        ]


mainDecoder : Int -> D.Decoder Image
mainDecoder size =
    decodeSignature
        |> D.andThen (\_ -> decodeInfo)
        |> D.andThen
            (\info ->
                D.bytes (size - 13)
                    |> D.map
                        (\rest ->
                            Lazy (Gif { width = info.width, height = info.height })
                                (\header ->
                                    D.decode (decodeLazyImage info) rest
                                        |> Maybe.withDefault Array.empty
                                        |> ImageData.Array header
                                )
                        )
            )


decodeLazyImage : GifInfo -> D.Decoder (Array Int)
decodeLazyImage info =
    decodePalette info.bits
        |> D.map
            (\palette ->
                { width = info.width
                , height = info.height
                , m = info.m
                , cr = info.cr
                , bits = info.bits
                , background = info.background
                , palette = palette
                }
            )
        |> D.andThen
            (\info_ ->
                decodeFrames info_ ( ( Nothing, 0, 0 ), Array.empty ) |> D.map (\( ext, dsc ) -> ( info_, ext, dsc ))
            )
        |> D.andThen
            (\( { background, palette }, ( transColor, _, _ ), dsc ) ->
                let
                    updatedPalette =
                        transColor
                            |> Maybe.map (\c -> Array.set c 0 palette)
                            |> Maybe.withDefault palette
                in
                case Array.get 0 dsc of
                    Just { data } ->
                        data
                            |> Array.map (\i -> Array.get i updatedPalette |> Maybe.withDefault 0)
                            |> D.succeed

                    Nothing ->
                        D.fail
            )


decodeSignature : D.Decoder ()
decodeSignature =
    D.listR 6 D.unsignedInt8
        |> D.andThen
            (\signature ->
                if signature == [ 97, 55, 56, 70, 73, 71 ] || signature == [ 97, 57, 56, 70, 73, 71 ] then
                    D.succeed ()

                else
                    D.fail
            )


decodePalette : Int -> D.Decoder (Array Int)
decodePalette bits =
    D.array (2 ^ bits) (D.unsignedInt24 BE |> D.map (Bitwise.shiftLeftBy 8 >> (+) 0xFF))


decodeFrames info (( ext, dsc ) as acc) =
    D.unsignedInt8
        |> D.andThen
            (\headSymbol ->
                if headSymbol == 0x21 then
                    -- Extension block starts with 0x21 - followed by type, then size, and other useful information
                    D.unsignedInt8
                        |> D.andThen
                            (\extension ->
                                case extension of
                                    0x01 ->
                                        --Plain Text Extension
                                        decodeExtensionBlocks
                                            |> D.andThen (\_ -> decodeFrames info ( ext, dsc ))

                                    0xFF ->
                                        --Application Extension
                                        decodeExtensionBlocks
                                            |> D.andThen (\_ -> decodeFrames info ( ext, dsc ))

                                    0xFE ->
                                        --Comment Extension
                                        decodeExtensionBlocks
                                            |> D.andThen (\_ -> decodeFrames info ( ext, dsc ))

                                    0xF9 ->
                                        --Graphics Control Extension
                                        decodeControlExtension
                                            |> D.andThen (\a -> decodeFrames info ( a, dsc ))

                                    _ ->
                                        D.fail
                            )

                else if headSymbol == 0x2C then
                    D.map5
                        (\left top w h mipx ->
                            { left = left
                            , top = top
                            , w = w
                            , h = h
                            , m = Bitwise.shiftRightBy 7 mipx
                            , px = Bitwise.and 7 mipx
                            , i = Bitwise.and 1 <| Bitwise.shiftRightBy 6 mipx
                            , data = Array.empty
                            }
                        )
                        (D.unsignedInt16 LE)
                        (D.unsignedInt16 LE)
                        (D.unsignedInt16 LE)
                        (D.unsignedInt16 LE)
                        D.unsignedInt8
                        |> D.andThen
                            (\dsc_ ->
                                D.map2
                                    (\firstCodeSize count_firstBlock ->
                                        Lzw.decoder (2 ^ info.bits - 1) (firstCodeSize + 1) count_firstBlock
                                    )
                                    D.unsignedInt8
                                    D.unsignedInt8
                                    |> D.andThen identity
                                    |> D.map
                                        (\data ->
                                            --TODO add decoding for rest frames
                                            ( ext, Array.push { dsc_ | data = data } dsc )
                                        )
                            )

                else
                    D.succeed acc
            )


type alias GifInfo =
    { width : Int
    , height : Int
    , m : Int
    , cr : Int
    , bits : Int
    , background : Int
    }


decodeControlExtension : Decoder ( Maybe Int, Int, Int )
decodeControlExtension =
    D.map5
        (\_ packedField delay colorIndex _ ->
            let
                transColor =
                    if Bitwise.and 1 packedField /= 0 then
                        Just colorIndex

                    else
                        Nothing

                disposal =
                    Bitwise.and 28 packedField
                        |> Bitwise.shiftRightBy 2
            in
            ( transColor, delay, disposal )
        )
        D.unsignedInt8
        D.unsignedInt8
        (D.unsignedInt16 LE)
        D.unsignedInt8
        D.unsignedInt8


decodeExtensionBlocks : D.Decoder { body : List Bytes }
decodeExtensionBlocks =
    let
        bodyLoop acc =
            D.unsignedInt8
                |> D.andThen
                    (\count ->
                        if count > 0 then
                            D.bytes count
                                |> D.andThen (\a -> bodyLoop (a :: acc))

                        else
                            D.succeed acc
                    )
    in
    bodyLoop []
        |> D.map (\ext -> { body = ext })


decodeInfo : D.Decoder GifInfo
decodeInfo =
    -- 7 Bytes
    D.map4
        (\width height mCrPixel background ->
            let
                m =
                    Bitwise.shiftRightBy 7 mCrPixel

                cr =
                    Bitwise.and 7 <| Bitwise.shiftRightBy 4 mCrPixel

                pixel =
                    Bitwise.and 7 mCrPixel
            in
            { width = width
            , height = height
            , m = m
            , cr = cr + 1
            , bits = pixel + 1
            , background = background
            }
        )
        (D.unsignedInt16 LE)
        (D.unsignedInt16 LE)
        D.unsignedInt8
        D.unsignedInt8
        |> D.andThen (\info -> D.unsignedInt8 |> D.map (\_ -> info))
