module Image.Internal.GIF exposing (decode, encode)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D exposing (Decoder)
import Bytes.Encode as E
import Image.Internal.Array2d as Array2d
import Image.Internal.Decode as D
import Image.Internal.Encode as E
import Image.Internal.ImageData as ImageData exposing (Image(..), Order(..), PixelFormat(..))
import Image.Internal.Lzw as Lzw
import Image.Internal.Meta exposing (Header(..))
import Image.Internal.Util as Util



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
    let
        { width, height } =
            ImageData.dimensions image

        ( colorArray, imageDataRaw ) =
            ImageData.toPalette image

        ( paletteColors, transparentColorIndex ) =
            colorArray
                |> Util.indexedFoldl
                    (\backgroundIndex color ( encoded, transColor ) ->
                        ( E.unsignedInt24 BE (Bitwise.shiftRightZfBy 8 color) :: encoded
                        , if color == 0 then
                            --Set transparent index if it is fully transparent
                            Just backgroundIndex

                          else
                            transColor
                        )
                    )
                    ( [], Nothing )
                |> Tuple.mapFirst List.reverse

        colorCount =
            Array.length colorArray

        globalTableSize =
            calcGlobalTableSize colorCount

        totalColorCount =
            2 ^ (globalTableSize + 1)

        globalColorTable =
            E.sequence
                [ E.sequence paletteColors

                -- Fill missing space with black
                , E.sequence (List.repeat (totalColorCount - colorCount) (E.unsignedInt24 BE 0))
                ]

        imageDescriptor =
            E.sequence
                [ E.unsignedInt8 0x2C
                , E.unsignedInt32 LE 0x00 -- Frame offset Top/left
                , E.unsignedInt16 LE width -- Frame Width
                , E.unsignedInt16 LE height -- Frame Height
                , E.unsignedInt8 0x00 -- LOCAL options (color table flag, interlace flag, sort flag, _, size of local color table)
                ]

        graphicsControlExtensionFlags =
            case transparentColorIndex of
                Just _ ->
                    0x01

                Nothing ->
                    0x00

        graphicsControlExtension =
            [ E.unsignedInt8 0x21 -- Extension Introducer (Always 0x21)
            , E.unsignedInt8 0xF9 -- Graphics Control Label (Always 0xF9)
            , E.unsignedInt8 0x04 -- Byte Size
            , E.unsignedInt8 graphicsControlExtensionFlags -- Packed Field (Bits: 000 - Reserved, 000 - Disposal Method, 0 - user input flag, 1 - transparent color flag)
            , E.unsignedInt16 BE 0x00 -- Delay Time
            , E.unsignedInt8 (Maybe.withDefault 0x00 transparentColorIndex) -- Transparent Color Index
            , E.unsignedInt8 0x00 -- Block Terminator (Always 0x00)
            ]
                |> E.sequence
    in
    [ encodeSignature
    , encodeScreenDescriptor
        { globalTableSize = globalTableSize
        , backgroundColorIndex = 0x00
        , width = width
        , height = height
        }
    , globalColorTable
    , graphicsControlExtension
    , imageDescriptor
    , imageDataRaw |> Array.toList |> Lzw.encode (totalColorCount - 1)
    , E.unsignedInt8 0x3B -- trailer
    ]
        |> E.sequence
        |> E.encode



--- Decoder Helper


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
                                        |> Maybe.map (\arr -> Array2d.fromArray info.width arr)
                                        |> Maybe.withDefault Array.empty
                                        |> ImageData.ImageRaw header
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
    D.array (2 ^ bits) (D.unsignedInt24 BE |> D.map (Bitwise.shiftLeftBy 8 >> (+) 0xFF >> Bitwise.shiftRightZfBy 0))


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
                                Lzw.decoder (2 ^ info.bits - 1)
                                    |> D.map
                                        (\data ->
                                            --TODO add decoding for rest frames and images bigger than 256
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



------ Encode Helpers


calcGlobalTableSize : number -> number
calcGlobalTableSize i =
    if i > 128 then
        7

    else if i > 64 then
        6

    else if i > 32 then
        5

    else if i > 16 then
        4

    else if i > 8 then
        3

    else if i > 4 then
        2

    else if i > 2 then
        1

    else
        --if i > 0 then
        0


packedByte_ =
    { globalTableFlag = 0x80
    , colorResolution = 0x10
    , sortFlag = 0
    }


encodeScreenDescriptor :
    { globalTableSize : Int
    , backgroundColorIndex : Int
    , width : Int
    , height : Int
    }
    -> E.Encoder
encodeScreenDescriptor opt =
    let
        packedByte =
            packedByte_.globalTableFlag
                |> Bitwise.or packedByte_.colorResolution
                |> Bitwise.or packedByte_.sortFlag
                |> Bitwise.or opt.globalTableSize
    in
    E.sequence
        [ E.unsignedInt16 LE opt.width
        , E.unsignedInt16 LE opt.height
        , E.unsignedInt8 packedByte
        , E.unsignedInt8 opt.backgroundColorIndex -- color index to use as transparent
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
