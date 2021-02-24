module Image.Internal.Lzw exposing (decodeGifList, decoder, encode, encodeGifStream)

import Array exposing (Array)
import Bytes exposing (Endianness(..))
import Bytes.Decode as D exposing (Decoder, Step(..))
import Bytes.Encode as E
import Dict
import Image.Internal.BitReader as BitReader
import Image.Internal.BitWriter as BitWriter exposing (BitWriter)
import Image.Internal.Decode as D


type alias CodeStream =
    List Int


type alias Pixels =
    List Int


decoder : Int -> D.Decoder (Array Int)
decoder colorCountPowerOfTwo =
    D.andThen2
        (\firstCodeSize_ count_firstBlock ->
            let
                firstCodeSize =
                    firstCodeSize_ + 1
            in
            D.bytes count_firstBlock
                |> D.andThen
                    (\bytes ->
                        let
                            ( cc, eoi, table ) =
                                initDecodeTable colorCountPowerOfTwo
                        in
                        BitReader.readBits firstCodeSize 0
                            |> BitReader.andThen
                                (\reset ->
                                    if reset /= cc then
                                        BitReader.error "No reset Bit present"

                                    else
                                        BitReader.readBits firstCodeSize 0
                                            |> BitReader.andThen
                                                (\first ->
                                                    let
                                                        value =
                                                            Dict.get first table
                                                                |> Maybe.withDefault []

                                                        acc =
                                                            { eoi = eoi -- end of information code
                                                            , table = table
                                                            , indexStream = Array.fromList value
                                                            , indexBuffer = value
                                                            , read = firstCodeSize
                                                            }
                                                    in
                                                    BitReader.loop acc bitDecoder
                                                )
                                )
                            |> BitReader.decode bytes
                            |> Result.toMaybe
                            |> Maybe.map D.succeed
                            |> Maybe.withDefault D.fail
                    )
        )
        D.unsignedInt8
        D.unsignedInt8


bitDecoder ({ eoi, table, indexStream, read, indexBuffer } as acc) =
    BitReader.readBits read 0
        |> BitReader.map
            (\code ->
                if code /= eoi && (2 ^ read - 1) < 4095 then
                    case Dict.get code table of
                        Just v ->
                            let
                                k =
                                    List.head v |> Maybe.withDefault -404

                                tableValue =
                                    indexBuffer ++ [ k ]

                                tableKey =
                                    Dict.size table

                                newRead =
                                    if tableKey >= 2 ^ read - 1 then
                                        read + 1

                                    else
                                        read
                            in
                            Loop
                                { acc
                                    | table = Dict.insert tableKey tableValue table
                                    , indexStream = Array.append indexStream (Array.fromList v)
                                    , indexBuffer = v
                                    , read = newRead
                                }

                        Nothing ->
                            let
                                k =
                                    indexBuffer
                                        |> List.head
                                        |> Maybe.withDefault -404

                                tableValue =
                                    indexBuffer ++ [ k ]

                                newRead =
                                    if code >= 2 ^ read - 1 then
                                        read + 1

                                    else
                                        read
                            in
                            Loop
                                { acc
                                    | table = Dict.insert code tableValue table
                                    , indexStream = Array.append indexStream (Array.fromList tableValue)
                                    , indexBuffer = tableValue
                                    , read = newRead
                                }

                else
                    -- TODO add reset on 4095
                    Done indexStream
            )


decodeGifList : Int -> List Int -> CodeStream
decodeGifList size data =
    let
        ( cc, eoi, table ) =
            initDecodeTable size
    in
    case data of
        _ :: first :: rest ->
            let
                value =
                    Dict.get first table |> Maybe.withDefault []
            in
            decodeGifList_ eoi table value rest value

        _ ->
            []


decodeGifList_ eoi table indexStream codeStream code__1 =
    case codeStream of
        code :: rest ->
            if code == eoi then
                indexStream

            else
                case Dict.get code table of
                    Just v ->
                        let
                            k =
                                v
                                    |> List.head
                                    |> Maybe.withDefault -404

                            tableValue =
                                code__1 ++ [ k ]

                            tableKey =
                                Dict.size table
                        in
                        decodeGifList_ eoi (Dict.insert tableKey tableValue table) (indexStream ++ v) rest v

                    Nothing ->
                        let
                            k =
                                code__1
                                    |> List.head
                                    |> Maybe.withDefault -404

                            tableValue =
                                code__1 ++ [ k ]
                        in
                        decodeGifList_ eoi (Dict.insert code tableValue table) (indexStream ++ tableValue) rest tableValue

        _ ->
            indexStream


type alias DecodeTable =
    ( Int, Int, Dict.Dict Int (List Int) )


initDecodeTable : Int -> DecodeTable
initDecodeTable lastColorIndex =
    let
        cc =
            lastColorIndex + 1

        eoi =
            lastColorIndex + 2

        table =
            List.range 0 lastColorIndex
                |> List.foldl (\k -> Dict.insert k [ k ]) Dict.empty
                |> Dict.insert cc [ cc ]
                |> Dict.insert eoi [ eoi ]
    in
    ( cc, eoi, table )



---------------------------------------------------------------------------------------------------------------------------------------------------


type alias EncodeTable =
    ( Int, Int, Dict.Dict String Int )


initEncodeTable : Int -> EncodeTable
initEncodeTable lastColorIndex =
    let
        cc =
            lastColorIndex + 1

        eoi =
            lastColorIndex + 2

        table =
            List.range 0 lastColorIndex
                |> List.foldl (\k -> Dict.insert (String.fromInt k) k) Dict.empty
                |> Dict.insert (String.fromInt cc) cc
                |> Dict.insert (String.fromInt eoi) eoi
    in
    ( cc, eoi, table )


encode : Int -> Pixels -> E.Encoder
encode lastColorIndex data =
    let
        ( cc, eoi, table ) =
            initEncodeTable lastColorIndex
    in
    case data of
        [] ->
            E.sequence []

        i :: rest ->
            let
                firstCodeSize =
                    lzwCodeSize cc

                output =
                    BitWriter.empty
                        |> BitWriter.writeBits (firstCodeSize + 1) cc

                resultBytes =
                    encode_ eoi table rest output (String.fromInt i) 0
                        |> BitWriter.flush
                        |> BitWriter.run
                        |> E.sequence
                        |> E.encode
            in
            E.sequence
                [ E.unsignedInt8 firstCodeSize
                , E.unsignedInt8 (Bytes.width resultBytes)
                , E.bytes resultBytes
                ]


encode_ : Int -> Dict.Dict String Int -> List Int -> BitWriter -> String -> Int -> BitWriter
encode_ eoi table input output indexBuffer bytesWritten =
    case input of
        k :: rest ->
            let
                key =
                    indexBuffer ++ "," ++ String.fromInt k
            in
            case Dict.get key table of
                Nothing ->
                    let
                        tableIndex =
                            Dict.size table

                        ( newTable, newOutput ) =
                            if tableIndex <= 4095 then
                                let
                                    newTable_ =
                                        Dict.insert key tableIndex table

                                    newOutput_ =
                                        output
                                            |> BitWriter.writeBits (lzwCodeSize tableIndex)
                                                (Dict.get indexBuffer table |> Maybe.withDefault -404)
                                in
                                ( newTable_, newOutput_ )

                            else
                                let
                                    ( cc, _, newTable_ ) =
                                        initEncodeTable (eoi - 1)

                                    --TODO validate that it works
                                    newOutput_ =
                                        output |> BitWriter.writeBits (lzwCodeSize cc + 1) cc
                                in
                                ( newTable_, newOutput_ )
                    in
                    encode_ eoi newTable rest newOutput (String.fromInt k) bytesWritten

                Just _ ->
                    encode_ eoi table rest output key bytesWritten

        [] ->
            let
                bitsToWrite =
                    Dict.size table
                        |> lzwCodeSize
            in
            output
                |> BitWriter.writeBits bitsToWrite (Dict.get indexBuffer table |> Maybe.withDefault -404)
                |> BitWriter.writeBits bitsToWrite eoi


lzwCodeSize : Int -> Int
lzwCodeSize input =
    if input <= 4 then
        2

    else if input <= 8 then
        3

    else if input <= 16 then
        4

    else if input <= 32 then
        5

    else if input <= 64 then
        6

    else if input <= 128 then
        7

    else if input <= 256 then
        8

    else if input <= 512 then
        9

    else if input <= 1024 then
        10

    else if input <= 2048 then
        11

    else if input <= 4096 then
        12

    else
        0


encodeGifStream_ : Int -> Dict.Dict String Int -> List Int -> List Int -> String -> CodeStream
encodeGifStream_ eoi table data codeStream indexBuffer =
    --https://www.matthewflickinger.com/lab/whatsinagif/lzw_image_data.asp
    case data of
        k :: rest ->
            let
                key =
                    indexBuffer ++ "," ++ String.fromInt k
            in
            case Dict.get key table of
                Nothing ->
                    let
                        newTable =
                            Dict.insert key (Dict.size table) table

                        newCodeStream =
                            (Dict.get indexBuffer table |> Maybe.withDefault -404) :: codeStream
                    in
                    encodeGifStream_ eoi newTable rest newCodeStream (String.fromInt k)

                Just _ ->
                    encodeGifStream_ eoi table rest codeStream key

        [] ->
            eoi
                :: (Dict.get indexBuffer table |> Maybe.withDefault -404)
                :: codeStream
                |> List.reverse


encodeGifStream : Int -> Pixels -> CodeStream
encodeGifStream lastColorIndex data =
    let
        ( cc, eoi, table ) =
            initEncodeTable lastColorIndex
    in
    case data of
        [] ->
            []

        i :: rest ->
            encodeGifStream_ eoi table rest [ cc ] (String.fromInt i)



----DELME PART!!!


reverseList : Int -> Decoder a -> Decoder (List a)
reverseList len decoder_ =
    D.loop ( len, [] ) (listStep decoder_)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder_ ( n, xs ) =
    if n <= 0 then
        D.succeed (Done xs)

    else
        D.map (\x -> Loop ( n - 1, x :: xs )) decoder_
