module LzwTest exposing (..)

import Bytes.Decode as D
import Bytes.Encode as E
import Expect
import Image.Internal.Lzw as Lzw
import Test exposing (..)


suite =
    describe "Lzw"
        [ test "Encode List" <|
            \_ ->
                Lzw.encodeGifList (2 ^ 2 - 1) decodedData
                    |> Expect.equalLists encodedData
        , test "Decode List" <|
            \_ ->
                Lzw.decodeGifList (2 ^ 2 - 1) encodedData
                    |> Expect.equalLists decodedData
        , test "Bytes Decoder" <|
            \_ ->
                D.decode (Lzw.decoder (2 ^ 2 - 1) (2 + 1) 22) encodedBytes
                    |> Maybe.map (Expect.equalLists decodedData)
                    |> Maybe.withDefault (Expect.fail "Decode fail")
        ]


encodedData =
    [ 4, 1, 6, 6, 2, 9, 9, 7, 8, 10, 2, 12, 1, 14, 15, 6, 0, 21, 0, 10, 7, 22, 23, 18, 26, 7, 10, 29, 13, 24, 12, 18, 16, 36, 12, 5 ]


decodedData =
    [ 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 0, 0, 0, 0, 2, 2, 2, 1, 1, 1, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1 ]


encodedBytes =
    encodedBytesList
        |> List.map E.unsignedInt8
        |> E.sequence
        |> E.encode


encodedBytesList =
    [ 140, 45, 153, 135, 42, 28, 220, 51, 160, 2, 117, 236, 149, 250, 168, 222, 96, 140, 4, 145, 76, 1 ]
