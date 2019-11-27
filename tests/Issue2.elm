module Issue2 exposing (..)

import Array
import Base64
import Expect
import Image
import Test exposing (..)


listData =
    [ List.repeat 4 0x00
    , [ 0xFFFFFFAA ] ++ List.repeat 3 0xFF0000FF
    , List.repeat 4 0xFF0000FF
    , List.repeat 4 0xFF0000FF
    ]


list =
    Image.fromList2d
        listData


array : Image.Image
array =
    Image.fromArray2d
        ([ List.repeat 4 0x00 |> Array.fromList
         , [ 0xFFFFFFAA ] ++ List.repeat 3 0xFF0000FF |> Array.fromList
         , List.repeat 4 0xFF0000FF |> Array.fromList
         , List.repeat 4 0xFF0000FF |> Array.fromList
         ]
            |> Array.fromList
        )


suite =
    describe "Issue #2"
        [ test "fromList2d is equivalent to fromArray2d for bmp" <|
            \_ ->
                let
                    process image =
                        image
                            |> Image.toBmp
                            |> Base64.fromBytes
                            |> Maybe.withDefault ""
                in
                process list
                    |> Expect.equal (process array)
        , test "fromList2d is equivalent to (fromList n << List.concat) for bmp" <|
            \_ ->
                let
                    process image =
                        image
                            |> Image.toBmp
                            |> Base64.fromBytes
                            |> Maybe.withDefault ""
                in
                process (Image.fromList 4 <| List.concat listData)
                    |> Expect.equal (process (Image.fromList2d listData))
        , test "fromArray2d is equivalent to (fromArray n << List.concat) for bmp" <|
            \_ ->
                let
                    arrayData =
                        listData
                            |> List.map Array.fromList
                            |> Array.fromList

                    flatArrayData =
                        listData
                            |> List.concat
                            |> Array.fromList

                    process image =
                        image
                            |> Image.toBmp
                            |> Base64.fromBytes
                            |> Maybe.withDefault ""
                in
                process (Image.fromArray 4 flatArrayData)
                    |> Expect.equal (process (Image.fromArray2d arrayData))
        , test "fromList2d is equivalent to fromArray2d for png" <|
            \_ ->
                let
                    process image =
                        image
                            |> Image.toPng
                            |> Base64.fromBytes
                            |> Maybe.withDefault ""
                in
                process list
                    |> Expect.equal (process array)
        , test "fromList2d >> toList2d = identity" <|
            \_ ->
                listData
                    |> Image.fromList2d
                    |> Image.toList2d
                    |> Expect.equal listData
        , test "fromArray2d >> toList2d = identity" <|
            \_ ->
                listData
                    |> List.map Array.fromList
                    |> Array.fromList
                    |> Image.fromArray2d
                    |> Image.toList2d
                    |> Expect.equal listData
        ]
