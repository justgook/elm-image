module ColorTest exposing (suite)

import Array
import Base64
import Color
import Expect exposing (Expectation)
import Image
import Image.Color
import Image.Internal.ImageData exposing (Order(..), PixelFormat(..), defaultOptions)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Color"
        (let
            colorList =
                [ Color.rgb255 0 0 0
                , Color.rgb255 255 0 0
                , Color.rgb255 0 255 0
                , Color.rgb255 0 0 255
                , Color.rgb255 255 255 255
                ]

            intList =
                [ 0xFF
                , 0xFF0000FF
                , 0x00FF00FF
                , 0xFFFF
                , 0xFFFFFFFF
                ]

            smallData : Image.Image
            smallData =
                List.range 0 9
                    |> List.repeat 3
                    |> List.indexedMap (\i -> List.map (\v -> 1 + i + v * (i + 1)))
                    |> Image.fromList2d

            smallImage : Maybe String
            smallImage =
                process smallData

            process image =
                image
                    |> Image.toBmp
                    |> Base64.fromBytes
         in
         [ test "List to Image" <|
            \_ ->
                Image.Color.fromList 1 colorList
                    |> Expect.equal (Image.fromList 1 intList)
         , test "List2d to Image" <|
            \_ ->
                Image.Color.fromList2d [ colorList ]
                    |> Expect.equal (Image.fromList2d [ intList ])
         , test "Array to Image" <|
            \_ ->
                Image.Color.fromArray 1 (Array.fromList colorList)
                    |> Expect.equal (Image.fromArray 1 (Array.fromList intList))
         , test "Array2D to Image" <|
            \_ ->
                Image.Color.fromArray2d (Array.fromList [ Array.fromList colorList ])
                    |> Expect.equal (Image.fromArray2d (Array.fromList [ Array.fromList intList ]))
         , test "Image to List" <|
            \_ ->
                smallImage
                    |> Expect.equal
                        (smallData
                            |> Image.Color.toList
                            |> Image.Color.fromList 10
                            |> process
                        )
         , test "Image to List2d" <|
            \_ ->
                smallImage
                    |> Expect.equal
                        (smallData
                            |> Image.Color.toList2d
                            |> Image.Color.fromList2d
                            |> process
                        )
         , test "Image to Array" <|
            \_ ->
                smallImage
                    |> Expect.equal
                        (smallData
                            |> Image.Color.toArray
                            |> Image.Color.fromArray 10
                            |> process
                        )
         , test "Image to Array2d" <|
            \_ ->
                smallImage
                    |> Expect.equal
                        (smallData
                            |> Image.Color.toArray2d
                            |> Image.Color.fromArray2d
                            |> process
                        )
         ]
        )
