module Issue13 exposing (..)

import Array
import Expect
import Image
import Test exposing (..)


suite : Test
suite =
    describe "Issue #13"
        [ test "Image.toPng stack safety test" <|
            \_ ->
                Array.repeat 2000 (Array.repeat 2000 0xFFFFFFFF)
                    |> Image.fromArray2d
                    |> Image.toPngUrl
                    |> always Expect.pass
        , test "Regression test" <|
            \_ ->
                List.range 0 450
                    |> Image.fromList 45
                    |> Image.toPngUrl
                    |> Expect.equal "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAC0AAAALCAYAAADr7wSxAAAAVElEQVR4nNXNURVAUBgEYVtAEwk0kUATCTSRQBNNzJDAk7P3nG/2f7sZ3ufWMBPcGmaBW8NscGuYA24Nc8H9wfP/ZySjRxOS2aMJyerRhGT3aEJy3sPlBnVYnINYAAAAAElFTkSuQmCC"
        ]
