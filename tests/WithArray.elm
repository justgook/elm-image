module WithArray exposing (suite)

import Base64
import Bytes exposing (Bytes)
import Bytes.Encode as E
import Expect
import Image
import Test exposing (test)


suite =
    test "fromArray << toArray is equivalent to identity" <|
        \_ ->
            case Image.decode spriteBytes of
                Just image1 ->
                    let
                        image2 =
                            image1 |> Image.toArray |> Image.fromArray 10
                    in
                    Expect.equalLists
                        (Image.toList image1)
                        (Image.toList image2)

                Nothing ->
                    Expect.fail "invalid image"


spriteBytes : Bytes
spriteBytes =
    "iVBORw0KGgoAAAANSUhEUgAAAAUAAAAFCAMAAAC6sdbXAAAAAXNSR0IArs4c6QAAAGBQTFRFAAAAIiA0RSg8Zjkxj1Y733Em2aBm7sOa+/I2meVQar4wN5RuS2kvUkskMjw5Pz90MGCCW27hY5v/X83ky9v8////m623hH6HaWpqWVZSdkKKrDIy2Vdj13u6j5dKim8w+2O8zwAAACB0Uk5TAP////////////////////////////////////////+Smq12AAAAIklEQVQImQ3FsQ0AAAzCsAxsnBDx/5utF4Nh1PLMN9ui0QMRtAFhb9VL9AAAAABJRU5ErkJggg=="
        |> Base64.toBytes
        |> Maybe.withDefault (E.encode (E.sequence []))
