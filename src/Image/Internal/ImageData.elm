module Image.Internal.ImageData exposing
    ( EncodeOptions
    , Image(..)
    , Order(..)
    , PixelFormat(..)
    , bytesPerPixel
    , defaultOptions
    , dimensions
    , eval
    , forceColor
    , getInfo
    , map
    , toArray
    , toArray2d
    , toList
    , toList2d
    , width
    )

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D exposing (Decoder, Step(..))
import Image.Info as Metadata exposing (BmpBitsPerPixel(..), FromDataColor(..), Info(..), PngColor(..))
import Image.Internal.Array2D as Array2D


type Image
    = List Info (List Int)
    | List2d Info (List (List Int))
    | Array Info (Array Int)
    | Array2d Info (Array (Array Int))
    | Bytes Info (Decoder Image) Bytes


eval : Image -> Image
eval image =
    case image of
        Bytes _ d b ->
            case D.decode d b of
                Just (Bytes _ _ _) ->
                    image

                Just newData ->
                    newData

                Nothing ->
                    image

        _ ->
            image


forceColor : Metadata.FromDataColor -> Image -> Image
forceColor color image =
    case image of
        List meta im ->
            List (toFromData color meta) im

        List2d meta im ->
            List2d (toFromData color meta) im

        Array meta im ->
            Array (toFromData color meta) im

        Array2d meta im ->
            Array2d (toFromData color meta) im

        Bytes _ d b ->
            case D.decode d b of
                Just (Bytes _ _ _) ->
                    image

                Just newData ->
                    forceColor color newData

                Nothing ->
                    image


toFromData : Metadata.FromDataColor -> Info -> Info
toFromData color meta =
    let
        dim =
            Metadata.dimensions meta
    in
    FromData
        { width = dim.width
        , height = dim.height
        , color = color
        }


type alias EncodeOptions =
    { format : PixelFormat
    , order : Order
    }


type PixelFormat
    = RGBA
    | RGB
    | LUMINANCE_ALPHA
    | ALPHA


{-| Pixel render order in image
-}
type Order
    = RightDown
    | RightUp
    | LeftDown
    | LeftUp


{-| -}
defaultOptions : EncodeOptions
defaultOptions =
    { format = RGBA
    , order = RightDown
    }


{-| -}
map : (Int -> Int) -> Image -> Image
map fn image =
    case image of
        List meta l ->
            List meta (List.map fn l)

        List2d meta l ->
            List2d meta (List.map (List.map fn) l)

        Array meta arr ->
            Array meta (Array.map fn arr)

        Array2d meta arr ->
            Array2d meta (Array.map (Array.map fn) arr)

        Bytes meta d b ->
            case D.decode d b of
                Just (Bytes _ _ _) ->
                    Bytes meta d b

                Just newData ->
                    map fn newData

                Nothing ->
                    Bytes meta d b


toList : Image -> List Int
toList image =
    case image of
        List _ l ->
            l

        List2d _ l ->
            List.concat l

        Array _ arr ->
            Array.toList arr

        Array2d _ arr ->
            Array.foldr (\line acc1 -> Array.foldr (\px acc2 -> px :: acc2) acc1 line) [] arr

        Bytes _ d b ->
            case D.decode d b of
                Just (Bytes _ _ _) ->
                    []

                Just newData ->
                    toList newData

                Nothing ->
                    []


toList2d : Image -> List (List Int)
toList2d info =
    case info of
        List meta l ->
            greedyGroupsOf (Metadata.dimensions meta).width l

        List2d _ l ->
            l

        Array meta arr ->
            Array.toList arr |> greedyGroupsOf (Metadata.dimensions meta).width

        Array2d _ arr ->
            Array.foldr
                (\line acc1 ->
                    Array.foldr (\px acc2 -> px :: acc2) [] line
                        |> (\l -> l :: acc1)
                )
                []
                arr

        Bytes _ d b ->
            case D.decode d b of
                Just (Bytes _ _ _) ->
                    []

                Just newData ->
                    toList2d newData

                Nothing ->
                    []


toArray : Image -> Array Int
toArray image =
    case image of
        List _ l ->
            Array.fromList l

        List2d _ l ->
            List.foldl (Array.fromList >> (\a b -> Array.append b a)) Array.empty l

        Array _ arr ->
            arr

        Array2d _ arr ->
            Array.foldr Array.append Array.empty arr

        Bytes _ d b ->
            case D.decode d b of
                Just (Bytes _ _ _) ->
                    Array.empty

                Just newData ->
                    toArray newData

                Nothing ->
                    Array.empty


toArray2d : Image -> Array (Array Int)
toArray2d image =
    case image of
        List meta l ->
            fromList (Metadata.dimensions meta).width l (Array.fromList [ Array.empty ])

        List2d _ l ->
            List.foldl (Array.fromList >> Array.push) Array.empty l

        Array meta arr ->
            fromArray (Metadata.dimensions meta).width arr Array.empty

        Array2d _ arr ->
            arr

        Bytes _ d b ->
            case D.decode d b of
                Just (Bytes _ _ _) ->
                    Array.empty

                Just newData ->
                    toArray2d newData

                Nothing ->
                    Array.empty


fromList w l acc =
    case l of
        a :: rest ->
            let
                newAcc =
                    applyIf (Array2D.lastLength acc >= w) (Array.push Array.empty) acc
            in
            fromList w rest (Array2D.push a newAcc)

        [] ->
            acc


fromArray : Int -> Array a -> Array (Array a) -> Array (Array a)
fromArray w arr acc =
    if Array.length arr > w then
        let
            ( a1, a2 ) =
                splitAt w arr
        in
        fromArray w a2 (Array.push a1 acc)

    else
        Array.push arr acc


getInfo : Image -> Info
getInfo image =
    case image of
        Bytes meta _ _ ->
            meta

        Array2d meta _ ->
            meta

        List2d meta _ ->
            meta

        Array meta _ ->
            meta

        List meta _ ->
            meta


dimensions : Image -> { width : Int, height : Int }
dimensions image =
    getInfo image |> Metadata.dimensions


width : Image -> Int
width image =
    (dimensions image).width


applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f a =
    if bool then
        f a

    else
        a


splitAt : Int -> Array a -> ( Array a, Array a )
splitAt index xs =
    let
        len =
            Array.length xs
    in
    case ( index > 0, index < len ) of
        ( True, True ) ->
            ( Array.slice 0 index xs, Array.slice index len xs )

        ( True, False ) ->
            ( xs, Array.empty )

        ( False, True ) ->
            ( Array.empty, xs )

        ( False, False ) ->
            ( Array.empty, Array.empty )


greedyGroupsOf : Int -> List a -> List (List a)
greedyGroupsOf size xs =
    greedyGroupsOfWithStep size size xs


greedyGroupsOfWithStep : Int -> Int -> List a -> List (List a)
greedyGroupsOfWithStep size step xs =
    let
        xs_ =
            List.drop step xs

        okayArgs =
            size > 0 && step > 0

        okayXs =
            List.length xs > 0
    in
    if okayArgs && okayXs then
        List.take size xs :: greedyGroupsOfWithStep size step xs_

    else
        []


bytesPerPixel : Info -> Int
bytesPerPixel meta =
    case meta of
        Png { color } ->
            case color of
                Greyscale _ ->
                    1

                GreyscaleAlpha _ ->
                    2

                TrueColour _ ->
                    3

                TrueColourAlpha _ ->
                    4

                IndexedColour _ ->
                    4

        Bmp { bitsPerPixel } ->
            case bitsPerPixel of
                BmpBitsPerPixel8 ->
                    1

                BmpBitsPerPixel16 ->
                    2

                BmpBitsPerPixel24 ->
                    3

                BmpBitsPerPixel32 ->
                    4

        FromData { color } ->
            case color of
                FromDataChannel1 _ ->
                    1

                FromDataChannel2 _ ->
                    2

                FromDataChannel3 _ ->
                    3

                FromDataChannel4 _ ->
                    4

        Gif _ ->
            1
