module Image.Internal.Meta exposing
    ( BitDepth1_2_4_8(..)
    , BitDepth1_2_4_8_16(..)
    , BitDepth8_16(..)
    , BmpBitsPerPixel(..)
    , BmpHeader
    , FromDataBitDepth(..)
    , FromDataColor(..)
    , FromDataInfo
    , GifHeader
    , Header(..)
    , PngColor(..)
    , PngHeader
    , PngTextChunk
    , PngTextCompression(..)
    , PngTextKeyword(..)
    , PngTextEncoding(..)
    , dimensions
    , stringToTextKeyword
    , textKeywordToString
    )

{-| -}

import Bytes exposing (Bytes)
import Dict exposing (Dict)


{-| -}
dimensions : Header -> { width : Int, height : Int }
dimensions meta =
    case meta of
        Png { width, height } ->
            { width = width, height = height }

        Bmp { width, height } ->
            { width = width, height = height }

        Gif { width, height } ->
            { width = width, height = height }

        FromData { width, height } ->
            { width = width, height = height }


type Header
    = Png PngHeader
    | Bmp BmpHeader
    | Gif GifHeader
    | FromData FromDataInfo


{-| -}
type alias FromDataInfo =
    { width : Int
    , height : Int
    , color : FromDataColor
    }


{-| -}
type alias PngHeader =
    { width : Int
    , height : Int
    , color : PngColor
    , adam7 : Bool
    , chunks : Dict String Bytes
    , textChunks : List PngTextChunk
    }


type alias PngTextChunk =
    { keyword : PngTextKeyword
    , compression : PngTextCompression
    , encoding : PngTextEncoding
    , text : String
    }


type PngTextCompression
    = PngTextUncompressed
    | PngTextCompressed


type PngTextEncoding
    = PngTextEncodingLatin1
    | PngTextEncodingUtf8
        { language : String
        , translatedKeyword : String
        }


type PngTextKeyword
    = TitleKeyword
    | AuthorKeyword
    | DescriptionKeyword
    | CopyrightKeyword
    | CreationTimeKeyword
    | SoftwareKeyword
    | DisclaimerKeyword
    | WarningKeyword
    | SourceKeyword
    | CommentKeyword
    | XMPKeyword
    | OtherKeyword String


textKeywordToString : PngTextKeyword -> String
textKeywordToString keyword =
    case keyword of
        TitleKeyword ->
            "Title"

        AuthorKeyword ->
            "Author"

        DescriptionKeyword ->
            "Description"

        CopyrightKeyword ->
            "Copyright"

        CreationTimeKeyword ->
            "Creation Time"

        SoftwareKeyword ->
            "Software"

        DisclaimerKeyword ->
            "Disclaimer"

        WarningKeyword ->
            "Warning"

        SourceKeyword ->
            "Source"

        CommentKeyword ->
            "Comment"

        XMPKeyword ->
            "XML:com.adobe.xmp"

        OtherKeyword oth ->
            oth


stringToTextKeyword : String -> PngTextKeyword
stringToTextKeyword keyword =
    case keyword of
        "Title" ->
            TitleKeyword

        "Author" ->
            AuthorKeyword

        "Description" ->
            DescriptionKeyword

        "Copyright" ->
            CopyrightKeyword

        "Creation Time" ->
            CreationTimeKeyword

        "Software" ->
            SoftwareKeyword

        "Disclaimer" ->
            DisclaimerKeyword

        "Warning" ->
            WarningKeyword

        "Source" ->
            SourceKeyword

        "Comment" ->
            CommentKeyword

        "XML:com.adobe.xmp" ->
            XMPKeyword

        _ ->
            OtherKeyword keyword


{-| -}
type alias BmpHeader =
    { width : Int
    , height : Int
    , fileSize : Int
    , pixelStart : Int
    , dibHeader : Int
    , color_planes : Int
    , bitsPerPixel : BmpBitsPerPixel
    , compression : Int
    , dataSize : Int
    }


{-| -}
type alias GifHeader =
    { width : Int
    , height : Int
    }


{-| -}
type FromDataColor
    = FromDataChannel1 FromDataBitDepth
    | FromDataChannel2 FromDataBitDepth
    | FromDataChannel3 FromDataBitDepth
    | FromDataChannel4 FromDataBitDepth


{-| -}
type FromDataBitDepth
    = FromDataBitDepth1
    | FromDataBitDepth2
    | FromDataBitDepth4
    | FromDataBitDepth8
    | FromDataBitDepth16


{-| -}
type BmpBitsPerPixel
    = BmpBitsPerPixel8
    | BmpBitsPerPixel16
    | BmpBitsPerPixel24
    | BmpBitsPerPixel32


{-| -}
type PngColor
    = Greyscale BitDepth1_2_4_8_16
    | GreyscaleAlpha BitDepth8_16
    | TrueColour BitDepth8_16
    | TrueColourAlpha BitDepth8_16
    | IndexedColour BitDepth1_2_4_8


{-| -}
type BitDepth1_2_4_8_16
    = BitDepth1_2_4_8_16__1
    | BitDepth1_2_4_8_16__2
    | BitDepth1_2_4_8_16__4
    | BitDepth1_2_4_8_16__8
    | BitDepth1_2_4_8_16__16


{-| -}
type BitDepth1_2_4_8
    = BitDepth1_2_4_8__1
    | BitDepth1_2_4_8__2
    | BitDepth1_2_4_8__4
    | BitDepth1_2_4_8__8


{-| -}
type BitDepth8_16
    = BitDepth8_16__8
    | BitDepth8_16__16
