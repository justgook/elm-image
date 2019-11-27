[![Build Status](https://travis-ci.org/justgook/elm-image.svg?branch=master)](https://travis-ci.org/justgook/elm-image)

A library for building base64 encoded images in elm

## Motivation

[WebGL for Elm](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/) do not support arrays, so I need to build lookup tables for that, to prevent doing it in preprocess was created this library that can convert matrix (`List Int`) into image, and then used in shader ([Example3](#example3)).

## Usage

### Encoding

Package provides tools for encoding elm data types into [`Image`][Image]:
 - `List Pixel`
 - `List (List Pixel)`
 - `Array Pixel`
 - `Array (Array Pixel)`

> Where each `Pixel` is 4 channel color is `Int` (`0xRRGGBBAA`)

Other way is use [`avh4/elm-color#Color`][elm-color]: 
 - `List Color`
 - `List (List Color)`
 - `Array Color`
 - `Array (Array Color)`
 
[`Image`][Image] can be encoded into `Bytes` with [`Image.toPng`](Image#Image.toPng) or [`Image.toBmp`](Image#Image.toBmp) or directly to base64-url ([`Image.toPngUrl`](Image#toPngUrl) or [`Image.toBmpUrl`](Image.toBmpUrl)) that can be used directly in `img [src]` ([Example1](#example1)). 

You can find [package][package] to use in your project and [Demo][demo].

### Decoding

Package can take `Bytes` of png or bmp image (even those that some browsers can not display), and decode them to [`Image`][Image] ([Example2](#example2))

Use case:
 - get image [dimensions](Image#dimensions), without rendering image
 - [mirror](Image-Magic#mirror) image
 - crop image (coming soon)
 - change / update / remove color using [`Image.Advanced.map`](Image-Advanced#map) 
 - get image header inf (format, color, size..) [`Image.Advanced.info`](Image-Advanced#info)
 - convert image from one format ot other

[package]: https://package.elm-lang.org/packages/justgook/elm-image/latest/
[demo]: https://justgook.github.io/elm-image/
[Image]: Image#Image
[elm-color]: https://package.elm-lang.org/packages/avh4/elm-color/latest/

## Example1

Data to image

```elm
import Base64
import Html exposing (img)
import Html.Attributes exposing (src)
import Image
import Image.Data as Image exposing (Image)
import Image.Options

main =
    let
        imageData : Image
        imageData =
            Image.fromList2d
                [ List.repeat 4 0xFFFF
                , List.repeat 4 0xFF0000FF
                , List.repeat 4 0xFFFF
                , List.repeat 2 0x00FFFFFF
                ]

        pngEncodeBase64Url =
            Image.toPngUrl imageData

    in
    img [ src pngEncodeBase64Url ] []
```

## Example2

Getting image from server

```elm
import Http

   type Msg
       = GotImage (Result Http.Error (Maybe Image))

   getImage : Cmd Msg
   getImage =
       Http.get
           { url = "/image.png"
           , expect = Http.expectBytes GotImage Image.decode
           }
```
## Example3

Create texture using base64 encoded image and load it as [`Texture`](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/WebGL-Texture#load)
```elm
textureTask = WebGL.Texture.load pngEncodeBase64Url
-- then use resulting texture as lookup table
```

You can use simple function to get data from lookup table, where `color` is pixel color from just created texture
```glsl
float color2float(vec4 color) {
    return
    color.a * 255.0
    + color.b * 256.0 * 255.0
    + color.g * 256.0 * 256.0 * 255.0
    + color.r * 256.0 * 256.0 * 256.0 * 255.0;
    }
```
