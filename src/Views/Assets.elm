module Views.Assets exposing (error, src)

{-| Assets, such as images, videos, and audio. (We only have images for now.)
We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!
-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attr


type Image
    = Image String



-- IMAGES --


error : Image
error =
    Image "https://www.packagor.com/wp-content/uploads/2017/03/404-error-not-found.jpg"



-- USING IMAGES --


src : Image -> Attribute msg
src (Image url) =
    Attr.src url
