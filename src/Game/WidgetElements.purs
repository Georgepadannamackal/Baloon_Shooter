module Game.WidgetElements where

import Prelude
import Data.Number.Format
import Data.Int
import Game.Types
import Halogen.VDom (VDom)
import UI.Core (Attr)

import UI.Elements (imageView, linearLayout, relativeLayout, textView)
import UI.Events (onClick)
import UI.Properties (background, height, id_, imageUrl, margin, text, width, textSize, centerInParent)

baloonDraw :: forall a. StateType -> Baloon -> VDom Attr a
baloonDraw s idpos =
              imageView
              [id_ idpos.id
              , width "50"
              , height "100"
              , margin ((toString (toNumber (idpos.x)))<>","<>(toString ((toNumber (idpos.y))))<>",0,0")
              , imageUrl idpos.image
              ]

arrowDraw :: forall a. StateType -> Arrow -> VDom Attr a
arrowDraw s id = imageView
            [ id_ id.id
            , width "60"
            , height "7"
            , margin ((toString (toNumber id.x))<>","<>(toString (toNumber (id.y + 47) ))<>",0,0")
            , imageUrl "arrow"
            ]
