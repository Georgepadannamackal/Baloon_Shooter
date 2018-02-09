module Main where

import Data.String
import Data.String
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (stack)
import Control.Plus ((<|>))
import DOM.HTML.Event.ErrorEvent (lineNo)
import DOM.HTML.History (back)
import FRP as F
import FRP.Event as E
import Halogen.VDom.Types (graft)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util as U

foreign import click :: MEvent
foreign import change :: MEvent

widget state = linearLayout
              [ id_ "1"
              , height "match_parent"
              , width "match_parent"
              , background "#A4D3EE"
              , gravity "center"
              , orientation "vertical"
              ]
              [ textView
                  [
                    id_ "2"
                  , height "30"
                  , width "100"
                  , gravity "center"
                  , text "User Name"
                  ],
                editText
                  [
                    id_ "3"
                  , height "30"
                  , width "200"
                  , text ""
                  , padding "10,10,10,10"
                  , onChange (Some change)
                  ],
                textView
                  [
                    id_ "4"
                  , height "30"
                  , width "200"
                  , gravity "center"
                  , margin "0,20,0,0"
                  , text "Password"
                  ],
                editText
                  [
                    id_ "5"
                  , height "30"
                  , width "200"
                  , padding "10,10,10,10"
                  , text ""
                  , onChange (Some change)
                  ],
                linearLayout
                  [
                    id_ "6"
                  , height "20"
                  , width "100"
                  , background (state.background)
                  , margin "0,20,0,0"
                  , text "Login"
                  , gravity "center"
                  , color "#ffffff"
                  , onClick (Some click)
                  ]
                  []
                  ]

main = do
  --- Init State {} empty record--
  U.initializeState

  --- Update State ----
  state <- U.updateState "background" "#C0C0C0"
  _ <- U.updateState "x" ""
  _ <- U.updateState "y" ""

  ---- Render Widget ---
  U.render (widget state) listen

  pure unit

eval x y = do
      let xBol = length x /= 0
      let yBol = length y /= 0
      let s = xBol && yBol

      _ <- U.updateState "x" x
      _ <- U.updateState "y" y

      if s
          then
           U.updateState "background" "blue"
        else
           U.updateState "background" "#C0C0C0"


listen = do
  sig3 <- U.signal "3" ""
  sig5 <- U.signal "5" ""
  sig6 <- U.signal "6" false

  _ <- sig6.event `E.subscribe` (\_ -> do
                                        s <- U.getState
                                        log s.x
                                        log s.y
                                         )

  
  let behavior = eval <$> sig3.behavior <*> sig5.behavior 
  let events = (sig3.event <|> sig5.event)

  U.patch widget behavior events