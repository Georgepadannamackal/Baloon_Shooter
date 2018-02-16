module Main where
import Data.Array
import Data.Int
import Data.Maybe
import Data.String
import Math
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Plus ((<|>))
import DOM.HTML.HTMLProgressElement (position)
import DOM.Node.Document (doctype)
import Data.Int (toNumber)
import Data.Number.Format (toString)
import Data.Traversable (traverse)
import FRP (FRP)
import FRP as F
import FRP.Behavior (behavior)
import FRP.Behavior.Mouse as Mouse
import FRP.Event as E
import FRP.Event.Mouse (down)
import FRP.Event.Time (animationFrame)
import FRP.Event.Time as T
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Properties (width, height)
import UI.Util as U

foreign import click :: MEvent
foreign import change :: MEvent

type Baloon = {
  x  :: Int
, y  :: Int
, id :: String
}

type Arrow ={
  x   :: Eff(random::RANDOM) Int
, y   :: Eff(random::RANDOM) Int
, id  :: String
, shot :: Boolean
}

type Bow ={
  y    :: Int
}

type StateType = {
   baloon :: Array Baloon
,  arrow  :: Array Arrow
,  bow    :: Bow
,  y      :: Int
}

widget s = linearLayout
                [  id_ "1"
                  , height "match_parent"
                  , width "match_parent"
                  , background "#00FF00"
                  , gravity "center"
                  , orientation "vertical"
                ]
                [
                  relativeLayout
                  [ id_ "2"
                  , height "800"
                  , width "1000"
                  , background "#ffffff"
                  ]
                    ((baloonDraw s <$> s.baloon)<>
                    [
                    linearLayout
                    [id_ "bowup"
                    , width "50"
                    , height "850"
                    , onClick (Some click)
                    , margin ( "1000,"<>(toString (toNumber (s.bow.y - 800)))<>",0,0")]
                    [],
                    imageView
                    [
                      id_ "bow"
                    , width "50"
                    , height "100"
                    , onClick (Some click)
                    , margin ( "1000,"<>(toString (toNumber s.bow.y))<>",0,0")
                    , imageUrl "bow"
                    ]
                    ,linearLayout
                    [id_ "bowdn"
                    , width "50"
                    , height "800"
                    , onClick (Some click)
                    , margin ( "1000,"<>(toString (toNumber (s.bow.y + 50)))<>",0,0")]
                    []
                  ])
               ]
--baloonVal:: Int -> Eff(random :: RANDOM) Baloon
baloonVal a =
  randomInt 50 850 >>= \n -> pure{ x: n, y: (toNumber( n * n ) % 1000.0), id: ("b" <> (toString (toNumber a)))}

baloonDraw s idpos =
              imageView
              [id_ idpos.id
              , width "50"
              , height "100"
              , margin ((toString (toNumber (idpos.x)))<>","<>(toString ((900.0 - toNumber (s.y - idpos.y) % 900.0 - 100.0)))<>",0,0")
              , imageUrl "baloon"
              ]
--traverse :: ∀ a b m t. Traversable t ⇒ Applicative m ⇒ (a → m b) → t a → m (t b)
resetGame = do
  (s::StateType) <- U.getState
  a <- (traverse baloonVal (1 .. 10))
  _ <- U.updateState "baloon" a
  _ <- U.updateState "y" 0
  _ <- U.updateState "arrow" [{id:"a1",x:0,y:0}]
  logShow 23
  U.updateState "bow" {y: 0}

main = do
  U.initializeState
  state <- resetGame
  U.render (widget state) listen
  pure unit

--eval :: ∀ t45 t50 t51. t45 → Eff t50 { | t51 }
eval l = do
  (s :: StateType) <- U.getState
  _ <- U.updateState "y" (s.y + 2)
  if l
    then
      U.updateState "arrow" s.arrow
    else
      U.updateState "arrow" s.arrow


eval1 l =do
  s <- U.getState
  if l
    then do
      U.updateState "bow" {y:(s.bow.y - 20)}
    else
      U.updateState "bow" s.arrow

eval2 l =do
  s <- U.getState
  if l
    then do
      U.updateState "bow" {y:(s.bow.y + 20)}
    else
      U.updateState "bow" s.bow

--listen :: forall t133. Eff ( frp ∷ FRP , console ∷ CONSOLE | t133 ) (Eff ( frp ∷ FRP , console ∷ CONSOLE | t133 ) Unit )
listen = do
  state <- resetGame
  shoot <- U.signal "bow" false
  bu    <- U.signal "bowup" false
  bd    <- U.signal "bowdn" false

  let behavior = eval <$> shoot.behavior
  let events = (animationFrame)

  let behavior1 = eval1 <$> bu.behavior
  let events1 = (bu.event)

  let behavior2 = eval2 <$> bd.behavior
  let events2 = (bd.event)

  _ <- U.patch widget behavior1 events1
  _ <- U.patch widget behavior2 events2
  U.patch widget behavior events
