module Main where
import DOM (DOM)
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
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt)
--import Control.Plus ((<|>))
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
import Halogen.VDom (VDom)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Properties (width, height)
import UI.Util as U

foreign import click :: MEvent
foreign import change :: MEvent

baloonWidth  :: Int
baloonWidth  =  50

baloonHeight :: Int
baloonHeight =  100

arrowWidth   :: Int
arrowWidth   =  60

arrowHeight  :: Int
arrowHeight  =  7

arrowCount   :: Int
arrowCount   =  10

type Baloon = {
  x     :: Int
, y     :: Int
, id    :: String
, image :: String
, poped :: Boolean
}

baloonSort :: Baloon -> Baloon -> Ordering
baloonSort a b
  | a.id /= b.id = (compare a.id b.id)
  | otherwise = (compare b.poped a.poped)

baloonEqual :: Baloon -> Baloon -> Boolean
baloonEqual a b = (a.id == b.id)-- && (a.poped == b.poped)

type Arrow ={
  x    :: Int
, y    :: Int
, id   :: String
, shot :: Boolean
}

type Bow ={
  y    :: Int
}

type StateType = {
   baloon    :: Array Baloon
,  arrow     :: Array Arrow
,  bow       :: Bow
,  shotCount :: Int
}

widget :: forall a. { baloon ∷ Array Baloon , arrow ∷ Array Arrow , bow ∷ Bow , shotCount ∷ Int } → VDom Attr a
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
                     (arrowDraw s <$> s.arrow)<>
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

--baloonVal ::forall a. Int -> Eff(random :: RANDOM | a) Baloon
baloonVal a =
  randomInt 50 850 >>= \n -> pure{ x: n, y: (toNumber( n * n ) % 1000.0), id: ("b" <> (toString (toNumber a))), image: "baloon",poped: false}

arrowVal :: Int -> Arrow
arrowVal a = {x:1000,y:0,id:"a"<>(toString (toNumber a)),shot:false}

arrowSot :: String -> Arrow -> Arrow
arrowSot check a
  | a.id == check = {x:a.x,y:a.y,id:a.id,shot:true }
  | otherwise = a

arrowYUpdater :: Int -> Arrow -> Arrow
arrowYUpdater bowY a
  | a.shot = a
  | otherwise = {x:a.x,y:bowY,id:a.id,shot:a.shot }

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

--traverse :: ∀ a b m t. Traversable t ⇒ Applicative m ⇒ (a → m b) → t a → m (t b)
resetGame :: forall a b. Eff(random :: RANDOM, console :: CONSOLE | a) { |b }
resetGame = do
  (s::StateType) <- U.getState
  a <- (traverse baloonVal (1 .. 10))
  _ <- U.updateState "baloon" a
  _ <- U.updateState "arrow" (arrowVal <$> (1.. arrowCount))
  _ <- U.updateState "shotCount" 0
  logShow 23
  U.updateState "bow" {y: 0}

main :: forall a. Eff(random :: RANDOM, console :: CONSOLE, frp ::FRP, dom ::DOM |a) Unit
main = do
  U.initializeState
  state <- resetGame
  U.render (widget state) listen
  pure unit

baloonCollision :: Baloon -> Arrow -> Baloon
baloonCollision bal arr
  | (((bal.x > arr.x) && (bal.x < (arr.x + arrowWidth))) || ((arr.x > bal.x) && (arr.x < (bal.x + baloonWidth)))) && (((bal.y < arr.y) && (bal.y > (arr.y - arrowHeight))) || ((arr.y < bal.y) && (arr.y > (bal.y + baloonHeight)))) = {x: bal.x , y: (bal.y - 2), id: bal.id, image : "empty",poped : true}
  | bal.y < -100  = {x: bal.x , y: 900, id: bal.id,image : bal.image, poped : bal.poped}
  | otherwise = {x: bal.x , y: (bal.y - 2), id: bal.id, image : bal.image,poped : bal.poped}

-- baloonMove :: Baloon -> Baloon
-- baloonMove bal
--   |bal.y < -100  = {x: bal.x , y: 900, id: bal.id,image : bal.image, poped : bal.poped}
--   |otherwise      = {x: bal.x , y: (bal.y - 2), id: bal.id, image: bal.image,poped : bal.poped}

arrowShoot :: Arrow -> Arrow
arrowShoot a
  |a.shot = {x : a.x - 2, y: a.y, id: a.id, shot: a.shot}
  |otherwise = a


eval :: forall t50 t51. Boolean → Eff t50 { | t51 }
eval l = do
  (s :: StateType) <- U.getState
  -- _ <- U.updateState "baloon" (baloonMove <$> s.baloon)
  let c = sortBy baloonSort (baloonCollision <$> s.baloon <*> s.arrow)
  let d = nubBy baloonEqual c
  _ <- U.updateState "baloon" d --(baloonMove <$> s.baloon)
  if l
    then
      U.updateState "arrow" (arrowShoot <$> s.arrow)
    else
      U.updateState "arrow" s.arrow

eval0 :: forall a b. Boolean -> Eff a { | b }
eval0 l = do
  (s :: StateType) <- U.getState
  if l
    then do
      _ <- U.updateState "shotCount" (s.shotCount + 1)
      U.updateState "arrow" ((arrowSot ("a"<> (toString $ toNumber s.shotCount))) <$> s.arrow)
    else
      U.updateState "showCount" (s.shotCount + 1)

eval1 :: forall a b. Boolean -> Eff a { | b }
eval1 l =do
  (s :: StateType) <- U.getState
  if l
    then do
      (s :: StateType) <- U.updateState "bow" {y:(s.bow.y - 20)}
      U.updateState "arrow" ((arrowYUpdater s.bow.y) <$> s.arrow)
    else
      U.updateState "bow" s.arrow

eval2 :: forall a b. Boolean -> Eff a { | b }
eval2 l =do
  (s :: StateType) <- U.getState
  if l
    then do
      (s :: StateType) <- U.updateState "bow" {y:(s.bow.y + 20)}
      U.updateState "arrow" ((arrowYUpdater s.bow.y) <$> s.arrow)
    else
      U.updateState "bow" s.bow

listen :: forall t133. Eff ( random::RANDOM,frp ∷ FRP , console ∷ CONSOLE | t133 ) (Eff (random::RANDOM, frp ∷ FRP , console ∷ CONSOLE | t133 ) Unit )
listen = do
  state <- resetGame
  shoot <- U.signal "bow" false
  bu    <- U.signal "bowup" false
  bd    <- U.signal "bowdn" false

  let behavior = eval <$> shoot.behavior
  let events = (animationFrame)

  let behavior0 = eval0 <$> shoot.behavior
  let events0 = (shoot.event)

  let behavior1 = eval1 <$> bu.behavior
  let events1 = (bu.event)

  let behavior2 = eval2 <$> bd.behavior
  let events2 = (bd.event)

  _ <- U.patch widget behavior0 events0
  _ <- U.patch widget behavior1 events1
  _ <- U.patch widget behavior2 events2
  U.patch widget behavior events
