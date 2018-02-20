module Main where
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import DOM (DOM)
import Data.Array (foldl, nubBy, sortBy, (..))
import Data.Int (toNumber)
import Data.Number.Format (toString)
import Data.Traversable (traverse)
import FRP (FRP)
import FRP.Event as E
import FRP.Event.Keyboard as K
import FRP.Event.Time (animationFrame)
import Halogen.VDom (VDom)
import UI.Core (MEvent, AttrValue(Some), Attr)
import UI.Elements (imageView, linearLayout, relativeLayout, textView)
import UI.Events (onClick)
import UI.Properties (background, height, id_, imageUrl, margin, text, width, textSize, centerInParent)
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
arrowCount   =  25

baloonCount  :: Int
baloonCount  = 5

baloonSpeed  :: Int
baloonSpeed  = 4

arrowSpeed   :: Int
arrowSpeed   = 8

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
,  score     :: Int
,  scorePos  :: {x :: String, y :: String}
,  arrows    :: Int
}

widget :: forall a. StateType → VDom Attr a
widget s = relativeLayout
                [  id_ "1"
                  , height "match_parent"
                  , width "match_parent"
                ]
                [
                 imageView
                  [ id_ "background"
                  , width "match_parent"
                  , height "match_parent"
                  , imageUrl "background"
                  ],
                  relativeLayout
                  [
                    id_ "gameHolder"
                  , width "900"
                  , height "match_parent"
                  , centerInParent "true,-1"
                  ]
                  [
                    textView
                    [ id_ "InstructionsHeading"
                    , width "250"
                    , height "900"
                    , textSize "25"
                    , text "HOW TO PLAY :"
                    , margin "-250, 100, 0, 0"
                    ]
                    ,textView
                    [ id_ "Instructions"
                    , width "250"
                    , height "900"
                    , textSize "20"
                    , text "1. Pop as many baloons as possible using 25 arrows\n2.Release arrows by clicking on the bow or Pressing Space\n3.Move bow Up and Down by clicking above & below it or Using arrow Keys\n4.The game is over when you run out of arrows"
                    , margin "-250, 150, 0, 0"
                    ]
                    ,
                    relativeLayout
                    [ id_ "2"
                    , height "match_parent"
                    , width "1000"
                    , background "#ffffff"
                    ]
                      ((baloonDraw s <$> s.baloon)<>
                       (arrowDraw s <$> s.arrow)<>
                      [
                      textView
                      [ id_ "Arrow Count"
                      , width "200"
                      , height "50"
                      , textSize "30"
                      , text (("Arrows :") <> (toString $ toNumber s.arrows))
                      , margin "850, 0, 0, 0"
                      ]
                      ,
                      textView
                      [ id_ "Score Board"
                      , text (("GAME OVER\nSCORE : \t\t") <> (toString $ toNumber s.score))
                      , textSize "30"
                      , width "350"
                      , height "50"
                      , margin (s.scorePos.x <> "," <> s.scorePos.y <> ",0,0")
                      ]
                      ,
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
               ]

baloonVal ::forall a. Int -> Eff(random :: RANDOM | a) Baloon
baloonVal a =
  randomInt 0 85 >>= \n ->
    randomInt 0 85 >>= \m ->
        pure{ x: n * 10, y: (m * 10) + 900, id: ("b" <> (toString (toNumber a))), image: "baloon",poped: false}

arrowVal :: Int -> Arrow
arrowVal a = {x:1000,y:320,id:"a"<>(toString (toNumber a)),shot:false}

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
  a <- (traverse baloonVal (1 .. baloonCount))
  _ <- U.updateState "baloon" a
  _ <- U.updateState "arrow" (arrowVal <$> (1.. arrowCount))
  _ <- U.updateState "shotCount" 0
  _ <- U.updateState "score" 0
  _ <- U.updateState "arrows" arrowCount
  _ <- U.updateState "scorePos" {x: "0", y:"-30"}
  logShow 23
  U.updateState "bow" {y: 320}

main :: forall a. Eff(random :: RANDOM, console :: CONSOLE, frp ::FRP, dom ::DOM |a) Unit
main = do
  U.initializeState
  state <- resetGame
  U.render (widget state) listen
  pure unit

baloonCollision :: Baloon -> Arrow -> Baloon
baloonCollision bal arr
  | (((bal.x > arr.x) && (bal.x < (arr.x + arrowWidth))) || ((arr.x > bal.x) && (arr.x < (bal.x + baloonWidth)))) && ((bal.y < (arr.y + 47)) && ((bal.y + baloonHeight) > (arr.y + 47))) = {x: bal.x , y: 900, id: bal.id, image : "empty",poped : true}
  | bal.y < -100  = {x: bal.x , y: 900, id: bal.id,image : bal.image, poped : bal.poped}
  | bal.y <  900  = {x: bal.x , y: (bal.y - baloonSpeed), id: bal.id,image : "baloon", poped : false}
  | otherwise = {x: bal.x , y: (bal.y - baloonSpeed), id: bal.id, image : bal.image,poped : bal.poped}

arrowShoot :: Arrow -> Arrow
arrowShoot a
  |a.shot = {x : a.x - arrowSpeed, y: a.y, id: a.id, shot: a.shot}
  |otherwise = a

scoreUpdater:: Baloon -> Int
scoreUpdater a
  | a.poped = 10
  | otherwise = 0

randomizeBaloonRespawn :: forall a. StateType -> Baloon -> Eff(random :: RANDOM | a) Baloon
randomizeBaloonRespawn s bal
  | s.arrows == 0 && bal.y == 900 = pure {x: bal.x , y: -200, id: bal.id,image : bal.image, poped : false}
  | bal.y == 900  = randomInt 0 85 >>= \n -> pure {x: (n * 10) , y: (bal.y - baloonSpeed), id: bal.id,image : "baloon", poped : false}
  | otherwise     = pure bal

eval :: forall t50 t51. Boolean → Eff (random :: RANDOM|t50) { | t51 }
eval l = do
  (s :: StateType) <- U.getState
  e <- (traverse (randomizeBaloonRespawn s) s.baloon)
  let c = sortBy baloonSort (baloonCollision <$> e <*> s.arrow)
  let d = nubBy baloonEqual c
  let f = foldl (+) s.score (scoreUpdater <$> d)
  (s :: StateType) <- U.updateState "baloon" d
  (s :: StateType) <- U.updateState "score" f
  _ <- U.updateState "arrow" (arrowShoot <$> s.arrow)
  if s.arrows == 0
    then
      U.updateState "scorePos" {x: "440", y: "300"}
    else
      U.updateState "scorePos" s.scorePos

eval0 :: forall a b. Boolean -> Eff a { | b }
eval0 l = do
  (s :: StateType) <- U.getState
  if (l) && (s.arrows /= 0)
    then do
      _ <- U.updateState "shotCount" (s.shotCount + 1)
      _ <- U.updateState "arrows" (s.arrows - 1)
      U.updateState "arrow" ((arrowSot ("a"<> (toString $ toNumber s.shotCount))) <$> s.arrow)
    else
      U.updateState "showCount" (s.shotCount)

eval1 :: forall a b. Boolean -> Eff a { | b }
eval1 l=do
  (s :: StateType) <- U.getState
  if (l)
    then do
      (s :: StateType) <- U.updateState "bow" {y:(s.bow.y - 20)}
      U.updateState "arrow" ((arrowYUpdater s.bow.y) <$> s.arrow)
    else
      U.updateState "bow" s.arrow

eval2 :: forall a b. Boolean -> Eff a { | b }
eval2 l=do
  (s :: StateType) <- U.getState
  if (l)
    then do
      (s :: StateType) <- U.updateState "bow" {y:(s.bow.y + 20)}
      U.updateState "arrow" ((arrowYUpdater s.bow.y) <$> s.arrow)
    else
      U.updateState "bow" s.bow

listen :: forall t133. Eff ( random::RANDOM,frp ∷ FRP , console ∷ CONSOLE | t133 ) (Eff (random::RANDOM, frp ∷ FRP , console ∷ CONSOLE | t133 ) Unit )
listen = do
  (state :: StateType) <- resetGame
  shoot <- U.signal "bow" false
  bu    <- U.signal "bowup" false
  bd    <- U.signal "bowdn" false
  _<- K.down `E.subscribe` (\key -> void $ case key of
      38 -> do
            _ <- U.updateState "bow" {y:(state.bow.y - 20)}
            _ <-U.updateState "arrow" ((arrowYUpdater state.bow.y) <$> state.arrow)
            U.getState
      40 -> do
            _ <- U.updateState "bow" {y:(state.bow.y + 20)}
            _ <-U.updateState "arrow" ((arrowYUpdater state.bow.y) <$> state.arrow)
            U.getState
      32 ->
            if (state.arrows /= 0)
            then do
              _ <- U.updateState "shotCount" (state.shotCount + 1)
              _ <- U.updateState "arrows" (state.arrows - 1)
              U.updateState "arrow" ((arrowSot ("a"<> (toString $ toNumber state.shotCount))) <$> state.arrow)
            else
              U.updateState "showCount" (state.shotCount)
      _  -> U.getState
    )


  let behavior = eval <$> shoot.behavior
  let events = (animationFrame)

  let behavior0 = eval0 <$> shoot.behavior
  let events0 = (shoot.event)
  --
  let behavior1 = eval1 <$> bu.behavior
  let events1 = (bu.event)

  let behavior2 = eval2 <$> bd.behavior
  let events2 = (bd.event)

  _ <- U.patch widget behavior0 events0
  _ <- U.patch widget behavior1 events1
  _ <- U.patch widget behavior2 events2
  U.patch widget behavior events
