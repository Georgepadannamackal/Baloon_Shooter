module Main where

import Control.Monad.Eff.Random
import Data.Array
import Data.Int
import Data.Maybe
import Data.Number.Format
import Data.Traversable
import FRP.Event.Time
import Prelude
import PrestoDOM.Core
import PrestoDOM.Elements
import PrestoDOM.Events
import PrestoDOM.Properties
import PrestoDOM.Types

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Plus ((<|>))
import DOM (DOM)
import Data.Lens ((^.))
import Data.String (length)
import FRP (FRP)
import FRP.Behavior (Behavior, animate, sample_)
import FRP.Behavior.Keyboard (key)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Keyboard (down)
import PrestoDOM.Util (getBeh, getEvent, logNode, render, updateState)
import PrestoDOM.Util as U

balloonWidth  :: Int
balloonWidth  =  50

balloonHeight :: Int
balloonHeight =  100

arrowWidth   :: Int
arrowWidth   =  60

arrowHeight  :: Int
arrowHeight  =  7

arrowCount   :: Int
arrowCount   =  25

balloonCount  :: Int
balloonCount  = 5

balloonSpeed  :: Int
balloonSpeed  = 4

arrowSpeed   :: Int
arrowSpeed   = 8

type Balloon = {
  x     :: Int
, y     :: Int
, id    :: String
, image :: String
, popped :: Boolean
}

type Arrow ={
  x    :: Int
, y    :: Int
, id   :: String
, shot :: Boolean
}

type Bow ={
  y    :: Int
}

type State = -- Eff (random :: RANDOM)
  {  balloon   :: Array Balloon
  ,  arrow     :: Array Arrow
  ,  bow       :: Bow
  ,  shotCount :: Int
  ,  score     :: Int
  ,  scorePos  :: {x :: String, y :: String, font :: String}
  ,  arrows    :: Int
  }


balloonSort :: Balloon -> Balloon -> Ordering
balloonSort a b
  | a.id /= b.id = (compare a.id b.id)
  | otherwise = (compare b.popped a.popped)

balloonEqual :: Balloon -> Balloon -> Boolean
balloonEqual a b = (a.id == b.id)-- && (a.popped == b.popped)

arrowShot :: String -> Arrow -> Arrow
arrowShot check a
  | a.id == check = a{ shot = true }
  | otherwise = a

arrowYUpdater :: Int -> Arrow -> Arrow
arrowYUpdater bowY a
  | a.shot = a
  | otherwise = a{y=bowY}

scoreUpdater:: Balloon -> Int
scoreUpdater a
  | a.popped = 5
  | otherwise = 0

arrowShoot :: Arrow -> Arrow
arrowShoot a
  |a.shot = a {x = a.x - arrowSpeed}
  |otherwise = a

balloonCollision :: Int -> Balloon -> Arrow -> Balloon
balloonCollision arrows bal arr
  | (((bal.x > arr.x) && (bal.x < (arr.x + arrowWidth)))
    || ((arr.x > bal.x) && (arr.x < (bal.x + balloonWidth))))
    && ((bal.y < (arr.y + 47))
    && ((bal.y + balloonHeight) > (arr.y + 47))) = bal { y= 900,image = "empty",popped = true}
  | bal.y < -100 && arrows > 0 = bal {y = 900}
  | bal.y <  900                = bal { y= (bal.y - balloonSpeed), image = "balloon", popped = false}
  | otherwise = bal{y = (bal.y - balloonSpeed)}

balloonVal ::forall a. Int -> Eff(random :: RANDOM | a) Balloon
balloonVal a =
  randomInt 0 85 >>= \n ->
    randomInt 0 85 >>= \m ->
        pure{ x: n * 10, y: (m * 10) + 900, id: ("b" <> (toString (toNumber a))), image: "balloon",popped: false}

arrowVal :: Int -> Arrow
arrowVal a = {x:1000,y:320,id:"a"<>(toString (toNumber a)),shot:false}

randomizeballoonRespawn :: forall a.Int -> Balloon -> Balloon
randomizeballoonRespawn s bal
  | bal.y == 900  = bal { x= 10 * s}
  | otherwise     = bal

shotUpdater :: Boolean -> Int -> Int
shotUpdater space value
  | space = value + 1
  | otherwise = value

bowUpdate :: Boolean -> Boolean -> Int -> Int
bowUpdate a b c
  | a = c + 10
  | b = c - 10
  | otherwise = c

main :: forall eff. Eff ( console :: CONSOLE, random::RANDOM, frp :: FRP, dom :: DOM | eff ) Unit
main = do
    a <- (traverse balloonVal (1 .. balloonCount))
    let initialState = { balloon : a
                        , arrow : (arrowVal <$> (1.. arrowCount))
                        , bow : {y : 320}
                        , shotCount : 0
                        , score: 0
                        , scorePos : { x : "0" , y : "-30", font : "30"}
                        , arrows : arrowCount }
    { stateBeh, updateState, push } <- render view initialState

    _ <- ((sample_ stateBeh animationFrame) `subscribe` (\state -> randomInt 0 85 >>= \n -> push state{balloon = (randomizeballoonRespawn n) <$> state.balloon} ))

    _ <- updateState (continouseval <$> stateBeh) (interval 25)
    _ <- updateState (keyboardEval <$> key 38 <*> key 40 <*> key 32 <*>stateBeh) (down)
    pure unit
  where continouseval oldState =do
          let c = sortBy balloonSort ((balloonCollision oldState.arrows) <$> oldState.balloon <*> oldState.arrow)
          let d = nubBy balloonEqual c
          let e = foldl (&&) true (balloongone <$> d)
          let f = foldl (+) oldState.score (scoreUpdater <$> d)
          let g = (arrowShoot <$> oldState.arrow)
          let h = ((arrowYUpdater oldState.bow.y )<$> g)
          oldState{ score = f, balloon= d , arrow = (arrowShot ("a" <> show oldState.shotCount)) <$> h, arrows = checkZero (25 - oldState.shotCount), scorePos = (getPos e)}

        keyboardEval up down space oldState = do
          let b = {y : (bowUpdate down up oldState.bow.y)}
          oldState{ bow = b, shotCount = shotUpdater space oldState.shotCount}

getPos :: Boolean -> {x :: String, y :: String, font :: String}
getPos a
  | a = {x: "300", y: "300", font: "70"}
  | otherwise = {x: "0", y: "-30", font: "30"}

balloongone :: Balloon -> Boolean
balloongone bal = bal.y < -100

checkZero :: Int -> Int
checkZero a
  | a < 0 = 0
  | otherwise = a

view :: forall w i. State
  -> PrestoDOM i w
view state =
  relativeLayout
    [ height Match_Parent
    , width Match_Parent
    ]
    [ imageView
      [ height Match_Parent
      , width Match_Parent
      , imageUrl "background"
      ],
      textView
      [  width $ V 250
      , height $ V 900
      , textSize "25"
      , text "HOW TO PLAY :"
      , margin "0,100,0,0"
      ],
      textView
      [ width $ V 250
      , height $ V 900
      , textSize "20"
      , margin "0,150,0,0"
      , text """1. Pop as many balloons as possible using 25 arrows
              2.Release arrows by clicking on the bow or Pressing Space
              3.Move bow Up and Down by clicking above & below it or Using arrow Keys
              4.The game is over when you run out of arrows"""
      ]
      ,
      relativeLayout
      [ height Match_Parent
      , width $ V 1000
      , background "#FFFFFF"
      , margin "250,0,0,0"
      ]
      ((balloonDraw state <$> state.balloon)<>
       (arrowDraw state <$> state.arrow)<>
      [textView
        [ width  $ V 200
        , height $ V 50
        , textSize "30"
        , text ("Arrows :" <> (toString $ toNumber state.arrows))
        , margin "850, 0, 0, 0"
        ] ,
        textView
        [ text ("GAME OVER\n\t\tSCORE : \t\t" <> (toString $ toNumber state.score))
        , textSize state.scorePos.font
        , width $ V 700
        , height $ V 50
        , margin (state.scorePos.x <> "," <> state.scorePos.y <> ",0,0")
        ]
        ,
        linearLayout
        [id_ "bowup"
        , width $ V 50
        , height $ V 850
        --, onClick (Some click)
        , margin ( "1000,0,0,0")]
        [],
        imageView
        [
          id_ "bow"
        , width $ V 50
        , height $ V 100
        --, onClick (Some click)
        , margin ( "1000,"<> show state.bow.y <>",0,0")
        , imageUrl "bow"
        ]
        ,linearLayout
        [id_ "bowdn"
        , width $ V 50
        , height $ V 800
        --, onClick (Some click)
        , margin ( "1000,0,0,0")]
        []
      ])
    ]

balloonDraw :: forall i w. State -> Balloon -> PrestoDOM i w
balloonDraw s idpos =
              imageView
              [ width $ V 50
              , height $ V 100
              , margin ((toString (toNumber (idpos.x)))<>","<>(toString ((toNumber (idpos.y))))<>",0,0")
              , imageUrl idpos.image
              ]

arrowDraw :: forall i w. State -> Arrow -> PrestoDOM i w
arrowDraw s id = imageView
            [ width $ V 60
            , height $ V 7
            , margin ((toString (toNumber id.x))<>","<>(toString (toNumber (id.y + 47) ))<>",0,0")
            , imageUrl "arrow"
            ]
