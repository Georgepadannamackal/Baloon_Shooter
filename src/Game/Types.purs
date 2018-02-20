module Game.Types where

type Baloon = {
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

type StateType = {
   baloon    :: Array Baloon
,  arrow     :: Array Arrow
,  bow       :: Bow
,  shotCount :: Int
,  score     :: Int
,  scorePos  :: {x :: String, y :: String}
,  arrows    :: Int
}
