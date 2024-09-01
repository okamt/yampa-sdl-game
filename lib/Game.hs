module Game where

import Controls
import FRP.Yampa
import RL

data Settings = Settings
  { controls :: Controls
  }
  deriving (Show)

data SystemState = SystemState
  { window :: RL.WindowResources,
    settings :: Settings
  }
  deriving (Show)

data Sensed = Sensed
  { down :: Controls' Bool
  }
  deriving (Show)

data External = External
  {
  }
  deriving (Show)

data GameState = GameState
  { playerState :: PlayerState
  }
  deriving (Show)

data PlayerState = PlayerState
  { position :: RL.Vector2
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''Settings
makeFieldLabelsNoPrefix ''SystemState
makeFieldLabelsNoPrefix ''Sensed
makeFieldLabelsNoPrefix ''External
makeFieldLabelsNoPrefix ''GameState
makeFieldLabelsNoPrefix ''PlayerState

type Game = SF Sensed GameState

game :: Game
game = proc sensed -> do
  playerState <- player 200 -< sensed

  returnA -< GameState {playerState = playerState}

type Player = SF Sensed PlayerState

player :: Float -> Player
player velocity = proc sensed -> do
  let down = sensed ^. #down
      vec =
        sum $
          map
            (\(b, v) -> (if b then v else RL.Vector2 0 0))
            [ (down ^. #upKey, RL.Vector2 0 (-1)),
              (down ^. #leftKey, RL.Vector2 (-1) 0),
              (down ^. #downKey, RL.Vector2 0 1),
              (down ^. #rightKey, RL.Vector2 1 0)
            ]

  ivec <- integral -< vec

  returnA
    -<
      PlayerState
        { position = ivec RL.|* velocity
        }