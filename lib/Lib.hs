module Lib (game) where

import Controls
import Data.IORef
import Effectful.Environment
import Effectful.Reader.Static
import Effectful.State.Static.Local
import FRP.Yampa
import GHC.Float (float2Double)
import RL

targetFPS, screenWidth, screenHeight :: (Num a) => a
targetFPS = 60
screenWidth = 1280
screenHeight = 720

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

makeFieldLabelsNoPrefix ''Settings
makeFieldLabelsNoPrefix ''SystemState
makeFieldLabelsNoPrefix ''Sensed
makeFieldLabelsNoPrefix ''External

sense :: (State External :> es, Reader SystemState :> es, IOE :> es) => Eff es Sensed
sense = do
  SystemState {settings = Settings {controls}} <- ask
  down <- liftIO $ traverse RL.isKeyDown controls
  return
    Sensed
      { down = down
      }

velVec :: Float -> SF Sensed RL.Vector2
velVec velocity = proc sensed -> do
  let down = sensed ^. #down

  let vec =
        sum $
          map
            (\(b, v) -> (if b then v else RL.Vector2 0 0))
            [ (down ^. #upKey, RL.Vector2 0 (-1)),
              (down ^. #leftKey, RL.Vector2 (-1) 0),
              (down ^. #downKey, RL.Vector2 0 1),
              (down ^. #rightKey, RL.Vector2 1 0)
            ]

  ivec <- integral -< vec

  returnA -< ivec RL.|* velocity

initialize :: (Environment :> es, IOE :> es) => Eff es SystemState
initialize = do
  window <- liftIO $ RL.initWindow screenWidth screenHeight "Game"
  liftIO $ RL.setTargetFPS targetFPS
  controls <- getDefaultControls

  return
    SystemState
      { window = window,
        settings =
          Settings
            { controls = controls
            }
      }

render :: (Reader SystemState :> es, IOE :> es) => RL.Vector2 -> Eff es Bool
render vec = do
  SystemState {window} <- ask

  liftIO $ do
    RL.beginDrawing

    RL.clearBackground RL.white
    RL.drawRectangle (round $ vec ^. _1) (round $ vec ^. _2) 64 64 RL.red

    RL.endDrawing

  return False

game :: IO ()
game = do
  externalRef <-
    liftIO $
      newIORef
        External
          {
          }

  systemState <- runEff $ runEnvironment initialize

  let doSense :: IO Sensed
      doSense = do
        external <- readIORef externalRef
        (sensed, external') <- runEff $ runReader systemState $ runState external sense
        writeIORef externalRef external' >> return sensed

  reactimate
    doSense
    ( \_ -> do
        frameTime <- RL.getFrameTime
        sensed <- doSense
        return (float2Double frameTime, Just sensed)
    )
    (\_ val -> runEff $ runReader systemState $ render val)
    (velVec 200)
