module Lib where

import Controls
import Eff
import FRP.Yampa
import GHC.Float (float2Double)
import Game
import RL

targetFPS, screenWidth, screenHeight :: (Num a) => a
targetFPS = 60
screenWidth = 1280
screenHeight = 720

sense :: (Eff.State External :> es, Eff.Reader SystemState :> es, IOE :> es) => Eff es Sensed
sense = do
  SystemState {settings = Settings {controls}} <- Eff.ask
  down <- liftIO $ traverse RL.isKeyDown controls
  return
    Sensed
      { down = down
      }

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

render :: (Eff.Reader SystemState :> es, IOE :> es) => GameState -> Eff es Bool
render gameState = do
  liftIO $ do
    RL.beginDrawing

    RL.clearBackground RL.white
    let RL.Vector2 x y = (gameState ^. #playerState % #position)
     in RL.drawRectangle (round x) (round y) 64 64 RL.red

    RL.endDrawing

  return False

runGame :: IO ()
runGame = do
  externalRef <-
    liftIO $
      newIORef
        External
          {
          }

  systemState <- runEff $ runEnvironment initialize

  let senseWithExternal = do
        external <- readIORef externalRef
        (sensed, external') <- runEff $ Eff.runReader systemState $ Eff.runState external sense
        writeIORef externalRef external' >> return sensed
      frameSense _ = do
        frameTime <- RL.getFrameTime
        sensed <- senseWithExternal
        return (float2Double frameTime, Just sensed)
      actuate _ = runEff . Eff.runReader systemState . render
   in reactimate
        senseWithExternal
        frameSense
        actuate
        game
