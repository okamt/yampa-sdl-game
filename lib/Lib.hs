module Lib (main) where

import Control.Monad.Representable.Reader (Representable)
import Data.Distributive
import Data.Distributive.Generic (genericCollect)
import Data.IORef
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import FRP.Yampa
import GHC.Float (float2Double)
import GHC.Generics
import Optics
import qualified Raylib.Core as RL
import qualified Raylib.Core.Shapes as RL
import qualified Raylib.Types as RL
import qualified Raylib.Util as RL
import qualified Raylib.Util.Colors as RL
import qualified Raylib.Util.Math as RL
import RaylibUtil ()

targetFPS :: (Num a) => a
targetFPS = 60

screenWidth :: (Num a) => a
screenWidth = 1280

screenHeight :: (Num a) => a
screenHeight = 720

data Controls' a = Controls
  { upKey :: a,
    leftKey :: a,
    downKey :: a,
    rightKey :: a
  }
  deriving (Show, Functor, Foldable, Traversable, Generic1)

type Controls = Controls' RL.KeyboardKey

instance Distributive Controls' where collect = genericCollect

instance Representable Controls'

makeFieldLabelsNoPrefix ''Controls'

data Settings = Settings
  { controls :: Controls
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''Settings

data SystemState = SystemState
  { window :: RL.WindowResources,
    settings :: Settings
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''SystemState

data GameState = GameState
  {
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''GameState

defaultControls :: Controls
defaultControls =
  Controls
    { upKey = RL.KeyW,
      leftKey = RL.KeyA,
      downKey = RL.KeyS,
      rightKey = RL.KeyD
    }

defaultControlsDev :: Controls
defaultControlsDev =
  Controls
    { upKey = RL.KeyL,
      leftKey = RL.KeyN,
      downKey = RL.KeyR,
      rightKey = RL.KeyT
    }

getDefaultControls :: Controls
getDefaultControls = defaultControlsDev

data Sensed = Sensed
  { down :: Controls' Bool
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''Sensed

data External = External
  {
  }

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

initialize :: IO SystemState
initialize = do
  window <- RL.initWindow screenWidth screenHeight "Game"
  RL.setTargetFPS targetFPS

  return
    SystemState
      { window = window,
        settings =
          Settings
            { controls = getDefaultControls
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

  systemState <- initialize

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

main :: IO ()
main = game
