module Lib (main) where

import Control.Monad.Representable.Reader (Representable, liftR2)
import Data.Distributive
import Data.Distributive.Generic (genericCollect)
import Data.IORef
import Data.List (mapAccumL)
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import FRP.Yampa
import GHC.Generics
import Optics
import qualified SDL
import SDLUtil

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

type Controls = Controls' SDL.Scancode

instance Distributive Controls' where collect = genericCollect

instance Representable Controls'

makeFieldLabelsNoPrefix ''Controls'

data Settings = Settings
  { controls :: Controls
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''Settings

data SystemState = SystemState
  { window :: SDL.Window,
    renderer :: SDL.Renderer,
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
    { upKey = SDL.ScancodeW,
      leftKey = SDL.ScancodeA,
      downKey = SDL.ScancodeS,
      rightKey = SDL.ScancodeD
    }

defaultControlsDev :: Controls
defaultControlsDev =
  Controls
    { upKey = SDL.ScancodeL,
      leftKey = SDL.ScancodeN,
      downKey = SDL.ScancodeR,
      rightKey = SDL.ScancodeT
    }

getDefaultControls :: Controls
getDefaultControls = defaultControlsDev

data Sensed = Sensed
  { down :: Controls' Bool
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''Sensed

data External = External
  { keyBuffer :: Controls' Bool
  }

makeFieldLabelsNoPrefix ''External

sense :: (State External :> es, Reader SystemState :> es, IOE :> es) => Eff es Sensed
sense = do
  SystemState {settings = Settings {controls}} <- ask
  let handle :: Controls' Bool -> SDL.Event -> (Controls' Bool, ())
      handle keyBuffer ev = (,()) $
        case SDL.eventPayload ev of
          SDL.KeyboardEvent keyboardEvent ->
            let motion = SDL.keyboardEventKeyMotion keyboardEvent
                scancode = SDL.keysymScancode $ SDL.keyboardEventKeysym keyboardEvent
             in liftR2
                  ( \scancode' current ->
                      if scancode' == scancode then motion == SDL.Pressed else current
                  )
                  controls
                  keyBuffer
          _ -> keyBuffer

  events <- SDL.pollEvents
  External {keyBuffer} <- get
  let (down, _) = mapAccumL handle keyBuffer events
  put External {keyBuffer = down}
    >> return
      Sensed
        { down = down
        }

velVec :: (RealFloat a) => a -> SF Sensed (SDL.V2 a)
velVec velocity = proc sensed -> do
  let down = sensed ^. #down

  let vec =
        sum $
          map
            (\(b, v) -> (if b then v else SDL.V2 0 0))
            [ (down ^. #upKey, SDL.V2 0 (-1)),
              (down ^. #leftKey, SDL.V2 (-1) 0),
              (down ^. #downKey, SDL.V2 0 1),
              (down ^. #rightKey, SDL.V2 1 0)
            ]

  ivec <- integral -< vec

  returnA -< ivec SDL.^* velocity

initialize :: IO SystemState
initialize = do
  window <-
    SDL.createWindow
      "App"
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight
        }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  return
    SystemState
      { window = window,
        renderer = renderer,
        settings =
          Settings
            { controls = getDefaultControls
            }
      }

render :: (Reader SystemState :> es, IOE :> es) => SDL.V2 Double -> Eff es Bool
render vec = do
  SystemState {window, renderer} <- ask

  liftIO $ do
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
    SDL.clear renderer

    _ <- SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
    _ <- SDL.fillRect renderer (Just (mkRect (round $ vec ^. _1) (round $ vec ^. _2) 16 16))

    SDL.present renderer

    return False

game :: IO ()
game = do
  lastTicksRef <- liftIO $ newIORef 0
  externalRef <-
    liftIO $
      newIORef
        External
          { keyBuffer = fmap (const False) defaultControls
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
        currentTicks <- liftIO SDL.ticks
        lastTicks' <- liftIO $ atomicModifyIORef lastTicksRef (currentTicks,)
        let dt = fromIntegral (currentTicks - lastTicks') / 1000
        sensed <- doSense
        return (dt, Just sensed)
    )
    (\_ val -> runEff $ runReader systemState $ render val)
    (velVec 200)

main :: IO ()
main = game
