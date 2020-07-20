module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, readTQueue, writeTQueue)
import Control.Exception
import Control.Monad             (foldM, liftM, unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List                 (intercalate)
import Data.Maybe                (isJust, fromJust)
import Data.IntMap.Lazy (IntMap (..))
import qualified Data.IntMap.Lazy          as IntMap
import Data.Maybe                (catMaybes)
import Options.Applicative
import System.IO
import Text.PrettyPrint   hiding ((<>))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Message (Expr (..), Prim (..))
import GalaxyTxt (getGalaxyExprs, galaxyKey)
import qualified Interact
import qualified Send
import qualified NFEval

--------------------------------------------------------- -----------------------

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    , envExpr          :: IntMap Expr
    }

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateFBWidth         :: !Int
    , stateFBHeight        :: !Int
    , statePicture         :: [Interact.Image]
    , statePoint           :: Maybe (Int,Int)
    , stateState           :: Interact.State
    , stateHistory         :: [(Int, Int)]
    }

type Demo = RWST Env () State IO

sizeX, sizeY :: Int
sizeX = 180
sizeY = 130

--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !Bool
  | EventWindowIconify   !GLFW.Window !Bool
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

data Options
  = Options
  { optState :: Maybe Interact.State
  , optHistory :: Maybe [(Int, Int)]
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser = Options <$> state <*> history
  where
    state :: Parser (Maybe Interact.State)
    state = optional $ option auto $ mconcat
      [ long "state"
      , metavar "STR"
      , help "state (type: Interact.State)"
      -- , value Interact.SNil
      -- , showDefault
      ]

    history :: Parser (Maybe [(Int, Int)])
    history = optional $ option auto $ mconcat
      [ long "history"
      , metavar "STR"
      , help "history (type: [(Int,Int)])"
      ]

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) $ fullDesc

main :: IO ()
main = do
    opt <- execParser parserInfo
    when (isJust (optState opt) && isJust (optHistory opt)) $ do
      error "you can specify only one of --state and --history"

    let width  = 640
        height = 480

    eventsChan <- newTQueueIO :: IO (TQueue Event)
    clickChan <- newTQueueIO :: IO (TQueue (Int,Int))

    withWindow width height "GLFW-b-demo" $ \win -> do
        GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
        GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
        GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
        GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
        GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
        GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
        GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
        GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
        GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
        GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
        GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
        GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
        GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
        GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

        GLFW.swapInterval 1

        (fbWidth, fbHeight) <- GLFW.getFramebufferSize win
        (winWidth, winHeight) <- GLFW.getWindowSize win

        ps <- getGalaxyExprs
        let env = Env
              { envEventsChan    = eventsChan
              , envWindow        = win
              , envExpr          = IntMap.fromList ps
              }
            state_ = State
              { stateWindowWidth     = winWidth
              , stateWindowHeight    = winHeight
              , stateFBWidth         = fbWidth
              , stateFBHeight        = fbHeight
              , statePicture         = []
              , statePoint           = Just (0,0)
              , stateState           = Interact.SNil
              , stateHistory         = []
              }
        state <-
          case optState opt of
            Just s ->
              return $
                state_
                { statePoint           = Nothing
                , stateState           = s
                }
            Nothing ->
              case optHistory opt of
                Just history -> do
                  -- FIXME: run のものと共有する
                  let send val = do
                        -- FIXME
                        e <- Send.sendExpr (Interact.svToExpr val)
                        let px = asPixel $ NFEval.reduceNF' IntMap.empty e --XXX
                        return px
                      m = envExpr env
                      f s pt = liftM fst $ Interact.interact send m (m IntMap.! galaxyKey) s pt
                  s <- foldM f Interact.SNil history
                  return $ 
                    state_
                    { statePoint           = Nothing
                    , stateState           = s
                    , stateHistory         = history
                    }
                Nothing -> return state_
                  
        runDemo env state

    putStrLn "ended!"

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state =
    void $ evalRWST (adjustWindow >> run) env state

run :: Demo ()
run = do
    state <- get
    if isJust (statePoint state)
      then do
      let send val = do
            -- FIXME
            e <- Send.sendExpr (Interact.svToExpr val)
            let px = asPixel $ NFEval.reduceNF' IntMap.empty e --XXX
            return px
          st = stateState state
          pt = fromJust (statePoint state)
      m <- asks envExpr
      let h :: SomeException -> IO a
          h ex = do
            hPutStrLn stderr $ show ex
            hPutStrLn stderr $ "history: " ++ show (reverse (stateHistory state))
            hPutStrLn stderr $ "state: " ++ show st
            hPutStrLn stderr $ "click: " ++ show pt
            throwIO ex
      (st', images) <- liftIO $ handle h $ Interact.interact send m (m IntMap.! galaxyKey) st pt
      modify $ \s -> s
        { statePicture = images
        , stateState   = st'
        , statePoint   = Nothing
        , stateHistory = pt : stateHistory s
        }
      draw
      else
        return ()

    win <- asks envWindow
    liftIO $ do
        GLFW.swapBuffers win
        GL.flush  -- not necessary, but someone recommended it
        GLFW.pollEvents
    processEvents

    q <- liftIO $ GLFW.windowShouldClose win
    if q then do
      state <- get
      liftIO $ hPutStrLn stderr $ "history: " ++ show (reverse (stateHistory state))
      liftIO $ hPutStrLn stderr $ "state: " ++ show (stateState state)
    else do
      run

asPixel :: NFEval.NFValue -> (Int,Int)
asPixel (NFEval.NFPAp Cons [x,y]) = (NFEval.asNum x, NFEval.asNum y)
asPixel x = error $ "asPixel: " ++ show x

processEvents :: Demo ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()

processEvent :: Event -> Demo ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowPos _ x y) ->
          printEvent "window pos" [show x, show y]

      (EventWindowSize _ width height) -> do
          printEvent "window size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventWindowRefresh _) ->
          printEvent "window refresh" []

      (EventWindowFocus _ fs) ->
          printEvent "window focus" [show fs]

      (EventWindowIconify _ is) ->
          printEvent "window iconify" [show is]

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modify $ \s -> s
            { stateFBWidth  = width
            , stateFBHeight = height
            }
          adjustWindow

      (EventMouseButton _ mb mbs mk) -> do
          printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
          when (mb == GLFW.MouseButton'1 && mbs == GLFW.MouseButtonState'Released) $ do
              win <- asks envWindow
              (x,y) <- liftIO $ GLFW.getCursorPos win
              state <- get
              let width = stateWindowWidth state
                  height = stateWindowHeight state
                  x' = ((round x-(width`div`2))*2*sizeX)`div`width
                  y' = ((round y-(height`div`2))*2*sizeY)`div`height-- because GL.ortho (-sizeX) (sizeX)
              modify $ \s -> s
                { statePoint = Just (x', y')
                }
              printEvent "mouse clicked" [show x', show y']

      (EventCursorPos _ x y) -> do
          state <- get
          let width = stateWindowWidth state
              height = stateWindowHeight state
              x' = ((round x-(width`div`2))*2*sizeX)`div`width
              y' = ((round y-(height`div`2))*2*sizeY)`div`height
          printEvent "cursor pos" [show x', show y']

      (EventCursorEnter _ cs) -> do
          printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']

      (EventKey win k scancode ks mk) -> do
          printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]

      (EventChar _ c) ->
          printEvent "char" [show c]

adjustWindow :: Demo ()
adjustWindow = do
    state <- get
    let width  = stateFBWidth  state
        height = stateFBHeight state
        pos   = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
    liftIO $ do
        GL.viewport   GL.$= (pos, size)
        GL.matrixMode GL.$= GL.Projection
        GL.loadIdentity
        GL.ortho (fromIntegral(-sizeX)) (fromIntegral sizeX) (fromIntegral sizeY) (fromIntegral (-sizeY)) (-1.5) (1.5::GL.GLdouble)
    draw

draw :: Demo ()
draw = do
    env   <- ask
    state <- get
    let pics = statePicture state
    if pics /= []
      then liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        flip mapM_ (zip [1, 0.7, 0.5, 0.25, 0.10, 0.10, 0.10] pics) $ \(i,pic) -> do
          GL.color (GL.Color3 i i i :: GL.Color3 GL.GLfloat)
          GL.renderPrimitive GL.Quads $ mapM_ GL.vertex $ concatMap box pic
      else
        return ()

box :: (Int,Int) -> [GL.Vertex2 GL.GLdouble]
box (x,y) =
  [GL.Vertex2 a' b' | (a',b')<-[(a,b),(a+1,b),(a+1,b+1),(a,b+1)]]
  where
    a, b :: GL.GLdouble
    a = fromIntegral x
    b = fromIntegral y

--------------------------------------------------------------------------------

printEvent :: String -> [String] -> Demo ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

