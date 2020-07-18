module Main where

import Graphics.UI.GLUT
import Control.Exception
import qualified Data.ByteString as B
import Control.Monad
import Text.Printf
import Data.IORef
import Data.Int
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

data State = State
  { stateProg :: IORef (Maybe Program)
  , stateTime :: IORef Float
  }

vertShaderFile :: FilePath
vertShaderFile = "shaders/shader.vert"

fragShaderFile :: FilePath
fragShaderFile = "shaders/epic-animation.frag"

uResolution :: String
uResolution = "u_resolution"

uMousePosition :: String
uMousePosition = "u_mouse"

uTime :: String
uTime = "u_time"

backgroundColor :: Color4 GLfloat
backgroundColor = Color4 0.0 0.0 0.0 1.0

display :: State -> DisplayCallback
display State {stateProg = progRef} = do
  prog <- readIORef progRef
  setUniform
    prog
    uResolution
    (Vector2
       (fromIntegral windowWidth :: GLfloat)
       (fromIntegral windowHeight :: GLfloat))

  clearColor $= backgroundColor
  clear [ColorBuffer, DepthBuffer]
  drawArrays Quads 0 8
  flush
  swapBuffers

keyboard :: State -> KeyboardCallback
keyboard state@State{stateProg = progRef} 'r' _ = do
  prog <- readIORef progRef
  currentProgram $= Nothing
  deleteObjectNames $ maybeToList prog
  newProg <- reloadShaders
  writeIORef progRef newProg
keyboard _ _ _ = return ()

motion :: State -> MotionCallback
motion State {stateProg = progRef} (Position x y) = do
  prog <- readIORef progRef
  setUniform prog uMousePosition $
    Vector2 (fromIntegral x :: GLfloat) (fromIntegral y :: GLfloat)

timerFrequencyMillis :: Timeout
timerFrequencyMillis = 16

timer :: State -> TimerCallback
timer state@State {stateProg = progRef, stateTime = timeRef} = do
  time <- readIORef timeRef
  prog <- readIORef progRef
  setUniform prog uTime time
  modifyIORef' timeRef $ \time -> time + 1.0 / fromIntegral timerFrequencyMillis
  postRedisplay Nothing
  addTimerCallback timerFrequencyMillis (timer state)

readAndCompileShader :: ShaderType -> FilePath -> IO Shader
readAndCompileShader st filePath = do
  src <- B.readFile filePath
  shader <- createShader st
  shaderSourceBS shader $= src
  compileShader shader
  reportErrors
  ok <- get (compileStatus shader)
  infoLog <- get (shaderInfoLog shader)
  mapM_ putStrLn ["Shader info log for '" ++ filePath ++ "':", infoLog, ""]
  unless ok $ do
    deleteObjectNames [shader]
    ioError (userError "shader compilation failed")
  return shader

setUniform :: Uniform a => Maybe Program -> String -> a -> IO ()
setUniform (Just prog) var val = do
  location <- uniformLocation prog var
  reportErrors
  uniform location $= val
setUniform Nothing _ _ = return ()

objectVelocity :: GLfloat
objectVelocity = 0.01

installShaders :: [Shader] -> IO Program
installShaders shaders = do
  prog <- createProgram
  attachedShaders prog $= shaders
  linkProgram prog
  reportErrors
  ok <- get (linkStatus prog)
  infoLog <- get (programInfoLog prog)
  mapM_ putStrLn ["Program info log:", infoLog, ""]
  unless ok $ do
    deleteObjectNames [prog]
    ioError (userError "linking failed")
  currentProgram $= Just prog
  return prog

checkGLSLSupport :: IO ()
checkGLSLSupport = do
  version <- get (majorMinor glVersion)
  printf "OpenGL version: %s\n" $ show version
  putStrLn "Extensions:"
  extensions <- get glExtensions
  mapM_ (\x -> printf "  %s...\n" x) extensions
  unless (version >= (2, 0)) $ do
    unless ("GL_ARB_shading_language_100" `elem` extensions) $
      ioError (userError "No GLSL support found.")

windowWidth :: Int32
windowWidth = 800

windowHeight :: Int32
windowHeight = 480

reloadShaders :: IO (Maybe Program)
reloadShaders =
  catch
    (do currentProgram $= Nothing
        vs <- readAndCompileShader VertexShader vertShaderFile
        fs <- readAndCompileShader FragmentShader fragShaderFile
        prog <- installShaders [vs, fs]
        return $ Just prog)
    (\e -> do
       print (e :: IOException)
       return Nothing)

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [RGBMode, WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size windowWidth windowHeight
  _ <- createWindow "if it compiles, it twerks haHAA"
  checkGLSLSupport
  prog <- reloadShaders
  state <- do
    timeRef <- newIORef 0.0
    progRef <- newIORef prog
    return $ State progRef timeRef
  displayCallback $= display state
  motionCallback $= Just (motion state)
  keyboardCallback $= Just (keyboard state)
  addTimerCallback timerFrequencyMillis (timer state)
  mainLoop
