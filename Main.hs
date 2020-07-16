module Main where

import Graphics.UI.GLUT
import Control.Exception
import qualified Data.ByteString as B
import Control.Monad
import Text.Printf
import Data.IORef

data State = State
  { stateProg :: Program
  , statePosition :: IORef (Vector2 GLfloat)
  }

backgroundColor :: Color4 GLfloat
backgroundColor = Color4 1.0 0.0 0.0 1.0

display :: State -> DisplayCallback
display State {stateProg = prog, statePosition = positionRef} = do
  clearColor $= backgroundColor
  clear [ColorBuffer, DepthBuffer]
  position <- readIORef positionRef
  setUniform prog objectPositionUniform position
  drawArrays Quads 0 8
  flush
  swapBuffers

timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

timer :: State -> TimerCallback
timer state@(State {statePosition = positionRef}) = do
  position <- readIORef positionRef
  modifyIORef positionRef (fmap (+ objectVelocity))
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

setUniform :: Uniform a => Program -> String -> a -> IO ()
setUniform prog var val = do
  location <- uniformLocation prog var
  reportErrors
  uniform location $= val

objectPositionUniform :: String
objectPositionUniform = "objectPosition"

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

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [RGBMode, WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size 640 480
  _ <- createWindow "if it compiles, it twerks haHAA"
  checkGLSLSupport
  vs <- readAndCompileShader VertexShader "shader.vert"
  fs <- readAndCompileShader FragmentShader "shader.frag"
  prog <- installShaders [vs, fs]
  state <- do
    position <- newIORef $ Vector2 0.0 0.0
    return $ State prog position
  displayCallback $= display state
  addTimerCallback timerFrequencyMillis (timer state)
  mainLoop
