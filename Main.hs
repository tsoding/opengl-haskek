module Main where

import Graphics.UI.GLUT
import Control.Exception
import qualified Data.ByteString as B
import Control.Monad
import Text.Printf
import Data.IORef
import Data.Int

data State = State
  { stateProg :: Program
  , statePosition :: IORef (Vector2 GLfloat)
  }

vertShaderFile :: FilePath
vertShaderFile = "shader.vert"

fragShaderFile :: FilePath
fragShaderFile = "random.frag"

objectPositionUniform :: String
objectPositionUniform = "objectPosition"

uResolution :: String
uResolution = "u_resolution"

uMousePosition :: String
uMousePosition = "u_mouse_position"

backgroundColor :: Color4 GLfloat
backgroundColor = Color4 0.0 0.0 0.0 1.0

display :: State -> DisplayCallback
display State {stateProg = prog, statePosition = positionRef} = do
  clearColor $= backgroundColor
  clear [ColorBuffer, DepthBuffer]
  position <- readIORef positionRef
  setUniform prog objectPositionUniform position
  drawArrays Quads 0 8
  flush
  swapBuffers

motion :: State -> MotionCallback
motion State {stateProg = prog} (Position x y) =
  setUniform prog uMousePosition $
  Vector2 (fromIntegral x :: GLfloat) (fromIntegral y :: GLfloat)

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

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [RGBMode, WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size windowWidth windowHeight
  _ <- createWindow "if it compiles, it twerks haHAA"
  checkGLSLSupport
  vs <- readAndCompileShader VertexShader vertShaderFile
  fs <- readAndCompileShader FragmentShader fragShaderFile
  prog <- installShaders [vs, fs]
  state <-
    do position <- newIORef $ Vector2 0.0 0.0
       return $ State prog position
  setUniform
    prog
    uResolution
    (Vector2
       (fromIntegral windowWidth :: GLfloat)
       (fromIntegral windowHeight :: GLfloat))
  displayCallback $= display state
  motionCallback $= Just (motion state)
  addTimerCallback timerFrequencyMillis (timer state)
  mainLoop

------------------------------

clamp :: Ord a => a -> a -> a -> a
clamp x low high = min (max low x) high

roughstep :: Float -> Float -> Float -> Float
roughstep edge0 edge1 x = t
    where t = clamp ((x - edge0) / (edge1 - edge0)) 0.0 1.0

smoothstep :: Float -> Float -> Float -> Float
smoothstep edge0 edge1 x = t * t * (3.0 - 2.0 * t)
    where t = clamp ((x - edge0) / (edge1 - edge0)) 0.0 1.0

edge0 :: Float
edge0 = 0.0

edge1 :: Float
edge1 = 200.0

xs :: [Float]
xs = [edge0 .. edge1]

roughYs :: [(Float, Float)]
roughYs = zip xs (map (roughstep edge0 edge1) xs)

smoothYs :: [(Float, Float)]
smoothYs = zip xs (map (smoothstep edge0 edge1) xs)

renderYs :: [(Float, Float)] -> String
renderYs ys = unlines $ map (\(x, y) -> printf "%f,%f" x y) ys

dumpYs :: IO ()
dumpYs = do
  writeFile "rough.csv" $ renderYs roughYs
  writeFile "smooth.csv" $ renderYs smoothYs
