{-|
Module: Step
Description: Attempts at understanding smoothstep

Quick Start:
1. $ runghci Step.hs
2. $ feh rough.png
3. $ feh smooth.png
-}
module Step where

import System.Process

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

roughYs :: [Float]
roughYs = map (roughstep edge0 edge1) xs

smoothYs :: [Float]
smoothYs = map (smoothstep edge0 edge1) xs

renderYs :: [Float] -> String
renderYs ys = unlines $ map show ys

dumpYs :: IO ()
dumpYs = do
  writeFile "rough.csv" $ renderYs roughYs
  writeFile "smooth.csv" $ renderYs smoothYs

plotYs :: IO ()
plotYs = do
  dumpYs
  callCommand "gnuplot -c step.gnuplot"

main :: IO ()
main = plotYs
