{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Turtle
import Diagrams.TwoD.Types
import           Diagrams.Coordinates   
import           Diagrams.Prelude

import Diagrams.Backend.SVG

import           Diagrams.TwoD.Layout.Tree
import           Diagrams.TwoD.Path.Turtle

import           Control.Monad

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

evolve :: Int -> Int -> T.Text -> T.Text
evolve maxIterations currentIterations text
    | currentIterations < maxIterations = evolve maxIterations (currentIterations + 1) replaced
    | otherwise = replaced
  where
    replaced = T.replace "F" "FF" (T.replace "X" "F-[[X]+X]+F[+FX]-X" text)

blahFace2 = [
    setHeading 0,
    forward 5,
    right 90,
 --   replicateM 5 (forward 0.9 >> right 36),
    forward 0.9,
    left 135,
    forward 3]

blahFace = 
    setHeading 0 >> 
    forward 5 >>
    right 90 >> 
    replicateM 5 (forward 0.9 >> right 36) >> 
    forward 0.9 >> 
    left 135 >> 
    forward 3

blahFace3 = foldl (>>) (setHeading 0) blahFace2

drawTree :: Double -> Diagram B
drawTree recursions = sketchTurtle blahFace3
  # reversePath
  # stroke
  # lw 5
  # lineJoin LineJoinRound
  # lineCap LineCapRound
  # lc green

f :: Double -> String -> Diagram B
f recursions start = drawTree recursions

main = mainWith f 