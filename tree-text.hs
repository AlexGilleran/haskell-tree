{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Turtle
import Diagrams.TwoD.Types
import           Diagrams.Coordinates   
import           Diagrams.Prelude

import Diagrams.Backend.SVG
import Control.Monad

import           Diagrams.TwoD.Layout.Tree
import           Diagrams.TwoD.Path.Turtle

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

evolve :: Int -> Int -> T.Text -> T.Text
evolve maxIterations currentIterations text
    | currentIterations < maxIterations = evolve maxIterations (currentIterations + 1) replaced
    | otherwise = replaced
  where
    replaced = T.replace "F" "FF" (T.replace "X" "F-[[X]+X]+F[+FX]-X" text)

--stepsToTurtle :: (OrderedField n, Monad m) => [TurtleT n m ()] -> Turtle n ()
stepsToTurtle steps = foldl (>>) (setHeading 0) steps

--drawTree :: (OrderedField n, Monad m) => [TurtleT n m ()] -> Diagram B
drawTree turtleSteps = sketchTurtle (stepsToTurtle turtleSteps)
  # reversePath
  # stroke
  # lw 5
  # lineJoin LineJoinRound
  # lineCap LineCapRound
  # lc green

charToFunction :: (OrderedField n,  Monad m) => Char -> TurtleT n m ()
charToFunction char
  | char == 'F' = forward 5
  | char == '-' = left 25
  | char == '+' = right 25
  | otherwise = forward 0

generateTreeSource:: Int -> T.Text -> T.Text
generateTreeSource recursions start = evolve recursions 0 start

generateSteps :: (OrderedField n, Monad m) => T.Text -> [TurtleT n m ()]
generateSteps source = map charToFunction (T.unpack source)

f :: Int -> String -> Diagram B
f recursions start = drawTree (generateSteps (generateTreeSource recursions (T.pack start)))

main = mainWith f 