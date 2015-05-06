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
import qualified Control.Monad.State                as ST

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
stepsToTurtle steps = foldl (>>) (setHeading 90) steps

--drawTree :: (OrderedField n, Monad m) => TurtleT n m () -> Diagram B
drawTree turtleSteps = sketchTurtle turtleSteps
  # reversePath
  # stroke
  # lw 5
  # lineJoin LineJoinRound
  # lineCap LineCapRound
  # lc green

-- 
-- charToFunctionAccumulator :: (OrderedField n,  Monad m) => ([(a, b)], [TurtleT n m ()]) -> Char -> ([(a, b)], [TurtleT n m ()])
--charToFunctionAccumulator :: (OrderedField n, Monad m) => ([(Double, Double)], TurtleT n m ()) -> Char -> ([(Double, Double)], TurtleT n m ())
charToFunctionAccumulator acc char
  | char == 'F' = (stack, monad >> forward 5)
  | char == '-' = (stack, monad >> left 25)
  | char == '+' = (stack, monad >> right 25) 
  | char == '[' = (push (ST.get monad heading, ST.get monad pos) stack, monad)
  | char == ']' = (poppedStack, monad >> setHeading 25)
  | otherwise = (stack, monad) --forward 0
  where
    stack = fst acc -- :: [(OrderedField n, Point V2 a)]
    monad = snd acc
    ((poppedHeading, poppedPosition), poppedStack) = pop stack :: ((Double, Double), [(Double, Double)])

-- >> setPos (snd poppedState)

-- third :: TurtleT n m n -> n
-- third (_, _, x) = x

generateTreeSource:: Int -> T.Text -> T.Text
generateTreeSource recursions start = evolve recursions 0 start

-- generateSteps :: (OrderedField n, Monad m) => T.Text -> TurtleT n m ()
generateSteps source = snd (foldl charToFunctionAccumulator ([], setHeading 90) (T.unpack source))

f :: Int -> String -> Diagram B
f recursions start = drawTree (generateSteps (generateTreeSource recursions (T.pack start)))

main = mainWith f 

push :: (a, b) -> [(a, b)] -> [(a, b)]
push elem stack = elem : stack

pop :: [(a, b)] -> ((a, b), [(a, b)])  -- return a tuple containing the popped element and the new stack
pop [] = error "Can't pop from an empty stack!"
pop ((:) x stack) = (x, stack)
