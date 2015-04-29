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


-- example :: Diagram B
-- example = circle 1 # fc red # lw none ||| circle 1 # fc green # lw none

r = sketchTurtle (
    setHeading 90 >> 
    forward 5 >>
    right 90 >> 
    replicateM 5 (forward 0.9 >> right 36) >> 
    forward 0.9 >> 
    left 135 >> 
    forward 3
  )
  # reversePath
  # stroke
  # lw 5
  # lineJoin LineJoinRound
  # lineCap LineCapRound
  # lc orange
--  # (withName "end" $ atop . place turtle . location)
--  where
--    turtle = eqTriangle 1 # scaleY 1.3 # rotate (-135 @@ deg)
 --            # lw 0.1

-- logo = (hcat' with {sep = 0.5} . map alignB $ [ r ]) # centerXY
example :: Diagram B
example = r

main = mainWith (example :: Diagram B)