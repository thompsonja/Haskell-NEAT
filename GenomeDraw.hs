module GenomeDraw where

import Genome
import Graphics.UI.GLUT

genomePoints :: Genome.Genome -> [(GLfloat, GLfloat, GLfloat)]
genomePoints genome = sensor ++ hidden ++ output
    where nodes = getNodeCounts genome
          scaleFn len x = (2 * fromIntegral x) / (1 + fromIntegral len) - 1
          sensor = [ (a, -0.5, 0) | a <- map (scaleFn (nodes !! 0)) [1..nodes !! 0] ]
--          hidden = [ (a, 0, 0) | a <- map (scaleFn (nodes !! 1)) [1..nodes !! 1] ]
          hidden = []
          output = [ (a, 0.5, 0) | a <- map (scaleFn (nodes !! 2)) [1..nodes !! 2] ]
