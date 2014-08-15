module GenomeDraw where

import Genome
import Graphics.UI.GLUT

color3f r g b = color $ Color3 r g (b :: GLfloat)

genomePoints :: Genome.Genome -> [(GLfloat, GLfloat, GLfloat)]
genomePoints genome = sensor ++ hidden ++ output
    where nodes = getNodeCounts genome
          scaleFn len x = (2 * fromIntegral x) / (1 + fromIntegral len) - 1
          sensor = [ (a, -0.5, 0) | a <- map (scaleFn (nodes !! 0)) [1..nodes !! 0] ]
          hidden = [ (a, 0, 0) | a <- map (scaleFn (nodes !! 1)) [1..nodes !! 1] ]
          output = [ (a, 0.5, 0) | a <- map (scaleFn (nodes !! 2)) [1..nodes !! 2] ]

getConnectionWeightColor conn
    | conn > 0 = color3f (1.0 - conn) (1.0 - conn) 1.0
    | conn <= 0 = color3f 1.0 (1.0 + conn) (1.0 + conn)
