import Graphics.UI.GLUT
import Data.IORef
import Genome
import GenomeDraw

main :: IO()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display (Genome.createInitialGenome 2 3)
  mainLoop

display :: Genome.Genome -> DisplayCallback
display genome = do
  clear [ ColorBuffer ]
--  GenomeDraw.getConnectionWeightColor $ negate 1.0
  renderPrimitive Points $
    mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) (GenomeDraw.genomePoints genome)
  flush
