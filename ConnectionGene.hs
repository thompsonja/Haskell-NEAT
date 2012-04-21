module ConnectionGene where
	   
import NodeGene

data ConnectionGene =
  ConnectionGene { from         :: NodeGene
                 , to           :: NodeGene
                 , weight       :: Double
                 , enabled      :: Bool
                 , innovationId :: Int
                 }
  deriving (Eq)

isLoopedConnection :: ConnectionGene -> Bool
isLoopedConnection connection = (from connection) == (to connection)

instance Show ConnectionGene where
  show (ConnectionGene from to weight True innovationId) = show innovationId ++ ":" ++ show from ++ " -> " ++ show to ++ " (C:" ++ show weight ++ ")\n"
  show (ConnectionGene from to weight False innovationId) = show innovationId ++ ":" ++ show from ++ " -> " ++ show to ++ " Disabled\n"