module NodeGene where

data NodeType = Sensor | Hidden | Output
  deriving (Eq, Enum)

instance Show NodeType where
  show Sensor = "S";
  show Hidden = "H";
  show Output = "O";

data NodeGene =
  NodeGene { nodeType :: NodeType
           , activationLevel :: Double
           , nodeId :: Int
           }
  
instance Show NodeGene where
  show (NodeGene ntype level nid) = show nid ++ show ntype ++ show level
  
instance Eq NodeGene where
  x == y = nodeId x == nodeId y
  x /= y = nodeId x /= nodeId y
