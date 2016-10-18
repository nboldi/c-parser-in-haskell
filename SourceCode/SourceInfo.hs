module SourceCode.SourceInfo where

-- | Source information for an AST
class SourceInfo a where

  -- | Generates an information for a node from the information of the closest ancestor
  --  node with it's own info, and the information of it's children.
  generateInfo :: a -> [a] -> a
  
  -- | Generates a node with no info
  noNodeInfo :: a
  
  