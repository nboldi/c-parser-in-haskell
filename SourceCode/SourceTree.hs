{-# LANGUAGE LambdaCase, NamedFieldPuns, DeriveFunctor, DeriveDataTypeable #-}

-- | A simpler representation of the original AST. Enables easy relative indexing of the nodes.
module SourceCode.SourceTree where

import Data.Maybe
import Data.Data

-- | Relative indexing of nodes in a 'SourceRose' tree
data NodeIndex = Current 
               | ParentOf NodeIndex 
               | NthChildOf Int NodeIndex
     deriving (Eq, Ord)
          
instance Show NodeIndex where
  show Current = "."
  show (ParentOf ni) = show ni ++ "/.."
  show (NthChildOf i ni) = show ni ++ "/" ++ show i
   
-- | Evaluates a node indexing, from the path to current elem generates the path to the indexed elem
resolveRoseInd :: Show a => NodeIndex -> [SourceRose a] -> Either String [SourceRose a]
resolveRoseInd Current path = Right path
resolveRoseInd (ParentOf i) path 
  = resolveRoseInd i path >>= \case (_:par) -> Right par
                                    [] -> Left "empty path"
resolveRoseInd (NthChildOf n i) path
  = resolveRoseInd i path >>= 
      \case (path@(SourceRose {roseChildren} : _)) -> 
              if n >= 0 && n < length roseChildren 
                then Right ((roseChildren !! n) : path) 
                else Left $ "no " ++ show n ++ "th child"
            [] -> Left "empty path"
   
-- | Modifies a root index by a relative one
addRelPath :: RootIndex -> NodeIndex -> RootIndex
addRelPath ind Current = ind
addRelPath ind1 (ParentOf ind2) 
  = case ind1 `addRelPath` ind2 of RootIndex _ ind -> ind
                                   _ -> error "No parent"
addRelPath ind1 (NthChildOf i2 ind2) 
  = RootIndex i2 (ind1 `addRelPath` ind2)

-- | Concatenates two relative indexes
addPath :: NodeIndex -> NodeIndex -> NodeIndex
ind `addPath` Current = ind
ind `addPath` (ParentOf ni) = ParentOf (ind `addPath` ni)
ind `addPath` (NthChildOf i ni) = NthChildOf i (ind `addPath` ni)
   
-- | Indexing of nodes in a 'SourceRose' tree from root
data RootIndex = Root | RootIndex Int RootIndex
     deriving (Eq)
     
instance Show RootIndex where
  show Root = ""
  show (RootIndex i ni) = show ni ++ "/" ++ show i

rootPrefixOf :: RootIndex -> RootIndex -> Bool
Root `rootPrefixOf` Root = True
(RootIndex i1 ind1) `rootPrefixOf` (RootIndex i2 ind2) | i1 == i2 
  = ind1 `rootPrefixOf` ind2
ind1 `rootPrefixOf` (RootIndex _ ind2)
  = ind1 `rootPrefixOf` ind2
_ `rootPrefixOf` _
  = False
     
resolveRootInd :: RootIndex -> SourceRose a -> SourceRose a
resolveRootInd (RootIndex i ind) tree 
  = let SourceRose {roseChildren} = resolveRootInd ind tree
     in roseChildren !! i
resolveRootInd Root tree = tree

pathToRoot :: RootIndex -> NodeIndex
pathToRoot (RootIndex _ ind) = ParentOf (pathToRoot ind)
pathToRoot Root = Current

pathFromRoot :: RootIndex -> NodeIndex
pathFromRoot (RootIndex i ind) = NthChildOf i (pathFromRoot ind)
pathFromRoot Root = Current
     
depth :: RootIndex -> Int
depth Root = 0
depth (RootIndex _ ind) = depth ind + 1
     
indRelativelyTo :: RootIndex -> RootIndex -> NodeIndex
indRelativelyTo r1 r2 = indRel r1 r2 Current
  where indRel r1 r2 p
          | r1 == r2
          = p
        indRel r1 r2@(RootIndex i2 ind2) p
          | depth r1 < depth r2 
          = indRel r1 ind2 (ParentOf p)
        indRel (RootIndex i1 ind1) r2 p
          = NthChildOf i1 (indRel ind1 r2 p)

     
-- | A rose tree containing additional node information         
data SourceRose a = SourceRose { roseInfo :: a 
                               , original :: Bool
                               , roseChildren :: [SourceRose a]
                               } deriving (Eq, Functor, Typeable, Data)

instance Show a => Show (SourceRose a) where
  show sr = show' 0 sr
    where show' i (SourceRose {roseInfo,original,roseChildren})
             = "\n" ++ replicate (2*i) (if original then '#' else '-')
                    ++ show roseInfo 
                    ++ concatMap (show' (i+1)) roseChildren