module SourceTemplates where

      
-- | Creates source templates from simple source strings
correctSourceInfo :: node BasicInfo -> node TemplateInfo
correctSourceInfo = cutOutTemplates 
                      . addRangesToGenerated 
                      . expandNodes
                     
-- | Expands nodes to contain all their children
expandNodes :: node BasicInfo -> node BasicInfo
expandNodes = runState $ traverse $ \case 
    InheritInfo -> expandNode <$> get <*> pure Nothing  
  
  error "expandNodes: Node cannot inherit info, because it has no children."
  where save :: State (node BasicInfo) -> 
  
        expandNode :: [BasicInfo] -> Maybe SourceRange -> Maybe SourceRange
        expandNode
  
expandNodes sr@(SourceRose _ []) = sr
expandNodes sr@(SourceRose (GeneratedInfo {}) children) 
  = sr { roseChildren = map expandNodes children }
expandNodes (SourceRose info children) 
  = let expandedChildren = map expandNodes children
        usedChildren = filter (not . isGenerated . roseInfo) expandedChildren 
        sortedInfo = sortBy (compare `on` biRange) (map roseInfo usedChildren)
        addSelfToRng = case info of BasicInfo { biRange = rng } -> srcRngUnion rng
                                    _ -> id
        rng = addSelfToRng ( (biRange . head $ sortedInfo) 
                                `srcRngUnion` (biRange . last $ sortedInfo) )
        input = case info of BasicInfo { biRange = rng, biInput = inp } -> 
                               if srcRangeBegin rng < srcRangeBegin (biRange $ head sortedInfo)
                                 then inp else (biInput $ head sortedInfo)
                             _ -> biInput $ head sortedInfo
     in SourceRose (BasicInfo rng input) expandedChildren

-- | Assigns source ranges to generated members.
-- Otherwise they would not appear as nodes to cut out from higher nodes.
addRangesToGenerated :: SourceRose BasicInfo -> SourceRose BasicInfo
addRangesToGenerated tree = undefined


-- addRanges' tree Root tree
  -- where addRanges' tree currInd (SourceRose gi@(GeneratedInfo {giTemplate}) children)
          -- = SourceRose (gi { giRange = collectSourceRange giTemplate tree currInd }) 
                       -- (updateChildren tree currInd children)
        -- addRanges' tree currInd sr@(SourceRose _ children)
          -- = sr { roseChildren = updateChildren tree currInd children }
        
        -- updateChildren tree currInd children 
          -- = map (\(child,i) -> addRanges' tree (RootIndex i currInd) child) (zip children [0..])
        
        -- collectSourceRange :: SourceTemplate -> SourceRose BasicInfo -> RootIndex -> Maybe SourceRange
        -- collectSourceRange templ tree currInd 
          -- = foldl (\curr ind -> curr <|> (getRange $ roseInfo $ resolveRootInd ind tree)) 
                  -- Nothing (collectRefTemplateRanges templ currInd) 
          
        -- collectRefTemplateRanges :: SourceTemplate -> RootIndex -> [RootIndex]
        -- collectRefTemplateRanges templ currInd 
          -- = let refNodes = catMaybes (map getIndexedNode templ) 
             -- in map (addRelPath currInd) refNodes
             
-- | Replaces assigned input of nodes with source templates.
-- Goes top-down and replaces the info in every node according to structure of the whole tree.
cutOutTemplates :: SourceRose BasicInfo -> SourceRose TemplateInfo
cutOutTemplates tree = undefined 
  
  
-- cutOutTemplates' tree Root (generateRangeTree Root tree) Root tree
  -- where cutOutTemplates' :: SourceRose BasicInfo -> RootIndex -> [RangeTree] -> RootIndex -> SourceRose BasicInfo -> SourceRose TemplateInfo
        -- cutOutTemplates' tree rootInd rngTree currInd (SourceRose (GeneratedInfo {giRange,giTemplate}) children)
          -- = SourceRose (TemplateInfo (giRange) (Just giTemplate)) 
                       -- (updateChildren tree rootInd rngTree currInd children)
        -- cutOutTemplates' tree rootInd rngTree currInd (SourceRose (BasicInfo rng input) children)
          -- = let relativeIndAndRng ind = ( fromMaybe (error "No range for a node that is to be cut out")
                                            -- . getRange . roseInfo $ resolveRootInd ind tree
                                        -- , ind `indRelativelyTo` currInd ) 
             -- in SourceRose (TemplateInfo (Just rng) $ Just 
                              -- $ createTemplate (map relativeIndAndRng (findContainedWhere (\_ ind -> not (ind `rootPrefixOf` currInd)) rng rngTree)) rng input)
                           -- (updateChildren tree rootInd rngTree currInd children)
        -- updateChildren tree rootInd rngTree currInd children 
          -- = map (\(child,i) -> cutOutTemplates' tree rootInd rngTree (RootIndex i currInd) child) (zip children [0..])
