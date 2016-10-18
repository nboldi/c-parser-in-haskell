{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RebindableSyntax #-}

-- | A module to generate nodes with default source templates
module MiniC.GenTemplate where

import MiniC.AST
import MiniC.Instances
import MiniC.Representation
import MiniC.SourceNotation
import SourceCode.SourceTree
import SourceCode.ASTElems
import MiniC.Semantics
import Data.Maybe
import Control.Lens
import Debug.Trace

import qualified Prelude
import Prelude hiding (fromInteger)

-- A little hack to disable polymorphism of numeric literals
fromInteger :: Integer -> Int
fromInteger = Prelude.fromInteger

genInfo :: SourceTemplate -> NI
genInfo st = NodeInfo (TemplateInfo Nothing (Just st)) emptySemaInfo

infixr 6 +>

(+>) :: ToTemplateElem a => a -> SourceTemplate -> SourceTemplate
a +> st = toTemplateElem a : st

infixl 9 //

class ToNodeDir nd where
  (//) :: NodeIndex -> nd -> NodeIndex
  
instance ToNodeDir Int where
  ni // i = NthChildOf i ni
  
data Up = Up

instance ToNodeDir Up where
  ni // Up = ParentOf ni
  
class ToTemplateElem a where
  toTemplateElem :: a -> SourceTemplateElem

instance ToTemplateElem String where
  toTemplateElem = TextElem
  
instance ToTemplateElem NodeIndex where
  toTemplateElem = NodeElem
  
  
genScalarQual :: String -> TypeQualifierNI
genScalarQual s = Scalar (ASTJust (Ident s (genInfo (s +> [])))) (genInfo ( Current//0 +> [] ))
  
-- | Adds a pointer to the qualifier, looks for cases when have to put the qualifier in parentheses.
addPtrToQual :: Maybe TypeQualifierNI -> TypeQualifierNI -> TypeQualifierNI
addPtrToQual outer
  = wrap . genPtrQual
  where wrap = case outer of Just (ArrayQual {}) -> genParenQual
                             _                   -> id
        
removePtrFromQual :: Maybe TypeQualifierNI -> TypeQualifierNI -> TypeQualifierNI
removePtrFromQual _ ptrq@(PtrQual{}) = ptrq ^?! qualBase
removePtrFromQual _ tq = tq
        
genParenQual :: TypeQualifierNI -> TypeQualifierNI
genParenQual tq = ParenQual tq (genInfo ( "(" +> Current//0 +> ")" +> [] ))
        
genPtrQual :: TypeQualifierNI -> TypeQualifierNI
genPtrQual tq = PtrQual tq (genInfo ( "*" +> Current//0 +> [] ))
        
genArrayQual :: TypeQualifierNI -> TypeQualifierNI
genArrayQual elem
  = wrap $ ArrayQual (ArrayTypeQual ASTNothing ASTNothing (genInfo ( "[]" +> [] ))) elem
                     (genInfo ( Current//1 +> Current//0 +> [] ))
  where wrap = case elem of PtrQual {} -> \q -> ParenQual q (genInfo ( "(" +> Current//0 +> ")" +> [] ))
                            _          -> id 
    
genDerefExpr :: ExpressionNI -> ExpressionNI
genDerefExpr e 
  = let op = DereferenceOp (genInfo ( "*" +> [] ))
        unexpr = Unary op e (genInfo ( Current//0 +> Current//1 +> [] ))
     in ParenExpr unexpr (genInfo ( "(" +> Current//0 +> ")" +> [] ))
      
genMemberDeref :: MemberDerefNI
genMemberDeref = DerefMember (genInfo ( "->" +> [] ))    
  
genMemberSimple :: MemberDerefNI
genMemberSimple = SimpleMember (genInfo ( "." +> [] ))
                      
astFilter :: (e NI -> Maybe (f NI)) -> ASTList e NI -> ASTList f NI
astFilter pred (ASTCons listHead listTail listInfo) 
  = case pred listHead of Just x  -> ASTCons x (astFilter pred listTail) listInfo
                          Nothing -> astFilter pred listTail
astFilter pred ASTNil = ASTNil



