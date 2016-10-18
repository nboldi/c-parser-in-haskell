{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
-- | Derived and generated instances for AST elements.
module MiniC.Instances where

import MiniC.AST
import MiniC.Representation
import MiniC.Semantics
import SourceCode.ASTNode
import SourceCode.ASTElems
import SourceCode.ToSourceTree
import SourceCode.SourceInfo
import Control.Lens
import Control.Applicative
import GHC.Generics
import Data.SmartTrav
import Data.Data.Lens (uniplate)
import Data.Data

-- * Derived type classes for AST elements
     
type TranslationUnitBI = TranslationUnit BI
type TranslationUnitNI = TranslationUnit NI
instance (SourceInfo a, Show a) => ToSourceRose TranslationUnit a
instance ASTNode TranslationUnit a
instance Data a => Plated (TranslationUnit a) where plate = uniplate
makeLenses ''TranslationUnit
deriveSmartTrav ''TranslationUnit

type DeclarationBI = Declaration BI
type DeclarationNI = Declaration NI
instance (SourceInfo a, Show a) => ToSourceRose Declaration a
instance ASTNode Declaration a
instance Data a => Plated (Declaration a) where plate = uniplate
makeLenses ''Declaration
deriveSmartTrav ''Declaration

type StorageSpecifierBI = StorageSpecifier BI
type StorageSpecifierNI = StorageSpecifier NI
instance (SourceInfo a, Show a) => ToSourceRose StorageSpecifier a
instance ASTNode StorageSpecifier a
instance Data a => Plated (StorageSpecifier a) where plate = uniplate
makeLenses ''StorageSpecifier
deriveSmartTrav ''StorageSpecifier

type TypeDefinitionBI = TypeDefinition BI
type TypeDefinitionNI = TypeDefinition NI
instance (SourceInfo a, Show a) => ToSourceRose TypeDefinition a
instance ASTNode TypeDefinition a
instance Data a => Plated (TypeDefinition a) where plate = uniplate
makeLenses ''TypeDefinition
deriveSmartTrav ''TypeDefinition

type FunctionDeclarationBI = FunctionDeclaration BI
type FunctionDeclarationNI = FunctionDeclaration NI
instance (SourceInfo a, Show a) => ToSourceRose FunctionDeclaration a
instance ASTNode FunctionDeclaration a
instance Data a => Plated (FunctionDeclaration a) where plate = uniplate
makeLenses ''FunctionDeclaration
deriveSmartTrav ''FunctionDeclaration

type VariableDeclarationBI = VariableDeclaration BI
type VariableDeclarationNI = VariableDeclaration NI
instance (SourceInfo a, Show a) => ToSourceRose VariableDeclaration a
instance ASTNode VariableDeclaration a
instance Data a => Plated (VariableDeclaration a) where plate = uniplate
makeLenses ''VariableDeclaration
deriveSmartTrav ''VariableDeclaration

type ParameterDeclarationBI = ParameterDeclaration BI
type ParameterDeclarationNI = ParameterDeclaration NI
instance (SourceInfo a, Show a) => ToSourceRose ParameterDeclaration a
instance ASTNode ParameterDeclaration a
instance Data a => Plated (ParameterDeclaration a) where plate = uniplate
makeLenses ''ParameterDeclaration
deriveSmartTrav ''ParameterDeclaration

type VariableDefinitionBI = VariableDefinition BI
type VariableDefinitionNI = VariableDefinition NI
instance (SourceInfo a, Show a) => ToSourceRose VariableDefinition a
instance ASTNode VariableDefinition a
instance Data a => Plated (VariableDefinition a) where plate = uniplate
makeLenses ''VariableDefinition
deriveSmartTrav ''VariableDefinition

type InlineAssemblyBI = InlineAssembly BI
type InlineAssemblyNI = InlineAssembly NI
instance (SourceInfo a, Show a) => ToSourceRose InlineAssembly a
instance ASTNode InlineAssembly a
instance Data a => Plated (InlineAssembly a) where plate = uniplate
makeLenses ''InlineAssembly
deriveSmartTrav ''InlineAssembly

type AssemblyStringBI = AssemblyString BI
type AssemblyStringNI = AssemblyString NI
instance (SourceInfo a, Show a) => ToSourceRose AssemblyString a
instance ASTNode AssemblyString a
instance Data a => Plated (AssemblyString a) where plate = uniplate
makeLenses ''AssemblyString
deriveSmartTrav ''AssemblyString

type AsmSpecifierBI = AsmSpecifier BI
type AsmSpecifierNI = AsmSpecifier NI
instance (SourceInfo a, Show a) => ToSourceRose AsmSpecifier a
instance ASTNode AsmSpecifier a
instance Data a => Plated (AsmSpecifier a) where plate = uniplate
makeLenses ''AsmSpecifier
deriveSmartTrav ''AsmSpecifier

type StatementBI = Statement BI
type StatementNI = Statement NI
instance (SourceInfo a, Show a) => ToSourceRose Statement a
instance ASTNode Statement a
instance Data a => Plated (Statement a) where plate = uniplate
makeLenses ''Statement
deriveSmartTrav ''Statement

type AssemblyParamBI = AssemblyParam BI
type AssemblyParamNI = AssemblyParam NI
instance (SourceInfo a, Show a) => ToSourceRose AssemblyParam a
instance ASTNode AssemblyParam a
instance Data a => Plated (AssemblyParam a) where plate = uniplate
makeLenses ''AssemblyParam
deriveSmartTrav ''AssemblyParam

type ExpressionBI = Expression BI
type ExpressionNI = Expression NI
instance (SourceInfo a, Show a) => ToSourceRose Expression a
instance ASTNode Expression a
instance Data a => Plated (Expression a) where plate = uniplate
makeLenses ''Expression
deriveSmartTrav ''Expression

type BinaryOpBI = BinaryOp BI
type BinaryOpNI = BinaryOp NI
instance (SourceInfo a, Show a) => ToSourceRose BinaryOp a
instance ASTNode BinaryOp a
instance Data a => Plated (BinaryOp a) where plate = uniplate
makeLenses ''BinaryOp
deriveSmartTrav ''BinaryOp

type UnaryOpBI = UnaryOp BI
type UnaryOpNI = UnaryOp NI
instance (SourceInfo a, Show a) => ToSourceRose UnaryOp a
instance ASTNode UnaryOp a
instance Data a => Plated (UnaryOp a) where plate = uniplate
makeLenses ''UnaryOp
deriveSmartTrav ''UnaryOp

type ArrayIndexBI = ArrayIndex BI
type ArrayIndexNI = ArrayIndex NI
instance (SourceInfo a, Show a) => ToSourceRose ArrayIndex a
instance ASTNode ArrayIndex a
instance Data a => Plated (ArrayIndex a) where plate = uniplate
makeLenses ''ArrayIndex
deriveSmartTrav ''ArrayIndex

type MemberDerefBI = MemberDeref BI
type MemberDerefNI = MemberDeref NI
instance (SourceInfo a, Show a) => ToSourceRose MemberDeref a
instance ASTNode MemberDeref a
instance Data a => Plated (MemberDeref a) where plate = uniplate
makeLenses ''MemberDeref
deriveSmartTrav ''MemberDeref

type LiteralBI = Literal BI
type LiteralNI = Literal NI
instance (SourceInfo a, Show a) => ToSourceRose Literal a
instance ASTNode Literal a
instance Data a => Plated (Literal a) where plate = uniplate
makeLenses ''Literal
deriveSmartTrav ''Literal

type IdentBI = Ident BI
type IdentNI = Ident NI
instance (SourceInfo a, Show a) => ToSourceRose Ident a
instance ASTNode Ident a
instance Data a => Plated (Ident a) where plate = uniplate
makeLenses ''Ident
deriveSmartTrav ''Ident

type QualifiedTypeBI = QualifiedType BI
type QualifiedTypeNI = QualifiedType NI
instance (SourceInfo a, Show a) => ToSourceRose QualifiedType a
instance ASTNode QualifiedType a
instance Data a => Plated (QualifiedType a) where plate = uniplate
makeLenses ''QualifiedType
deriveSmartTrav ''QualifiedType

type TypeBI = Type BI
type TypeNI = Type NI
instance (SourceInfo a, Show a) => ToSourceRose Type a
instance ASTNode Type a
instance Data a => Plated (Type a) where plate = uniplate
makeLenses ''Type
deriveSmartTrav ''Type

type TypeQualifierBI = TypeQualifier BI
type TypeQualifierNI = TypeQualifier NI
instance (SourceInfo a, Show a) => ToSourceRose TypeQualifier a
instance ASTNode TypeQualifier a
instance Data a => Plated (TypeQualifier a) where plate = uniplate
makeLenses ''TypeQualifier
deriveSmartTrav ''TypeQualifier

type ArrayTypeQualBI = ArrayTypeQual BI
type ArrayTypeQualNI = ArrayTypeQual NI
instance (SourceInfo a, Show a) => ToSourceRose ArrayTypeQual a
instance ASTNode ArrayTypeQual a
instance Data a => Plated (ArrayTypeQual a) where plate = uniplate
makeLenses ''ArrayTypeQual
deriveSmartTrav ''ArrayTypeQual

type StringLiteralBI = StringLiteral BI
type StringLiteralNI = StringLiteral NI
instance (SourceInfo a, Show a) => ToSourceRose StringLiteral a
instance ASTNode StringLiteral a
instance Data a => Plated (StringLiteral a) where plate = uniplate
makeLenses ''StringLiteral
deriveSmartTrav ''StringLiteral

type CompoundLitElemBI = CompoundLitElem BI
type CompoundLitElemNI = CompoundLitElem NI
instance (SourceInfo a, Show a) => ToSourceRose CompoundLitElem a
instance ASTNode CompoundLitElem a
instance Data a => Plated (CompoundLitElem a) where plate = uniplate
makeLenses ''CompoundLitElem
deriveSmartTrav ''CompoundLitElem

type DesignatorBI = Designator BI
type DesignatorNI = Designator NI
instance (SourceInfo a, Show a) => ToSourceRose Designator a
instance ASTNode Designator a
instance Data a => Plated (Designator a) where plate = uniplate
makeLenses ''Designator
deriveSmartTrav ''Designator

type StructUnionBI = StructUnion BI
type StructUnionNI = StructUnion NI
instance (SourceInfo a, Show a) => ToSourceRose StructUnion a
instance ASTNode StructUnion a
instance Data a => Plated (StructUnion a) where plate = uniplate
makeLenses ''StructUnion
deriveSmartTrav ''StructUnion

type EnumerationBI = Enumeration BI
type EnumerationNI = Enumeration NI
instance (SourceInfo a, Show a) => ToSourceRose Enumeration a
instance ASTNode Enumeration a
instance Data a => Plated (Enumeration a) where plate = uniplate
makeLenses ''Enumeration
deriveSmartTrav ''Enumeration

type VariantBI = Variant BI
type VariantNI = Variant NI
instance (SourceInfo a, Show a) => ToSourceRose Variant a
instance ASTNode Variant a
instance Data a => Plated (Variant a) where plate = uniplate
makeLenses ''Variant
deriveSmartTrav ''Variant

-- * Instances for generic AST elements

type ASTListBI e = ASTList e BI
type ASTListNI e = ASTList e NI
instance (Show a, ToSourceRose e a, Generic (e a)) => ToSourceRose (ASTList e) a
instance (ASTNode e a, Generic (e a)) => ASTNode (ASTList e) a
deriveSmartTrav ''ASTList
  
type ASTWrapperBI e = ASTWrapper e BI
type ASTWrappereNI e = ASTWrapper e NI
instance (Show a, ToSourceRose e a, Generic (e a)) => ToSourceRose (ASTWrapper e) a
instance (ASTNode e a, Generic (e a)) => ASTNode (ASTWrapper e) a
deriveSmartTrav ''ASTWrapper 

type ASTMaybeBI e = ASTMaybe e BI
type ASTMaybeNI e = ASTMaybe e NI
instance (Show a, ToSourceRose e a, Generic (e a)) => ToSourceRose (ASTMaybe e) a
instance (ASTNode e a, Generic (e a)) => ASTNode (ASTMaybe e) a
deriveSmartTrav ''ASTMaybe

type ASTEitherBI n m = ASTEither n m BI
type ASTEitherNI n m = ASTEither n m NI
instance (Show a, ToSourceRose n a, ToSourceRose m a
         , Generic (n a), Generic (m a)) => ToSourceRose (ASTEither n m) a
instance (ASTNode n a, ASTNode m a, Generic (n a), Generic (m a)) 
           => ASTNode (ASTEither n m) a
deriveSmartTrav ''ASTEither
           
type ASTPairBI n m = ASTPair n m BI
type ASTPairNI n m = ASTPair n m NI
instance (Show a, ToSourceRose n a, ToSourceRose m a
         , Generic (n a), Generic (m a)) => ToSourceRose (ASTPair n m) a
instance (ASTNode n a, ASTNode m a, Generic (n a), Generic (m a)) 
           => ASTNode (ASTPair n m) a
deriveSmartTrav ''ASTPair

-- * AST data elements used in C AST nodes
  
instance ASTData Bool
instance ASTData Int
instance ASTData Char
instance ASTData String
instance ASTData Rational
instance ASTData Integer
  
instance ASTData IntLitSize
instance ASTData IntLitSign
instance ASTData IntLitSpec
instance ASTData FloatLitSize
instance ASTData FloatingSize
instance ASTData IntSize
instance ASTData Sign

instance ASTData a => ASTData (Maybe a)

makeLenses ''IntLitSpec