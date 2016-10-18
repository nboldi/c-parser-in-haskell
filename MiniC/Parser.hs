{-# LANGUAGE TupleSections, FlexibleContexts #-}

-- | The parser that generates the Abstract Syntax Tree that is represented in 'MiniC.AST'.
-- C language elements are highly recursive, so this module could only be broken up
-- with a lot of effort and increasing complexity with passing functions arguments.
-- Also the lexical analysis of C requires knowledge about the defined symbols 
-- (this is the so called "Lexer Hack"), so registering and querying of symbol table elements
-- is also incorporated into the parser.
module MiniC.Parser where

import SourceCode.ASTElems
import SourceCode.Parsec
import SourceCode.SourceInfo
import SourceCode.ASTNode
import MiniC.AST
import MiniC.Representation
import MiniC.Semantics(BI)
import MiniC.Instances
import MiniC.Helpers
import MiniC.Parser.Lexical
import MiniC.Parser.Expr
import MiniC.Parser.Base
import MiniC.SourceNotation

import GHC.Generics (Generic)
import Text.Parsec
import Text.Parsec.ExtraCombinators
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Debug.Trace

-- * Declarations

translationUnit :: CParser TranslationUnitBI
translationUnit = withInfo $ TranslationUnit <$> astMany declaration
 
declaration :: CParser DeclarationBI
declaration 
  = (FuncDecl <$> try functionDeclaration)
       -- function declarations can be parsed as variable declarations
       <|> (VarDecl <$> try variableDeclaration)
       -- type declaration can be a prefix of variable declaration
       <|> (TypeDecl <$> typeDefinition)
              
typeDefinition :: CParser TypeDefinitionBI
typeDefinition 
  = withInfo (TypeDefinition <$> definedType <* semicolon) >>= registerDeclaration 
       
functionDeclaration :: CParser FunctionDeclarationBI
functionDeclaration 
  = withInfo (
      do storageSpec <- astMany storageSpecifier
         funcType <- unqualifiedFunctionType
         asmSpec <- astOptionMaybe asmSpecifier
         -- register the function early, to be able to recursively call it
         let funDecl = FunctionDeclaration storageSpec funcType asmSpec
         withInfo (pure (funDecl ASTNothing)) >>= registerDeclaration
         -- return the function declaration with body
         funDecl <$> ((return ASTNothing <* semicolon) 
                        <|> (ASTJust <$> insideScope' (getId funcType) statement))
      ) >>= registerDeclaration 
                          
variableDeclaration :: CParser VariableDeclarationBI
variableDeclaration 
  = -- other things can be parsed as variableDeclaration, but then there is no id
    onlyWhen "No id in variable definition" 
             (\vd -> not ( null $ astGetList $ _varDeclDefinitions vd ) 
                      || isJust (getId $ _qualTypQual (_varDeclQualTyp vd))) $ withInfo (
        do storage <- astMany storageSpecifier
           qualTyp <- try qualifiedFunctionType <|> nonFunctionTypeSpec
           -- to register all separate variables, the multi-declaration must supply additional data
           let registerDefinition vdef 
                 = view ast_triple_1 <$> registerDeclaration (ASTTriple vdef storage qualTyp (vdef ^. info))
           
           VariableDeclaration storage qualTyp
              <$> astOptionMaybe asmSpecifier
              <*> ((ASTJust <$> (symbol "=" *> expressionWithoutComma)) 
                            <|> (optional comma *> return ASTNothing))
              <*> (optional comma *> (variableDefinition >>= registerDefinition) `astSepBy` comma)
              <* semicolon
    ) >>= registerDeclaration
              
parameterDeclaration :: CParser ParameterDeclarationBI
parameterDeclaration 
  = withInfo (ParameterDeclaration <$> qualifiedType) >>= registerDeclaration
              
-- | Parses a variable definition, without type, possibly with qualifier and/or initialization
variableDefinition :: CParser VariableDefinitionBI
variableDefinition = withInfo ( 
  VariableDefinition <$> typeQualifierWithId
                     <*> astOptionMaybe asmSpecifier
                     <*> astOptionMaybe (symbol "=" *> expressionWithoutComma)
  )

                            
storageSpecifier :: CParser StorageSpecifierBI
storageSpecifier = withInfo $ (reserved "auto" *> return Auto) 
               <|> (reserved "register" *> return Register) 
               <|> (reserved "static" *> return Static) 
               <|> (reserved "extern" *> return Extern)
               <|> (reserved "inline" *> return Inline)
               
asmSpecifier :: CParser AsmSpecifierBI
asmSpecifier = withInfo $ AsmSpecifier <$> (reserved "asm" *> parens simpleStringLiteral)
               
-- * Types
   
-- | Parses a type with type qualifiers.
-- Examples: @int@, @int *@, @struct { ... }@, @int *a[10]@, @void f(...)@, ...
qualifiedType :: CParser QualifiedTypeBI
qualifiedType = try unqualifiedFunctionType <|> try qualifiedFunctionType <|> nonFunctionTypeSpec
          
-- | Examples: @void f()@, @int g(int a)@
unqualifiedFunctionType :: CParser QualifiedTypeBI
unqualifiedFunctionType = functionType scalarWithId

-- | Examples: @void (*f)()@, @void (*)()@, @void (f[10])()@
qualifiedFunctionType :: CParser QualifiedTypeBI
qualifiedFunctionType = functionType typeQualifiedIdentifier
        
-- | Parses a function type with a parser given to parse the qualified or unqualified function name.
functionType :: CParser TypeQualifierBI -> CParser QualifiedTypeBI
functionType qualParser 
  = withInfo $
      do retTyp <- returnTypeSpec
         qualId <- qualParser
         -- parameters are in the scope of the function
         let paramDecl = insideScope' (getId qualId) parameterDeclaration
         QualifiedType qualId <$>
            withInfo (FunType retTyp 
                           <$> withInfo (ASTWrapper <$> parens (paramDecl `astSepBy` comma)))      

-- | First parse a nonrecursive type, then parse any 
nonFunctionTypeSpec :: CParser QualifiedTypeBI
nonFunctionTypeSpec = withInfo $ 
                        flip applyQualTo <$> nonRecursiveTypeSpec 
                                         <*> typeQualifiedIdentifier 
           
-- | Parses a type without identifier. The identifier-related type qualifiers will
-- not be parsed unless there is no identifier.
-- Examples: @float@, @int*[10]@, @const int*[10]@
returnTypeSpec :: CParser QualifiedTypeBI
returnTypeSpec = withInfo $ 
                   flip applyQualTo <$> nonRecursiveTypeSpec 
                                    <*> typeQualifierNoId
              
-- | Type qualifiers with or without identifier
-- Examples: @*[10]@, @*a[10]@, @(*a)[10]@
typeQualifiedIdentifier :: CParser TypeQualifierBI
typeQualifiedIdentifier = try typeQualifierWithId
              <|> try (withInfo $ ParenQual <$> parens typeQualifierWithIdStrict)
              <|> withInfo (ParenQual <$> parens typeQualifierNoId)
              <|> typeQualifierNoId
         
-- | Examples: @*[10]@ in @int*[10]@          
-- Semantics: @int*[10]@ = array of ptrs to ints
typeQualifierNoId :: CParser TypeQualifierBI
typeQualifierNoId 
  = ((beforeQual <*> typeQualifierNoId) 
          <|> inheritInfo (ArrayQual <$> afterQual <*> typeQualifierNoId))
      <|> scalarNoId
               
              
-- | Type qualifiers with an identifier, for example @*a[10]@ in @int *a[10]@,
-- @*(*a[10])[10]@
-- Semantics: int **(*a)[2][3] = ptr to array[2] of array[3] of ptr to ptr to int
typeQualifierWithId :: CParser TypeQualifierBI
typeQualifierWithId 
  = let noParenWhenNotNeeded bef inner aft 
         = guard $ not (null bef && null aft 
                                 && case inner of ParenQual {} -> True; _ -> False)
     in typeQualifierWithId' noParenWhenNotNeeded

typeQualifierWithIdStrict 
  = let doesNeedParenOutside bef _ aft = guard $ not (null bef && null aft)
     in typeQualifierWithId' doesNeedParenOutside

typeQualifierWithId' guard
  = do before <- many beforeQual
       inner <- withInfo (ParenQual <$> parens typeQualifierWithId) 
                  <|> scalarWithId
       arrs <- many afterQual
       guard before inner arrs
       return (foldr ($) (foldl (\tq ar -> ArrayQual ar tq noNodeInfo) inner arrs) before)      
       
-- | Parses a qualifier that stand between the type and the id
beforeQual :: CParser (TypeQualifierBI -> TypeQualifierBI)
beforeQual = withInfo (flip <$> (reservedOp "*" *> pure PtrQual)) 
               <|> typBoundedQual
         
-- | Parses an array qualifier         
afterQual :: CParser ArrayTypeQualBI
afterQual 
  = withInfo $ brackets $ ArrayTypeQual 
               <$> astOptionMaybe ((typBoundedQual <*> scalarNoId)
                                              <|> withInfo (reserved "static" *> return StaticQual))
               <*> astOptionMaybe expression 

-- | Parses a qualifier that can happen before and after the type
typBoundedQual :: CParser (TypeQualifierBI -> TypeQualifierBI)
typBoundedQual = withInfo $ flip <$> (
   (reserved "const" *> pure ConstQual)
      <|> (reserved "volatile" *> pure VolatileQual)
      <|> (reserved "restrict" *> pure RestrictQual) )

-- | Parses a type that does not capture identifier and identifier-bounded type qualifiers
-- Examples : @void@, @const int@, @(const int* a[10])@
nonRecursiveTypeSpec :: CParser QualifiedTypeBI
nonRecursiveTypeSpec = 
   withInfo (QualifiedType <$> scalarNoId <*> unqualifiedType)
     <|> withInfo (applyQualTo <$> ( typBoundedQual <*> scalarNoId ) 
                               <*> nonRecursiveTypeSpec)
 
scalarNoId = withInfo $ pure (Scalar ASTNothing)
scalarWithId = withInfo (Scalar <$> ASTJust <$> identifier)
  
unqualifiedType :: CParser TypeBI
unqualifiedType = 
      withInfo (reserved "void" *> return VoidType)
  <|> withInfo (reserved "bool" *> return BoolType)
  <|> withInfo (IntType <$> integerSign <*> integerSize)
  <|> withInfo (FloatingType <$> floatingSize)
  <|> (TypeName <$> (identifier >>= checkIsTypeSymbol))
  <|> withInfo (reserved "typeof" *> parens ( (TypeOfExpr <$> expression) 
                                                 <|> (TypeOfType <$> try returnTypeSpec) ))
  <|> definedType
  
-- | A type that can be defined in a type definition. May have an identifier.
definedType :: CParser TypeBI
definedType = withInfo $ (StructType <$> (reserved "struct" *> structUnionType))
              <|> (UnionType <$> (reserved "union" *> structUnionType) )
              <|> (EnumType <$> (reserved "enum" *> enumeration) )       
              <|> (TypeDef <$> (reserved "typedef" *> qualifiedType) 
                           <*> astOptionMaybe identifier)  

integerSign :: CParser Sign
integerSign = (reserved "unsigned" *> return Unsigned)
                <|> (optional (reserved "signed") *> return Signed)
                              
integerSize :: CParser IntSize
integerSize = reserved "char" *> return Char 
  <|> reserved "short" *> optional (reserved "int") *> return Short 
  <|> reserved "int" *> return Int 
  <|> reserved "long" *> optional (reserved "int") *> return LongInt 
  <|> reserved "long" *> reserved "long" *> optional (reserved "int") *> return LongLongInt 

floatingSize :: CParser FloatingSize
floatingSize = reserved "float" *> return Float 
  <|> reserved "double" *> return Double 
  <|> reserved "long" *> reserved "double" *> return LongDouble

enumeration :: CParser EnumerationBI
enumeration 
  = withInfo $ Enumeration 
                 <$> astOptionMaybe identifier 
                 <*> astOptionMaybe ( braces (withInfo $ ASTCons <$> variant 
                                                                 <*> astMany (try $ comma *> variant) 
                                                                 <* optional comma))

variant :: CParser VariantBI
variant = withInfo $ Variant <$> identifier 
                      <*> astOptionMaybe (reservedOp "=" *> expression)

structUnionType :: CParser StructUnionBI
structUnionType = withInfo $ do
  id <- astOptionMaybe identifier
  StructUnion id <$> astOptionMaybe (braces (astMany (insideScope' (view astMaybe id) declaration)))
                              
-- * Statements

statement :: CParser StatementBI
statement = withInfo $
      try (Label <$> identifier <* colon <*> statement <?> "labeled statement")
  <|> (reserved "case" *> ( 
        try (CaseInterval <$> (ASTPair <$> expression <* reservedOp ".." <*> expression) <* colon <*> statement) 
          <|> (Case <$> expression <* colon <*> statement) ) )
  <|> (Default <$> (reserved "default" *> colon *> statement))
  <|> try (Expr <$> expression <* semicolon)
  <|> (Compound <$> (openBrace *> insideAnonymScope (astMany (try (astParseEither (try declaration) statement ))) <* closeBrace 
         <?> "compound statement"))
  <|> (If <$> (reserved "if" *> parens expression) <*> statement 
          <*> astOptionMaybe (reserved "else" *> statement))
  <|> (Switch <$> (reserved "switch" *> parens expression) <*> statement)
  <|> (While <$> (reserved "while" *> parens expression) <*> statement)
  <|> (DoWhile <$> (reserved "do" *> statement) 
               <*> (reserved "while" *> parens expression <* semicolon))
  <|> insideAnonymScope (
         For <$> (reserved "for" *> openParen 
                                 *> astOptionMaybe (astParseEither (try expression <* semicolon) declaration) )
             <*> astOptionMaybe expression <* semicolon
             <*> astOptionMaybe expression <* closeParen 
             <*> statement )
  <|> (Goto <$> (reserved "goto" *> expression) <* semicolon)
  <|> (reserved "continue" *> return Continue <* semicolon)
  <|> (reserved "break" *> return Break <* semicolon)
  <|> (Return <$> (reserved "return" *> astOptionMaybe expression) <* semicolon)
  <|> (semicolon *> return EmptyStmt <?> "empty instruction")
  <|> (AsmStmt <$> (reserved "asm" *> withInfo
        (InlineAssembly <$> ifAccept (reserved "volatile") 
                        <*> (openParen *> astMany (withInfo $ AssemblyString <$> simpleStringLiteral)) 
                        <*> astOptionMaybe (colon *> asmParams) 
                        <*> astOptionMaybe (colon *> asmParams) 
                        <*> astOptionMaybe (colon *> asmParams) ) <* closeParen <* semicolon ) )
  
asmParams :: CParser (ASTListBI AssemblyParam)
asmParams = withInfo (AssemblyParam <$> simpleStringLiteral 
                                    <*> astOptionMaybe (parens identifier)) `astSepBy` comma
                       
-- * Expressions

expression = chainl1 expressionWithoutComma 
                     (do operator <- withInfo (comma *> return CommaOp) 
                         return (\e1 e2 -> Binary operator e1 e2 noNodeInfo))
                     
-- | Expressions without comma need special treatment, because they
-- cannot appear in function calls.    
expressionWithoutComma = genExpression term

genExpression :: CParser ExpressionBI -> CParser ExpressionBI
genExpression term = buildExpressionParser precedenceTable (unaryExpressionWithSizeOf term) <?> "expression"
  where precedenceTable = 
          [ -- arithmetic operators
            [ binOpL "*" MulOp
            , binOpL "/" DivOp
            , binOpL "%" RemainderOp 
            ]
          , [ binOpL "+" AddOp
            , binOpL "-" SubOp
            ]
          -- bit shifting
          , [ binOpL ">>" ShiftLeftOp
            , binOpL "<<" ShiftRightOp
            ]
          -- compare operators
          , [ binOpL ">" LessOp
            , binOpL "<" GreaterOp
            , binOpL ">=" LessOrEqOp
            , binOpL "<=" GreaterOrEqOp
            ] 
          , [ binOpL "==" EqOp
            , binOpL "!=" NotEqOp
            ]
          -- bitwise operators
          , [ binOpL "&" BitAndOp ] 
          , [ binOpL "^" BitXorOp ]
          , [ binOpL "|" BitOrOp ]
          -- logic operators
          , [ binOpL "&&" LogicAndOp ]
          , [ binOpL "||" LogicOrOp ]
          -- ternary conditional op
          , [ Infix ((\e2 e1 e3 -> Cond e1 e2 e3 noNodeInfo) 
                       <$> (reservedOp "?" *> astOptionMaybe expression <* reservedOp ":")
                    ) AssocRight ]
          -- assignments
          , [ binOpR "=" AssignOp
            , binOpR "*=" MulAssOp
            , binOpR "/=" DivAssOp
            , binOpR "%=" RemainderAssOp
            , binOpR "+=" AddAssOp
            , binOpR "-=" SubAssOp
            , binOpR "<<=" ShiftLeftAssOp
            , binOpR ">>=" ShiftRightAssOp
            , binOpR "&=" BitAndAssOp
            , binOpR "^=" BitXorAssOp
            , binOpR "|=" BitOrAssOp
            ]
          ]
          
        binOpL sign sema = Infix (binOp sign sema) AssocLeft
        binOpR sign sema = Infix (binOp sign sema) AssocRight
        
        binOp sign sema 
          = do operator <- withInfo (reservedOp sign *> return sema)
               return (\e1 e2 -> Binary operator e1 e2 noNodeInfo) <?> "binary operator"
  
-- | Sizeof needs a special treatment because it can either be used on types and expressions,
-- and has the same precedence in both cases.  
unaryExpressionWithSizeOf :: CParser ExpressionBI -> CParser ExpressionBI
unaryExpressionWithSizeOf term
  = withInfo (reserved "sizeof" *> parens ((SizeOfExpr <$> try expression) 
                                               <|> (SizeOfType <$> qualifiedType)))
      <|> unaryExpression term
        
unaryExpression :: CParser ExpressionBI -> CParser ExpressionBI
unaryExpression term = buildExpressionParser precedenceTable term <?> "expression"
  where precedenceTable = 
          [ [ Postfix $ unaryOp "++" PostIncOp
            , Postfix $ unaryOp "--" PostDecOp 
            ]
          , [ Prefix $ unaryOp "++" PreIncOp
            , Prefix $ unaryOp "--" PreDecOp 
            , Prefix $ unaryOp "&" AddressOp
            , Prefix $ unaryOp "*" DereferenceOp
            , Prefix $ unaryOp "+" PrePlusOp
            , Prefix $ unaryOp "-" PreMinOp
            , Prefix $ unaryOp "~" ComplementOp
            , Prefix $ unaryOp "!" LogicNegOp
            ]
          ]
        unaryOp sign sema = do operator <- withInfo (reservedOp sign *> return sema) 
                               return (\expr -> Unary operator expr noNodeInfo)
        
term :: CParser ExpressionBI
term = do base <- nonrecursiveTerm
          termIndexes base
        <?> "term"
        
-- | Tries to parse an array indexing, member access or function call 
--     and applies it to the given expressions. 
-- If does not succeed gives back the original expression.
-- This definition must be separated otherwise grammar would be left-recursive.
termIndexes :: ExpressionBI -> CParser ExpressionBI
termIndexes base 
  = (inheritInfo ( 
         (Indexing base <$> withInfo (ArrayIndex <$> brackets expression) )
            <|> (Member base <$> withInfo ((reservedOp "." *> pure SimpleMember) 
                                              <|> (reservedOp "->" *> pure DerefMember))
                             <*> identifier)
            <|> (Call base <$> withInfo (ASTWrapper <$> parens (expressionWithoutComma `astSepBy` comma)))
         ) >>= termIndexes)
    <|> return base
        
nonrecursiveTerm :: CParser ExpressionBI
nonrecursiveTerm = withInfo $
  (try $ Cast <$> parens qualifiedType <*> expression)
  -- can be prefix of type cast
  <|> (ParenExpr <$> parens expression)
  <|> (LitExpr <$> try literal)
  <|> (NameExpr <$> (identifier >>= checkIsExpression))
  
literal :: CParser LiteralBI
literal = (try float 
    <|> integer -- integer literal can be a prefix of a float literal
    <|> try charLiteral
    <|> (StrLitList <$> astMany1 stringLiteral)
    <|> (withInfo $ CompoundLit <$> braces (compoundElem `astSepBy` comma))
  ) <?> "literal"

compoundElem :: CParser CompoundLitElemBI
compoundElem 
  = withInfo $
     ( CompoundLitElem <$> astMany1 ( withInfo $ ( MemberDesignator <$> (symbol "." *> identifier) )
                                  <|> ( ArrDesignator <$> brackets expression ) )
          <*> ( symbol "=" *> expressionWithoutComma ) )
     <|> (CompoundLitElem <$> pure ASTNil <*> expressionWithoutComma)

-- * Helper parsers. Used to parse common AST elements in special ways.
            
astMany :: CParser (e BI) -> CParser (ASTList e BI)
astMany p = withInfo (ASTCons <$> p <*> astMany p) <|> pure ASTNil

astMany1 :: CParser (e BI) -> CParser (ASTList e BI)
astMany1 p = withInfo (ASTCons <$> p <*> astMany p)

astSepBy :: CParser (e BI) -> CParser sep -> CParser (ASTList e BI)
astSepBy p sep = astSepBy1 p sep <|> pure ASTNil

astSepBy1 :: CParser (e BI) -> CParser sep -> CParser (ASTList e BI)
astSepBy1 p sep = withInfo (ASTCons <$> p <*> astMany (sep *> p))

astOptionMaybe :: CParser (e BI) -> CParser (ASTMaybe e BI)
astOptionMaybe = (view (from astMaybe) <$>) . optionMaybe
     
   