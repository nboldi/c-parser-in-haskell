{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric, FlexibleContexts #-}

-- | Abstract Syntax Tree of a C program.
-- C language elements are highly recursive, so this module could only be broken up
-- with a lot of effort and increasing complexity with type arguments.
module MiniC.AST where

import GHC.Generics
import Data.Data
import Data.Typeable

import SourceCode.ASTElems

data TranslationUnit a
  = TranslationUnit { _tuDecls :: ASTList Declaration a
                    , _tuInfo :: a
                    }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)

data Declaration a
  = TypeDecl { _typeDecl :: TypeDefinition a }
  | FuncDecl { _funDecl :: FunctionDeclaration a }
  | VarDecl { _varDecl :: VariableDeclaration a }
  | ParamDecl { _paramDecl :: ParameterDeclaration a }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
data TypeDefinition a
  = TypeDefinition { _typeDefType :: Type a 
                   , _typeDefInfo :: a
                   } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)

data FunctionDeclaration a
  = FunctionDeclaration { _funDeclStorageSpec :: ASTList StorageSpecifier a
                        , _funDeclQualType    :: QualifiedType a
                        , _funDeclAsmSpec     :: ASTMaybe AsmSpecifier a
                        , _funDeclBody        :: ASTMaybe Statement a
                        , _funDeclInfo        :: a
                        } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
data VariableDeclaration a
  = VariableDeclaration { _varDeclStorageSpec :: ASTList StorageSpecifier a
                        , _varDeclQualTyp     :: QualifiedType a 
                        , _varDeclAsmSpec     :: ASTMaybe AsmSpecifier a
                        , _varDeclInit        :: ASTMaybe Expression a
                        , _varDeclDefinitions :: ASTList VariableDefinition a
                        , _varDeclInfo        :: a
                        } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
data VariableDefinition a 
  = VariableDefinition { _varDefQualTyp :: TypeQualifier a
                       , _varDefAsmSpec :: ASTMaybe AsmSpecifier a
                       , _varDefInit    :: ASTMaybe Expression a
                       , _varDefInfo    :: a
                       } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)                        
                        
data ParameterDeclaration a
  = ParameterDeclaration { _paramDeclQualTyp    :: QualifiedType a
                         , _paramDeclInfo       :: a
                         } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
data AsmSpecifier a = AsmSpecifier String a
                      deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
-- | C storage class specifier
data StorageSpecifier a
  = Auto     { _storageSpecInfo :: a }    -- ^ automatic scope for variables
  | Register { _storageSpecInfo :: a }    -- ^ suggest storing a variable on register
  | Static   { _storageSpecInfo :: a }    -- ^ function / variable static scope
  | Extern   { _storageSpecInfo :: a }    -- ^ function / variable external definition
  | Inline   { _storageSpecInfo :: a }    -- ^ function inlining
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
-- | C statement
data Statement a
  -- | A label followed by a statement
  = Label { _stmtLabel    :: Ident a
          , _stmtLabelled :: Statement a
          , _stmtInfo     :: a
          }
  -- | A statement of the form @case expr : stmt@
  | Case { _stmtCaseExpr :: Expression a
         , _stmtCaseBody :: Statement a
         , _stmtInfo :: a
         }
  -- | A statement of the form @case fromExpr .. toExpr : stmt@
  | CaseInterval { _stmtCaseRng :: ASTPair Expression Expression a
                 , _stmtCaseBody :: Statement a
                 , _stmtCaseInfo :: a
                 }
  -- | The default case @default : stmt@
  | Default { _stmtCaseBody :: Statement a
            , _stmtInfo :: a
            }
  -- | A simple statement, that is in C: evaluating an expression with
  --   side-effects and discarding the result.
  | Expr { _stmtExpr :: Expression a
         , _stmtInfo :: a 
         }
  -- | compound statement @{ statements }@
  | Compound { _compoundStmts :: ASTList (ASTEither Declaration Statement) a
             , _stmtInfo :: a 
             }
  -- | conditional statement @CIf ifExpr thenStmt maybeElseStmt at@
  | If { _stmtPred :: Expression a
       , _stmtThen :: Statement a
       , _stmtElse :: ASTMaybe Statement a
       , _stmtInfo :: a
       }
  -- | switch statement @CSwitch selectorExpr switchStmt@, where
  -- @switchStmt@ usually includes /case/, /break/ and /default/
  -- statements
  | Switch { _stmtSwitchExpr :: Expression a
           , _stmtSwitchBody :: Statement a
           , _stmtInfo :: a
           }
  -- | while-do loop
  | While { _stmtPred :: Expression a
          , _stmtLoopBody :: Statement a
          , _stmtInfo :: a
          }          
  -- | do-while loop
  | DoWhile { _stmtLoopBody :: Statement a
            , _stmtPred :: Expression a
            , _stmtInfo :: a
            }            
  | For { _stmtForInit :: ASTMaybe (ASTEither Expression Declaration) a
        , _stmtForTest :: ASTMaybe Expression a
        , _stmtForIncrement :: ASTMaybe Expression a
        , _stmtLoopBody :: Statement a
        , _stmtInfo :: a
        }
  | Goto { _stmtGotoExpr :: Expression a
         , _stmtInfo :: a
         }
  | Continue { _stmtInfo :: a }
  | Break { _stmtInfo :: a }
  | Return { _stmtReturnValue :: ASTMaybe Expression a
           , _stmtInfo :: a
           }
  -- | a simple semicolon
  | EmptyStmt { _stmtInfo :: a }
  | AsmStmt { _stmtInlineAsm :: InlineAssembly a
            , _stmtInfo :: a
            }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
-- | Inline assembly fragment
data InlineAssembly a
 = InlineAssembly { _inlAsmVolatile :: Bool
                  , _inlAsmCode     :: ASTList AssemblyString a
                  , _inlAsmOutput   :: ASTMaybe (ASTList AssemblyParam) a -- ^ If colon exists: Just [], if not: Nothing  
                  , _inlAsmInput    :: ASTMaybe (ASTList AssemblyParam) a -- ^ If colon exists: Just [], if not: Nothing 
                  , _inlAsmTemp     :: ASTMaybe (ASTList AssemblyParam) a -- ^ If colon exists: Just [], if not: Nothing 
                  , _inlAsmInfo     :: a
                  } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
                  
data AssemblyString a 
  = AssemblyString { _asmCodeStr  :: String 
                   , _asmCodeInfo :: a
                   } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
  
data AssemblyParam a
  = AssemblyParam { _asmParMarker :: String
                  , _asmParIdent  :: ASTMaybe Ident a
                  , _asmParInfo   :: a
                  } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
                  
data AsmParamMarker a
  = AsmParamMarker { _asmParMarkStr  :: String 
                   , _asmParMarkInfo :: a
                   } deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
-- * Expressions
 
data Expression a
  = -- | cond ? then_expr : else_expr
    Cond        { _exprCond          :: Expression a
                , _exprTrue          :: ASTMaybe Expression a -- ^ true-expression, can be omitted in GNU-C
                , _exprFalse         :: Expression a
                , _exprInfo          :: a
                }
  | Binary      { _exprBinOperator   :: BinaryOp a
                , _exprLeftSide      :: Expression a
                , _exprRightSide     :: Expression a
                , _exprInfo          :: a
                }
  | Cast        { _exprCastTyp       :: QualifiedType a
                , _exprCasted        :: Expression a
                , _exprInfo          :: a
                }
  | Unary       { _exprUnaryOp       :: UnaryOp a
                , _exprOperand       :: Expression a
                , _exprInfo          :: a
                }
  | SizeOfExpr  { _exprSizeOfExpr    :: Expression a
                , _exprInfo          :: a
                }
  | SizeOfType  { _exprSizeOfType    :: QualifiedType a
                , _exprInfo          :: a
                }
  | Call        { _exprCallFun       :: Expression a
                , _exprCallArgs      :: ASTParenList Expression a
                , _exprInfo          :: a
                }
  | -- | Both . and -> member operators
    Member      { _exprBase          :: Expression a
                , _exprMemberDeref   :: MemberDeref a
                , _exprMember        :: Ident a
                , _exprInfo          :: a
                }
  | -- | arr[ind] expressions
    Indexing    { _exprBase          :: Expression a
                , _exprIndex         :: ArrayIndex a
                , _exprInfo          :: a
                }
  -- | Identifier (including enumeration variants)
  | NameExpr    { _exprIdent         :: Ident a 
                , _exprInfo          :: a
                }             
  -- | Integer, character, floating point and string constants
  | LitExpr     { _exprLiteral       :: Literal a 
                , _exprInfo          :: a
                }                         
  | ParenExpr   { _parenExpr         :: Expression a
                , _exprInfo          :: a
                }                
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
data MemberDeref a 
  = SimpleMember { _derefInfo :: a } -- ^ The "." operator
  | DerefMember { _derefInfo :: a }  -- ^ The "->" operator
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data) 
  
data ArrayIndex a 
  = ArrayIndex { _indexExpr :: Expression a
               , _indexInfo :: a
               }
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
-- | C binary operators (K&R A7.6-15)
--
data BinaryOp a 
  = MulOp           { _binOpInfo :: a }   -- ^ * operator
  | DivOp           { _binOpInfo :: a }   -- ^ / operator
  | RemainderOp     { _binOpInfo :: a }   -- ^ % operator, remainder of division
  | AddOp           { _binOpInfo :: a }   -- ^ + operator
  | SubOp           { _binOpInfo :: a }   -- ^ - operator
  | ShiftLeftOp     { _binOpInfo :: a }   -- ^ << operator
  | ShiftRightOp    { _binOpInfo :: a }   -- ^ >> operator
  | LessOp          { _binOpInfo :: a }   -- ^ < operator
  | GreaterOp       { _binOpInfo :: a }   -- ^ > operator
  | LessOrEqOp      { _binOpInfo :: a }   -- ^ <= operator
  | GreaterOrEqOp   { _binOpInfo :: a }   -- ^ >= operator
  | EqOp            { _binOpInfo :: a }   -- ^ == operator
  | NotEqOp         { _binOpInfo :: a }   -- ^ != operator
  | BitAndOp        { _binOpInfo :: a }   -- ^ & operator, bitwise and
  | BitXorOp        { _binOpInfo :: a }   -- ^ ^ operator, exclusive bitwise or
  | BitOrOp         { _binOpInfo :: a }   -- ^ | operator, inclusive bitwise or
  | LogicAndOp      { _binOpInfo :: a }   -- ^ && operator, logical and
  | LogicOrOp       { _binOpInfo :: a }   -- ^ || operator, logical or
  | CommaOp         { _binOpInfo :: a }   -- ^ , operator, discards the result of the first expression
  | AssignOp        { _binOpInfo :: a }   -- ^ = operator, assignment
  | MulAssOp        { _binOpInfo :: a }   -- ^ *= operator, inplace multiplication
  | DivAssOp        { _binOpInfo :: a }   -- ^ /= operator, inplace division
  | RemainderAssOp  { _binOpInfo :: a }   -- ^ %= operator, inplace remainder
  | AddAssOp        { _binOpInfo :: a }   -- ^ += operator, inplace addition
  | SubAssOp        { _binOpInfo :: a }   -- ^ -= operator, inplace substraction
  | ShiftLeftAssOp  { _binOpInfo :: a }   -- ^ <<= operator, inplace left shift
  | ShiftRightAssOp { _binOpInfo :: a }   -- ^ >>= operator, inplace right shift
  | BitAndAssOp     { _binOpInfo :: a }   -- ^ &= operator, inplace bitwise and
  | BitXorAssOp     { _binOpInfo :: a }   -- ^ ^= operator, inplace bitwise exclusive or
  | BitOrAssOp      { _binOpInfo :: a }   -- ^ |= operator, inplace bitwise or
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
 
-- | C unary operator (K&R A7.3-4)
--
data UnaryOp a 
  = PreIncOp      { _unaryOpInfo :: a }   -- ^ ++ operator, prefix increment operator
  | PreDecOp      { _unaryOpInfo :: a }   -- ^ -- operator, prefix decrement operator
  | PostIncOp     { _unaryOpInfo :: a }   -- ^ ++ operator, postfix increment operator
  | PostDecOp     { _unaryOpInfo :: a }   -- ^ -- operator, postfix decrement operator
  | AddressOp     { _unaryOpInfo :: a }   -- ^ & operator, address operator
  | DereferenceOp { _unaryOpInfo :: a }   -- ^ * operator, dereferencing operator
  | PrePlusOp     { _unaryOpInfo :: a }   -- ^ + operator, prefix plus (no function)
  | PreMinOp      { _unaryOpInfo :: a }   -- ^ - operator, prefix minus (negation)
  | ComplementOp  { _unaryOpInfo :: a }   -- ^ ~ operator, one's complement
  | LogicNegOp    { _unaryOpInfo :: a }   -- ^ ! operator, logical negation
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)

data Literal a
  = IntLit   { _litInt         :: Integer
             , _litIntSpec     :: IntLitSpec
             , _litInfo        :: a
             }
  | CharLit  { _unicodeLit     :: Bool
             , _litChar        :: Char 
             , _litInfo        :: a
             }
  | FloatLit { _litFloat       :: Rational
             , _litFloatSize   :: Maybe FloatLitSize
             , _litInfo        :: a
             }
  | StrLitList { _strLits :: ASTList StringLiteral a }
  | CompoundLit { _litCompund  :: ASTList CompoundLitElem a
                , _litInfo :: a 
                }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
data StringLiteral a
  = StringLiteral { _strLitUnicode  :: Bool
                  , _litText        :: String 
                  , _strLitInfo     :: a
                  } 
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
data CompoundLitElem a 
  = CompoundLitElem { _compLitDesigns :: ASTList Designator a
                    , _compLitValue   :: Expression a
                    , _compLitInfo    :: a
                    }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
-- | Compound literal designated identifier
data Designator a 
  = MemberDesignator { _designatorMember :: Ident a
                     , _designatorInfo :: a
                     }
  | ArrDesignator    { _designatorIndex :: Expression a
                     , _designatorInfo :: a
                     }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
-- | Identifier
data Ident a = Ident { _identStr :: String
                     , _identInfo :: a 
                     }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
-- * Types
    
data QualifiedType a 
  = QualifiedType { _qualTypQual :: TypeQualifier a
                  , _qualTypTyp  :: Type a
                  , _qualTypInfo :: a
                  } 
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
data Type a
  = VoidType     { _voidTypInfo :: a }
  | IntType      { _intTypSign :: Sign
                 , _intTypSize :: IntSize
                 , _typInfo :: a
                 }
  | FloatingType { _floatTypSize :: FloatingSize
                 , _floatTypInfo :: a
                 }
  | BoolType     { _typInfo :: a }
  | StructType   { _structTypFields :: StructUnion a
                 , _typInfo :: a
                 }
  | UnionType    { _unionTypFields :: StructUnion a
                 , _typInfo :: a
                 }
  | EnumType     { _enumTypVariants :: Enumeration a
                 , _typInfo :: a
                 }
  | TypeDef      { _typDefTyp :: QualifiedType a
                 , _typDefIdent :: ASTMaybe Ident a
                 , _typInfo :: a
                 }
  | FunType      { _funTypRet :: QualifiedType a
                 , _funTypParams :: ASTParenList ParameterDeclaration a
                 , _typInfo :: a
                 }
  | TypeName     { _typNameId :: Ident a }
  | TypeOfType   { _typOfTyp :: QualifiedType a
                 , _typInfo :: a
                 }
  | TypeOfExpr   { _typOfExpr :: Expression a
                 , _typInfo :: a
                 }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
data TypeQualifier a
  = Scalar       { _qualIdent :: ASTMaybe Ident a
                 , _qualInfo :: a
                 }
  | PtrQual      { _qualBase :: TypeQualifier a
                 , _qualInfo :: a
                 }
  | ConstQual    { _qualBase :: TypeQualifier a
                 , _qualInfo :: a
                 }
  | VolatileQual { _qualBase :: TypeQualifier a
                 , _qualInfo :: a
                 }
  | ArrayQual    { _arrTypQual  :: ArrayTypeQual a
                 , _qualBase :: TypeQualifier a
                 , _qualInfo :: a
                 }
  | RestrictQual { _qualBase :: TypeQualifier a
                 , _qualInfo :: a
                 }
  | ParenQual    { _qualBase :: TypeQualifier a
                 , _qualInfo :: a
                 }
  | StaticQual   { _qualInfo :: a }
   deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
data ArrayTypeQual a 
  = ArrayTypeQual { _arrElemQual     :: ASTMaybe TypeQualifier a
                  , _arrQualSize     :: ASTMaybe Expression a
                  , _arrTypQualInfo  :: a
                  }
  deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
    
-- | C structure or union specifiers
--  Either @identifier@ or the declaration list @struct-decls@ (or both)
--  have to be present.

data StructUnion a
  = StructUnion { _structUnionId :: ASTMaybe Ident a           -- ^ identifier (Nothing if anonymous)
                , _structUnionFields :: ASTMaybe (ASTList Declaration) a -- ^ member declarations (Nothing if just declared not defined)
                , _structUnionInfo :: a
                }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
   
-- | C enumeration specifier
--
--  * Either the identifier or the enumerator-list (or both) have to be present.
--
data Enumeration a
  = Enumeration { _enumName :: ASTMaybe Ident a   -- ^ identifier (Nothing if anonymous)
                , _enumVariants :: ASTMaybe (ASTList Variant) a -- ^ variant declarations
                , _enumInfo :: a
                } 
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)
-- | The enumerator list is of the form @(enumeration-constant, enumeration-value?)@, where the latter
--   is an optional constant integral expression.
--
data Variant a 
  = Variant { _variantName :: Ident a               -- ^ variant name 
            , _variantValue :: ASTMaybe Expression a  -- ^ explicit variant value
            , _variantInfo :: a
            }
    deriving (Show, Eq, Ord, Functor, Generic, Typeable, Data)

-- * Types to store AST data

data IntLitSpec 
  = IntLitSpec { _intLitSize :: Maybe IntLitSize
               , _intLitSign :: Maybe IntLitSign
               } 
    deriving (Show, Eq, Ord, Generic, Typeable, Data)
                             
data IntLitSize = LitLong | LitLongLong 
    deriving (Show, Eq, Ord, Generic, Typeable, Data)
    
data IntLitSign = LitUnsigned  
    deriving (Show, Eq, Ord, Generic, Typeable, Data)
    
data FloatLitSize = LitFloat | LitLongDouble
    deriving (Show, Eq, Ord, Generic, Typeable, Data) 
    
data FloatingSize = Float | Double | LongDouble
    deriving (Show, Eq, Ord, Generic, Typeable, Data)

data IntSize = Char | Short | Int | LongInt | LongLongInt
    deriving (Show, Eq, Ord, Generic, Typeable, Data)

data Sign = Signed | Unsigned
    deriving (Show, Eq, Ord, Generic, Typeable, Data)
  