module Data.XPath.Types where

import Data.Default(Default(..))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.XML.Types(Name, Event)
 
data Step = Step Axis NodeTest [Expr]
    deriving Show
            
data Axis 
    = Child | Descendant | FollowingSibling | Following | DescendantOrSelf 
    | Self | Attribute | Namespace
    | Parent | Ancestor | Preceding | PrecedingSibling | AncestorOrSelf
    deriving Show
            
data NodeTest 
    = NameTest (Maybe T.Text) (Maybe T.Text) 	-- | prefix and name
    | Comment | Text | ProcIns (Maybe T.Text) | Node 
    deriving Show

data BinOp = Plus | Minus | Mul | Union | Div | Mod | Eq | NEq | Lt | LE | Gt | GE | Or | And
    deriving (Show, Eq)
    
data Expr                                               -- | Parsed xpath-expression or calculated expression result
    = PrimExpr PrimaryExpr [Expr] [Step]
    | BinOp BinOp Expr Expr 
    | Negate Expr 
    | ExRes ExprResult 
    deriving Show
            
data PrimaryExpr  
    = LocPath Bool [Step]
    | Var Name
    | Expr Expr
    | FunC Name [Expr] -- ^ call function with Name and list of arguments
    deriving Show
    
data ExprResult        -- | Expression result. Node is presented as [Event].
    = ExResNum Double
    | ExResText T.Text
    | ExResBool Bool
    | ExResNodes [[Event]]
    deriving Show

instance Default ExprResult where
    def = ExResNodes []
    
data Context
    = VarBinds (M.Map T.Text ExprResult)
    | FuncBinds (M.Map T.Text [ExprResult] -> ExprResult)
    | Namespaces (M.Map T.Text T.Text)