module Data.XPath.Types where

import qualified Data.Text as T
import Data.XML.Types(Name)
 
data Step = Step Axis NodeTest [Expr]
    deriving Show
            
data Axis 
    = Child | Descendant | FollowingSibling | Following | DescendantOrSelf 
    | Self | Attribute | Namespace
    | Parent | Ancestor | Preceding | PrecedingSibling | AncestorOrSelf
    deriving Show
            
data NodeTest 
    = NameTest (Maybe T.Text) (Maybe T.Text) | Comment | Text | ProcIns (Maybe T.Text) | Node 
    deriving Show

data BinOp = Plus | Minus | Mul | Union | Div | Mod | Eq | NEq | Lt | LE | Gt | GE | Or | And
    deriving Show

data Expr
    = EPE PrimaryExpr [Expr] [Step]
    | EBinOp BinOp Expr Expr
    | Negate Expr
    deriving Show
            
data PrimaryExpr 
    = LP Bool [Step]
    | VR Name
    | Expr Expr
    | Lit T.Text
    | Num Double
    | FC Name [Expr] -- | call function with Name and list of arguments
    deriving Show
            
                   