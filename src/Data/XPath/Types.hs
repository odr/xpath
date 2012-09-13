module Data.XPath.Types where

import Data.Fixed(Pico)
import qualified Data.Text as T
import Data.XML.Types(Name)


data LocationPath = LP { lpAbsolute :: Bool, lpSteps ::[Step] }
            deriving Show

-- . => Step Self Any Nothing; .. => Step Parent Any Nothing
data Step = Step Axis NodeTest [Predicate]
            deriving Show
            
-- @ => Attribute
data Axis = Child | Descendant | Self | FollowingSibling | Following | DescendantOrSelf | Attribute | Namespace
                | Parent | Ancestor | Preceding | PrecedingSibling | AncestorOrSelf
            deriving Show
            
data NodeTest = NameTest (Maybe T.Text) (Maybe T.Text) | Comment | Text | ProcIns (Maybe T.Text) | Node 
            deriving Show

type Predicate = Expr

data BinOp = Plus | Minus | Mul | Union | Div | Mod | Eq | NEq | Lt | LE | Gt | GE | Or | And
        deriving Show

data Expr = ELP LocationPath
           | EFE PrimaryExpr [Predicate] [Step]
           | EBinOp BinOp Expr Expr
           | Negate Expr
            deriving Show
            
data PrimaryExpr = VR Name
                   | Expr Expr
                   | Lit T.Text
                   | Num Pico
                   | FC Name [Expr] 
            deriving Show
                   