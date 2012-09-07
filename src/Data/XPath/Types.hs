module Data.XPath.Types where

import Data.Fixed(Pico)
import qualified Data.Text as T
import Data.XML.Types(Name)


data Abb = (://) | (:/)
            deriving Show

data LocationPath = LP (Maybe Step) [(Abb, Step)]
            deriving Show

-- . => Step Self Any Nothing; .. => Step Parent Any Nothing
data Step  = Step AxisName NodeTest [Predicate]
            deriving Show
            
-- @ => Attribute
data AxisName = Child | Descendant | Self | FollowingSibling | Following | DescendantOrSelf | Attribute | Namespace
                | Parent | Ancestor | Preceding | PrecedingSibling | AncestorOrSelf
            deriving Show


data NodeTest = NameTest (Maybe T.Text) (Maybe T.Text) | Comment | Text | ProcIns (Maybe T.Text) | Node 
            deriving Show

type Predicate = Expr

data Expr = ELP LocationPath
           | EFE FilterExpr [(Abb, Step)]
           | Expr :+: Expr
           | Expr :-: Expr
           | Negate Expr
           | Expr :*: Expr
           | Expr :|: Expr
           | Div Expr Expr
           | Mod Expr Expr
           | Expr :=: Expr
           | Expr :!=: Expr
           | Expr :<: Expr
           | Expr :<=: Expr
           | Expr :>: Expr
           | Expr :>=: Expr
           | Expr :||: Expr
           | Expr :&&: Expr
            deriving Show
            
data FilterExpr = FE PrimaryExpr [Predicate]
            deriving Show
                  
data PrimaryExpr = VR Name
                   | Expr Expr
                   | Lit T.Text
                   | Num Pico
                   | FC Name [Expr] 
            deriving Show
                   