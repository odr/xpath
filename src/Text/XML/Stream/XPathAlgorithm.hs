{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Text.XML.Stream.XPathAlgorithm where

-- import Control.Arrow
-- import Control.Monad(liftM2)
import Data.Function(on)
import Data.Maybe(listToMaybe)
import Data.Monoid(Monoid(..), (<>))
import qualified Data.Text as T
import Data.XML.Types(Event(..), Content(..)) -- , Name(..)
import Data.XPath.Types
import Safe(readMay)

process :: Context -> Event -> (Expr, [Expr]) -> (Expr, [Expr])
process ctx (EventBeginElement name attrs) zipper@(expr, exprs) = (go expr, expr:exprs)
    where
        go e@(ExRes _)      = e
        go (Negate e)       = neg $ go e
        go (BinOp op e1 e2) = (doOp op `on` go) e1 e2
        go (PrimExpr pe preds steps) = undefined

neg :: Expr -> Expr
neg (ExRes er)  = ExRes $ ExResNum $ negate $ getNumber er
neg ex          = Negate ex

doOp :: BinOp -> Expr -> Expr -> Expr
doOp op (ExRes er1) (ExRes er2) = case op of
    Plus    -> ExRes $ ExResNum $ n1 + n2 
    Minus   -> ExRes $ ExResNum $ n1 - n2 
    Mul     -> ExRes $ ExResNum $ n1 * n2
    Union   -> case (er1, er2) of
        (ExResNodes ns1, ExResNodes ns2)    -> ExRes $ ExResNodes $ ns1 <> ns2 
        _                                   -> error "Error: Union arguments should be node sets"
    Div     -> ExRes $ ExResNum $ n1 / n2
    Mod     -> ExRes $ ExResNum $ n1 `modXP` n2
    Eq      -> compBool (==)
    NEq     -> compBool (/=)
    Lt      -> compBool (<)
    LE      -> compBool (<=)
    Gt      -> compBool (>)
    GE      -> compBool (>=)
    Or      -> ExRes $ ExResBool $ b1 || b2
    And     -> ExRes $ ExResBool $ b1 && b2
    where
        n1 = getNumber er1
        n2 = getNumber er2
        b1 = getBool er1
        b2 = getBool er2
        modXP x y = x - y * fromInteger (round x `quot` round y)

        compBool :: (forall a. Ord a => a -> a -> Bool) -> Expr
        compBool op0 = ExRes $ ExResBool $ comp op0 er1 er2
            where
                comp :: (forall a. Ord a => a -> a -> Bool) -> ExprResult -> ExprResult -> Bool
                comp op' (ExResNodes ns1) (ExResNodes ns2) = compNodes
                    where
                        compNodes = let ss1 = map getStrValue ns1
                                        ss2 = map getStrValue ns2 
                                    in
                                        not $ null [() | s1 <- ss1, s2 <- ss2 , s1 `op'` s2]
                comp op' er1'@(ExResNum _) (ExResNodes ns2)     
                    = any (comp op' er1' . number . ExResText . getStrValue) ns2
                comp op' er1'@(ExResText _) (ExResNodes ns2)    
                    = any (comp op' er1' . ExResText . getStrValue) ns2
                comp op' er1'@(ExResBool _) er2'@(ExResNodes _) 
                    = comp op' er1' $ boolean er2'
                comp op' er1'@(ExResNodes _) er2' = comp (flip op') er2' er1'

                comp op' er1' er2'@(ExResBool b2') 
                    | op `elem` [Eq, NEq]   = getBool er1' `op'` b2'
                    | otherwise             = (comp op' `on` number) er1' er2'
                comp op' er1'@(ExResBool _) er2' = comp (flip op') er2' er1'
                    
                comp op' er1' er2'@(ExResNum n2') 
                    | op `elem` [Eq, NEq] = getNumber er1' `op'` n2'
                    | otherwise         = (op' `on` getNumber) er1' er2'
                comp op' er1'@(ExResNum _) er2' = comp (flip op') er2' er1'
                    
                comp op' er1'@(ExResText t1) er2'@(ExResText t2) 
                    | op `elem` [Eq, NEq]   = t1 `op'` t2
                    | otherwise         = (comp op' `on` number) er1' er2'

doOp op e1' e2' = BinOp op e1' e2'


number :: ExprResult -> ExprResult
number = ExResNum . getNumber 

getNumber :: ExprResult -> Double
getNumber (ExResNum n)          = n                  
getNumber (ExResText t)         = maybe (0/0) id $ readMay $ T.unpack t                  
getNumber (ExResBool True)      = 1
getNumber (ExResBool False)     = 0
getNumber er@(ExResNodes _)    = getNumber $ string er
        
string :: ExprResult -> ExprResult
string = ExResText . getString

getString :: ExprResult -> T.Text
getString (ExResNum n)          = T.pack $ reverse $ convert $ reverse $ show n
    where
        convert ('0':s) = convert s
        convert ('.':s) = s
        convert s = s
getString (ExResText t)         = t
getString (ExResBool True)      = "True"
getString (ExResBool False)     = "False"
getString (ExResNodes xs)       = maybe "" getStrValue $ listToMaybe xs

getStrValue :: [Event] -> T.Text
getStrValue xs = mconcat [toText ct | EventContent ct <- xs]
    where
        toText (ContentText t)      = t
        toText (ContentEntity t)    = t

boolean :: ExprResult -> ExprResult
boolean = ExResBool . getBool
        
getBool :: ExprResult -> Bool
getBool (ExResNum n) = not (isNaN n) && abs n > 1/2^(32::Int) 
getBool (ExResText t) = t /= mempty        
getBool (ExResBool b) = b
getBool (ExResNodes []) = False
getBool (ExResNodes _) = True
    
    
    
{-
EventBeginDocument	 
EventEndDocument	 
EventBeginDoctype Text (Maybe ExternalID)	 
EventEndDoctype	 
    EventInstruction Instruction	 
    EventBeginElement Name [(Name, [Content])]	 
EventEndElement Name	 
    EventContent Content	 
    EventComment Text	 
    EventCDATA Text	 
-}