{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Stream.XPathProcess where

-- import Control.Arrow
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import Data.Default
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.XML.Types
import qualified Filesystem.Path as FSP
import Safe
import qualified Text.XML.Stream.Parse as XP

import Data.XPath.Types
import Text.XML.Stream.XPath

data XPPState = XPPState
instance Default XPPState where
    def = XPPState

type XPP f l u m r = StateT XPPState (ReaderT Expr (Pipe l (f Event) [f Event] u m)) r

processIdentity :: Monad m => Pipe l i Event u m r1 -> Expr -> Pipe l i [Event] u m r2
processIdentity p xpe = mapOutput Identity p
                    >+> mapOutput (map runIdentity) (processXPath xpe)
                    
processFunctor :: (Monad m,  Functor f) => Pipe l i (f Event) u m r1 -> Expr -> Pipe l i [f Event] u m r2
processFunctor p xpe = p >+> processXPath xpe

processEither :: (Expr -> a) -> T.Text -> Either String a
processEither p = fmap p . parseXPath
                    
processXPathCFile   :: MonadResource m => XP.ParseSettings -> FSP.FilePath -> Expr -> Pipe l i [Event] u m ()
processXPathCFile ps = processIdentity . XP.parseFile ps
                    
processXPathFile    :: MonadResource m => XP.ParseSettings -> FSP.FilePath -> T.Text -> Either String (Pipe l i [Event] u m ())
processXPathFile ps = processEither . processXPathCFile ps

processXPathCText   :: MonadThrow m => XP.ParseSettings -> Expr -> Pipe l T.Text [XP.EventPos] u m ()
processXPathCText   = processFunctor . XP.parseText

processXPathText    :: MonadThrow m => XP.ParseSettings -> T.Text -> Either String (Pipe l T.Text [XP.EventPos] u m ())
processXPathText    = processEither . processXPathCText

processXPathCBytes  :: MonadThrow m => XP.ParseSettings -> Expr -> Pipe l BS.ByteString [Event] u m ()
processXPathCBytes = processIdentity . XP.parseBytes

processXPathBytes   :: MonadThrow m => XP.ParseSettings -> T.Text -> Either String (Pipe l BS.ByteString [Event] u m ())
processXPathBytes   = processEither . processXPathCBytes

processXPathCBytesPos :: MonadThrow m => XP.ParseSettings -> Expr -> Pipe l BS.ByteString [XP.EventPos] u m ()
processXPathCBytesPos = processFunctor . XP.parseBytesPos

processXPathBytesPos :: MonadThrow m => XP.ParseSettings -> T.Text -> Either String (Pipe l BS.ByteString [XP.EventPos] u m ())
processXPathBytesPos = processEither . processXPathCBytesPos

processXPathCLBS    :: MonadThrow m => XP.ParseSettings -> LBS.ByteString -> Expr -> Pipe l i [Event] u m ()
processXPathCLBS ps = processIdentity . XP.parseLBS ps

processXPathLBS     :: MonadThrow m => XP.ParseSettings -> LBS.ByteString -> T.Text -> Either String (Pipe l i [Event] u m ())
processXPathLBS ps  = processEither . processXPathCLBS ps

processXPath :: (Functor f, Monad m) => Expr -> Pipe l (f Event) [f Event] u m r
processXPath xpe = runReaderT (evalStateT processXPathInt def) xpe

processXPathInt :: Functor f => XPP f l u m r
processXPathInt = undefined

data ExprResult = ERN Double
                  | ERT T.Text
                  | ERB Bool
                  | ERNo [[Event]]
instance Default ExprResult where
    def = ERNo []
                      
type ExprPartial = Either ExprResult Expr

eRtoBool :: ExprResult -> Bool
eRtoBool (ERN n) = n /= 0                  
eRtoBool (ERT t) = not $ T.null t                  
eRtoBool (ERB b) = b
eRtoBool (ERNo xs) = not $ null xs

{-
ePtoBool :: ExprPartial -> ExprPartial
ePtoBool (Finished e) = Finished $ ERB $ eRtoBool e
ePtoBool p = p
-}
boolToNum :: Bool -> Double
boolToNum b = if b then 1 else 0

eRtoNum :: ExprResult -> Maybe Double
eRtoNum (ERN n) = Just n                  
eRtoNum (ERT t) = readMay $ T.unpack t                  
eRtoNum (ERB b) = Just $ boolToNum b
eRtoNum (ERNo xs) = listToMaybe xs >>= \es -> eRtoNum (ERT $ mconcat [toText ct | EventContent ct <- es])
    where
        toText (ContentText t) = t
        toText (ContentEntity t) = t
        
--toComp :: ExprResult -> 

calcExpr :: Event -> Expr -> ExprPartial
calcExpr ev ex = case ex of
    EPE pe exs steps -> undefined
    EBinOp bo e1 e2 -> calcbo bo e1 e2
    Negate e1 -> bimap (ERB . not) Negate $ calcb e1
    where
        calc = calcExpr ev
        calcn = first eRtoNum . calc 
        calcb = first eRtoBool . calc
        calcbo bo e1 e2 = case bo of
            Plus -> enbo (+) 
            Minus -> enbo (-)
            Mul -> enbo (*)
            Union -> undefined 
            Div -> enbo (/)
            Mod -> enbo (\a b -> let a' = abs a; b' = abs b in signum a * (a' - b'*(fromInteger $ floor $ a'/b')))
           {-
            Eq -> ebbo (==)
            NEq -> ebbo (/=)
            Lt -> ebbo (<)
            LE -> ebbo (<=)
            Gt -> ebbo (>)
            GE -> ebbo (>=)
           -}
            Or -> ebbo (||)
            And -> ebbo (&&)
        
            where
                enbo f = case (calcn e1, calcn e2) of
                    (Left a,  Left b ) -> Left $ maybe def ERN $ f <$> a <*> b
                    (Right a, Left b ) -> maybe (Left def) (\b' -> Right $ EBinOp bo a $ EPE (Num b') [] []) b
                    (Left a,  Right b) -> maybe (Left def) (\a' -> Right $ EBinOp bo (EPE (Num a') [] []) b) a
                    (Right a, Right b) -> Right $ EBinOp bo a b
                ebbo :: (Bool->Bool->Bool) -> ExprPartial
                ebbo f = case (calcb e1, calcb e2) of
                    (Left a,  Left b ) -> Left $ ERB $ f a b
                    (Right a, Left b ) -> Right $ EBinOp bo a $ EPE (Num $ boolToNum b) [] []
                    (Left a,  Right b) -> Right $ EBinOp bo (EPE (Num $ boolToNum a) [] []) b
                    (Right a, Right b) -> Right $ EBinOp bo a b
                {-
                ebbo' f = case (calc e1, calc e2) of
                    (Left a, Left b) -> Left $ ERB $ f a b
                    (Right a, Left b) -> Right $ EBinOp bo a $ EFE (Num $ boolToNum b) [] []
                    (Left a, Right b) -> Right $ EBinOp bo (EFE (Num $ boolToNum a) [] []) b
                    (Right a, Right b) -> Right $ EBinOp bo a b  
                -}
        
                    
