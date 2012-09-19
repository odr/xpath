{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Stream.XPathProcess where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit
import Data.Default
import Data.Functor.Identity
import qualified Data.Text as T
import Data.XML.Types
import qualified Filesystem.Path as FSP
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
processEither p = fmap p . parseXPathExpr
                    
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