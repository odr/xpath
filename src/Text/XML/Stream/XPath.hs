{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Stream.XPath where
-- module Text.XML.Stream.XPath(parseXPath) where

-- import Control.Arrow
import Control.Applicative
import Control.Monad(guard)
import Data.Attoparsec.Text
import Data.List(foldl')
import qualified Data.Text as T
import Data.XML.Types(Name(..))
import Data.XPath.Types

parseXPath :: T.Text -> Either String Expr
parseXPath = parseOnly expr
                  
spaces :: Parser ()
spaces = (many $ oneOf "\x20\x9\xD\xA") *> return ()

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

lexb :: T.Text -> T.Text -> Parser a -> Parser a
lexb t1 t2 p = lexc t1 *> p <* lexc t2 

bracket :: Parser a -> Parser a
bracket = lexb "(" ")"

square :: Parser a -> Parser a
square  = lexb "[" "]"

lexc :: T.Text -> Parser ()
lexc = (() <$) . lexeme . string

choiceConst :: [(T.Text, a)] -> Parser a
choiceConst = choice . map (\(s,c) -> c <$ lexc s)

literal :: Parser T.Text
literal = lexeme $ choice [lit '"', lit '\'']  
        where 
                lit ch = char ch >> many (noneOf [ch]) >>= \s -> char ch >> return (T.pack s)

noneOf :: String -> Parser Char
noneOf xs = satisfy (`notElem`xs)

oneOf :: String -> Parser Char
oneOf xs = satisfy (`elem`xs)

numberXP :: Parser Double
numberXP  = lexeme rational

{-
locationPath :: Parser LocationPath
locationPath = choice [ spaces <*. "/" *> (LocPath True <$> steps)
                      , LocPath False <$> steps1 ]
-}

steps :: Parser [Step]
steps = concat <$> sepBy step (string "/")
        
steps1 :: Parser [Step]
steps1 = concat <$> sepBy1 step (string "/")

step :: Parser [Step]
step    = choice 
        [ string "/" >> spaces >> step' >>= \s -> return [Step DescendantOrSelf Node [], s]
        , string "/" >> spaces >> return [Step DescendantOrSelf Node []]
        , (:[]) <$> step' ]
    where 
        step'   = choice 
                [ lexc "@"  *> (Step Attribute <$> nodeTest <*> many predicate)
                , lexc ".." *> pure (Step Parent (NameTest Nothing Nothing) [])
                , lexc "."  *> pure (Step Self (NameTest Nothing Nothing) [])
                , Step <$> (axisName <* lexc "::") <*> nodeTest <*> many predicate
                , Step Child <$> nodeTest <*> many predicate ]

axisName :: Parser Axis
axisName        = choiceConst
                [ ("ancestor-or-self", AncestorOrSelf)                
                , ("ancestor", Ancestor)
                , ("attribute", Attribute)
                , ("child", Child)
                , ("descendant-or-self", DescendantOrSelf)
                , ("descendant", Descendant)
                , ("following-sibling", FollowingSibling)
                , ("following", Following)
                , ("namespace", Namespace)
                , ("parent", Parent)
                , ("preceding-sibling", PrecedingSibling)
                , ("preceding", Preceding)
                , ("self", Self) ]

nodeTest :: Parser NodeTest                
nodeTest        = choice
                [ choiceConst [("*", NameTest Nothing Nothing)]
                , choiceBracket [ ("comment", Comment)
                                , ("node", Node)
                                , ("text", Text)
                                , ("processing-instruction", ProcIns Nothing) ]                 
                , (ProcIns . Just) <$> (lexc "processing-instruction" *> bracket literal)
                , choice [ NameTest <$> (Just <$> ncName) <*> (lexc ":" *> choice [ lexc "*" *> pure Nothing
                                                                                  , Just <$> ncName ])
                         , NameTest Nothing <$> (Just <$> ncName) ] ]
    where
        choiceBracket :: [(T.Text, a)] -> Parser a 
        choiceBracket = choice . map (\(a,b) -> b <$ (lexc a *> bracket spaces))
        
isStartChar :: Char -> Bool
isStartChar c   =  c == '_' || ci 'A' 'Z' || ci 'a' 'z' || ci '\xC0' '\xD6'   
                || ci '\xD8' '\xF6'|| ci '\xF8' '\x2FF' 
                || ci '\x370' '\x37D'|| ci '\x37F' '\x1FFF'|| ci '\x200C' '\x200D'|| ci '\x2070' '\x218F'
                || ci '\x2C00' '\x2FEF'|| ci '\x3001' '\xD7FF'|| ci '\xF900' '\xFDCF'|| ci '\xFDF0' '\xFFFD'
                || ci '\x10000' '\xEFFFF'
    where
        ci a b = c >= a && c <= b

isNameChar :: Char -> Bool
isNameChar c = isStartChar c || c `elem` "-.\xB7" || ci '0' '9' || ci '\x0300' '\x036F'|| ci '\x203F' '\x2040'
    where
        ci a b = c >= a && c <= b

ncName :: Parser T.Text
ncName = fmap T.pack $ (:) <$> satisfy isNameChar <*> many (satisfy isNameChar)

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = choice  [ Just <$> p, pure Nothing ]

qName :: Parser Name
qName = (\n1 n2 -> Name { nameLocalName = n2
                        , namePrefix = n1
                        , nameNamespace = Nothing }
                ) <$> optionMaybe (ncName <* lexc ":") <*> ncName

predicate :: Parser Expr
predicate = square expr

chainExp :: Parser a -> [[(T.Text, a -> a -> a)]] -> Parser a
chainExp p zs = go zs 
    where
        go [] = p
        go xs@(x:xs') = go xs' >>= \r -> choice (map (\(t,c) -> lexc t *> (c r <$> go xs)) x ++ [pure r])

expr :: Parser Expr
expr = chainExp unaryExpr 
                [ [("or"	, (BinOp Or))]
                , [("and"	, (BinOp And))]
                , [("!="	, (BinOp NEq))	, ("="	, BinOp Eq)]
                , [("<="	, (BinOp LE))	, (">="	, BinOp GE)	, ("<", BinOp Lt)	, (">", BinOp Gt)]
                , [("+"		, (BinOp Plus))	, ("-"	, BinOp Minus)]
                , [("*"		, (BinOp Mul))	, ("div", BinOp Div), ("mod", BinOp Mod)] 
                ]
           
unaryExpr :: Parser Expr
unaryExpr = choice [ (foldl' (\a _ -> not a) False <$> many (lexc "-")) >>= guard >> (Negate <$> unionExpr)
                   , unionExpr ]
                   
unionExpr :: Parser Expr
unionExpr = chainExp pathExpr [[("|", BinOp Union)]]

pathExpr :: Parser Expr
pathExpr    = choice
            [ ExRes <$> (ExResNum <$> numberXP)
            , ExRes <$> (ExResText <$> literal) 
            , PrimExpr <$> primaryExpr <*> many predicate <*> choice ["/" .*> steps, pure []] ]
            
primaryExpr :: Parser PrimaryExpr
primaryExpr = choice 
            [ Var  <$> (lexc "$" *> qName)
            , Expr <$> bracket expr
            , FunC <$> qName <*> bracket (choice [expr `sepBy` lexc ",", pure []]) 
            , choice    [ spaces <*. "/" *> (LocPath True <$> steps)
                        , LocPath False <$> steps1 ] ]
            -- , Num  <$> numberXP
            -- , Lit  <$> literal ]
