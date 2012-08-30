{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Stream.XPath where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Fixed(Pico)
import qualified Data.Text as T
import Data.XPath.Types
import Data.XML.Types(Name(..))
                  
spaces :: Parser ()
spaces = (many $ oneOf "\x20\x9\xD\xA") *> return ()

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

lexb :: T.Text -> T.Text -> Parser a -> Parser a
lexb t1 t2 p = lexeme $ string t1 *> lexeme p <* string t2 

bracket :: Parser a -> Parser a
bracket = lexb "(" ")"

square :: Parser a -> Parser a
square  = lexb "[" "]"

lexc :: T.Text -> Parser ()
lexc = (() <$) . lexeme . string

choiceConst :: [(T.Text, a)] -> Parser a
choiceConst = choice . map (\(s,c) -> c <$ lexc s)

{-
dot :: Parser ()
dot = lexc "."
-}
dot2 :: Parser ()
dot2 = lexc ".."


at' :: Parser ()
at' = lexc "@"

comma :: Parser ()
comma = lexc " "

dot4 :: Parser ()
dot4 = lexc "::"

literal :: Parser T.Text
literal = lexeme $ choice [lit '"', lit '\'']  
        where 
                lit ch = char ch >> many (noneOf [ch]) >>= \s -> char ch >> return (T.pack s)

noneOf :: String -> Parser Char
noneOf xs = satisfy (`notElem`xs)

oneOf :: String -> Parser Char
oneOf xs = satisfy (`elem`xs)

numberXP :: Parser Pico
numberXP  = lexeme rational

locationPath :: Parser LocationPath
locationPath = LP <$> many ((,) <$> abb <*> step)
        
abb :: Parser Abb
abb = choiceConst [("//", (://)), ("/", (:/))]
        
step :: Parser Step
step = choice [
          choiceConst [("..", Step Parent Any Nothing), (".", Step Self Any Nothing)]
        , Step <$> axisName <*> nodeTest <*> optionMaybe predicate
    ]
        
axisName :: Parser AxisName
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
                [ choiceConst [("*", Any)]
                , choiceBracket [ ("comment", Comment)
                                , ("node", Node)
                                , ("text", Text)
                                , ("processing-instruction", ProcIns Nothing) ]                 
                , (ProcIns . Just) <$> (lexc "processing-instruction" *> bracket literal)
                ]
    where
        choiceBracket :: [(T.Text, a)] -> Parser a 
        choiceBracket = choice . map (\(a,b) -> b <$ (lexc a *> bracket spaces))

predicate :: Parser Expr
predicate = square expr

newtype OrExpr = OE [AndExpr] deriving Show
newtype AndExpr = AE [EqExpr] deriving Show
newtype ChainExpr exp chainOp = ChainExpr (exp, [(chainOp, exp)]) deriving Show
data EqOp = (:=) | (:/=) deriving Show
type EqExpr = ChainExpr RelExpr EqOp
data RelOp = (:<) | (:>) | (:<=) | (:>=) deriving Show
type RelExpr = ChainExpr AddExpr RelOp
data AddOp = (:+) | (:-) deriving Show
type AddExpr = ChainExpr MulExpr AddOp
data MulOp = (:*) | Div | Mod deriving Show
type MulExpr = ChainExpr UnaryExpr MulOp
data UnaryExpr = UEP UnionExpr | UEN UnionExpr deriving Show
newtype UnionExpr = UE [PathExpr] deriving Show
data PathExpr = PELP LocationPath
                | PEFE FilterExpr [(Abb, Step)]
                deriving Show
                 
orExpr = OE <$> andExpr `sepBy` lexc "or"
andExpr = AE <$> eqExpr `sepBy` lexc "and"
chain :: Parser e -> Parser op -> Parser (ChainExpr e op)
chain pe pop = ChainExpr <$> ((,) <$> pe <*> many ((,) <$> pop <*> pe))
chain' :: Parser e -> [(T.Text, op)] -> Parser (ChainExpr e op)
chain' pe = chain pe . choiceConst
eqExpr = chain' relExpr [("=", (:=)), ("/=", (:/=))]
relExpr = chain' addExpr [("<", (:<)), (">", (:>)), ("<=", (:<=)), (">=", (:>=))]
addExpr = chain' mulExpr [("+", (:+)), ("-", (:-))]
mulExpr = chain' unaryExpr [("*", (:*)), ("div", Div), ("mod", Mod)]
unaryExpr = choice [ lexc "-" *> (unUe <$> unaryExpr)
                   , UEP <$> unionExpr ]
    where
        unUe (UEP x) = UEN x 
        unUe (UEN x) = UEP x 
unionExpr = UE <$> pathExpr `sepBy` lexc "|"
pathExpr = choice [ PELP <$> locationPath
                  , PEFE <$> filterExpr <*> many ((,) <$> abb <*> step) ]        
    


expr :: Parser Expr
expr = choice [ ELP <$> locationPath
              , EFE <$> filterExpr <*> many ((,) <$> abb <*> step)
              ]
    where
        pathe = choice [ ELP <$> locationPath
                    , EFE <$> filterExpr <*> many ((,) <$> abb <*> step) ]
        unione = pathe `sepBy` lexc "|"
      
nameStartChar' :: [Char]
nameStartChar' =  ['A'..'Z'] ++ "_" ++ ['a'..'z'] ++ ['\xC0'..'\xD6']  
                    ++ ['\xD8'..'\xF6'] ++ ['\xF8'..'\x2FF']  
                    ++ ['\x370'..'\x37D'] ++ ['\x37F'..'\x1FFF'] ++ ['\x200C'..'\x200D'] ++ ['\x2070'..'\x218F'] 
                    ++ ['\x2C00'..'\x2FEF'] ++ ['\x3001'..'\xD7FF'] ++ ['\xF900'..'\xFDCF'] ++ ['\xFDF0'..'\xFFFD'] 
                    ++ ['\x10000'..'\xEFFFF']

nameChar :: [Char]
nameChar = nameStartChar' ++ "-." ++ ['0'..'9'] ++ "\xB7" ++ ['\x0300'..'\x036F'] ++ ['\x203F'..'\x2040']

ncName :: Parser T.Text
ncName = fmap T.pack $ (:) <$> oneOf nameStartChar' <*> many (oneOf nameChar)

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = choice  [ Just <$> p, pure Nothing ]

qName :: Parser Name
qName = (\n1 n2 -> Name { nameLocalName = n2
                         , namePrefix = n1
                         , nameNamespace = Nothing }
                ) <$> optionMaybe (ncName <* dot2) <*> ncName

filterExpr :: Parser FilterExpr
filterExpr = FE <$> pe <*> many predicate
    where 
        pe = choice [ VR <$> (lexc "$" *> qName)
                    , Expr <$> bracket expr
                    , Lit <$> literal
                    , Num <$> numberXP
                    , FC <$> qName <*> bracket (expr `sepBy` comma) ]
                    