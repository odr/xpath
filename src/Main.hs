{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.XPath.Types(Expr)
import qualified Data.Text as T
import Text.XML.Stream.XPath
import System.Environment

main :: IO ()
main = getArgs >>= mapM_ (\x -> do
        let lp = parseXPath x
        putStrLn $ T.unpack x ++ "\t\t\t- " ++ show lp ++ "\n\n"
    ) . flip take tests . read . head

tests :: [T.Text]
tests = [ "child::para"
-- {-
        , "child::*"
        , "child::text()"
        , "child::node()"
        , "attribute::name"
        , "attribute::*"
        , "descendant::para"
        , "ancestor::div"
        , "ancestor-or-self::div"
        , "descendant-or-self::para"
        , "self::para"
        , "child::chapter/descendant::para"
        , "child::*/child::para"
        , "/"
        , "/descendant::para"
        , "/descendant::olist/child::item"
        , "child::para[position()=1]"
        , "child::para[position()=last()]"
        , "child::para[position()=last()-1]"
        , "child::para[position()>1]"
        , "following-sibling::chapter[position()=1]"
        , "preceding-sibling::chapter[position()=1]"
        , "/descendant::figure[position()=42]"
        , "/child::doc/child::chapter[position()=5]/child::section[position()=2]"
-- -}
        , "child::para[attribute::type=\"warning\"]"
-- {-
        , "child::para[attribute::type='warning'][position()=5]"
        , "child::para[position()=5][attribute::type=\"warning\"]"
        , "child::chapter[child::title='Introduction']"
        , "child::chapter[child::title]"
        , "child::*[self::chapter or self::appendix]"
        , "child::*[self::chapter or self::appendix][position()=last()]" 
        , "para"
        , "*"
        , "text()"
        , "text"
        , "@name"
        , "@*"
        , "para[1]"
        , "para[last()]"
        , "*/para"
        , "/doc/chapter[5]/section[2]"
        , "chapter//para"
        , "//para"
        , "//olist/item"
        , "."
        , ".//"
        , ".."
        , "../@lang"
        , "para[@type=\"warning\"]"
        , "para[@type=\"warning\"][5]"
        , "para[5][@type=\"warning\"]"
        , "chapter[title=\"Introduction\"]"
        , "chapter[title]"
-- -}
        , "employee[@secretary and @assistant]"
         ]
{-
child::para находит элемент para, являющийся непосредственным потомком узла контекста

child::* собирает все элементы, являющиеся непосредственными потомками узла контекста

child::text() собирает все текстовые узлы, являющиеся непосредственными потомками узла контекста

child::node() собирает все непосредственные потомки текущего узла контекста независимо от типа этих узлов

attribute::name в текущем узле контекста находит атрибут name

attribute::* собирает все атрибуты в текущем узле контекста

descendant::para среди потомков узла контекста находит элементы para

ancestor::div собирает все предки div текущего узла контекста

ancestor-or-self::div собирает предки div текущего узла контекста и также, если сам узел контекста тоже является элементом div, включает в набор и его

descendant-or-self::para среди потомков узла контекста выбирает элементы para а также, если сам узел контекста является элементом para, то включает в набор и его

self::para выбирает текущий узел контекста, если это элемент para, либо в противном случае не выбирает ничего

child::chapter/descendant::para выбирает элементы para среди потомков элемента chapter, являющегося непосредственным потомком текущего узла контекста

child::*/child::para собирает все para, являющиеся потомками текущего узла контекста во втором поколении

/ находит корень документа (который всегда является родителем элемента документа)

/descendant::para в документе, которому принадлежит узел контекста, находит все элементы para

/descendant::olist/child::item находит все элементы item, которые имеют родителем olist и находятся в пределах документа, в котором располагается узел контекста

child::para[position()=1] находит первый непосредственный потомок para текущего узла контекста

child::para[position()=last()] находит последний непосредственный потомок para текущего узла контекста

child::para[position()=last()-1] находит предпоследний непосредственный потомок para текущего узла контекста

child::para[position()>1] среди непосредственных потомков текущего узла контекста собирает все para, за исключением первого

following-sibling::chapter[position()=1] находит следующий chapter, имеющий с узлом контекста общего родителя

preceding-sibling::chapter[position()=1] находит предыдущий chapter, имеющий с узлом контекста общего родителя

/descendant::figure[position()=42] находит в документе сорок второй элемент figure

/child::doc/child::chapter[position()=5]/child::section[position()=2] находит второй section в пятом chapter в элементе документа doc

child::para[attribute::type="warning"] находит все непосредственные потомки para текущего узла контекста, имеющие атрибут type со значением warning

child::para[attribute::type='warning'][position()=5] среди непосредственных потомков текущего узла контекста с названием para и имеющих атрибут type со значением warning находит пятый элемент

child::para[position()=5][attribute::type="warning"] среди непосредственных потомков para узла контекста выбирает пятый элемент, при условии что он имеет атрибут type со значением warning

child::chapter[child::title='Introduction'] среди непосредственных потомков chapter текущего узла контекста выбирает тот, у которого в свою очередь имеется один или несколько непосредственных потомков title со строковым значением Introduction

child::chapter[child::title] среди непосредственных потомков текущего узла контекста chapter находит тот, у которого имеется один или несколько непосредственных потомков title

child::*[self::chapter or self::appendix] среди непосредственных потомков текущего узла контекста находит chapter и appendix

child::*[self::chapter or self::appendix][position()=last()] из множества непосредственных потомков текущего узла контекста chapter и appendix выбирает последний





para находит элемент para, являющийся непосредственным потомком текущего узла контекста

* находит все элементы, являющиеся непосредственными потомками текущего узла контекста

text() находит все текстовые узлы, являющиеся непосредственными потомками текущего узла контекста

@name выделяет атрибут name в текущем узле контекста

@* находит все атрибуты текущего узла контекста

para[1] находит первый непосредственный потомок para текущего узла контекста

para[last()] находит последний непосредственный потомок para текущего узла контекста

*/para находит все потомки во втором поколении para текущего узла контекста

/doc/chapter[5]/section[2] в doc в пятом chapter находит второй section

chapter//para собирает элементы para, являющиеся потомками элемента chapter, который является непосредственным потомком текущего узла контекста

//para собирает все para, являющиеся потомками корневого узла документа, то есть находит все элементы para в том документе, где располагается текущий узел контекста

//olist/item в документе, где располагается текущий узел контекста, находит все элементы item, имеющие родителем olist

. выделяет текущий узел контекста

.//para собирает элементы para, являющиеся потомками текущего узла контекста

.. выделяет родителя текущего узла контекста

../@lang выделяет атрибут lang, принадлежащий родителю текущего узла контекста

para[@type="warning"] находит все непосредственные потомки para текущего узла контекста, имеющие атрибут type со значением warning

para[@type="warning"][5] находит пятый по счету из непосредственных потомков para текущего узла контекста, имеющих атрибут type со значением warning

para[5][@type="warning"] извлекает пятый непосредственный потомок para текущего узла контекста, если этот потомок имеет атрибут type со значением warning

chapter[title="Introduction"] получает непосредственный потомок текущего узла контекста chapter, который в свою очередь имеет один или несколько непосредственных потомков title со строковым значением, равным Introduction

chapter[title] находит непосредственный потомок chapter текущего узла контекста, который имеет один или несколько непосредственных потомков title

employee[@secretary and @assistant] находит все непосредственные потомки employee данного узла контекста, которые имеют оба атрибута secretary и assistant
-}
