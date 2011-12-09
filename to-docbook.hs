{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}
import Prelude hiding (writeFile)
import DITA.Parse
import qualified DITA.Types as D
import qualified DITA.Util as DU
import Data.DTD.Cache
import Network.URI.Enumerator
import Network.URI.Enumerator.File
import Text.XML
import qualified Text.XML as X
import Text.Hamlet.XML

catalogFile :: FilePath
catalogFile = "catalog-dita.xml"

ditamap :: FilePath
ditamap = "book/yesod-web-framework-book.ditamap"

getSettings :: IO (DITASettings IO)
getSettings = do
    cache <- newDTDCacheFile catalogFile
    let sm = toSchemeMap [fileScheme]
    return DITASettings
        { dsDTDCache = cache
        , dsSchemeMap = sm
        , dsGetFileId = Nothing
        , dsStrict = True
        , dsDitaval = def
        , dsIsPrint = True
        }

main :: IO ()
main = do
    ds <- getSettings
    dm <- decodeString ditamap
    doc <- runDITA_ ds $ loadDoc dm
    let doctype = Doctype "book" $ Just $ PublicID
                    "-//OASIS//DTD DocBook XML V4.2//EN"
                    "http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd"
    writeFile def
        { rsPretty = False
        } "yesod.xml" $ Document
        (Prologue [] (Just doctype) [])
        (docToElem doc)
        []

docToElem :: D.Doc -> X.Element
docToElem doc = Element "book" [] $ concatMap (fixDoubleParas False) $ map (toChapters 0) [xml|
<title>#{D.docTitle doc}
^{concatMap navToSection $ D.docNavs doc}
|]

navToSection :: D.Nav -> [X.Node]
navToSection nav = [xml|
<section>
    <title>#{D.navTitle nav}
    <para>
    $maybe tt <- D.navTopicTree nav
        ^{ttToSection False tt}
    ^{concatMap navToSection $ D.navChildren nav}
|]

ttToSection :: Bool -> D.TopicTree -> [X.Node]
ttToSection wrap tt = [xml|
$if wrap
    <section>
        ^{renderTopic True $ D.ttTopic tt}
        ^{inside}
$else
    ^{renderTopic False $ D.ttTopic tt}
    ^{inside}
|]
  where
    inside = concatMap (ttToSection True) $ D.ttChildren tt

renderTopic :: Bool -> D.Topic -> [X.Node]
renderTopic title t = [xml|
$if title
    <title>#{DU.text $ D.topicTitle t}
<para>
^{concatMap render $ D.topicContent t}
|]

render :: D.Element -> [X.Node]
render e =
    case D.elementName e of
        "conbody" -> kids
        "p" -> to "para"
        "ul" -> to "itemizedlist"
        "li" -> toPara "listitem"
        "apiname" -> to "function" -- FIXME
        "xref"
            | Just url <- DU.getAttrText "href" e -> [xml|
<ulink url=#{url}>^{kids}
|]
            | otherwise -> kids -- FIXME
        "codeph" -> to "literal"
        "q" -> to "quote"
        "i" -> to "emphasis"
        "b" -> [NodeElement $ Element "emphasis"
                    (("role", "bold") : attrs) kids]
        "term" -> to "glossterm"
        "note" -> toPara "note"
        "lq" -> toPara "blockquote"
        "codeblock" -> to "programlisting"
        "varname" -> to "varname"
        "filepath" -> to "filename"
        "ol" -> to "orderedlist"
        "cite" ->
            [ NodeElement $ Element "para" []
                [ NodeElement $ Element "citation" attrs kids
                ]
            ]
        "dl" -> to "variablelist"
        "dlentry" -> to "varlistentry"
        "dt" -> to "term"
        "userinput" -> to "userinput"
        "fig" -> [xml|<para>FIXME figures not implemented|] -- FIXME to "figure"
        "title" -> to "title"
        "dd" -> toPara "listitem"
        "image" -> [] -- FIXME
        name -> [xml|
<para>FIXME: Unknown element: #{nameLocalName name}
|]
  where
    kids = concatMap renderN $ D.elementChildren e
    to name = [NodeElement $ Element name attrs kids]
    toPara name =
        [NodeElement $ Element name attrs kids']
      where
        kids' =
            case D.elementChildren e of
                (D.NodeElement (D.Element "p" _ _ _):_) -> kids
                _ -> [xml|<para>^{kids}|]
    attrs = [] -- FIXME

renderN :: D.Node -> [X.Node]
renderN (D.NodeElement e) = render e
renderN (D.NodeContent t) = [NodeContent t]
renderN (D.NodeComment t) = [NodeComment t]
renderN (D.NodeInstruction t) = [NodeInstruction t]

fixDoubleParas :: Bool -> Node -> [Node]
fixDoubleParas False (NodeElement (Element "para" attrs inside)) =
    [NodeElement $ Element "para" attrs $ concatMap (fixDoubleParas True) inside]
fixDoubleParas True (NodeElement (Element "para" attrs inside)) = inside
fixDoubleParas _ (NodeElement (Element name attrs inside))
    | name `elem` ["note", "listitem", "blockquote"] =
        [NodeElement $ Element name attrs $ concatMap (fixDoubleParas False) inside]
fixDoubleParas _ (NodeElement (Element "section" b c)) =
    if length c < 2
        then [NodeElement $ Element "section" b $ c ++ [xml|<para>|]]
        else [NodeElement $ Element "section" b $ concatMap (fixDoubleParas False) c]
fixDoubleParas x (NodeElement (Element a b c)) =
    [NodeElement $ Element a b $ concatMap (fixDoubleParas x) c]
fixDoubleParas _ n = [n]

toChapters :: Int -> Node -> Node
toChapters 0 (NodeElement (Element "section" a b)) =
    (NodeElement (Element "part" a $ map (toChapters 1) $ filter noPara b))
toChapters 1 (NodeElement (Element "section" a b)) =
    (NodeElement (Element "chapter" a b))
toChapters i (NodeElement (Element a b c)) = NodeElement $ Element a b $ map (toChapters i) c
toChapters _ n = n

noPara :: Node -> Bool
noPara (NodeElement (Element "para" _ _)) = False
noPara _ = True
