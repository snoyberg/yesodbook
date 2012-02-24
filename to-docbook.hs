{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}
import Prelude hiding (writeFile)
import Dita.Parse
import qualified Dita.Types as D
import qualified Dita.Util as DU
import Data.DTD.Cache
import Network.URI.Conduit
import Network.URI.Conduit.File
import Text.XML
import qualified Text.XML as X
import Text.Hamlet.XML
import Text.XML.Cursor.Generic
import qualified Data.Text as T

catalogFile :: FilePath
catalogFile = "catalog-dita.xml"

ditamap :: FilePath
ditamap = "book/yesod-web-framework-book.ditamap"

getSettings :: IO (DitaSettings IO)
getSettings = do
    cache <- newDTDCacheFile catalogFile
    let sm = toSchemeMap [fileScheme]
    return DitaSettings
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
    doc <- runDita_ ds $ loadDoc dm
    let doctype = Doctype "book" $ Just $ PublicID
                    "-//OASIS//DTD DocBook XML V5.0//EN"
                    "http://www.docbook.org/xml/5.0/dtd/docbook.dtd"
    writeFile def
        { rsPretty = False
        } "yesod.xml" $ Document
        (Prologue [] (Just doctype) [])
        (docToElem doc)
        []

docToElem :: D.Doc -> X.Element
docToElem doc = Element "book" [] $ addPreface $ concatMap (fixDoubleParas False) $ map (toChapters 0) [xml|
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
^{concatMap (render t) $ D.topicContent t}
|]

render :: D.Topic -> D.Element -> [X.Node]
render topic e =
    case D.elementName e of
        "conbody" -> kids
        "p" -> to "para"
        "ul" -> to "itemizedlist"
        "li" -> toPara "listitem"
        "apiname" ->
            case D.elementChildren e of
                [D.NodeContent t] -> [xml|<function>#{snd $ T.breakOnEnd ":" t}|]
                _ -> to "function"
        "xref"
            | Just url <- DU.getAttrText "href" e -> [xml|
<link xlink:href=#{url}>^{kids}
|]
            | Just (D.Href { D.hrefElem = D.HrefElemElem (D.TopicId tid) (D.ElemId eid) }) <- DU.getAttrHref "href" e, Just "xref" <- DU.getAttrText "outputclass" e -> [xml|
<xref linkend=#{tid}-#{eid}
|]
            | otherwise -> kids
        "codeph" -> to "literal"
        "q" -> to "quote"
        "i" -> to "emphasis"
        "b" -> [NodeElement $ Element "emphasis"
                    (("role", "bold") : attrs) kids]
        "term" -> to "glossterm"
        "note" -> toPara "note"
        "lq" -> toPara "blockquote"
        "codeblock" ->
            case D.elementChildren e of
                [D.NodeContent t] ->
                    case DU.getAttrText "outputclass" e of
                        Just "haskell" -> [xml|<programlisting language=haskell>#{removeStartStop t}|]
                        _ -> [xml|<programlisting >#{removeStartStop t}|]
                _ -> to "programlisting"
        "msgblock" ->
            case D.elementChildren e of
                [D.NodeContent t] -> [xml|<programlisting>#{removeStartStop t}|]
                _ -> to "msgblock"
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
        "fig" -> to "figure"
        "title" -> to "title"
        "dd" -> toPara "listitem"
        "cmdname" -> toPara "command"
        "image" ->
            case DU.getAttrHref "href" e of
                Just (D.Href uri _) -> [xml|
<mediaobject>
    <imageobject>
        <imagedata fileref=images/#{T.drop (T.length "/home/snoyman/haskell/book/book/yesod-web-framework-book/") $ uriPath uri}>
|]
                Nothing -> error $ "image missing href: " ++ show e
        "simpletable" -> simpletable topic e
        name -> [xml|
<para>FIXME: Unknown element: #{nameLocalName name}
|]
  where
    kids = concatMap (renderN topic) $ D.elementChildren e
    to name = [NodeElement $ Element name attrs kids]
    toPara name =
        [NodeElement $ Element name attrs kids']
      where
        kids' =
            case D.elementChildren e of
                (D.NodeElement (D.Element "p" _ _ _):_) -> kids
                _ -> [xml|<para>^{kids}|]
    attrs =
        case (D.elementName e, DU.getAttrText "id" e) of
            ("fig", Just id') -> [("xml:id", T.concat [D.unTopicId $ D.topicId topic, "-", id'])]
            _ -> [] -- FIXME

simpletable topic e = [xml|
<table>
    <title>
    <tgroup cols=#{count}>
        $forall sthead <- stheads
            <thead>^{row sthead}
            <tbody>
                $forall strow <- strows
                    ^{row strow}
|]
  where
    c = fromNode $ D.NodeElement e
    stheads = c $/ element "sthead"
    strows = c $/ element "strow"
    firstRow =
        case strows of
            [] -> error "simpletable with no strow"
            a:_ -> a
    count = T.pack $ show $ length $ firstRow $/ element "stentry"

    row x = return $ NodeElement $ Element "row" [] $ concatMap entry $ x $/ element "stentry"

    entry x = [xml|
<entry>^{concatMap (renderN topic) $ D.elementChildren e}
|]
      where
        D.NodeElement e = node x

removeStartStop t
    | any (== "-- START") ls = T.unlines $ go False ls
    | otherwise = t
  where
    ls = T.lines t
    go _ [] = []
    go _ ("-- START":rest) = go True rest
    go _ ("-- STOP":rest) = go False rest
    go True (x:xs) = x : go True xs
    go False (_:xs) = go False xs

text :: Cursor D.Node -> [T.Text]
text c =
    case node c of
        D.NodeContent t -> [t]
        _ -> []

element :: Name -> Cursor D.Node -> [Cursor D.Node]
element name c =
    case node c of
        D.NodeElement e
            | D.elementName e == name -> [c]
        _ -> []

fromNode :: D.Node -> Cursor D.Node
fromNode =
    toCursor children
  where
    children (D.NodeElement e) = D.elementChildren e
    children _ = []

renderN :: D.Topic -> D.Node -> [X.Node]
renderN topic (D.NodeElement e) = render topic e
renderN _ (D.NodeContent t) = [NodeContent t]
renderN _ (D.NodeComment t) = [NodeComment t]
renderN _ (D.NodeInstruction t) = [NodeInstruction t]

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

addPreface :: [Node] -> [Node]
addPreface (t1:(NodeElement (Element a b (t2:rest2))):rest1) =
    t1 : (NodeElement (Element a b (t2:helper:rest2))) : rest1
  where
    helper = NodeElement $ Element "{http://www.w3.org/2001/XInclude}include" [("href", "ch00.xml")] []

noPara :: Node -> Bool
noPara (NodeElement (Element "para" _ _)) = False
noPara _ = True
