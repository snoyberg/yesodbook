{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Dita.Parse
import Network.URI.Conduit
import Network.URI.Conduit.File
import System.Environment (getArgs)
import Data.DTD.Cache (newDTDCacheFile)
import Data.Default (def)
import Dita.Output.XML
import Dita.Util.Render
import qualified Data.Map as Map
import Text.XML
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Text.Hamlet.XML (xml)
import Dita.Util.ClassMap
import Control.Monad.Trans.Reader
import qualified Dita.Types

main :: IO ()
main = do
    [catalogFile, classmapFile, ditamapFile, outputFile] <- getArgs
    dtdCache <- newDTDCacheFile catalogFile
    ditamapUri <- decodeString ditamapFile
    outputUri <- decodeString $ outputFile ++ "/"
    classmapUri <- decodeString classmapFile
    classmap <- loadClassMap (toSchemeMap [fileScheme]) classmapUri
    let ds = DitaSettings
            { dsDTDCache = dtdCache
            , dsSchemeMap = toSchemeMap [fileScheme]
            , dsGetFileId = Nothing
            , dsStrict = False
            , dsDitaval = def
            , dsIsPrint = False
            }
    runDita_ ds $ do
        doc <- loadDoc ditamapUri
        writeXmlOutput outputUri $ (roXmlToHtml classmap) $ docToXmlOutput def doc

roXmlToHtml :: ClassMap -> RenderOutput -> RenderOutput
roXmlToHtml cm ro = ro { roDocs = Map.map (xmlToHtml cm) $ roDocs ro }

xmlToHtml :: ClassMap -> Document -> Document
xmlToHtml classmap d@(Document a e@(Element _ _ cs0) b)
    | hasClass "topic/topic" e = Document a (Element "html" [] cs) b
    | otherwise = d
  where
    cs = tags ++ concatMap getBody cs0
    tags = filter isTag cs0
    isTag (NodeElement (Element "bundle-tag" _ _)) = True
    isTag _ = False
    getBody (NodeElement e@(Element _ as cs))
        | hasClass "topic/body" e = runReader (fmap concat $ mapM goNode cs) classmap
        | hasClass "topic/topic" e = concatMap getBody cs
        | otherwise = []
    getBody _ = []

goNode :: Node -> Reader ClassMap [Node]
goNode (NodeContent t) = return [NodeContent t]
goNode (NodeElement e) = goElem e
goNode _ = return []

elementClasses :: Element -> [Class]
elementClasses (Element _ as _) =
    case lookup "class" as of
        Nothing -> []
        Just t -> drop 1 $ T.words t

goElem :: Element -> Reader ClassMap [Node]
goElem e = goElem' e $ elementClasses e

type Class = T.Text

goElem' :: Element -> [Class] -> Reader ClassMap [Node]
goElem' e [] = do
    ns <- mapM goNode $ elementNodes e
    return [xml|
<h1 style=color:red>Unknown classes: #{T.pack $ show $ elementClasses e}
<div>
    ^{concat ns}
|]
goElem' (Element _ as' cs) ("topic/image":_) = do
    cs' <- concatMapM goNode cs
    return [NodeElement $ Element "img" as cs']
  where
    as = concatMap go as'
    go ("src", _) = []
    go ("href", v) = [("src", v)]
    go ("class", _) = []
    go x = [x]
goElem' (Element _ as' cs) ("topic/xref":_) = do
    cs' <- concatMapM goNode cs
    return [NodeElement $ Element "a" as cs']
  where
    as = filter go as'
    go (k, _) = k `elem` ["id", "href"]
goElem' e ("topic/table":_) = do
    nottitle <- concatMapM goNode $ filter notTitle $ elementNodes e
    return $ addId' e [xml|
<table>
    $forall title <- filter (hasClass "topic/title") $ elements $ elementNodes e
        <caption>#{text title}
    ^{nottitle}
|]
  where
    notTitle (NodeElement e') = not $ hasClass "topic/title" e'
    notTitle _ = True
goElem' e ("topic/fig":_) = do
    nottitle <- concatMapM goNode $ filter notTitle $ elementNodes e
    return $ addId' e [xml|
<figure>
    $forall title <- filter (hasClass "topic/title") $ elements $ elementNodes e
        <figcaption>
            \#{text title}
    ^{nottitle}
|]
  where
    notTitle (NodeElement e') = not $ hasClass "topic/title" e'
    notTitle _ = True
goElem' e ("topic/tm":_) = do
    x <- concatMapM goNode (elementNodes e)
    return $ x ++ addId' e [xml|
<span class=tm>#{content}
|]
  where
    content =
        case lookup "tmtype" $ elementAttributes e of
            Just "tm" -> "\x2122"
            Just "reg" -> "\174"
            Just "service" -> "SM"
            _ -> ""
goElem' e (c:cs) = do
    classmap <- ask
    case Map.lookup (Dita.Types.Class c) classmap of
        Just (Converted name mclazz) -> do
            x <- concatMapM goNode $ elementNodes e
            return
                [ NodeElement $ Element (Name name Nothing Nothing)
                    (addId e $ case mclazz of
                        Nothing -> []
                        Just clazz -> [("class", clazz)]) x
                ]
        Just Stripped -> concatMapM goNode $ elementNodes e
        Just Deleted -> return []
        Nothing -> goElem' e cs

addId' :: Element -> [Node] -> [Node]
addId' e (NodeElement (Element n as cs):ns) =
    NodeElement (Element n (addId e as) cs) : ns
addId' _ x = x

addId :: Element -> [(Name, T.Text)] -> [(Name, T.Text)]
addId e others =
    case lookup "id" $ elementAttributes e of
        Nothing -> others
        Just x -> ("id", x) : others

hasClass :: Class -> Element -> Bool
hasClass c e = c `elem` elementClasses e

text :: Element -> T.Text
text (Element _ _ cs) =
    T.concat $ map go cs
  where
    go (NodeContent t) = t
    go (NodeElement e) = text e
    go _ = T.empty

elements :: [Node] -> [Element]
elements [] = []
elements (NodeElement e:ns) = e : elements ns
elements (_:ns) = elements ns

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f
