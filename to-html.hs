{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}
import Prelude hiding (writeFile)
import Dita.Parse
import qualified Dita.Types as D
import qualified Dita.Util as DU
import Dita.Util.ClassMap
import Data.DTD.Cache
import Network.URI.Conduit
import Network.URI.Conduit.File
import Text.XML
import qualified Text.XML as X
import Text.Hamlet.XML
import Text.XML.Cursor.Generic
import qualified Data.Text as T
import Dita.Output.HTML

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
    out <- decodeString "html/"
    classmapFile <- decodeString "classmap.css"
    classmap <- loadClassMap (toSchemeMap [fileScheme]) classmapFile
    runDita_ ds $ do
        doc <- loadDoc dm
        writeHtmlDocs out $ docToHtmlDocs def
            { hsClassMap = classmap
            , hsIncludeUI = False
            } doc
