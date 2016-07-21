module Wpl where

import Playlist
import System.IO
import System.Directory
import System.FilePath as F
import Control.Monad
import Data.Maybe
import Text.XML.Light

parseWpl :: String -> IO [Song]
parseWpl file = let cQName n = QName n Nothing Nothing
                    contents = parseXML file
                    media = concatMap (findElements $ cQName "media") (onlyElems contents)
                    songs = map (findAttr $ cQName "src") media
                in return $ map fromJust songs

wplPrelude :: String -> String
wplPrelude name = "<?xml version=\"1.0\"?>\n" ++
                  "<smil>\n" ++
                  "\t<head>\n" ++
                  "\t\t<title>" ++ name ++ "</title>\n" ++
                  "\t</head>\n" ++
                  "\t<body>\n" ++
                  "\t\t<seq>\n"

wplEpilogue :: String
wplEpilogue = "\t\t</seq>\n" ++ 
              "\t</body>\n" ++
              "</smil>"

writeSongWpl :: F.FilePath -> Song -> IO ()
writeSongWpl file song = let entry = "\t\t\t<media src=\""++song++"\"/>\n"
                         in appendFile file entry

writeWpl :: Playlist -> IO ()
writeWpl pl = let file = getPath pl
                  title = F.takeBaseName file
              in do writeFile file $ wplPrelude title
                    mapM_ (\a -> writeSongWpl file a) $ getSongs pl
                    appendFile file wplEpilogue
