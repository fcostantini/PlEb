module Wpl where

import Playlist
import System.IO
import System.Directory
import System.FilePath as F
import Control.Monad
import Data.Maybe
import Text.XML.Light

parseWpl :: String -> IO [Song]
parseWpl file = let contents = parseXML file
                    media = concatMap (findElements $ cQName "media") (onlyElems contents)
                    songs = map (findAttr $ cQName "src") media
                    cQName n = QName n Nothing Nothing
                in return $ map fromJust songs

wplPrelude name = "<?wpl version=\"1.0\"?>\n" ++
                  "<smil>\n" ++
                  "\t<head>\n" ++
                  "\t\t<title>" ++ name ++ "</title>\n" ++
                  "\t</head>\n" ++
                  "\t<body>\n" ++
                  "\t\t<seq>\n"

wplEpilogue = "\t\t</seq>\n" ++ 
              "\t</body>\n" ++
              "</smil>"

writeSongWpl file song = let entry = "\t\t\t<media src=\""++song++"\"/>\n" in
                             appendFile file entry

prettyWpl :: Playlist -> IO ()
prettyWpl pl = let newfile = getTitle pl ++ "mod.wpl" in
                   do writeFile newfile $ wplPrelude (getTitle pl)
                      mapM (\a -> writeSongWpl newfile a) $ getSongs pl
                      appendFile newfile wplEpilogue
