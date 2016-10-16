module Xspf where

import Control.Monad
import Data.Maybe
import Network.URI
import System.Directory
import System.FilePath as F
import System.IO
import Text.XML.Light

import Playlist

uriP = "file:///"
uriC = length uriP

normalizePath :: String -> String
normalizePath = map (\x -> if x == '/' then '\\' else x)

unNormalizePath :: String -> String
unNormalizePath = map (\x -> if x == '\\' then '/' else x)

fromURI :: String -> String
fromURI s = let u = unEscapeString s
            in normalizePath (drop uriC u)

toURI :: String -> String
toURI s = let ns = uriP ++ unNormalizePath s
          in escapeURIString isUnescapedInURI ns

parseXspf :: String -> IO [Song]
parseXspf file = let cQName n = QName n (Just "http://xspf.org/ns/0/") Nothing
                     contents = parseXML file
                     location = concatMap (findElements $ cQName "location") (onlyElems contents)
                     cdata = map (onlyText . elContent) location
                     songs = map cdData (concat cdata)
                     nsongs = map fromURI songs
                 in return nsongs

xspfPrelude :: String -> String
xspfPrelude name = "<?wpl version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
                   "<playlist xmlns=\"http://xspf.org/ns/0/\" xmlns:vlc=\"http://www.videolan.org/vlc/playlist/ns/0/\" version=\"1\">\n" ++
                   "<title>" ++ name ++ "</title>\n" ++
                   "<trackList>\n"

xspfEpilogue :: String
xspfEpilogue = "</trackList>\n" ++
               "</playlist>\n"

writeSongXspf :: F.FilePath -> Song -> IO ()
writeSongXspf file song = let entry = "<track>\n<location>" ++ toURI song ++ "</location>\n</track>\n"
                          in appendFile file entry

writeXspf :: Playlist -> IO ()
writeXspf pl = let file = getPath pl
                   title = F.takeBaseName file
               in do writeFile file $ xspfPrelude title
                     mapM_ (writeSongXspf file) $ getSongs pl
                     appendFile file xspfEpilogue
