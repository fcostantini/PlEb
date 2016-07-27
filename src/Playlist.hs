module Playlist where

import Data.List
import System.FilePath as F

type Song = F.FilePath
data Ext = M3u | Pls | Wpl | Xspf | Other deriving Eq
data Playlist = Pl {getPath :: F.FilePath,
                    getSongs :: [Song]} deriving Show

addP :: Playlist -> Song -> Playlist
addP pl s = pl {getSongs = (getSongs pl) ++ [s]}

--removes all occurrences of the song
rmP :: Playlist -> Song -> Playlist
rmP pl s = pl {getSongs = filter (/=s) (getSongs pl)}

getExt :: F.FilePath -> Ext
getExt fp = case F.takeExtension fp of
              ".m3u"  -> M3u
              ".m3u8" -> M3u
              ".pls"  -> Pls
              ".wpl"  -> Wpl
              ".xspf" -> Xspf
              _       -> Other
