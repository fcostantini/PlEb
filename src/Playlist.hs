module Playlist where

import Data.List
import System.FilePath as F

type Song = F.FilePath

data Ext = M3u | Pls | Wpl | Xspf | Other deriving Eq

--A playlist is basically its location and a list of its songs
data Playlist = Pl {getPath :: F.FilePath,
                    getSongs :: [Song]} deriving Show

addS :: Playlist -> Song -> Playlist
addS pl s = pl {getSongs = getSongs pl ++ [s]}

rmS :: Playlist -> Song -> Playlist
rmS pl s = pl {getSongs = filter (/=s) (getSongs pl)}

getExt :: F.FilePath -> Ext
getExt fp = case F.takeExtension fp of
              ".m3u"  -> M3u
              ".m3u8" -> M3u
              ".pls"  -> Pls
              ".wpl"  -> Wpl
              ".xspf" -> Xspf
              _       -> Other
