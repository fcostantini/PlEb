module Playlist where

import System.FilePath as F
import Data.List

type Title = String
type Song = F.FilePath
data Playlist = Pl {getPath :: F.FilePath,
                    getSongs :: [Song]}

addP :: Playlist -> Song -> Playlist
addP pl s = pl {getSongs = (getSongs pl) ++ [s]}

--removes all occurrences of the song
rmP:: Playlist -> Song -> Playlist
rmP pl s = pl {getSongs = filter (/=s) (getSongs pl)}
