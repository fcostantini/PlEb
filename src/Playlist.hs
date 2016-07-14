module Playlist where

import System.FilePath as F

type Title = String
type Song = F.FilePath
data Playlist = Pl {getTitle :: Title,
                    getSongs :: [Song]}

addP :: Playlist -> Song -> Playlist
addP pl s = pl {getSongs = (getSongs pl) ++ [s]}
