module Playlist where

import Control.Monad.Trans.State
import Data.List
import System.FilePath as F

type Song = F.FilePath

data Ext = M3u | Pls | Wpl | Xspf | Other deriving Eq

--A playlist is basically its location and a list of its songs
data Playlist = Pl {getPath :: F.FilePath,
                    getSongs :: [Song]} deriving (Eq, Show)

--Current playlist will be the state
type PlState = StateT Playlist IO

addS :: Song -> PlState ()
addS s = do pl <- get
            let songs = getSongs pl
            put $ pl {getSongs = songs ++ [s]}

rmS :: Song -> PlState ()
rmS s = do pl <- get
           let songs = getSongs pl
           put $ pl {getSongs = filter (/=s) songs}

getExt :: F.FilePath -> Ext
getExt fp = case filter (/= ' ') (F.takeExtension fp) of
              ".m3u"  -> M3u
              ".m3u8" -> M3u
              ".pls"  -> Pls
              ".wpl"  -> Wpl
              ".xspf" -> Xspf
              _       -> Other
