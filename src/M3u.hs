module M3u where

import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.FilePath as F
import Playlist

data M3uLine = Header | Info String | Path F.FilePath deriving Show
type M3uFile = [M3uLine]

eol :: Parser ()
eol = oneOf "\n\r" >> return ()

comment :: Parser ()
comment = do char '#'
             skipMany (noneOf "\r\n")

header :: Parser M3uLine
header = do string "#EXTM3U"
            eol
            return Header

info :: Parser M3uLine
info = do string "#EXTINF"
          i <- many (noneOf "\n\r")
          return $ Info i

path :: Parser M3uLine
path = do p <- many (noneOf "\n\r") --we parse anything
          return $ Path p

line :: Parser (Maybe M3uLine)
line = do skipMany space
          try (header >>= return . Just) <|> try (info >>= return . Just) <|> try (comment >> return Nothing) <|> (path >>= return . Just)

file :: Parser M3uFile
file = do lines <- sepBy line (oneOf "\n\r")
          eof
          return (catMaybes lines)

readM3u :: String -> IO M3uFile
readM3u f = let parsed = parse file "" f in
            case parsed of
              Left e -> putStrLn "Error parsing m3u" >> return []
              Right m3u -> return m3u

m3uToSong :: M3uFile -> [Song] --map
m3uToSong f = filter (not . null) $ (map lineToSong f)

lineToSong :: M3uLine -> Song
lineToSong Header = []
lineToSong (Info i) = []
lineToSong (Path f) = f

parseM3u :: F.FilePath -> IO [Song]
parseM3u f = readM3u f >>= (return . m3uToSong)                
