module Pls where

import Data.Maybe
import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.FilePath as F
import Playlist

data PlsLine = Header | Title String | Length Int | File F.FilePath | Entries Int | Version Int deriving Show
type PlsFile = [PlsLine]

comment :: Parser ()
comment = do char '#'
             skipMany (noneOf "\r\n")

header :: Parser PlsLine
header = do string "[playlist]"
            skipMany (noneOf "\r\n")
            return Header

title :: Parser PlsLine
title = do string "Title"
           many1 digit
           char '='
           i <- many (noneOf "\n\r")
           return $ Title i

len :: Parser PlsLine
len = do string "Length"
         many1 digit
         char '='
         i <- many1 digit
         skipMany (noneOf "\n\r")
         return $ Length (read i :: Int)

entries :: Parser PlsLine
entries = do string "NumberOfEntries="
             i <- many1 digit
             skipMany (noneOf "\n\r")
             return $ Entries (read i :: Int)

path :: Parser PlsLine
path = do string "File"
          many1 digit
          char '='
          i <- many (noneOf "\n\r")
          return $ File i

version :: Parser PlsLine
version = do string "Version="
             i <- digit
             skipMany (noneOf "\n\r")
             return $ Version (digitToInt i)

line :: Parser (Maybe PlsLine)
line = do skipMany space
          choice $ [try (header >>= return . Just), try (path >>= return . Just), try (title >>= return . Just)] ++
                   [try (version >>= return . Just), try (len >>= return . Just), try (entries >>= return . Just)] ++
                   [(comment >> return Nothing)]

file :: Parser PlsFile
file = do lines <- sepBy line endOfLine
          eof
          return (catMaybes lines)

readPls :: String -> IO PlsFile
readPls f = let parsed = parse file "" f
            in case parsed of
                 Left e -> putStrLn "Error parsing pls." >> return []
                 Right pls -> return pls

plsToSong :: PlsFile -> [Song]
plsToSong f = filter (not . null) $ (map lineToSong f)

lineToSong :: PlsLine -> Song
lineToSong (File f) = f
lineToSong _ = []

parsePls :: String -> IO [Song]
parsePls f = readPls f >>= (return . plsToSong)

plsPrelude ::  String
plsPrelude = "[playlist]\n"

plsEpilogue :: Int -> String
plsEpilogue n = "\nNumberOfEntries="++(show n)++"\nVersion=2"

writeSongsPls :: F.FilePath -> [Song] -> Int -> IO ()
writeSongsPls _ [] _ = return ()
writeSongsPls file (x:xs) n = let entry = "\nFile"++(show n)++"="++x++"\n"
                              in do appendFile file entry
                                    writeSongsPls file xs (n+1)

writePls :: Playlist -> IO ()
writePls pl = let file = getPath pl
                  songs = getSongs pl
                  plen = length songs
              in do writeFile file plsPrelude
                    writeSongsPls file songs 1
                    appendFile file $ plsEpilogue plen
