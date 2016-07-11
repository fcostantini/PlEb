module Main where

import System.Environment
import Data.Maybe
import Text.XML.Light

type Song = FilePath
type Playlist = [Song]

main :: IO ()
main = do
          getArgs >>= parseArgs

help :: String
help = "Usage: PlEb [-h] [playlist] [song]\nAvailable formats are: wpl"

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = putStrLn help 
parseArgs [file] = do cont <- readFile file
                      pl <- parseWpl cont
                      menu pl
parseArgs [] = putStrLn "Missing arguments"
parseArgs _ = putStrLn "Incorrect execution. Use -h for help"

parseWpl :: String -> IO Playlist
parseWpl file = let contents = parseXML file
                    media = concatMap (findElements $ cQName "media") (onlyElems contents)
                    songs = map (findAttr $ cQName "src") media
                    cQName n = QName n Nothing Nothing
                in return $ map fromJust songs

menu :: Playlist -> IO ()
menu pl = getLine >>= (parseCmd . words)

parseCmd :: [String] -> IO ()
parseCmd ["add", fp] = putStrLn ("adding " ++ fp ++ " to playlist")
parseCmd ["rm", fp] = putStrLn ("removing " ++ fp ++ " from playlist")
parseCmd ["check"] = putStrLn "checking playlist"
parseCmd ["export"] = putStrLn "exporting playlist"
parseCmd _ = putStrLn "wrong command"
