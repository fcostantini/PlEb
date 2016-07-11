module Main where

import System.Environment
import System.IO
import System.Directory
import System.FilePath as F
import Control.Monad
import Data.Maybe
import Text.XML.Light

type Song = F.FilePath
type Playlist = [Song]

main :: IO ()
main = do
          getArgs >>= parseArgs

help, mhelp :: String
help = "Usage: PlEb [-h] [playlist]\nAvailable formats are: wpl."
mhelp = "add song_path: adds song to the playlist (if it exists).\n"++
         "check: checks if the playlist has wrong paths.\n"++
         "exit: terminates the program.\n"++
         "export: creates a folder with the songs on the playlist.\n"++
         "print: prints the content of the playlist.\n"++
         "rmv song_path: removes song from the playlist (if it exists)."
     
load :: Bool -> F.FilePath -> IO ()
load b file = do cont <- readFile file
                 pl <- parseWpl cont
                 menu b pl    

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = putStrLn help 
parseArgs [file] = load True file
parseArgs [] = putStrLn "Missing arguments"
parseArgs _ = putStrLn "Incorrect execution. Use -h for help"

parseWpl :: String -> IO Playlist
parseWpl file = let contents = parseXML file
                    media = concatMap (findElements $ cQName "media") (onlyElems contents)
                    songs = map (findAttr $ cQName "src") media
                    cQName n = QName n Nothing Nothing
                in return $ map fromJust songs

menu :: Bool -> Playlist -> IO ()
menu b pl = do when b (putStrLn "Playlist loaded.\nAvailable commands: add, check, exit, export, help, print, rmv. Use help for further information.")
               putStr ">"
               hFlush stdout
               getLine >>= ((parseCmd pl) . words)

parseCmd :: Playlist -> [String] -> IO ()
parseCmd pl ["add", fp] = putStrLn ("adding " ++ fp ++ " to playlist") >> menu False pl
parseCmd pl ["check"] = putStrLn "checking playlist" >> menu False pl
parseCmd pl ["exit"] = putStrLn "Goodbye!"
parseCmd pl ["export"] = export pl >> menu False pl
parseCmd pl ["help"] = putStrLn mhelp >> menu False pl
parseCmd pl ["print"] = mapM_ putStrLn pl >> menu False pl
parseCmd pl ["rmv", fp] = putStrLn ("removing " ++ fp ++ " from playlist") >> menu False pl
parseCmd pl _ = putStrLn "wrong command" >> menu False pl

copySong fp = copyFileWithMetadata fp ("asd"++song) >>
              putStrLn ("copied"++song)
              where song = takeFileName fp

export :: Playlist -> IO ()
export pl = do createDirectoryIfMissing False "asd"
               mapM_ (\a -> copySong a) pl
               putStrLn "Export complete!"
