module Main where

import System.Environment
import System.IO
import System.Directory
import System.FilePath as F
import Control.Monad
import Data.Maybe
import Text.XML.Light
import Playlist
import Wpl

main :: IO ()
main = getArgs >>= parseArgs

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
                 songs <- parseWpl cont
                 let fname = F.takeBaseName file
                 menu b (Pl fname songs)    

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = putStrLn help 
parseArgs [file] = load True file
parseArgs [] = putStrLn "Missing arguments"
parseArgs _ = putStrLn "Incorrect execution. Use -h for help"

menu :: Bool -> Playlist -> IO ()
menu b pl = do when b (putStrLn "Playlist loaded.\nAvailable commands: add, check, exit, export, help, print, rmv. Use help for further information.")
               putStr ">"
               hFlush stdout
               getLine >>= ((parseCmd pl) . words)

parseCmd :: Playlist -> [String] -> IO ()
parseCmd pl ["add", fp] = do putStrLn ("adding " ++ fp ++ " to playlist")
                             let newpl = addP pl fp
                             prettyWpl newpl
                             menu False newpl
parseCmd pl ["check"] = putStrLn "checking playlist" >>
                        menu False pl
parseCmd pl ["exit"] = putStrLn "Goodbye!"
parseCmd pl ["export"] = export pl >>
                         menu False pl
parseCmd pl ["help"] = putStrLn mhelp >>
                       menu False pl
parseCmd pl ["print"] = putStrLn ("Playlist " ++ (getTitle pl)) >>
                        mapM_ putStrLn (getSongs pl) >>
                        menu False pl
parseCmd pl ["rmv", fp] = putStrLn ("removing " ++ fp ++ " from playlist") >>
                          menu False pl
parseCmd pl _ = putStrLn "wrong command" >>
                menu False pl

copySong name fp = copyFileWithMetadata fp (name++song) >>
                   putStrLn ("copied"++song)
                   where song = takeFileName fp

export :: Playlist -> IO ()
export pl = do let pname = getTitle pl
               createDirectoryIfMissing False pname
               mapM_ (\a -> copySong pname a) $ getSongs pl
               putStrLn "Export complete!"
