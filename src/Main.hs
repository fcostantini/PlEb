module Main where

import System.Environment
import System.IO
import System.IO.Strict as STR
import System.Directory
import System.FilePath as F
import Control.Monad
import Data.Maybe
import Data.List
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
load b file = do cont <- STR.readFile file
                 songs <- parseWpl cont
                 menu b (Pl file songs)    

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
parseCmd pl ("add":fp) = do let gfp = intercalate " " fp
                            putStrLn ("Adding " ++ gfp ++ " to playlist...")
                            let newpl = addP pl gfp
                            prettyWpl newpl
                            load False (getPath newpl)
parseCmd pl ["check"] = putStrLn "Checking playlist..." >>
                        check pl >>
                        menu False pl
parseCmd pl ["exit"] = putStrLn "Goodbye!"
parseCmd pl ["export"] = export pl >>
                         menu False pl
parseCmd pl ["help"] = putStrLn mhelp >>
                       menu False pl
parseCmd pl ["print"] = let pname = F.takeBaseName (getPath pl) in
                          putStrLn ("Playlist " ++ pname ++ "\n") >>
                          mapM_ putStrLn (getSongs pl) >>
                          menu False pl
parseCmd pl ("rmv":fp) = do let gfp = intercalate " " fp
                            putStrLn ("Removing " ++ gfp ++ " from playlist...")
                            let newpl = rmP pl gfp
                            prettyWpl newpl
                            load False (getPath newpl)
parseCmd pl _ = putStrLn "Wrong command." >>
                menu False pl

copySong :: Title -> Song -> IO ()
copySong name fp = copyFileWithMetadata fp (name++song) >>
                   putStrLn ("copied"++song)
                   where song = takeFileName fp

export :: Playlist -> IO ()
export pl = do let pname = F.takeBaseName (getPath pl)
               createDirectoryIfMissing False pname
               mapM_ (\a -> copySong pname a) $ getSongs pl
               putStrLn "Export complete!"

--TODO: find file with System.FilePath.Find

checkSong :: Song -> IO ()
checkSong s = do b <- doesFileExist s
                 case b of
                    True -> putStrLn ("Ok " ++ F.takeBaseName s)
                    False -> putStrLn (s ++ " does NOT exist in the file system!!!\nIt's recommended to remove it.")

check :: Playlist -> IO ()
check pl = do mapM_ checkSong $ getSongs pl
              putStrLn ("Checking complete!")
