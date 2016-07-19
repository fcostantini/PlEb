module Main where

import System.Environment
import System.IO
import System.IO.Strict as STR
import System.Directory
import System.FilePath as F
import System.Exit
import Control.Monad
import Control.Exception
import Data.Maybe
import Data.List
import Text.XML.Light

import HandleE
import Playlist
import Wpl

main :: IO ()
main = getArgs >>= parseArgs

help, mhelp :: String
help = "Usage: PlEb [-h] [playlist]\nAvailable formats are: wpl."
mhelp = "add song_path: adds song to the playlist (if it exists).\n"++
        "check: checks if the playlist has inexistent files.\n"++
        "exit/quit: terminates the program.\n"++
        "export: creates a folder with the songs on the playlist.\n"++
        "load pl: loads a playlist.\n"++
        "print: prints the content of the playlist.\n"++
        "rmv song_path: removes song from the playlist (if it exists)."
     
load :: Bool -> F.FilePath -> IO ()
load b file = let ext = getExt file in
                 if ext == Other then putStrLn ("Unsupported format.")
                 else do trycont <- tryJust handleRead (STR.readFile file)
                         case trycont of
                           Left e -> putStrLn e >> exitSuccess
                           Right cont -> do songs <- parse ext cont
                                            menu b (Pl file songs)

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = putStrLn help 
parseArgs [file] = load True file
parseArgs [] = putStrLn "Missing arguments"
parseArgs _ = putStrLn "Incorrect execution. Use -h for help"

menu :: Bool -> Playlist -> IO ()
menu b pl = do let pname = F.takeBaseName (getPath pl)
               when b (putStrLn $ "Playlist " ++ pname ++ " loaded.\nAvailable commands: add, check, exit/quit, export, help, load, print, rmv. Use help for further information.")
               putStr ">"
               hFlush stdout
               getLine >>= ((parseCmd pl) . words)

parseCmd :: Playlist -> [String] -> IO ()
parseCmd pl ("add":fp) = do let gfp = intercalate " " fp
                            exists <- doesFileExist gfp
                            if not exists then
                              putStrLn "Error adding: file does not exist." >> menu False pl
                            else do putStrLn ("Adding " ++ gfp ++ " to playlist...\n")
                                    let newpl = addP pl gfp
                                    let ext = getExt (getPath pl)
                                    write ext newpl
                                    load False (getPath newpl)
parseCmd pl ["check"] = putStrLn "Checking playlist...\n" >>
                        check pl >>
                        menu False pl
parseCmd pl [p] | p == "exit" || p == "quit" = putStrLn "Goodbye!"
parseCmd pl ["export"] = putStrLn "Exporting playlist...\n" >>
                         export pl >>
                         menu False pl
parseCmd pl ["help"] = putStrLn mhelp >>
                       menu False pl
parseCmd _ ("load":fp) = let gfp = intercalate " " fp in 
                           load True gfp
parseCmd pl ["print"] = let pname = F.takeBaseName (getPath pl) in
                          putStrLn ("\nPlaylist " ++ pname ++ "\n") >>
                          mapM_ putStrLn (getSongs pl) >> putStrLn "" >>
                          menu False pl
parseCmd pl ("rmv":fp) = do let gfp = intercalate " " fp
                            putStrLn ("Removing " ++ gfp ++ " from playlist...\n")
                            let newpl = rmP pl gfp
                            let ext = getExt (getPath pl)
                            write ext newpl
                            load False (getPath newpl)
parseCmd pl _ = putStrLn "Wrong command." >>
                menu False pl

--TODO: state
copySong :: String -> Song -> IO ()
copySong name fp = do exists <- doesFileExist fp
                      if not exists then
                        putStrLn ("Error exporting: file " ++ fp ++ " does not exist.")
                      else do copyFileWithMetadata fp (name F.</> song)
                              putStrLn ("Copied "++song)
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
                    False -> putStrLn (s ++ " does NOT exist in the file system!!! It's recommended to remove it from the playlist.")

check :: Playlist -> IO ()
check pl = do mapM_ checkSong $ getSongs pl
              putStrLn ("Checking complete!")

parse :: Ext -> String -> IO [Song]
--parse M3u = parseM3u
--parse Pls = parsePls
parse Wpl = parseWpl
--parse Xspf = parseXspf
parse _ = \_ -> return []

write :: Ext -> Playlist -> IO ()
--write M3u = writeM3u
--write Pls = writePls
write Wpl = writeWpl
--write Xspf = writeXspf
write _ = \_ -> return ()   
