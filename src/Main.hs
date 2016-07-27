module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath as F
import System.IO
import System.IO.Strict as STR
import Text.XML.Light

import HandleE
import M3u
import Playlist
import Pls
import Wpl
import Xspf

main :: IO ()
main = getArgs >>= parseArgs

help, mhelp, vers :: String
help = "Usage: PlEb [-h] [-v] [playlist]\nAvailable formats are: m3u, pls, wpl and xspf."
mhelp = "add song_path: adds song to the playlist (if it exists).\n"++
        "check: checks if the playlist has inexistent files.\n"++
        "convert format: converts to desired format.\n"++
        "exit/quit: terminates the program.\n"++
        "export: creates a folder with the songs on the playlist.\n"++
        "load pl: loads a playlist.\n"++
        "print: prints the content of the playlist.\n"++
        "rmv song_path: removes song from the playlist (if it exists)."
vers = "PlEb 1.0.0"
     
load :: Bool -> F.FilePath -> IO ()
load b file = let ext = getExt file
              in if ext == Other then putStrLn ("Unsupported format.")
                 else do trycont <- tryJust handleRead (STR.readFile file)
                         case trycont of
                           Left e -> putStrLn e >> exitSuccess
                           Right cont -> do songs <- parse ext cont
                                            menu b (Pl file songs)

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = putStrLn help 
parseArgs ["-v"] = putStrLn vers
parseArgs [file] = load True file
parseArgs [] = putStrLn "Missing arguments"
parseArgs _ = putStrLn "Incorrect execution. Use -h for help"

menu :: Bool -> Playlist -> IO ()
menu b pl = do let pname = F.takeBaseName (getPath pl)
               when b (putStrLn $ "Playlist " ++ pname ++ " loaded.\nAvailable commands: add, check, convert, exit/quit, export, help, load, print, rmv. Use help for further information.")
               putStr ">"
               hFlush stdout
               getLine >>= ((parseCmd pl) . words)

goodbye :: IO ()
goodbye = putStrLn "Goodbye!"

wrong :: IO ()
wrong = putStrLn "Wrong command."

parseCmd :: Playlist -> [String] -> IO ()
parseCmd pl [c] = case map toLower c of
                    "add"     -> putStrLn "add error: please write the path of the song." >> menu False pl
                    "check"   -> putStrLn "Checking playlist...\n" >>
                                 check pl >>
                                 menu False pl
                    "convert" -> putStrLn "convert error: please specify the format you want to convert to." >>
                                 menu False pl
                    "exit"    -> goodbye
                    "export"  -> putStrLn "Exporting playlist...\n" >>
                                 export pl >>
                                 menu False pl
                    "help"    -> putStrLn mhelp >> menu False pl
                    "load"    -> putStrLn "load error: please specify the playlist to load."
                    "print"   -> plPrint pl >> menu False pl 
                    "quit"    -> goodbye
                    "rmv"     -> putStrLn "rmv error: please write the path of the song." >> menu False pl
                    _         -> wrong >> menu False pl
parseCmd pl (c:fp) = case map toLower c of
                       "add"     -> do let gfp = intercalate " " fp
                                       exists <- doesFileExist gfp
                                       if not exists then
                                         putStrLn "add error: file does not exist." >> menu False pl
                                       else do putStrLn ("Adding " ++ gfp ++ " to playlist...\n")
                                               let newpl = addP pl gfp
                                               let ext = getExt (getPath pl)
                                               write ext newpl
                                               load False (getPath newpl)
                       "convert" -> let format = intercalate " " fp
                                    in convert pl format >>
                                       menu False pl
                       "load"    -> let gfp = intercalate " " fp
                                    in load True gfp
                       "rmv"     -> do let gfp = intercalate " " fp
                                       putStrLn ("Removing " ++ gfp ++ " from playlist...\n")
                                       let newpl = rmP pl gfp
                                       let ext = getExt (getPath pl)
                                       write ext newpl
                                       load False (getPath newpl)
                       _         -> wrong >> menu False pl
parseCmd pl _ = wrong >> menu False pl

--TODO: maybe add stats?
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

plPrint :: Playlist -> IO ()
plPrint pl = let pname = F.takeBaseName (getPath pl)
             in putStrLn ("\nPlaylist " ++ pname ++ "\n") >>
                mapM_ putStrLn (getSongs pl) >> putStrLn ""

checkSong :: Song -> IO ()
checkSong s = do b <- doesFileExist s
                 case b of
                    True -> putStrLn ("Ok " ++ F.takeBaseName s)
                    False -> putStrLn (s ++ " does NOT exist in the file system!!! It's recommended to remove it from the playlist.")

check :: Playlist -> IO ()
check pl = do mapM_ checkSong $ getSongs pl
              putStrLn ("Checking complete!")

convert :: Playlist -> String -> IO ()
convert pl fmat = if (length fmat) > 4 then putStrLn "convert error: wrong format" else --this check is stupid, improve it laterfget
                  let plf = tail $ F.takeExtension (getPath pl)
                      lfmat = map toLower fmat
                  in if plf == lfmat then putStrLn "convert error: the playlist is already in this format."
                     else let ext = getExt ("." ++ lfmat)
                          in case ext of
                               Other -> putStrLn "convert error: format not supported"
                               _     -> let newfp = F.replaceExtension (getPath pl) lfmat
                                            auxPl = (Pl newfp (getSongs pl))
                                        in write ext auxPl >>
                                           putStrLn "Conversion complete! Load the new file if you want to edit it."

parse :: Ext -> String -> IO [Song]
parse M3u = parseM3u
parse Pls = parsePls
parse Wpl = parseWpl
parse Xspf = parseXspf
parse _ = \_ -> return []

write :: Ext -> Playlist -> IO ()
write M3u = writeM3u
write Pls = writePls
write Wpl = writeWpl
write Xspf = writeXspf
write _ = \_ -> return ()
