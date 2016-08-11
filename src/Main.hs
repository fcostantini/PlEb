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
import Parsing
import Playlist
import Pls
import Wpl
import Xspf

main :: IO ()
main = do args <- getArgs
          runArgs (parseArgs args)

help, mhelp, vers :: String
help = "Usage: PlEb [-h] [-v] [playlist]\nAvailable formats are: m3u, m3u8, pls, wpl and xspf."
mhelp = "\nadd song_path: adds song to the playlist (if it exists).\n"++
        "add_dir dir: adds directory to the playlist (if it exists).\n" ++
        "check: checks if the playlist has inexistent files.\n"++
        "combine pl: combines present playlist with the provided one.\n"++
        "convert format: converts to desired format.\n"++
        "exit/quit: terminates the program.\n"++
        "export: creates a folder with the songs on the playlist.\n"++
        "load pl: loads a playlist.\n"++
        "print: prints the content of the playlist.\n"++
        "rmv song_path: removes song from the playlist (if it exists).\n"
vers = "PlEb 1.0.0"
warning = "\n---------------------------------------------------------------------------------\n"++
          "WARNING: found no songs in playlist (this is okay if you are using an empty one).\n"++
          "---------------------------------------------------------------------------------\n"

load :: Bool -> F.FilePath -> IO ()
load b file = do playlist <- getPlaylist file
                 when (null $ getSongs playlist) (putStrLn warning)
                 menu b playlist

getPlaylist :: F.FilePath -> IO Playlist
getPlaylist file = let ext = getExt file
                   in if ext == Other then putStrLn ("Unsupported format.") >> exitSuccess
                      else do trycont <- tryJust handleRead (STR.readFile file)
                              case trycont of
                                Left e -> putStrLn e >> exitSuccess
                                Right cont -> do songs <- parse ext cont
                                                 return $ (Pl file songs)

runArgs :: Arg -> IO ()
runArgs Help = putStrLn help 
runArgs Vers = putStrLn vers
runArgs (Playlist f) = load True f
runArgs Wrong = putStrLn "Incorrect execution. Use -h for help"

menu :: Bool -> Playlist -> IO ()
menu b pl = do let pname = F.takeBaseName (getPath pl)
               when b (putStrLn $ "\nPlaylist " ++ pname ++ " loaded.\n\nAvailable commands: add, add_dir, check, combine, convert, exit/quit, export, help, load, print, rmv. Use help for further information.\n")
               putStr ">"
               hFlush stdout
               getLine >>= ((parseCmd pl) . words)

goodbye :: IO ()
goodbye = putStrLn "\nGoodbye!\n"

wrong :: IO ()
wrong = putStrLn "\nWrong command.\n"

parseCmd :: Playlist -> [String] -> IO ()
parseCmd pl [c] = case map toLower c of
                    "add"     -> putStrLn "\nadd error: please write the path of the song.\n" >> menu False pl
                    "add_dir" -> putStrLn "\nadd_dir error: please write the path of the directory.\n" >> menu False pl
                    "check"   -> putStrLn "\nChecking playlist...\n" >>
                                 check pl >>
                                 menu False pl
                    "combine" -> putStrLn "\ncombine error: please specify the playlist you want to combine with.\n" >>
                                 menu False pl
                    "convert" -> putStrLn "\nconvert error: please specify the format you want to convert to.\n" >>
                                 menu False pl
                    "exit"    -> goodbye
                    "export"  -> putStrLn "\nExporting playlist...\n" >>
                                 export pl >>
                                 menu False pl
                    "help"    -> putStrLn mhelp >> menu False pl
                    "load"    -> putStrLn "\nload error: please specify the playlist to load.\n"
                    "print"   -> plPrint pl >> menu False pl 
                    "quit"    -> goodbye
                    "rmv"     -> putStrLn "\nrmv error: please write the path of the song.\n" >> menu False pl
                    _         -> wrong >> menu False pl
parseCmd pl (c:fp) = case map toLower c of
                       "add"     -> let gfp = intercalate " " fp
                                    in addSong pl gfp >>= (\pl -> menu False pl)
                       "add_dir" -> let gfp = intercalate " " fp
                                    in addDir gfp pl >>= (\pl -> menu False pl)
                       "combine" -> let comb = intercalate " " fp
                                    in combinePl pl comb >>
                                       menu False pl
                       "convert" -> let format = intercalate " " fp
                                    in convert pl format >>
                                       menu False pl
                       "load"    -> let gfp = intercalate " " fp
                                    in load True gfp 
                       "rmv"     -> let gfp = intercalate " " fp
                                    in rmvSong pl gfp >>= (\pl -> menu False pl)
                       _         -> wrong >> menu False pl
parseCmd pl _ = wrong >> menu False pl

addSong :: Playlist -> F.FilePath -> IO Playlist
addSong pl s = do exists <- doesFileExist s
                  if not exists then
                    putStrLn "\nadd error: file does not exist.\n" >> return pl
                  else do putStr ("Adding " ++ s ++ " to playlist... ")
                          let newpl = addP pl s
                          let ext = getExt (getPath pl)
                          write ext newpl
                          putStrLn "done!\n"
                          return newpl

addDir :: F.FilePath -> Playlist -> IO Playlist
addDir d pl = do exists <- doesDirectoryExist d
                 if not exists then
                   putStrLn "\nadd_dir error: directory does not exist.\n" >> return pl
                 else do putStr ("\nAdding songs in " ++ d ++ " to playlist... \n\n")
                         contents <- listDirectory d
                         newpl <- foldM addSong pl contents
                         return newpl

rmvSong :: Playlist -> F.FilePath -> IO Playlist
rmvSong pl s = do putStr ("\nRemoving " ++ s ++ " from playlist... ")
                  let newpl = rmP pl s
                  let ext = getExt (getPath pl)
                  write ext newpl
                  putStrLn "done!\n"
                  return newpl

copySong :: String -> Song -> IO ()
copySong name fp = do exists <- doesFileExist fp
                      if not exists then
                        putStrLn ("\nError exporting: file " ++ fp ++ " does not exist.")
                      else do copyFileWithMetadata fp (name F.</> song)
                              putStrLn ("Copied "++song)
                              where song = takeFileName fp

export :: Playlist -> IO ()
export pl = do let pname = F.takeBaseName (getPath pl)
               createDirectoryIfMissing False pname
               mapM_ (\a -> copySong pname a) $ getSongs pl
               putStrLn "\nExport complete!\n"

plPrint :: Playlist -> IO ()
plPrint pl = let pname = F.takeBaseName (getPath pl)
             in putStrLn ("\nPlaylist: " ++ pname ++ "\n") >>
                mapM_ putStrLn (getSongs pl) >> putStrLn ""

checkSong :: Song -> IO ()
checkSong s = do b <- doesFileExist s
                 case b of
                    True -> putStrLn ("Ok " ++ F.takeBaseName s)
                    False -> putStrLn (s ++ " does NOT exist in the file system!!! It's recommended to remove it from the playlist.")

check :: Playlist -> IO ()
check pl = do mapM_ checkSong $ getSongs pl
              putStrLn ("\nChecking complete!\n")

convert :: Playlist -> String -> IO ()
convert pl fmat = if (length fmat) > 5 then putStrLn "\nconvert error: wrong format.\n" else --this check is stupid, improve it later
                  let plf = tail $ F.takeExtension (getPath pl)
                      lfmat = map toLower fmat
                  in if plf == lfmat then putStrLn "\nconvert error: the playlist is already in this format.\n"
                     else let ext = getExt ("." ++ lfmat)
                          in case ext of
                               Other -> putStrLn "\nconvert error: format not supported\n"
                               _     -> let newfp = F.replaceExtension (getPath pl) lfmat
                                            auxPl = (Pl newfp (getSongs pl))
                                        in write ext auxPl >>
                                           putStrLn ("\nConversion complete! Load the new file ("++(takeFileName newfp)++") if you want to edit it.\n")

combinePl :: Playlist -> String -> IO ()
combinePl pl comb = do pl' <- getPlaylist comb
                       if (null $ getSongs pl') then (putStrLn "\ncombine error: trying to combine with an empty playlist.\n")
                       else let songs = getSongs pl
                                songs' = getSongs pl'
                                path = getPath pl
                                ext = getExt path
                                newpath = replaceBaseName path ((takeBaseName path) ++ (takeBaseName comb))
                                newsongs = songs ++ songs'
                                newpl = (Pl newpath newsongs)
                            in do write ext newpl
                                  putStrLn ("\nDone! Playlists combined in " ++ (takeFileName newpath) ++ "\n")

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
