module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import System.Console.Haskeline
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

getPlaylist :: F.FilePath -> IO Playlist
getPlaylist file = let ext = getExt file
                   in if ext == Other then putStrLn ("Unsupported format.") >> exitSuccess
                      else do trycont <- tryJust handleRead (STR.readFile file)
                              case trycont of
                                Left e -> putStrLn e >> exitSuccess
                                Right cont -> do songs <- parse ext cont
                                                 return $ (Pl file songs)

load :: Bool -> F.FilePath -> IO Playlist
load b file = do playlist <- getPlaylist file
                 when (null $ getSongs playlist) (putStrLn warning)
                 menu b playlist

runArgs :: Arg -> IO Playlist
runArgs Help = putStrLn help >> exitSuccess
runArgs Vers = putStrLn vers >> exitSuccess
runArgs (Playlist f) = load True f
runArgs Wrong = putStrLn "Incorrect execution. Use -h for help" >> exitSuccess

menu :: Bool -> Playlist -> IO Playlist
{-menu b pl = do let pname = F.takeBaseName (getPath pl)
               when b (putStrLn $ "\nPlaylist " ++ pname ++ " loaded.\n\nAvailable commands: add, add_dir, check, combine, convert, exit/quit, export, help, load, print, rmv. Use help for further information.\n")
               putStr ">"
               hFlush stdout
               input <- getLine
               cmd <- parseComd (input++"\n")
               runCmd cmd pl >>= (\p -> menu False p)-}
menu b pl = (runInputT (defaultSettings {historyFile = hfile}) loop) >>= (\p -> menu False p)
            where loop :: InputT IO Playlist
                  loop = do let pname = F.takeBaseName (getPath pl)
                            when b $ liftIO (putStrLn $ "\nPlaylist " ++ pname ++ " loaded.\n\nAvailable commands: add, add_dir, check, combine, convert, exit/quit, export, help, load, print, rmv. Use help for further information.\n")
                            input <- getInputLine "> "
                            case input of
                                Nothing -> return pl
                                Just c -> do cmd <- liftIO $ parseComd (c++"\n")
                                             liftIO $ runCmd cmd pl
                  hfile = Just ".PlEb_history" -- fixed path?

trim :: String -> String
trim = filter (/= ' ')

runCmd :: Cmd -> Playlist -> IO Playlist
runCmd (Add s) pl = case trim s of
                      "" -> putStrLn "\nadd error: please write the path of the song.\n" >> return pl
                      _  -> addSong pl s
runCmd (AddD d) pl = case trim d of
                       "" -> putStrLn "\nadd_dir error: please write the path of the directory.\n" >> return pl
                       _  -> addDir d pl
runCmd Check pl = putStrLn "\nChecking playlist...\n" >> check pl
runCmd (Comb p) pl = case trim p of
                       "" -> putStrLn "\ncombine error: please specify the playlist you want to combine with.\n" >> return pl
                       _  -> combinePl pl p
runCmd (Conv f) pl = case trim f of
                       "" -> putStrLn "\nconvert error: please specify the format you want to convert to.\n" >> return pl
                       _  -> convert pl f
runCmd Exit pl = putStrLn "\nGoodbye!\n" >> exitSuccess
runCmd Export pl = putStrLn "\nExporting playlist...\n" >> export pl
runCmd HelpC pl = putStrLn mhelp >> return pl
runCmd (Load p) pl = case trim p of
                       "" -> putStrLn "\nload error: please specify the playlist to load.\n" >> return pl
                       _  -> load True p
runCmd Print pl = plPrint pl
runCmd (Rmv s) pl = case trim s of
                      "" -> putStrLn "\nrmv error: please write the path of the song.\n" >> return pl
                      _  -> rmvSong pl s
runCmd (Seq c1 c2) pl = runCmd c1 pl >>= runCmd c2
runCmd CWrong pl = putStrLn "\nWrong command.\n" >> return pl

addSong :: Playlist -> F.FilePath -> IO Playlist
addSong pl s = do exists <- doesFileExist s
                  if not exists then
                    putStrLn "\nadd error: file does not exist.\n" >> return pl
                  else do putStr ("Adding " ++ s ++ " to playlist... ")
                          let newpl = addS pl s
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

checkSong :: Song -> IO ()
checkSong s = do b <- doesFileExist s
                 case b of
                    True -> putStrLn ("Ok " ++ F.takeBaseName s)
                    False -> putStrLn (s ++ " does NOT exist in the file system!!! It's recommended to remove it from the playlist.")

check :: Playlist -> IO Playlist
check pl = do mapM_ checkSong $ getSongs pl
              putStrLn ("\nChecking complete!\n")
              return pl

combinePl :: Playlist -> String -> IO Playlist
combinePl pl comb = do pl' <- getPlaylist comb
                       if (null $ getSongs pl') then (putStrLn "\ncombine error: trying to combine with an empty playlist.\n") >> return pl
                       else let songs = getSongs pl
                                songs' = getSongs pl'
                                path = getPath pl
                                ext = getExt path
                                newpath = replaceBaseName path ((takeBaseName path) ++ (takeBaseName comb))
                                newsongs = songs ++ songs'
                                newpl = (Pl newpath newsongs)
                            in do write ext newpl
                                  putStrLn ("\nDone! Playlists combined in " ++ (takeFileName newpath) ++ "\n")
                                  return newpl

convert :: Playlist -> String -> IO Playlist
convert pl fmat = if (length fmat) > 5 then putStrLn "\nconvert error: wrong format.\n" >> return pl else --this check is stupid, improve it later
                  let plf = tail $ F.takeExtension (getPath pl)
                      lfmat = map toLower fmat
                  in if plf == lfmat then putStrLn "\nconvert error: the playlist is already in this format.\n" >> return pl
                     else let ext = getExt ("." ++ lfmat)
                          in case ext of
                               Other -> putStrLn "\nconvert error: format not supported\n" >> return pl
                               _     -> let newfp = F.replaceExtension (getPath pl) lfmat
                                            auxPl = (Pl newfp (getSongs pl))
                                        in write ext auxPl >>
                                           putStrLn ("\nConversion complete! Load the new file ("++(takeFileName newfp)++") if you want to edit it.\n") >> return pl

exportSong :: String -> Song -> IO ()
exportSong name fp = do exists <- doesFileExist fp
                        if not exists then
                          putStrLn ("\nError exporting: file " ++ fp ++ " does not exist.")
                        else do copyFileWithMetadata fp (name F.</> song)
                                putStrLn ("Copied "++song)
                                where song = takeFileName fp

export :: Playlist -> IO Playlist
export pl = do let pname = F.takeBaseName (getPath pl)
               createDirectoryIfMissing False pname
               mapM_ (\a -> exportSong pname a) $ getSongs pl
               putStrLn "\nExport complete!\n"
               return pl

plPrint :: Playlist -> IO Playlist
plPrint pl = let pname = F.takeBaseName (getPath pl)
             in putStrLn ("\nPlaylist: " ++ pname ++ "\n") >>
                mapM_ putStrLn (getSongs pl) >> putStrLn "" >> return pl

rmvSong :: Playlist -> F.FilePath -> IO Playlist
rmvSong pl s = do putStr ("\nRemoving " ++ s ++ " from playlist... ")
                  let newpl = rmS pl s
                  let ext = getExt (getPath pl)
                  write ext newpl
                  putStrLn "done!\n"
                  return newpl

main :: IO ()
main = do args <- getArgs
          runArgs (parseArgs args)
          exitSuccess
