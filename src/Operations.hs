module Operations where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char
import System.Directory
import System.Exit
import System.FilePath as F
import Data.List
import Data.Maybe
import System.IO.Strict as STR

import Misc
import Playlist
import Report

--should paths be absolute?

getPlaylist :: F.FilePath -> IO Playlist
getPlaylist file = let ext = getExt file
                   in if ext == Other then putStrLn ("Unsupported format.") >> exitSuccess
                      else do tcont <- tryJust handleRead (STR.readFile file)
                              case tcont of
                                Left e -> putStrLn e >> exitSuccess
                                Right cont -> do songs <- parse ext cont
                                                 return $ (Pl file songs)

addSong :: Playlist -> F.FilePath -> IO Playlist
addSong pl s = do (newpl, r) <- runStateT (addSong' pl s) iReport
                  putStrLn $ ppReport r
                  return newpl

addSong' :: Playlist -> F.FilePath -> RState Playlist
addSong' pl s = do exists <- lift $ doesFileExist s
                   if not exists then
                     badError ("\nadd error: file " ++ s ++ " does not exist.\n") >> return pl
                   else do lift $ putStr ("\nAdding " ++ s ++ " to playlist... ")
                           let newpl = addS pl s
                           let ext = getExt (getPath pl)
                           lift $ write ext newpl
                           lift $ putStrLn "done!\n"
                           good >> return newpl

addDir :: F.FilePath -> Playlist -> IO Playlist
addDir d pl = do exists <- doesDirectoryExist d
                 if not exists then
                   putStrLn ("\nadd_dir error: directory " ++ d ++ " does not exist.\n") >> return pl
                 else do putStrLn ("\nAdding songs in " ++ d ++ " to playlist... \n")
                         contents <- listDirectory d
                         let paths = map (\x -> d ++ "/" ++ x) contents
                         (newpl, r) <- runStateT (foldM addSong' pl paths) iReport
                         putStrLn $ ppReport r
                         return newpl

checkSong :: Song -> IO ()
checkSong s = do b <- doesFileExist s
                 case b of
                    True -> putStrLn ("Ok " ++ F.takeBaseName s)
                    False -> putStrLn (s ++ " does NOT exist in the file system. It's recommended to remove it from the playlist.")

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
