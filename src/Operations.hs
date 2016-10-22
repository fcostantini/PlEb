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

--Should paths be absolute?

--Parses a playlist and returns its information
getPlaylist :: F.FilePath -> Maybe Playlist -> IO Playlist
getPlaylist file pl = let ext = getExt file
                        in if ext == Other then do putStrLn "load error: unsupported format."
                                                   case pl of
                                                     Nothing -> exitSuccess
                                                     Just p  -> return p
                           else do tcont <- tryJust handleRead (STR.readFile $ rstrip file)
                                   case tcont of
                                     Left e -> putStrLn e >> exitSuccess
                                     Right cont -> do songs <- parse ext cont
                                                      return $ Pl file songs

--Adds a song to the playlist
addSong :: Playlist -> F.FilePath -> IO Playlist
addSong pl s = do (newpl, r) <- runStateT (addSong' pl s) iReport
                  putStrLn $ ppReport r
                  return newpl

--Adds a song to the playlist, keeping a report of success  (does not check if it has a "song extension")
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

--Adds the content of a directory to the playlist
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

--Checks if a song in the playlist exists in the file system, keeping a report
checkSong :: Song -> RState ()
checkSong s = do b <- lift $ doesFileExist s
                 if b then lift (putStrLn ("Ok " ++ F.takeBaseName s)) >> good
                 else badError (s ++ " does NOT exist in the file system.\n")

--Checks if the contents of the playlist exist
check :: Playlist -> IO ()
check pl = do r <- execStateT (mapM_ checkSong (getSongs pl)) iReport
              putStrLn $ ppReport r

--Combines two playlists in a new file
combinePl :: Playlist -> String -> IO ()
combinePl pl comb = do pl' <- getPlaylist comb (Just pl)
                       when (pl == pl') (return ())
                       if null $ getSongs pl' then putStrLn "\ncombine error: trying to combine with an empty playlist.\n"
                       else let songs = getSongs pl
                                songs' = getSongs pl'
                                path = getPath pl
                                ext = getExt path
                                newpath = replaceBaseName path (takeBaseName path ++ takeBaseName comb)
                                newsongs = songs ++ songs'
                                newpl = Pl newpath newsongs
                            in do write ext newpl
                                  putStrLn ("\nDone! Playlists combined in " ++ takeFileName newpath ++ ". Load that file if you want to modify it.\n")

--Creates a copy of the given playlist with another format
convert :: Playlist -> String -> IO ()
convert pl fmat = if length fmat > 5 then putStrLn "\nconvert error: wrong format.\n" else
                  let plf = tail $ F.takeExtension (getPath pl)
                      lfmat = map toLower fmat
                  in if plf == lfmat then putStrLn "\nconvert error: the playlist is already in this format.\n"
                     else let ext = getExt ("." ++ lfmat)
                          in case ext of
                               Other -> putStrLn "\nconvert error: format not supported\n"
                               _     -> let newfp = F.replaceExtension (getPath pl) lfmat
                                            auxPl = Pl newfp (getSongs pl)
                                        in write ext auxPl >>
                                           putStrLn ("\nConversion complete! Load the new file (" ++ takeFileName newfp ++ ") if you want to modify it.\n")

--Exports a song, keeping a success report
exportSong :: String -> Song -> RState ()
exportSong name fp = do exists <- lift $ doesFileExist fp
                        if not exists then
                          badError ("\nexport error: file " ++ fp ++ " does not exist.")
                        else do lift $ copyFileWithMetadata fp (name F.</> song)
                                lift $ putStrLn ("Copied "++song)
                                good
                                where song = takeFileName fp

--Exports a playlist
export :: Playlist -> IO ()
export pl = do let pname = F.takeBaseName (getPath pl)
               createDirectoryIfMissing False pname
               r <- execStateT (mapM_ (exportSong pname) (getSongs pl)) iReport
               putStrLn $ ppReport r

--Prints the contents of a playlist
plPrint :: Playlist -> IO ()
plPrint pl = let pname = F.takeBaseName (getPath pl)
             in putStrLn ("\nPlaylist: " ++ pname ++ "\n") >>
                mapM_ putStrLn (getSongs pl) >> putStrLn ""

--Removes all ocurrencies of a song from a playlist
rmvSong :: Playlist -> F.FilePath -> IO Playlist
rmvSong pl s = if s `elem` getSongs pl then
                  do putStr ("\nRemoving " ++ s ++ " from playlist... ")
                     let newpl = rmS pl s
                     let ext = getExt (getPath pl)
                     write ext newpl
                     putStrLn "done!\n"
                     return newpl
               else putStrLn ("remove error: " ++ s ++ "is not in the playlist.\n") >> return pl
