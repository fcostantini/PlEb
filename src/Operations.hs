module Operations where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath as F
import System.IO.Strict as STR

import Misc
import Playlist
import Report

--Should paths be absolute?

--Parses a playlist and returns its information
getPlaylist :: F.FilePath -> IO (Maybe Playlist)
getPlaylist file = let ext = getExt file
                   in if ext == Other then do putStrLn "load error: unsupported format."
                                              return Nothing
                      else do tcont <- tryJust handleRead (STR.readFile $ rstrip file)
                              case tcont of
                                Left e -> putStrLn e >> return Nothing
                                Right cont -> do songs <- parse ext cont
                                                 return $ Just (Pl file songs)

--Adds a song to the playlist
addSong :: F.FilePath -> PlState ()
addSong s = do r <- execStateT (addSong' s) iReport
               lift $ putStrLn $ ebuffer r

--Adds a song to the playlist, keeping a report of success
addSong' :: F.FilePath -> RState ()
addSong' s = do exists <- lift . lift $ doesFileExist s
                if not exists then
                  badError ("add error: file " ++ s ++ " does not exist.\n")
                else do lift . lift $ putStr ("\nAdding " ++ s ++ " to playlist... ")
                        lift (addS s)
                        pl <- lift get
                        let ext = getExt (getPath pl)
                        lift . lift $ write ext pl
                        lift . lift $ putStrLn "done!\n"
                        good


--Adds the content of a directory to the playlist
addDir :: F.FilePath -> PlState ()
addDir d = do exists <- lift $ doesDirectoryExist d
              if not exists then
                 lift $ putStrLn ("add_dir error: directory " ++ d ++ " does not exist.\n")
              else do lift $ putStrLn ("\nAdding songs in " ++ d ++ " to playlist... \n")
                      contents <- lift $ listDirectory d
                      let paths = map (\x -> d ++ "/" ++ x) contents
                      r <- execStateT (mapM_ addSong' paths) iReport
                      lift $ putStrLn $ ppReport r

--Checks if a song in the playlist exists in the file system, keeping a report
checkSong :: Song -> RState ()
checkSong s = do b <- lift . lift $ doesFileExist s
                 if b then do lift . lift $ putStrLn ("Ok " ++ F.takeBaseName s)
                              good
                 else badError (s ++ " does NOT exist in the file system.\n")

--Checks if the contents of the playlist exist
check :: PlState ()
check = do pl <- get
           r <- execStateT (mapM_ checkSong (getSongs pl)) iReport
           lift $ putStrLn $ ppReport r

--Combines two playlists in a new file
combinePl :: String -> PlState ()
combinePl comb = do pl <- get
                    mpl' <- lift $ getPlaylist comb
                    case mpl' of
                      Nothing -> lift $ putStrLn "combine error: failed to read the second playlist.\n"
                      Just pl'-> if null $ getSongs pl' then lift $ putStrLn "\ncombine error: trying to combine with an empty playlist.\n"
                                 else let songs = getSongs pl
                                          songs' = getSongs pl'
                                          path = getPath pl
                                          ext = getExt path
                                          newpath = replaceBaseName path (takeBaseName path ++ takeBaseName comb)
                                          newsongs = songs ++ songs'
                                          newpl = Pl newpath newsongs
                                      in do lift $ write ext newpl
                                            lift $ putStrLn ("\nDone! Playlists combined in " ++ takeFileName newpath ++ ". Load that file if you want to modify it.\n")

--Creates a copy of the given playlist with another format
convert :: String -> PlState ()
convert fmat = do pl <- get
                  let text = map toLower fmat
                  let tfmat = getExt ('.':text)
                  let pfmat = getExt $ F.takeExtension (getPath pl)
                  if pfmat == tfmat then lift $ putStrLn "\nconvert error: the playlist is already in this format.\n" else
                       case tfmat of
                            Other -> lift $ putStrLn "\nconvert error: format not supported\n"
                            _     -> let newfp = F.replaceExtension (getPath pl) text
                                         auxPl = Pl newfp (getSongs pl)
                                     in do lift $ write tfmat auxPl
                                           lift $ putStrLn ("\nConversion complete! Load the new file (" ++ takeFileName newfp ++ ") if you want to modify it.\n")

--Exports a song, keeping a success report
exportSong :: String -> Song -> RState ()
exportSong pname fp = do exists <- lift . lift $ doesFileExist fp
                         if not exists then
                           badError ("\nexport error: file " ++ fp ++ " does not exist.")
                         else do lift . lift $ copyFileWithMetadata fp (pname F.</> song)
                                 lift . lift $ putStrLn ("Copied "++song)
                                 good
                                 where song = takeFileName fp

--Exports a playlist
export :: PlState ()
export = do pl <- get
            let pname = F.takeBaseName (getPath pl)
            lift $ createDirectoryIfMissing False pname
            r <- execStateT (mapM_ (exportSong pname) (getSongs pl)) iReport
            lift $ putStrLn $ ppReport r

loadPl :: F.FilePath -> PlState ()
loadPl file = do mpl <- lift $ getPlaylist file
                 case mpl of 
                   Nothing -> return ()
                   Just pl -> do lift $ putStrLn ("\nPlaylist " ++ takeFileName (getPath pl) ++ " loaded!\n")
                                 put pl

--Prints the contents of a playlist
plPrint :: PlState ()
plPrint = do pl <- get
             let pname = F.takeBaseName (getPath pl)
             lift $ putStrLn ("Playlist: " ++ pname ++ "\n")
             lift $ mapM_ putStrLn (getSongs pl)
             lift $ putStrLn ""

--Removes all ocurrencies of a song from a playlist
rmvSong :: F.FilePath -> PlState ()
rmvSong s = do pl <- get
               if s `elem` getSongs pl then
                 do lift $ putStr ("\nRemoving " ++ s ++ " from playlist... ")
                    rmS s
                    pl <- get
                    let ext = getExt (getPath pl)
                    lift $ write ext pl
                    lift $ putStrLn "done!\n"
               else lift $ putStrLn ("remove error: " ++ s ++ "is not in the playlist.\n")
