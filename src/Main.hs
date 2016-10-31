module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List
import System.Console.GetOpt
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.Exit
import System.FilePath as F
import System.IO

import Misc
import Operations
import Parsing
import Playlist
import Report

--Load a playlist; if given commands execute them
loadExec :: F.FilePath -> Maybe String -> IO ()
loadExec file c = do mpl <- getPlaylist file
                     case mpl of 
                        Nothing -> exitSuccess
                        Just pl -> case c of
                                     Nothing ->  menu True pl
                                     Just cmds -> do cmd <- parseComd cmds
                                                     evalStateT (runCmd cmd) pl

--Loop for interactive usage
menu :: Bool -> Playlist -> IO ()
menu b pl = do home <- getHomeDirectory --history file will be located here
               runInputT (mySettings home) loop >>= menu False
            where loop :: InputT IO Playlist
                  loop = do let pname = F.takeBaseName (getPath pl)
                            when b $ liftIO (putStrLn $ "\nPlaylist " ++ pname ++ " loaded.\n\nAvailable commands: add, add_dir, check, combine, convert, exit/quit, export, help, load, print, rmv. Use help for further information.\n")
                            input <- getInputLine "> "
                            case input of
                                Nothing -> return pl
                                Just c -> do cmd <- liftIO $ parseComd (c++"\n")
                                             liftIO $ execStateT (runCmd cmd) pl

--Run a command in the current playlist
runCmd :: Cmd -> PlState ()
runCmd (Add s) = case trim s of
                      "" -> lift $ putStrLn "\nadd error: please write the path of the song.\n"
                      _  -> addSong s
runCmd (AddD d) = case trim d of
                       "" -> lift $ putStrLn "\nadd_dir error: please write the path of the directory.\n"
                       _  -> addDir d
runCmd Check = lift (putStrLn "\nChecking playlist...\n") >> check
runCmd (Comb p) = case trim p of
                       "" -> lift $ putStrLn "\ncombine error: please specify the playlist you want to combine with.\n"
                       _  -> combinePl p
runCmd (Conv f) = case trim f of
                       "" -> lift $ putStrLn "\nconvert error: please specify the format you want to convert to.\n"
                       _  -> convert f
runCmd Exit = do lift $ putStrLn "\nGoodbye!\n"
                 lift exitSuccess
runCmd Export = do lift $ putStrLn "\nExporting playlist...\n"
                   export
runCmd HelpC = lift $ putStrLn mhelp
runCmd (Load p) = case trim p of
                       "" -> lift $ putStrLn "\nload error: please specify the playlist to load.\n"
                       _  -> loadPl p
runCmd Print = plPrint
runCmd (Rmv s) = case trim s of
                      "" -> lift $ putStrLn "\nrmv error: please write the path of the song.\n"
                      _  -> rmvSong s
runCmd (Seq c1 c2) = runCmd c1 >> runCmd c2 --should it stop when first command gives some error?
runCmd CWrong = lift $ putStrLn "\nWrong command.\n"

--Run the program, if help or version flags are active it will show those and end
runPleb :: [Flag] -> F.FilePath -> IO ()
runPleb [] p = loadExec p Nothing
runPleb fs p = do when (FHelp `elem` fs) (putStrLn (usageInfo header options) >> exitSuccess)
                  when (Version `elem` fs) (putStrLn vers >> exitSuccess)
                  let cfs = filter isCmds fs
                  case cfs of
                    [] -> loadExec p Nothing
                    [Cmds f] -> do tcont <- tryJust handleRead (readFile f)
                                   case tcont of
                                     Left e -> putStrLn e >> exitSuccess
                                     Right cmds -> loadExec p (Just cmds)
                    _ -> putStrLn "Incorrect use of commands flag.\n" >> exitSuccess
               where isCmds (Cmds _) = True
                     isCmds _ = False

main :: IO ()
main = do (args, files) <- getArgs >>= plebOpts
          runPleb args (concat files)
          exitSuccess
