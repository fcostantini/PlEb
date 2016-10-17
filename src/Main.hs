module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
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
loadExec :: Bool -> F.FilePath -> Maybe String -> IO Playlist
loadExec b file c = do playlist <- getPlaylist file
                       when (null $ getSongs playlist) (putStrLn warning)
                       case c of
                         Nothing -> menu b playlist
                         Just cmds -> do cmd <- parseComd cmds
                                         runCmd cmd playlist

--Load a playlist interactively
load :: Bool -> F.FilePath -> IO Playlist
load b file = loadExec b file Nothing

--Loop for interactive usage
menu :: Bool -> Playlist -> IO Playlist
menu b pl = do home <- getHomeDirectory --history file will be located here
               runInputT (mySettings home) loop >>= menu False
            where loop :: InputT IO Playlist
                  loop = do let pname = F.takeBaseName (getPath pl)
                            when b $ liftIO (putStrLn $ "\nPlaylist " ++ pname ++ " loaded.\n\nAvailable commands: add, add_dir, check, combine, convert, exit/quit, export, help, load, print, rmv. Use help for further information.\n")
                            input <- getInputLine "> "
                            case input of
                                Nothing -> return pl
                                Just c -> do cmd <- liftIO $ parseComd (c++"\n")
                                             liftIO $ runCmd cmd pl

--Run a command in the given playlist
runCmd :: Cmd -> Playlist -> IO Playlist
runCmd (Add s) pl = case trim s of
                      "" -> putStrLn "\nadd error: please write the path of the song.\n" >> return pl
                      _  -> addSong pl s
runCmd (AddD d) pl = case trim d of
                       "" -> putStrLn "\nadd_dir error: please write the path of the directory.\n" >> return pl
                       _  -> addDir d pl
runCmd Check pl = putStrLn "\nChecking playlist...\n" >> check pl >> return pl
runCmd (Comb p) pl = case trim p of
                       "" -> putStrLn "\ncombine error: please specify the playlist you want to combine with.\n" >> return pl
                       _  -> combinePl pl p >> return pl
runCmd (Conv f) pl = case trim f of
                       "" -> putStrLn "\nconvert error: please specify the format you want to convert to.\n" >> return pl
                       _  -> convert pl f >> return pl
runCmd Exit pl = putStrLn "\nGoodbye!\n" >> exitSuccess
runCmd Export pl = putStrLn "\nExporting playlist...\n" >> export pl >> return pl
runCmd HelpC pl = putStrLn mhelp >> return pl
runCmd (Load p) pl = case trim p of
                       "" -> putStrLn "\nload error: please specify the playlist to load.\n" >> return pl
                       _  -> load True p
runCmd Print pl = plPrint pl >> return pl
runCmd (Rmv s) pl = case trim s of
                      "" -> putStrLn "\nrmv error: please write the path of the song.\n" >> return pl
                      _  -> rmvSong pl s
runCmd (Seq c1 c2) pl = runCmd c1 pl >>= runCmd c2 --should it stop when first command gives some error?
runCmd CWrong pl = putStrLn "\nWrong command.\n" >> return pl

--Run the program, if help or version flags are active it will show those and end
runPleb :: [Flag] -> F.FilePath -> IO Playlist
runPleb [] p = load True p
runPleb fs p = do when (FHelp `elem` fs) (putStrLn (usageInfo header options) >> exitSuccess)
                  when (Version `elem` fs) (putStrLn vers >> exitSuccess)
                  let cfs = filter isCmds fs
                  case cfs of
                    [] -> load True p
                    [Cmds f] -> do tcont <- tryJust handleRead (readFile f)
                                   case tcont of
                                     Left e -> putStrLn e >> exitSuccess
                                     Right cmds -> loadExec True p (Just cmds)
                    _ -> putStrLn "Incorrect use of commands flag.\n" >> exitSuccess
               where isCmds (Cmds _) = True
                     isCmds _ = False

main :: IO ()
main = do (args, files) <- getArgs >>= plebOpts
          runPleb args (concat files)
          exitSuccess
