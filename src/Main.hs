module Main where

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

load :: Bool -> F.FilePath -> IO Playlist
load b file = do playlist <- getPlaylist file
                 when (null $ getSongs playlist) (putStrLn warning)
                 menu b playlist

menu :: Bool -> Playlist -> IO Playlist
menu b pl = do home <- getHomeDirectory --history file will be located here
               (runInputT (mySettings home) loop) >>= (\p -> menu False p)
            where loop :: InputT IO Playlist
                  loop = do let pname = F.takeBaseName (getPath pl)
                            when b $ liftIO (putStrLn $ "\nPlaylist " ++ pname ++ " loaded.\n\nAvailable commands: add, add_dir, check, combine, convert, exit/quit, export, help, load, print, rmv. Use help for further information.\n")
                            input <- getInputLine "> "
                            case input of
                                Nothing -> return pl
                                Just c -> do cmd <- liftIO $ parseComd (c++"\n")
                                             liftIO $ runCmd cmd pl

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

runPleb :: [Flag] -> F.FilePath -> IO Playlist
runPleb [] p = load True p
runPleb fs p = do when (FHelp `elem` fs) (putStrLn (usageInfo header options) >> exitSuccess)
                  when (Version `elem` fs) (putStrLn vers >> exitSuccess)
                  let cfs = filter isCmds fs
                  case cfs of
                    [] -> load True p
                    [Cmds f] -> load True p
                    _ -> putStrLn "Incorrect use of commands flag.\n" >> exitSuccess
               where isCmds (Cmds _) = True
                     isCmds _ = False

main :: IO ()
main = do (args, files) <- getArgs >>= plebOpts
          runPleb args (concat files)
          exitSuccess
