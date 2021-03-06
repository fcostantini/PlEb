module Misc where

import Data.List
import qualified Data.Text as T
import System.Console.Haskeline
import System.IO.Error

import M3u
import Playlist
import Pls
import Wpl
import Xspf

mhelp, vers :: String

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

vers = "PlEb 1.4.0"

handleRead :: IOError -> Maybe String
handleRead e | isDoesNotExistError e = Just "Error reading: file doesn't exist."
             | isPermissionError e = Just "Error reading: permission denied."
             | isIllegalOperation e = Just "Error reading: illegal operation." --just in case
             | otherwise = Nothing

commandList = ["add", "add_dir", "check", "combine", "convert", "exit", "export", "load", "print", "quit", "rmv"]

--This function can be used to autocomplete commands instead of filepaths
searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) commandList

--Settings for Haskeline
mySettings :: FilePath -> Settings IO
mySettings fp = Settings { historyFile = Just (fp++"/.PlEb_history"),
                           complete = completeFilename, --completeWord Nothing " \t" $ return . searchFunc, 
                           autoAddHistory = True }

trim :: String -> String
trim = filter (/= ' ')

rstrip :: String -> String
rstrip = T.unpack . T.stripEnd . T.pack

--Playlist parser
parse :: Ext -> String -> IO [Song]
parse M3u = parseM3u
parse Pls = parsePls
parse Wpl = parseWpl
parse Xspf = parseXspf
parse _ = \_ -> return []

--Playlist writer
write :: Ext -> Playlist -> IO ()
write M3u = writeM3u
write Pls = writePls
write Wpl = writeWpl
write Xspf = writeXspf
write _ = \_ -> return ()
