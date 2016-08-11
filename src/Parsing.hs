module Parsing where

import System.FilePath as F
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data Arg = Help | Vers | Playlist F.FilePath | Wrong

parseArgs :: [String] -> Arg
parseArgs ["-h"] = Help
parseArgs ["-v"] = Vers
parseArgs [file] = Playlist file
parseArgs _ = Wrong

data Cmd = Add F.FilePath
         | AddD F.FilePath
         | Check
         | Comb F.FilePath
         | Conv String
         | Exit
         | Export
         | HelpC
         | Load F.FilePath
         | Print
         | Rmv F.FilePath
         | CWrong deriving Show

lineCmd :: Parser [[Cmd]]
lineCmd = endBy fullCmd endOfLine

fullCmd :: Parser [Cmd]
fullCmd = sepBy cmd (char '>')

cmd :: Parser Cmd
cmd = choice [try add, try addD, try check, try comb, try conv, try exit, try export, try help, try load, try cprint, try rmv, wrong]

stuff = many (noneOf ">\n\r")
stuff1 = many1 (noneOf ">\n\r")

add :: Parser Cmd
add = do string "add "
         f <- stuff
         return $ Add f

addD :: Parser Cmd
addD = do string "add_dir "
          f <- stuff
          return $ Add f

check :: Parser Cmd
check = do string "check"
           choice [stuff1 >> return CWrong, return Check]

comb :: Parser Cmd
comb = do string "combine "
          f <- stuff
          return $ Comb f

conv :: Parser Cmd
conv = do string "convert"
          f <- stuff
          return $ Conv f

exit :: Parser Cmd
exit = (do string "exit"
           choice [stuff1 >> return CWrong, return Exit])
   <|> (do string "quit"
           choice [stuff1 >> return CWrong, return Exit])

export :: Parser Cmd
export = do string "export"
            choice [stuff1 >> return CWrong, return Check]

help :: Parser Cmd
help = do string "help"
          choice [stuff1 >> return CWrong, return HelpC]

load :: Parser Cmd
load = do string "load "
          f <- stuff
          return $ Load f

cprint :: Parser Cmd
cprint = do string "print"
            choice [stuff1 >> return CWrong, return Print]

rmv :: Parser Cmd
rmv = do string "rmv "
         f <- stuff
         return $ Rmv f

wrong :: Parser Cmd
wrong = stuff >> return CWrong 

parseCmd :: String -> IO [Cmd]
parseCmd c = let parsed = parse lineCmd "" c
             in case parsed of
                  Left e -> putStrLn ("Error parsing command " ++ (show e)) >> return []
                  Right cl -> return (concat cl)
