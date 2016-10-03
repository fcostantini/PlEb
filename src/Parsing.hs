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
         | Seq Cmd Cmd
         | CWrong deriving Show

lineCmd :: Parser [[Cmd]]
lineCmd = endBy fullCmd endOfLine

fullCmd :: Parser [Cmd]
fullCmd = sepBy cmd (char '>')

cmd :: Parser Cmd
cmd = choice [try addD, try addP, try checkP, try comb, try conv, try exitP, try exportP, try helpP, try loadP, try printP, try rmvP, wrongP]

stuff = many (noneOf ">\n\r")
stuff1 = many1 (noneOf ">\n\r")

addP :: Parser Cmd
addP = do string "add "
          f <- stuff
          return $ Add f

addD :: Parser Cmd
addD = do string "add_dir "
          f <- stuff
          return $ AddD f

checkP :: Parser Cmd
checkP = do string "check"
            choice [stuff1 >> return CWrong, return Check]

comb :: Parser Cmd
comb = do string "combine "
          f <- stuff
          return $ Comb f

conv :: Parser Cmd
conv = do string "convert"
          f <- stuff
          return $ Conv f

exitP :: Parser Cmd
exitP = (do string "exit"
            choice [stuff1 >> return CWrong, return Exit])
    <|> (do string "quit"
            choice [stuff1 >> return CWrong, return Exit])

exportP :: Parser Cmd
exportP = do string "export"
             choice [stuff1 >> return CWrong, return Check]

helpP :: Parser Cmd
helpP = do string "help"
           choice [stuff1 >> return CWrong, return HelpC]

loadP :: Parser Cmd
loadP = do string "load "
           f <- stuff
           return $ Load f

printP :: Parser Cmd
printP = do string "print"
            choice [stuff1 >> return CWrong, return Print]

rmvP :: Parser Cmd
rmvP = do string "rmv "
          f <- stuff
          return $ Rmv f

wrongP :: Parser Cmd
wrongP = stuff >> return CWrong 

toCmd :: [Cmd] -> Cmd
toCmd [] = error "Shouldn't happen..."
toCmd [e] = e
toCmd (c:cmds) = Seq c (toCmd cmds)

parseComd :: String -> IO Cmd
parseComd c = let parsed = parse lineCmd "" c
              in case parsed of
                  Left e -> putStrLn ("Error parsing command " ++ (show e)) >> return CWrong
                  Right cl -> return $ (toCmd . concat) cl
