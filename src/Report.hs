module Report where

import Control.Monad.Trans.State

data Report = Rep { ebuffer :: String,
                    goodCount :: Int,
                    badCount :: Int } deriving Show

iReport :: Report
iReport = Rep [] 0 0

type RState a = StateT Report IO a

addError :: String -> RState ()
addError e = do r <- get
                let b = ebuffer r
                put $ r {ebuffer = b ++ e}

good :: RState ()
good = do r <- get
          let gc = goodCount r
          put $ r {goodCount = gc + 1}

bad :: RState ()
bad = do r <- get
         let bc = badCount r
         put $ r {badCount = bc + 1}

badError :: String -> RState ()
badError e = addError e >> bad

ppReport :: Report -> String
ppReport (Rep e 1 0) = "\nCommand successful! The operation was completed without errors.\n"
ppReport (Rep e g 0) = "\nCommand successful! " ++ show g ++ " operations were completed without errors.\n"
ppReport (Rep e 0 1) = "\nCommand unsuccessful. One error was encountered:\n\n" ++ e
ppReport (Rep e 0 b) = "\nCommand unsuccessful. " ++ show b ++ " errors were encountered:\n\n" ++ e
ppReport (Rep e 1 1) = "\nOne operation completed correctly. One error was encountered:\n\n" ++ e
ppReport (Rep e g b) = "\n" ++ show g ++ " operations completed correctly. " ++ show b ++ " errors were encountered:\n\n" ++ e
