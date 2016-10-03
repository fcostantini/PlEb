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
ppReport (Rep e 1 0) = "Command successful! The operation was completed without errors.\n"
ppReport (Rep e g 0) = "Command successful! " ++ show g ++ " operations were completed without errors.\n"
ppReport (Rep e 0 1) = "Command unsuccessful. One error was encountered:\n" ++ e
ppReport (Rep e 0 b) = "Command unsuccessful. " ++ show b ++ " errors were encountered:\n" ++ e
ppReport (Rep e 1 1) = "One operation completed without errors. One error was encountered:\n" ++ e
ppReport (Rep e g b) = show g ++ "operations completed without errors. " ++ show b ++ " errors were encountered:\n" ++ e
