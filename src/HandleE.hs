module HandleE where

import System.IO.Error

handleRead :: IOError -> Maybe String
handleRead e | isDoesNotExistError e = Just "Error reading: file doesn't exist."
             | isPermissionError e = Just "Error reading: permission denied."
             | isIllegalOperation e = Just "Error reading: illegal operation." --just in case
             | otherwise = Nothing
