module Lib
    ( someFunc
    ) where

import Chapter2.SimpleFunctions

someFunc :: IO ()
--someFunc = putStrLn (firstOrEmpty ["abc", "def"])
someFunc = putStrLn (show (reverse2 [1,2,3,4,5]))
