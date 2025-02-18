module Lib
    ( someFunc
    ) where

import System.Process (callCommand)

data HopingAlphabet = HopingAlphabet { character :: Char,
                                       position :: (Float, Float),
                                       verocity :: (Float, Float) } deriving (Show)

someFunc :: IO ()
someFunc = do
    -- テスト出力
    print "Hello World!"
    print $ HopingAlphabet 'a' (3, 4.0) (2, 5.0)
    callCommand "cls"
    print "Hello World! 2"
    print $ HopingAlphabet 'b' (3, 4.0) (2, 5.0)