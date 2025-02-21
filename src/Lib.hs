module Lib
    ( plotHopingAlphabets
    ) where

import Data.List (find)
import System.Process (callCommand)

alphabetsNum :: Int
alphabetsNum = 5
height :: Int
height = 20
width :: Int
width = 60
gravity :: Float
gravity = 0.4

data HopingAlphabet = HopingAlphabet { character :: Char,
                                       position :: (Float, Float),
                                       verocity :: (Float, Float) } deriving (Show)

-- メイン
plotHopingAlphabets :: IO ()
plotHopingAlphabets = do
    let alphabets = generateHopingAlphabets alphabetsNum
    world alphabets

-- ホッピングするアルファベットの初期値を乱数で生成する
generateHopingAlphabets :: Int -> [HopingAlphabet]
generateHopingAlphabets num = [(HopingAlphabet 'a' ((fromIntegral height) / 2, (fromIntegral width) / 2) (0.0, 0.5))]

-- フレーム
world :: [HopingAlphabet] -> IO()
world alphabets = do
    let screen = makeScreen (height * width - 1) alphabets ""
    callCommand "cls"
    putStrLn screen
    world $ updateAlphabets alphabets

-- 画面を作る
makeScreen :: Int -> [HopingAlphabet] -> String -> String
makeScreen (-1) _ screen = screen
makeScreen index alphabets screen = makeScreen (index - 1) alphabets ((makeScreen2 index alphabets) ++ screen)
    where makeScreen2 i as = makePixel (index2hw i) as

-- インデックスを一次元から二次元に変換する
index2hw :: Int -> (Int, Int)
index2hw index = (index `div` width, index `mod` width)

-- 画面上のインデックス位置から、その位置の文字列を返す
makePixel :: (Int, Int) -> [HopingAlphabet] -> String
makePixel (h, w) alphabets
    | w == (width - 1)                      = "#\n"
    | w == 0 || h == 0 || h == (height - 1) = "#"
    | otherwise                             = makePixel2 (h, w) alphabets

makePixel2 :: (Int, Int) -> [HopingAlphabet] -> String
makePixel2 (h, w) alphabets = let result = find (\alphabet -> (round(fst(position alphabet))) == h && (round(snd(position alphabet))) == w) alphabets
                              in case result of
                                Just alpha -> [character alpha]
                                _ -> " "

-- アルファベットの時間を進める
updateAlphabets :: [HopingAlphabet] -> [HopingAlphabet]
updateAlphabets alphabets = map updateAlphabet alphabets
    where updateAlphabet (HopingAlphabet char pos vel) = let vel' = (fst(vel) + gravity, snd(vel))
                                                             pos' = (fst(pos) + fst(vel'), fst(pos) + snd(vel'))
                                                             vel'y = fst(vel')
                                                             pos'y = fst(pos')
                                                             (newVelY, newPosY) = if pos'y < 0 || pos'y >= (fromIntegral height)
                                                                                    then (-vel'y, pos'y - vel'y)
                                                                                    else (vel'y, pos'y)
                                                             vel'x = snd(vel')
                                                             pos'x = snd(pos')
                                                             (newVelX, newPosX) = if pos'x < 0 || pos'x >= (fromIntegral width)
                                                                                    then (-vel'x, pos'x - vel'x)
                                                                                    else (vel'x, pos'x)
                                                         in HopingAlphabet char (newPosY, newPosX) (newVelY, newVelX)