module Main where

import Data.List.Split
import qualified Data.IntSet as IntSet

parseNum :: String -> Int
parseNum (x:xs) | x == '+' = read xs
                | x == '-' = -1 * read xs


main :: IO ()
main = do
    handle <- readFile "./input.txt"
    let x  = init $ splitOn "\n" handle
    print $ test $  parseNum <$> x


repList :: [a] -> [a]
repList  = concat . repeat

thing ((x:xs), total) = (xs,  total + x)

genSumList xs = map snd <$> tail $ iterate thing ( xs, 0)

findRepeat :: [Int] -> Int
findRepeat (x : xs) = go x xs (IntSet.singleton 0)
    where
        go y as zs = if (IntSet.member) y zs
                     then y
                     else case as of
                               [] -> error "the grinch"
                               (b:bs) -> go b bs ((IntSet.insert) y zs)


test = findRepeat . genSumList . repList
