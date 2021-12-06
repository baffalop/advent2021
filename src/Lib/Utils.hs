module Lib.Utils where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

maybeIf :: Bool -> a -> Maybe a
maybeIf True x = Just x
maybeIf False _ = Nothing

pairs :: [a] -> [(a, a)]
pairs = withConsecutive (,)

withConsecutive :: (a -> a -> b) -> [a] -> [b]
withConsecutive f xs = zipWith f xs (drop 1 xs)

dup :: a -> (a, a)
dup x = (x, x)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

counts :: (Ord a, Integral n) => [a] -> Map a n
counts = foldr (flip (Map.insertWith (+)) 1) Map.empty