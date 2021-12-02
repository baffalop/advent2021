module Lib.Utils where

maybeIf :: Bool -> a -> Maybe a
maybeIf True x = Just x
maybeIf False _ = Nothing

pairs :: [a] -> [(a, a)]
pairs = withConsecutive (,)

withConsecutive :: (a -> a -> b) -> [a] -> [b]
withConsecutive f xs = zipWith f xs (drop 1 xs)

dup :: a -> (a, a)
dup x = (x, x)