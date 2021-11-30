module Lib.Utils where

maybeIf :: Bool -> a -> Maybe a
maybeIf True x = Just x
maybeIf False _ = Nothing
