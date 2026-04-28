module Graph () where

import qualified Data.AssocMap as M

type DiGraph a = M.AssocMap a [a]

empty :: DiGraph a
empty = M.empty

hasNode :: (Eq a) => DiGraph a -> a -> Bool
hasNode = flip M.member

addNode :: (Eq a) => a -> DiGraph a -> DiGraph a
addNode =
  M.alter
    ( \mNodes ->
        case mNodes of
          Nothing -> Just []
          value -> value
    )
