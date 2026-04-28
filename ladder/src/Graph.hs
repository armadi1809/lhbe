module Graph
  ( empty,
    addNode,
    hasNode,
    addEdge,
    addEdges,
    buildDiGraph,
    children,
    DiGraph,
  )
where

import qualified Data.AssocMap as M
import qualified Data.List as L

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

addEdge :: (Eq a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = M.alter insertEdge node
  where
    insertEdge Nothing = Just [child]
    insertEdge (Just nodes) = Just (L.nub (child : nodes))

addEdges :: (Eq a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges [] graph = graph
addEdges (x : xs) graph = addEdge x (addEdges xs graph)

buildDiGraph :: (Eq a) => [(a, [a])] -> DiGraph a
buildDiGraph nodes = go nodes M.empty
  where
    go [] graph = graph
    go ((key, value) : xs) graph = M.insert key value (go xs graph)

children :: (Eq a) => a -> DiGraph a -> [a]
children = M.findWithDefault []
