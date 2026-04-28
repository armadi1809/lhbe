module Ladder
  ( readDictionary,
    mkLadderGraph,
  )
where

import qualified Data.List as L
import qualified Graph as G
import qualified PermutationMap as PM

type Dictionary = [String]

readDictionary :: FilePath -> IO Dictionary
readDictionary filePath = do
  dictContent <- readFile filePath
  let ls = L.lines dictContent
      ws = L.map (L.filter (`L.elem` ['a' .. 'z'])) ls
  return (L.nub ws)

computeCandidates :: PM.PermutationMap -> String -> [String]
computeCandidates mp word =
  let candidates = modified ++ added ++ removed ++ [word]
      uniques = L.nub [L.sort w | w <- candidates]
      perms = L.concatMap (\x -> PM.findWithDefault [] x mp) uniques
   in L.delete word perms
  where
    added = [x : word | x <- ['a' .. 'z']]
    removed = [L.delete x word | x <- word]
    modified = [x : L.delete y word | x <- ['a' .. 'z'], y <- word, x /= y]

mkLadderGraph :: Dictionary -> G.DiGraph String
mkLadderGraph dict = G.buildDiGraph nodes
  where
    mp = PM.createPermutationMap dict
    nodes = L.map (\w -> (w, computeCandidates mp w)) dict