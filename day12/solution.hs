import Data.Char

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

type Cave = String
type Connection = (Cave, Cave)

lineToPair :: [String] -> Connection
lineToPair arr = (arr!!0, arr!!1)

directionsFrom :: Cave -> [Connection] -> [Cave]
directionsFrom cave [] = []
directionsFrom cave ((from, to):restConnections)
  | cave == from = [to] ++ restDirections
  | cave == to = [from] ++ restDirections
  | otherwise = restDirections
  where restDirections = directionsFrom cave restConnections

isBigCave :: Cave -> Bool
isBigCave = all isUpper

allowVisitEachSmallCaveOnlyOnce :: Cave -> [Cave] -> Bool
allowVisitEachSmallCaveOnlyOnce currentCave currentPath = not(elem currentCave currentPath) || isBigCave currentCave

canVisitCaveOnceMore :: [Cave] -> Bool
canVisitCaveOnceMore currentPath =
  all (\cave -> (length $ filter (==cave) currentPath) == 1) smallCaves
  where smallCaves = filter (\cave -> not(isBigCave cave)) currentPath

allowVisitOneSmallCaveTwice :: Cave -> [Cave] -> Bool
allowVisitOneSmallCaveTwice currentCave currentPath
  | isBigCave currentCave = True
  | currentCave == "start" && elem currentCave currentPath = False
  | not(elem currentCave currentPath) = True
  | canVisitCaveOnceMore currentPath = True
  | otherwise = False

findNextPaths :: Cave -> [Cave] -> [Connection] -> (Cave -> [Cave] -> Bool) -> [[Cave]]
findNextPaths "end" currentPath _ _ = [currentPath ++ ["end"]]
findNextPaths currentCave currentPath connections alllowVisit
  | alllowVisit currentCave currentPath = foldl (\acc nextCave -> acc ++ findNextPaths nextCave newPath connections alllowVisit) [] directions
  | otherwise = []
  where
    directions = directionsFrom currentCave connections
    newPath = (currentPath ++ [currentCave])

findAllPaths :: [Connection] -> (Cave -> [Cave] -> Bool) -> [[Cave]]
findAllPaths connections predicate = findNextPaths "start" [] connections predicate

main = do
  content <- readFile "input.txt"
  let strings = lines(content)
  let connections = map (\line -> lineToPair (wordsWhen (=='-') line)) strings

  let solution1 = length $ findAllPaths connections (\currentCave currentPath -> allowVisitEachSmallCaveOnlyOnce currentCave currentPath)
  putStrLn("Solution 1 is " ++ show(solution1))

  print $ canVisitCaveOnceMore ["a", "a"]

  let solution2 = length $ findAllPaths connections (\currentCave currentPath -> allowVisitOneSmallCaveTwice currentCave currentPath)
  putStrLn("Solution 2 is " ++ show(solution2))
