import Data.List
import Data.Maybe

type Rule = (String, String)

lineToPair :: String -> Rule
lineToPair str = (take 2 str, drop 6 str)

applyRule :: [Rule] -> Char -> Char -> [Char]
applyRule rules t1 t2 = t1:(snd ruleToApply)
  where
    ruleToApply = fromMaybe ("", "") maybeRule
    maybeRule = find (\rule -> fst rule == [t1] ++ [t2]) rules

insertPairs :: [Char] -> [Rule] -> [Char]
insertPairs (t1:[]) _ = [t1]
insertPairs (t1:t2:trest) rules = (applyRule rules t1 t2) ++ insertPairs (t2:trest) rules

applyRulesNTimes :: Integer -> [Char] -> [Rule] -> [Char]
applyRulesNTimes 0 template _ = template
applyRulesNTimes iterations template rules =
  applyRulesNTimes newIterations newTemplate rules
  where
    newIterations = (iterations - 1)
    newTemplate = insertPairs template rules

findFrequences :: String -> [Int]
findFrequences template =
  sort $ map (\char -> length $ filter (==char) template) allChars
  where allChars = nub template

main = do
  content <- readFile "input.txt"
  let strings = lines(content)
  let template = strings!!0
  let ruleStrings = drop 2 strings
  let rules = map (\line -> lineToPair line) ruleStrings

  let frequences = findFrequences $ applyRulesNTimes 10 template rules
  let solution1 = frequences!!(length frequences - 1) - frequences!!0
  putStrLn("Solution 1 is " ++ show(solution1))
