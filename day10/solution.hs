import Data.List

data Result = Invalid Char | Incomplete [Char] | Valid deriving(Show, Eq)

isOpenBrace :: Char -> Bool
isOpenBrace '(' = True
isOpenBrace '[' = True
isOpenBrace '{' = True
isOpenBrace '<' = True
isOpenBrace _ = False

openBraceFor :: Char -> Char
openBraceFor ')' = '('
openBraceFor ']' = '['
openBraceFor '}' = '{'
openBraceFor '>' = '<'

closingBraceFor :: Char -> Char
closingBraceFor '(' = ')'
closingBraceFor '[' = ']'
closingBraceFor '{' = '}'
closingBraceFor '<' = '>'

validate :: String -> Result
validate str = f str [] 0
f [] [] position = Valid
f [] openBraces position = Incomplete openBraces
f (currentBrace:str) openBraces position
  | isOpenBrace(currentBrace) = f str (currentBrace:openBraces) (position + 1)
  | otherwise = handleClosingBrace (currentBrace:str) expectedOpenBrace openBraces position
  where
    expectedOpenBrace = openBraceFor(currentBrace)

handleClosingBrace :: String -> Char -> [Char] -> Integer -> Result
handleClosingBrace (closingBrace:_) expectedOpenBrace [] position = Invalid(closingBrace)
handleClosingBrace (closingBrace:str) expectedOpenBrace (openBrace:restBraces) position
  | openBrace == expectedOpenBrace = f str restBraces (position + 1)
  | otherwise = Invalid(closingBrace)

errorScore :: Result -> Integer
errorScore (Invalid brace)
  | brace == ')' = 3
  | brace == ']' = 57
  | brace == '}' = 1197
  | brace == '>' = 25137
  | otherwise = 0
errorScore _ = 0

calculateErrorScore :: [Result] -> Integer
calculateErrorScore strings =
  foldr (\line acc -> acc + errorScore(line)) 0 strings

isIncomplete :: Result -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _ = False

autocomplete :: Result -> String
autocomplete (Incomplete openBraces) = map closingBraceFor openBraces
autocomplete _ = ""

autocompleteScoreForBrace :: Char -> Integer
autocompleteScoreForBrace ')' = 1
autocompleteScoreForBrace ']' = 2
autocompleteScoreForBrace '}' = 3
autocompleteScoreForBrace '>' = 4
autocompleteScoreForBrace _ = 0

autocompleteScoreForBraceSequence :: String -> Integer
autocompleteScoreForBraceSequence braces =
    foldl (\acc brace -> acc * 5 + autocompleteScoreForBrace(brace)) 0 braces

mid [] = []
mid t = m t t
  where m (x:_) [_] = [x]
        m (x:y:_) [_,_] = [x,y]
        m (_:t) (_:_:u) = m t u

main = do
  content <- readFile "input.txt"
  let strings = lines(content)
  let validatedStrings = map validate strings

  let solution1 = calculateErrorScore(validatedStrings)
  putStrLn("Solution 1 is " ++ show(solution1))

  let incompleteStrings = filter isIncomplete validatedStrings
  let autocompletes = map autocomplete incompleteStrings
  let solution2 = mid(sort(map autocompleteScoreForBraceSequence autocompletes))!!0
  putStrLn("Solution 2 is " ++ show(solution2))
