type Pattern = String

getDigitPatterns :: [String] -> [Pattern]
getDigitPatterns ("|":digits) = digits
getDigitPatterns arr = getDigitPatterns $ tail arr

detectDigit :: Pattern -> Maybe Int
detectDigit pattern
  | length pattern == 2 = Just 1
  | length pattern == 3 = Just 7
  | length pattern == 4 = Just 4
  | length pattern == 7 = Just 8
  | otherwise = Nothing

countDigits :: [Pattern] -> Int
countDigits row = countDigits' (map detectDigit row) 0
countDigits' [] count = count
countDigits' (current:rest) count
  | Just _ <- current = countDigits' rest (count + 1)
  | otherwise = countDigits' rest count

solution1 :: [[String]] -> Int
solution1 rows = foldl (\acc row -> acc + (countDigits (getDigitPatterns row))) 0 rows

main = do
  content <- readFile "input.txt"
  let rows = map words $ lines content

  print("Solution 1 is " ++ show(solution1 rows))
