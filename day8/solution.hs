import Data.List

type Pattern = String

getDigitPatterns :: [String] -> [Pattern]
getDigitPatterns ("|":digits) = digits
getDigitPatterns arr = getDigitPatterns $ tail arr

getPatterns :: [String] -> [Pattern]
getPatterns arr = getPatterns' arr []
getPatterns' ("|":_) acc = acc
getPatterns' (current:rest) acc = getPatterns' rest (acc ++ [current])

-- Decoded patterns

type DecodedPatterns = [(Pattern, Int)]

patternFor :: Int -> DecodedPatterns -> Pattern
patternFor _ [] = "" -- unreachable
patternFor expectedDigit ((pattern, digit):restPatterns)
  | digit == expectedDigit = pattern
  | otherwise = patternFor expectedDigit restPatterns

digitFor :: Pattern -> DecodedPatterns -> Int
digitFor _ [] = 0 -- unreachable
digitFor expectedPattern ((pattern, digit):restPatterns)
  | pattern == expectedPattern = digit
  | otherwise = digitFor expectedPattern restPatterns

-- --------------------------------------------------------

detectSimpleDigit :: Pattern -> Maybe Int
detectSimpleDigit pattern
  | length pattern == 2 = Just 1
  | length pattern == 3 = Just 7
  | length pattern == 4 = Just 4
  | length pattern == 7 = Just 8
  | otherwise = Nothing

countDigits :: [Pattern] -> Int
countDigits row = countDigits' (map detectSimpleDigit row) 0
countDigits' [] count = count
countDigits' (current:rest) count
  | Just _ <- current = countDigits' rest (count + 1)
  | otherwise = countDigits' rest count

solution1 :: [[String]] -> Int
solution1 rows = foldl (\acc row -> acc + (countDigits (getDigitPatterns row))) 0 rows

-- --------------------------------------------------------

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ []    = False
subList [] _    = True
subList (x:xs) (y:ys)
    | x == y    = subList xs ys
    | otherwise = subList (x:xs) ys

decodeSimpleDigits :: [Pattern] -> DecodedPatterns
decodeSimpleDigits [] = []
decodeSimpleDigits (currentPattern:restPatterns)
  | Just justDecodedDigit <- decodedDigit = [(currentPattern, justDecodedDigit)] ++ (decodeSimpleDigits restPatterns)
  | otherwise = decodeSimpleDigits restPatterns
  where
    decodedDigit = detectSimpleDigit currentPattern

decodeThree :: DecodedPatterns -> [Pattern] -> DecodedPatterns
decodeThree decodedPatterns patterns
  | Just justNinePattern <- ninePattern = [(justNinePattern, 3)]
  | otherwise = [] -- unreachable
  where
    ninePattern = find (\pattern -> (length pattern) == 5 && (subList onePattern pattern)) patterns
    onePattern = patternFor 1 decodedPatterns

decodeZero :: DecodedPatterns -> [Pattern] -> DecodedPatterns
decodeZero decodedPatterns patterns
  | Just justZeroPattern <- zeroPattern = [(justZeroPattern, 0)]
  | otherwise = [] -- unreachable
  where
    zeroPattern = find (\pattern -> (subList onePattern pattern) && not (subList threePattern pattern)) candidates
    onePattern = patternFor 1 decodedPatterns
    threePattern = patternFor 3 decodedPatterns

    candidates = filter ((==6) . length) patterns

decodeSixAndNine :: DecodedPatterns -> [Pattern] -> DecodedPatterns
decodeSixAndNine decodedPatterns patterns
  | Just justNinePattern <- ninePattern
  , Just justSixPattern <- sixPattern
  = [(justSixPattern, 6), (justNinePattern, 9)]
  | otherwise = [] -- unreachable
  where
    ninePattern = find (\pattern -> (subList onePattern pattern)) candidates
    sixPattern = find (\pattern -> not (subList onePattern pattern)) candidates
    onePattern = patternFor 1 decodedPatterns

    candidates = filter ((==6) . length) patterns

decodeTwoAndFive :: DecodedPatterns -> [Pattern] -> DecodedPatterns
decodeTwoAndFive decodedPatterns patterns
  | Just justTwoPattern <- twoPattern, Just justFivePattern <- fivePattern = [(justTwoPattern, 2), (justFivePattern, 5)]
  | otherwise = [] -- unreachable
  where
    twoPattern = find (\pattern -> differentSector `elem` pattern) candidates
    fivePattern = find (\pattern -> differentSector `notElem` pattern) candidates
    candidates = filter ((==5) . length) patterns
    differentSector = head $ (patternFor 8 decodedPatterns) \\ (patternFor 6 decodedPatterns)

decode :: [Pattern] -> DecodedPatterns
decode patterns = do
  let decodedPatterns = decodeSimpleDigits patterns
  let patterns2 = patterns \\ map fst decodedPatterns
  let decodedPatterns2 = decodedPatterns ++ (decodeThree decodedPatterns patterns2)

  let patterns3 = patterns \\ map fst decodedPatterns
  let decodedPatterns3 = decodedPatterns2 ++ (decodeZero decodedPatterns2 patterns3)

  let patterns4 = patterns2 \\ map fst decodedPatterns3
  let decodedPatterns4 = decodedPatterns3 ++ (decodeSixAndNine decodedPatterns3 patterns4)
  let patterns5 = patterns3 \\ map fst decodedPatterns4
  decodedPatterns4 ++ (decodeTwoAndFive decodedPatterns4 patterns5)

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl (\acc currentDigit -> acc * 10 + currentDigit) 0

solution2 :: [[String]] -> Int
solution2 rows = solution2' rows 0
solution2' [] acc = acc
solution2' (row:rest) acc =
  solution2' rest newAcc
  where
    decodedPatterns = decode patterns
    patterns = getPatterns row
    resultPatterns = getDigitPatterns row
    newAcc = acc + (digitsToNumber $ map (\pattern -> digitFor pattern decodedPatterns) resultPatterns)

-- --------------------------------------------------------

main = do
  content <- readFile "input.txt"
  let rows = map (\line -> map sort (words line)) $ lines content

  putStrLn("Solution 1 is " ++ show(solution1 rows))
  putStrLn("Solution 2 is " ++ show(solution2 rows))
