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
patternFor _ [] = error "unreachable"
patternFor expectedDigit ((digitPattern, digit):restPatterns)
  | digit == expectedDigit = digitPattern
  | otherwise = patternFor expectedDigit restPatterns

digitFor :: Pattern -> DecodedPatterns -> Int
digitFor _ [] = error "unreachable"
digitFor expectedPattern ((digitPattern, digit):restPatterns)
  | digitPattern == expectedPattern = digit
  | otherwise = digitFor expectedPattern restPatterns

-- --------------------------------------------------------

detectSimpleDigit :: Pattern -> Maybe Int
detectSimpleDigit digitPattern
  | length digitPattern == 2 = Just 1
  | length digitPattern == 3 = Just 7
  | length digitPattern == 4 = Just 4
  | length digitPattern == 7 = Just 8
  | otherwise = Nothing

countDigits :: [Pattern] -> Int
countDigits row = countDigits' (map detectSimpleDigit row) 0
countDigits' [] count = count
countDigits' (current:rest) count
  | Just _ <- current = countDigits' rest (count + 1)
  | otherwise = countDigits' rest count

solution1 :: [[String]] -> Int
solution1
  = foldl (\acc row -> acc + countDigits (getDigitPatterns row)) 0

-- --------------------------------------------------------

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ [] = False
subList [] _ = True
subList (x:xs) (y:ys)
  | x == y    = subList xs ys
  | otherwise = subList (x:xs) ys

decodeSimpleDigits :: DecodedPatterns -> [Pattern] -> DecodedPatterns
decodeSimpleDigits _ [] = []
decodeSimpleDigits decodedPatterns (currentPattern:restPatterns)
  | Just justDecodedDigit <- decodedDigit = newDecodedPatterns justDecodedDigit
  | otherwise = decodeSimpleDigits decodedPatterns restPatterns
  where
    newDecodedPatterns digit = (currentPattern, digit) : decodeSimpleDigits decodedPatterns restPatterns
    decodedDigit = detectSimpleDigit currentPattern

decodeThree :: DecodedPatterns -> [Pattern] -> DecodedPatterns
decodeThree decodedPatterns patterns
  | Just justNinePattern <- ninePattern = [(justNinePattern, 3)]
  | otherwise = error "unreachable"
  where
    ninePattern = find (\digitPattern -> length digitPattern == 5 && subList onePattern digitPattern) patterns
    onePattern = patternFor 1 decodedPatterns

decodeZero :: DecodedPatterns -> [Pattern] -> DecodedPatterns
decodeZero decodedPatterns patterns
  | Just justZeroPattern <- zeroPattern = [(justZeroPattern, 0)]
  | otherwise = error "unreachable"
  where
    zeroPattern = find (\digitPattern -> subList onePattern digitPattern && not (subList threePattern digitPattern)) candidates
    onePattern = patternFor 1 decodedPatterns
    threePattern = patternFor 3 decodedPatterns

    candidates = filter ((==6) . length) patterns

decodeSixAndNine :: DecodedPatterns -> [Pattern] -> DecodedPatterns
decodeSixAndNine decodedPatterns patterns
  | Just justNinePattern <- ninePattern
  , Just justSixPattern <- sixPattern
  = [(justSixPattern, 6), (justNinePattern, 9)]
  | otherwise = error "unreachable"
  where
    ninePattern = find (subList onePattern) candidates
    sixPattern = find (not . subList onePattern) candidates
    onePattern = patternFor 1 decodedPatterns

    candidates = filter ((==6) . length) patterns

decodeTwoAndFive :: DecodedPatterns -> [Pattern] -> DecodedPatterns
decodeTwoAndFive decodedPatterns patterns
  | Just justTwoPattern <- twoPattern
  , Just justFivePattern <- fivePattern
  = [(justTwoPattern, 2), (justFivePattern, 5)]
  | otherwise = error "unreachable"
  where
    twoPattern = find (\digitPattern -> differentSector `elem` digitPattern) candidates
    fivePattern = find (\digitPattern -> differentSector `notElem` digitPattern) candidates
    candidates = filter ((==5) . length) patterns
    differentSector = head $ patternFor 8 decodedPatterns \\ patternFor 6 decodedPatterns

applyDecoder :: (DecodedPatterns -> [Pattern] -> DecodedPatterns) -> [Pattern] -> DecodedPatterns -> ([Pattern], DecodedPatterns)
applyDecoder decoder patterns decodedPatterns =
  (restPatterns, newDecodedPatterns)
  where
    newDecodedPatterns = decodedPatterns ++ decoder decodedPatterns patterns
    restPatterns = patterns \\ map fst newDecodedPatterns

decode :: [Pattern] -> DecodedPatterns
decode patterns = do
  snd $ foldl (\(patterns, decodedPatterns) decoder -> applyDecoder decoder patterns decodedPatterns) (patterns, []) decoders
  where decoders = [decodeSimpleDigits, decodeThree, decodeZero, decodeSixAndNine, decodeTwoAndFive]

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
    newAcc = acc + digitsToNumber (map (`digitFor` decodedPatterns) resultPatterns)

-- --------------------------------------------------------

main = do
  content <- readFile "input.txt"
  let rows = map (map sort . words) $ lines content

  putStrLn("Solution 1 is " ++ show(solution1 rows))
  putStrLn("Solution 2 is " ++ show(solution2 rows))
