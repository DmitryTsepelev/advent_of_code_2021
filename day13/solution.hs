import Data.List

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

data Position = Position { x :: Int, y :: Int } deriving (Eq, Show)
data Fold = FoldX Int | FoldY Int deriving (Show)
type Paper = [Position]

printPaper :: Paper -> IO ()
printPaper paper = printPaper' paper (0, 0)
printPaper' paper position@(currentX, currentY)
  | currentY > maxY = return ()
  | currentX > maxX = do
    putStrLn ""
    printPaper' paper (0, currentY + 1)
  | Position currentX currentY `elem` paper = do
    putStr "#"
    printPaper' paper nextPosition
  | otherwise = do
    putStr "."
    printPaper' paper nextPosition
  where
    maxX = maximum $ map x paper
    maxY = maximum $ map y paper
    nextPosition = (currentX + 1, currentY)

-- Parse input

parseFold :: String -> Fold
parseFold line =
  constructor (read (drop 13 line)::Int)
  where
    constructor
      | "fold along x=" `isPrefixOf` line = FoldX
      | "fold along y=" `isPrefixOf` line = FoldY
      | otherwise = error "illegal fold"

parsePositions :: String -> Position
parsePositions line =
  Position (read (head cmp)::Int) (read (last cmp)::Int)
  where
    cmp = wordsWhen (==',') line

parseLine :: String -> Maybe (Either Position Fold)
parseLine line
  | "fold along" `isPrefixOf` line = Just (Right (parseFold line))
  | not (null line) = Just (Left (parsePositions line))
  | otherwise = Nothing

parseInput :: [String] -> (Paper, [Fold])
parseInput [] = ([], [])
parseInput (line:rest)
  | Just (Left position) <- parsedLine = (position:restPositions, restFolds)
  | Just (Right fold) <- parsedLine = (restPositions, fold:restFolds)
  | otherwise = (restPositions, restFolds)
  where
    parsedLine = parseLine line
    (restPositions, restFolds) = parseInput rest

-- Folding

addPosition :: Paper -> Paper -> Position -> Paper
addPosition initialPaper paper position
  | position `elem` initialPaper = paper
  | otherwise = position:paper

performFold :: Paper -> Fold -> Paper
performFold paper = performFold' paper paper
performFold' _ [] fold = []
performFold' initialPaper (position:rest) fold
  | FoldY foldY <- fold
  , y position > foldY = insert (Position (x position) (2 * foldY - y position))
  | FoldX foldX <- fold
  , x position > foldX = insert (Position (2 * foldX - x position) (y position))
  | otherwise = position:restFold
  where
    restFold = performFold' initialPaper rest fold
    insert position = addPosition initialPaper restFold position

performFolds :: Paper -> [Fold] -> Paper
performFolds = foldl performFold

main = do
  input <- readFile "input.txt"
  let (paper, folds) = parseInput $ lines input

  let solution1 = length $ performFold paper (head folds)
  putStrLn("Solution 1 is " ++ show solution1)

  putStrLn "Solution 2:"
  printPaper $ performFolds paper folds
