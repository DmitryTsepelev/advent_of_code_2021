import qualified Data.Map as Map

type Position = (Int, Int)

data CellType = CucumberRight | CucumberDown | Empty deriving (Eq)
instance Show CellType where
   show CucumberRight = ">"
   show CucumberDown = "v"
   show Empty = "."

type Field = Map.Map Position CellType

-- Input parsing

buildCell :: Int -> Int -> Char -> (Position, CellType)
buildCell rowIdx colIdx '>' = ((rowIdx, colIdx), CucumberRight)
buildCell rowIdx colIdx 'v' = ((rowIdx, colIdx), CucumberDown)
buildCell rowIdx colIdx '.' = ((rowIdx, colIdx), Empty)

parseLine :: String -> Int -> Field
parseLine lines rowIdx = parseLine' lines rowIdx 0
parseLine' [] _ _ = Map.empty
parseLine' (c:rest) rowIdx colIdx =
  Map.insert position cellType restMap
  where
    (position, cellType) = buildCell rowIdx colIdx c
    restMap = parseLine' rest rowIdx (colIdx + 1)

buildField :: [String] -> Field
buildField lines = buildField' lines 0
buildField' [] _ = Map.empty
buildField' (line:rest) rowIdx =
  Map.union
    (parseLine line rowIdx)
    (buildField' rest (rowIdx + 1))

-- Field functions

rowCount = 137
columnCount = 139

eachCell :: Field -> (Field -> Position -> Field) -> Field
eachCell field = eachCell' field (0, 0)

eachCell' field (rowIdx, colIdx) loopHandler
  | colIdx == columnCount = eachCell' field nextRowPosition loopHandler
  | rowIdx == rowCount = field
  | otherwise = eachCell' (loopHandler field (rowIdx, colIdx)) nextColPosition loopHandler
  where
    nextRowPosition = (rowIdx + 1, 0)
    nextColPosition = (rowIdx, colIdx + 1)

fieldToString :: Field -> String
fieldToString field = fieldToString' field (0, 0) False []
fieldToString' field (rowIdx, colIdx) newLine acc
  | colIdx == columnCount = fieldToString' field nextRowPosition True acc
  | rowIdx == rowCount = acc
  | newLine = fieldToString' field nextColPosition False (acc ++ "\n" ++ show currentType)
  | otherwise = fieldToString' field nextColPosition False (acc ++ show currentType)
  where
    nextRowPosition = (rowIdx + 1, 0)
    nextColPosition = (rowIdx, colIdx + 1)
    currentType = field Map.! (rowIdx, colIdx)

getRightPosition :: Position -> Position
getRightPosition (rowIdx, colIdx)
  | colIdx == columnCount - 1 = (rowIdx, 0)
  | otherwise = (rowIdx, colIdx + 1)

getDownPosition :: Position -> Position
getDownPosition (rowIdx, colIdx)
  | rowIdx == rowCount - 1 = (0, colIdx)
  | otherwise = (rowIdx + 1, colIdx)

-- Simulation

moveCucumbersRight :: Field -> Field
moveCucumbersRight initialField = eachCell initialField (moveCucumber CucumberRight getRightPosition initialField)

moveCucumbersDown :: Field -> Field
moveCucumbersDown initialField = eachCell initialField (moveCucumber CucumberDown getDownPosition initialField)

moveCucumber cucumberType getNextPosition initialField currentField position
  | cucumberType == currentType, nextType == Empty =
    foldl
      (\field (position, cellType) -> Map.insert position cellType field)
      currentField
      [(position, Empty), (nextPosition, currentType)]
  | otherwise = currentField
  where
    currentType = initialField Map.! position
    nextType = initialField Map.! nextPosition
    nextPosition = getNextPosition position

makeStep :: Field -> Field
makeStep = moveCucumbersDown . moveCucumbersRight

solve :: Field -> Int
solve field = solve' field 1
solve' field iteration
  | nextField == field = iteration
  | otherwise = solve' nextField (iteration + 1)
  where nextField = makeStep field

-- --------------------------------------------

main = do
  content <- readFile "input.txt"
  let field = buildField $ lines content
  putStrLn("Solution is " ++ show(solve field))
