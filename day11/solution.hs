type Position = (Int, Int)
type Cell = (Position, Int)
type Field = [Cell]

-- Input parsing

buildCell :: Int -> Int -> Char -> Cell
buildCell rowIdx colIdx char = ((rowIdx, colIdx), (read [char] :: Int))

parseLine :: String -> Int -> Field
parseLine lines rowIdx = parseLine' lines rowIdx 0
parseLine' [] _ _ = []
parseLine' (c:rest) rowIdx colIdx = (buildCell rowIdx colIdx c):(parseLine' rest rowIdx (colIdx + 1))

buildField :: [String] -> Field
buildField lines = buildField' lines 0
buildField' [] _ = []
buildField' (line:rest) rowIdx = (parseLine line rowIdx) ++ (buildField' rest (rowIdx + 1))

-- Field functions

fieldSize = 11
flashingNow = 10
justFlashed = 11

eachCell :: Field -> Position -> (Field -> Position -> t) -> (Field -> t) -> t
eachCell field (rowIdx, colIdx) loopHandler buildReturnValue
  | colIdx == fieldSize = eachCell field nextRowPosition loopHandler buildReturnValue
  | rowIdx == fieldSize = buildReturnValue(field)
  | otherwise = loopHandler field (rowIdx, colIdx)
  where
    nextRowPosition = (rowIdx + 1, 0)

replaceLevel :: Field -> Position -> (Int -> Bool) -> (Int -> Int) -> Field
replaceLevel [] _ _ _ = []
replaceLevel ((position, level):cells) positionToChange needsReplacement getNewLevel
  | position == positionToChange, needsReplacement level = [(position, (getNewLevel level))] ++ restCells
  | otherwise = (position, level):restCells
  where
    restCells = replaceLevel cells positionToChange needsReplacement getNewLevel

changeLevel :: Field -> Position -> Int -> Field
changeLevel cell positionToChange newLevel = replaceLevel cell positionToChange (const True) (const newLevel)

increaseLevel :: Field -> Position -> Field
increaseLevel field positionToChange = replaceLevel field positionToChange (<flashingNow) (+1)

getLevel :: Field -> Position -> Int
getLevel [] _ = -1
getLevel ((position, level):cells) positionToGet
  | position == positionToGet = level
  | otherwise = getLevel cells positionToGet

-- -----------------------------------------------------------------

flash :: Field -> Field
flash field = flash' field (0, 0)
flash' field position =
  eachCell field position checkOctopusFlashes id
  where
    checkOctopusFlashes field (rowIdx, colIdx)
      | currentLevel == flashingNow = flash' (flashNeighbours field (rowIdx, colIdx)) nextPosition
      | otherwise = flash' field nextPosition
      where
        currentLevel = getLevel field (rowIdx, colIdx)
        nextPosition = (rowIdx, colIdx + 1)

    flashNeighbours field (rowIdx, colIdx) =
      foldl (\acc position -> checkOctopusFlashes (increaseLevel acc position) position) fieldWithFlashedCell cellPositions
      where
        currentLevel = getLevel field (rowIdx, colIdx)
        fieldWithFlashedCell = changeLevel field (rowIdx, colIdx) justFlashed
        cellPositions = [(rowIdx + x, colIdx + y) | x <- [-1..1], y <-[-1..1], (x,y) /= (0,0)]

increaseLevels :: Field -> Position -> Field
increaseLevels field position =
  eachCell
    field
    position
    (\field position -> increaseLevels (increaseCellLevel position) (nextPosition position))
    id
  where
    increaseCellLevel position = increaseLevel field position
    nextPosition (rowIdx, colIdx) = (rowIdx, colIdx + 1)

countFlashes :: Field -> (Field, Int)
countFlashes field = countFlashes' field (0, 0) 0
countFlashes' :: Field -> Position -> Int -> (Field, Int)
countFlashes' field position flashCount =
  eachCell
    field
    position
    (\field position -> checkOctopusFlashes field position flashCount)
    (\field -> (field, flashCount))
  where
    checkOctopusFlashes field (rowIdx, colIdx) flashCount
      | currentLevel == 11 = countFlashes' fieldAfterReplacement (rowIdx, colIdx + 1) (flashCount + 1)
      | otherwise = countFlashes' field (rowIdx, colIdx + 1) flashCount
      where
        currentLevel = getLevel field (rowIdx, colIdx)
        fieldAfterReplacement = changeLevel field (rowIdx, colIdx) 0

-- Solutions

simulate :: Field -> Int -> Int
simulate field iterations = simulate' field iterations 0
simulate' _ 0 flashCount = flashCount
simulate' field iterations flashCount =
  simulate' newField (iterations - 1) (flashCount + newFlashCount)
  where
    (newField, newFlashCount) = countFlashes $ flash $ increaseLevels field (0, 0)

syncFlashOn :: Field -> Int
syncFlashOn field = syncFlashOn' field 0
syncFlashOn' field iterations
  | all (\(_, level) -> level == 0) field = iterations
  | otherwise = syncFlashOn' newField (iterations + 1)
  where
    (newField, newFlashCount) = countFlashes $ flash $ increaseLevels field (0, 0)

main = do
  content <- readFile "input.txt"
  let field = buildField $ lines(content)

  let solution1 = simulate field 100
  putStrLn("Solution 1 is " ++ show(solution1))

  let solution2 = syncFlashOn field
  putStrLn("Solution 2 is " ++ show(solution2))

