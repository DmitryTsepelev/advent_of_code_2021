import qualified Data.Map as Map
import Data.List

data Color = Light | Dark deriving ( Eq)
instance Show Color where
  show Light = "#"
  show Dark = "."

type Position = (Int, Int)
type Square = [Color]
type Image = Map.Map Position Color

type Algorithm = Map.Map Int Color

getColor :: Char -> Color
getColor '#' = Light
getColor '.' = Dark
getColor _ = error "wrong input"

-- Input parsing

buildLine :: Int -> String -> Image
buildLine rowIdx = fst . foldl
  (\(acc, colIdx) char ->
    (Map.insert (rowIdx, colIdx) (getColor char) acc, colIdx + 1))
  (Map.empty, 0)

buildImage :: [String] -> Image
buildImage = fst . foldl
  (\(image, rowIdx) line
    -> (Map.union image (buildLine rowIdx line), rowIdx + 1))
  (Map.empty, 0)

getInput :: [String] -> (Algorithm, Image)
getInput lines =
  (algorithm, image)
  where
    algorithm = Map.fromList $ fst $ foldl (\(acc, position) char -> (acc ++ [(position, getColor char)], position + 1)) ([], 0) (head lines)
    image = buildImage $ take (length lines - 2) $ drop 2 lines

-- Image helpers

getPixelSquare :: Image -> Position -> Color -> Square
getPixelSquare image (rowIdx, colIdx) outsideColor =
  map (\position -> Map.findWithDefault outsideColor position image) positions
  where
    positions = [(rowIdx + cx, colIdx + cy) | cx <- [-1..1], cy <- [-1..1]]

pixelSquareToInt :: Square -> Int
pixelSquareToInt =
  binaryToInt . reverse . map colorToBinary
  where
    binaryToInt [] = 0
    binaryToInt (x : xs) = x + 2 * binaryToInt xs

    colorToBinary Dark = 0
    colorToBinary Light = 1

addPadding :: Image -> Color -> Image
addPadding image outsideColor =
  Map.union image padding
  where
    padding =
      Map.fromList
        [
          (position, outsideColor)
          | rowIdx <- [newMinRowIdx..newMaxRowIdx]
          , colIdx <- [newMinColIdx..newMaxColIdx]
          , let position = (rowIdx, colIdx)
          , position `notElem` Map.keys image
        ]

    newMinRowIdx = getMinIndex fst - offset
    newMinColIdx = getMinIndex snd - offset
    getMinIndex = minimum . getPositions

    newMaxRowIdx = getMaxIndex fst + offset
    newMaxColIdx = getMaxIndex snd + offset
    getMaxIndex = maximum . getPositions

    getPositions getter = map getter (Map.keys image)

    offset = 1

transformPixel :: Image -> Algorithm -> Color -> (Position, Color) -> Color
transformPixel image algorithm outsideColor (position, _) = newColor
  where
    newColor = algorithm Map.! code
    square = getPixelSquare image position outsideColor
    code = pixelSquareToInt square

applyAlgorithm :: Image -> Algorithm -> Color -> Image
applyAlgorithm image algorithm outsideColor = Map.mapWithKey (curry (transformPixel image algorithm outsideColor)) image

enchance :: Image -> Algorithm -> Color -> (Image, Color)
enchance image algorithm outsideColor =
  (newImage, newOutsideColor)
  where
    newImage = applyAlgorithm (addPadding image outsideColor) algorithm outsideColor
    newOutsideColor
      | algorithm Map.! 0 == Dark || outsideColor == Light = Dark
      | otherwise = Light

enchanceN :: Int -> Color -> Image -> Algorithm -> Image
enchanceN 0 _ image _ = image
enchanceN iterations outsideColor image algorithm =
  enchanceN (iterations - 1) newOutsideColor newImage algorithm
  where
    (newImage, newOutsideColor) = enchance image algorithm outsideColor

-- Solutions

solve :: Image -> Algorithm -> Int -> Int
solve image algorithm iterations = length . filter (== Light) . Map.elems $ enchanceN iterations Dark image algorithm

main = do
  content <- readFile "input.txt"
  let (algorithm, image) = getInput $ lines content

  putStrLn("Solution 1 is " ++ show(solve image algorithm 2))
  putStrLn("Solution 2 is " ++ show(solve image algorithm 50))
