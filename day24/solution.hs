type VariableName = Char

data VariableOrValue = Variable VariableName | Value Int deriving Show

data Instruction =
  Inp VariableName |
  Add VariableName VariableOrValue |
  Mul VariableName VariableOrValue |
  Div VariableName VariableOrValue |
  Mod VariableName VariableOrValue |
  Eql VariableName VariableOrValue
  deriving Show

type BinaryConstructor = (VariableName -> VariableOrValue -> Instruction)

type Program = [Instruction]

type Input = [Int]

data Registers = Registers {
  w :: Int,
  x :: Int,
  y :: Int,
  z :: Int
} deriving Show

-- Lex and parse

buildProgram :: [[String]] -> Program
buildProgram = foldr (\current -> (++) [buildInstruction current]) []

tokenToBinaryInstruction :: String -> BinaryConstructor
tokenToBinaryInstruction "add" = Add
tokenToBinaryInstruction "mul" = Mul
tokenToBinaryInstruction "div" = Div
tokenToBinaryInstruction "mod" = Mod
tokenToBinaryInstruction "eql" = Eql

buildInstruction :: [String] -> Instruction
buildInstruction ("inp":args) = buildUnary Inp args
buildInstruction (token:args) = buildBinary (tokenToBinaryInstruction token) args

buildUnary :: (VariableName -> Instruction) -> [String] -> Instruction
buildUnary constructor (first:_) = constructor variable where variable = head first

buildBinary :: BinaryConstructor -> [String] -> Instruction
buildBinary constructor (first:second:_) =
  constructor variable variableOrValue
  where
    variable = head first
    variableOrValue
      | second `elem` ["w", "x", "y", "z"] = Variable (head second)
      | otherwise = Value (read second::Int)

-- Registers manipulation

updateRegister :: Registers -> VariableName -> Int -> Registers
updateRegister (Registers _ x y z) 'w' w = Registers w x y z
updateRegister (Registers w _ y z) 'x' x = Registers w x y z
updateRegister (Registers w x _ z) 'y' y = Registers w x y z
updateRegister (Registers w x y _) 'z' z = Registers w x y z

readRegister :: Registers -> VariableName -> Int
readRegister (Registers w _ _ _) 'w' = w
readRegister (Registers _ x _ _) 'x' = x
readRegister (Registers _ _ y _) 'y' = y
readRegister (Registers _ _ _ z) 'z' = z

-- VM

interpret :: Program -> Input -> Registers
interpret = interpret' (Registers 0 0 0 0)
interpret' registers [] _ = registers
interpret' registers (current:rest) inputs =
  interpret' newRegisters rest newInputs
  where
    (newRegisters, newInputs) = interpretInstruction current registers inputs

interpretInstruction :: Instruction -> Registers -> Input -> (Registers, Input)
interpretInstruction instruction registers inputs
  | Inp registerToWrite <- instruction = (updateRegister registers registerToWrite (head inputs), tail inputs)
  | Add registerToWrite value <- instruction = execute (+) registerToWrite value
  | Mul registerToWrite value <- instruction = execute (*) registerToWrite value
  | Div registerToWrite value <- instruction = execute div registerToWrite value
  | Mod registerToWrite value <- instruction = execute mod registerToWrite value
  | Eql registerToWrite value <- instruction = execute equality registerToWrite value
  where
    execute func registerToWrite value =
      (updateRegister registers registerToWrite result, inputs)
      where
        result = func firstValue secondValue

        firstValue = readRegister registers registerToWrite

        secondValue
          | Value intValue <- value = intValue
          | Variable registerToRead <- value = readRegister registers registerToRead

    equality firstValue secondValue
      | firstValue == secondValue = 1
      | otherwise = 0

-- Solution

numberToDigits :: Int -> [Int]
numberToDigits = numberToDigits' []
numberToDigits' acc number
  | number `div` 10 == 0 = number:acc
  | otherwise = numberToDigits' acc (number `div` 10) ++ [number `mod` 10] ++ acc

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl (\acc currentDigit -> acc * 10 + currentDigit) 0

solve :: Program -> [[Int]] -> [Int]
solve program [] = [0]
solve program (input:newCandidates)
  | z == 0 = input
  | otherwise = solve program newCandidates
  where
    z = readRegister registers 'z'
    registers = interpret program input

main = do
  content <- readFile "input.txt"
  let program = buildProgram $ map words $ lines content
  let candidates = [numberToDigits x | x <- [99999999999999, 99999999999998..11111111111111], '0' `notElem` show x]
  -- print $ solve program candidates

  let candidate = [1..9]
  print $ map (\n -> (n, readRegister (interpret program n) 'z')) [[1],[2],[3],[4],[5],[6],[7],[8],[9]]


  print $ digitsToNumber [1,2,3]
