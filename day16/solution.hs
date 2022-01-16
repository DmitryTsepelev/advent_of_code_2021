import Data.List
import qualified Data.Map as Map
import Numeric (readHex)
import Text.Printf (printf)

-- Conversions

hexCharToBinary :: Char -> String
hexCharToBinary c = case readHex [c] of
  (x,_):_ -> printf "%04b" (x::Int)
  _       -> "wrong character"

binaryToInt :: String -> Int
binaryToInt bits =
  fst $ foldr (\bit (result, power) -> (result + component bit * 2 ^ power, power + 1) ) (0, 0) bits
  where
    component '1' = 1
    component '0' = 0
    component _ = error "binaryToInt failed"

hexToBinary :: Hex -> Binary
hexToBinary = foldl (\acc char -> acc ++ hexCharToBinary char) ""

-- For both packet types

type Hex = String
type Binary = String

data PacketType =
  SumType | ProductType | MinumumType | MaximumType | LiteralType | GtType | LtType | EqType
  deriving (Show, Eq, Bounded, Enum, Ord)

numberToPacketType :: Int -> PacketType
numberToPacketType idx = [SumType .. EqType] !! idx

data Header = Header { version :: Int, packetType :: PacketType }
data LengthTypeId = BitLength Int | NumberOfSubpackets Int
data Payload = LiteralPayload Int | OperatorPayload { lengthTypeId :: LengthTypeId, subpackets :: [Packet] }
data Packet = Packet { header :: Header, payload :: Payload }

parsePacket :: Binary -> (Packet, Binary)
parsePacket binary =
  parse header binaryWithoutHeader
  where
    (header, binaryWithoutHeader) = parseHeader binary
    number = packetType header
    parse
      | LiteralType <- number = parseLiteral
      | otherwise = parseOperator

versionLength = 3
typeNumberLength = 3

parseHeader :: Binary -> (Header, Binary)
parseHeader binary =
  (Header version typeNumber, restBytes)
  where
    version = binaryToInt . take versionLength $ binary
    typeNumber = numberToPacketType . binaryToInt . take typeNumberLength . drop versionLength $ binary
    restBytes = drop (versionLength + typeNumberLength) binary

-- For Literal

parseLiteral :: Header -> Binary -> (Packet, Binary)
parseLiteral header binary =
  (Packet header (LiteralPayload value), restBinary)
  where
    (value, restBinary) = readLiteral binary

readLiteral :: Binary -> (Int, Binary)
readLiteral binary =
  (literal, restBinary)
  where
    literal = binaryToInt $ concat groups
    restBinary = drop (5 * length groups) binary
    groups = readLiteralGroups binary

readLiteralGroups :: Binary -> [Binary]
readLiteralGroups binary
  | "1" `isPrefixOf` binary = currentNumber:restNumbers
  | otherwise = [currentNumber]
  where
    currentNumber = take 4 . drop 1 $ paddedBinary
    restNumbers = readLiteralGroups $ drop 5 paddedBinary
    paddedBinary = binary ++ repeat '0'

-- For Operator

parseOperator :: Header -> Binary -> (Packet, Binary)
parseOperator header binary =
  (Packet header (OperatorPayload lengthTypeId subpackets), restBinary)
  where
    (lengthTypeId, binaryWithoutLengthTypeId) = parseLengthTypeId binary
    (subpackets, restBinary) = parseSubpackets binaryWithoutLengthTypeId lengthTypeId

bitLengthByte = 0
numberOfSubpacketsByte = 1

parseLengthTypeId :: Binary -> (LengthTypeId, Binary)
parseLengthTypeId binary
  | lengthByte == bitLengthByte = buildWith BitLength 15
  | lengthByte == numberOfSubpacketsByte = buildWith NumberOfSubpackets 11
  | otherwise = error "illegal length byte"
  where
    lengthByte = binaryToInt . take 1 $ binary
    binaryAfterLengthByte = drop 1 binary
    buildWith constructor length =
      (constructor (binaryToInt . take length $ binaryAfterLengthByte), drop length binaryAfterLengthByte)

parseSubpackets :: Binary -> LengthTypeId -> ([Packet], Binary)
parseSubpackets binary lengthTypeId
  | BitLength bitLength <- lengthTypeId = parseBitLength binary bitLength
  | NumberOfSubpackets numberOfSubpackets <- lengthTypeId = parseWithNumberOfSubpackets binary numberOfSubpackets
  | otherwise = error "illegal lengthTypeId"

parseBitLength :: Binary -> Int -> ([Packet], Binary)
parseBitLength binary 0 = ([], binary)
parseBitLength binary bitLength =
  (parsedPacket:restPackets, restBinary)
  where
    (parsedPacket, binaryWithoutFirstPacket) = parsePacket binary
    (restPackets, restBinary) = parseBitLength binaryWithoutFirstPacket newBitLength
    newBitLength = bitLength - (length binary - length binaryWithoutFirstPacket)

parseWithNumberOfSubpackets :: Binary -> Int -> ([Packet], Binary)
parseWithNumberOfSubpackets binary 0 = ([], binary)
parseWithNumberOfSubpackets binary numberOfSubpackets =
  (parsedPacket:restPackets, restBinary)
  where
    (parsedPacket, binaryWithoutFirstPacket) = parsePacket binary
    (restPackets, restBinary) = parseWithNumberOfSubpackets binaryWithoutFirstPacket (numberOfSubpackets - 1)

-- Solutions

execute :: Packet -> Int
execute packet
  | LiteralPayload value <- payload packet = value
  | OperatorPayload _ subpackets <- payload packet = performOperation (header packet) $ map execute subpackets
  where
    performOperation header = operations Map.! packetType header

    operations =
      Map.fromList $
        zip
        [SumType .. EqType]
        [sum, product, minimum, maximum, (\_ -> 0), buildComparison GT, buildComparison LT, buildComparison EQ]

    buildComparison expected values
      | expected == compare (head values) (values !! 1) = 1
      | otherwise = 0

sumVersions :: Packet -> Int
sumVersions packet
  | LiteralPayload _ <- payload packet = version $ header packet
  | OperatorPayload _ _ <- payload packet =
    version (header packet) + foldl (\acc packet -> acc + sumVersions packet) 0 (subpackets (payload packet))

main = do
  input <- readFile "input.txt"
  let hexPacket = head $ lines input
  let packet = fst . parsePacket . hexToBinary $ hexPacket
  putStrLn $ "Solution 1 is " ++ show (sumVersions packet)
  putStrLn $ "Solution 2 is " ++ show (execute packet)
