type Position = (Int, Int)
type Trajectory = [Position]
type Velocity = (Int, Int)
type Positions = [Int]

-- Ballistics

trajectoryY :: Int -> Positions -> Positions
trajectoryY velocity range =
  findTrajectory velocity range 0 []
  where
    findTrajectory velocity range y trajectory
      | newY < lowBorder = trajectory
      | otherwise = findTrajectory newVelocity range newY (trajectory ++ [newY])
      where
        newY = y + velocity
        lowBorder = minimum range
        newVelocity = velocity - 1

trajectoryX :: Int -> Positions -> Int -> Positions
trajectoryX velocity range maxSteps =
  findTrajectory velocity range maxSteps 0 []
  where
    findTrajectory velocity range maxSteps x trajectory
      | maxSteps == 0 = trajectory
      | otherwise = findTrajectory newVelocity range (maxSteps - 1) newX (trajectory ++ [newX])
      where
        newX = x + velocity
        leftBorder = minimum range
        rightBorder = maximum range
        newVelocity
          | velocity == 0 = 0
          | velocity < 0 = velocity + 1
          | otherwise = velocity - 1

-- Solution helpers

generateTrajectories :: Positions -> Positions -> [Trajectory]
generateTrajectories targetX targetY =
  [zip xPoints yPoints
    | yVelocity <- yVelocities
    , let yPoints = trajectoryY yVelocity targetY
    , xVelocity <- xVelocities
    , let xPoints = trajectoryX xVelocity targetX $ length yPoints
  ]
  where
    xVelocities = [0..maximum (map abs targetX)]
    yVelocities = [(-maxY)..maxY]
    maxY = maximum (map abs targetY)

hitsTargetArea :: Positions -> Positions -> Trajectory -> Bool
hitsTargetArea targetX targetY = any (\(x, y) -> x `elem` targetX && y `elem` targetY)

-- Part 1
findBestShot :: Positions -> Positions -> Int
findBestShot targetX targetY =
  findBest trajectories targetX targetY
  where
    trajectories = generateTrajectories targetX targetY

findBest :: [Trajectory] -> Positions -> Positions -> Int
findBest = findBest' 0
findBest' bestY [] _ _ = bestY
findBest' bestY (trajectory:trajectories) targetX targetY
  | hitsTargetArea targetX targetY trajectory && maxY > bestY = findBest' maxY trajectories targetX targetY
  | otherwise = findBest' bestY trajectories targetX targetY
  where
    maxY = maximum $ map snd trajectory

-- Part 2

countValidTrajectories :: Positions -> Positions -> Int
countValidTrajectories targetX targetY =
  length validTrajectories
  where
    validTrajectories = filter (hitsTargetArea targetX targetY) allTrajectories
    allTrajectories = generateTrajectories targetX targetY

main = do
  let targetX = [117..164]
  let targetY = [(-140)..(-89)]

  putStrLn("Solution 1 is " ++ show(findBestShot targetX targetY))
  putStrLn("Solution 2 is " ++ show(countValidTrajectories targetX targetY))
