import Data.List

main :: IO()
main = do
    print $ (setupRobots [0, 1] "LR") 3 == [-3, 4]
    print $ (setupRobots [-2, 0, 2] "RLL") 2 == [-2, 0, 0]
    print $ (setupRobots [-2, 0, 2] "RLL") 5 == [-5, -3, 3]
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1 == [-1,-1,0,2,5,8,9,13,14]
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3 == [-3,-2,0,1,7,7,10,12,15]
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5 == [-5,-4,-2,3,5,9,10,12,17]

type Position = Int
type Direction = Char
type Id = Int
type Robot = (Position, Direction, Id)

setupRobots :: [Int] -> String -> (Int -> [Int])
setupRobots xs ms = \seconds -> map (\(pos, _, _) -> pos) $ helper seconds (zip3 xs ms [0..])
  where
    helper :: Int -> [Robot] -> [Robot]
    helper 0 robots = robots
    helper seconds robots = helper (seconds - 1) (moveRobots robots)

    mqoveRobots :: [Robot] -> [Robot]
    moveRobots robots = checkForEquivalentPositions $ sortOn (\(pos, _, _) -> pos) (map moveOneRobot robots)

    moveOneRobot :: Robot -> Robot
    moveOneRobot (pos, dir, idx) = (pos + (if dir == 'R' then 1 else -1), dir, idx)

    checkForEquivalentPositions :: [Robot] -> [Robot]
    checkForEquivalentPositions (a@(posA, dirA, idxA) : b@(posB, dirB, idxB) : rest)
      | posA == posB = (posA, changeDirection dirA, idxA) : (posB, changeDirection dirB, idxB) : checkForEquivalentPositions rest
      | otherwise = a : checkForEquivalentPositions (b : rest)
    checkForEquivalentPositions robots = robots

    changeDirection :: Direction -> Direction
    changeDirection 'R' = 'L'
    changeDirection 'L' = 'R'