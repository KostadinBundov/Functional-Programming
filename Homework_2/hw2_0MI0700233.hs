main :: IO()

main = do
    print $ warmerAfter [20,21,20,19,18,20,25,24,23,20,26] == [1,5,4,2,1,1,4,3,2,1,0]
    print $ warmerAfter [0,10,20,30] == [1,1,1,0]
    print $ warmerAfter [21,22,23] == [1,1,0]
    print $ warmerAfter [23,24,25,21,19,23,26,23] == [1,1,4,2,1,1,0,0]

warmerAfter :: [Double] -> [Int]
warmerAfter ts = map (\(t, idx) -> getDays t (drop (idx + 1) ts)) (zip ts [0..])
  where
    getDays :: Double -> [Double] -> Int
    getDays currTemp remainingTemps = 
      let days = length $ takeWhile (\temp -> temp <= currTemp) remainingTemps
      in if days == length remainingTemps then 0 else days + 1


-----------------------------------------------------------------------------------------------

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

    moveRobots :: [Robot] -> [Robot]
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