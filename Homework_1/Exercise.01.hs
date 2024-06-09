main :: IO()
main = do
  print $ numStepCombinations 2 == 2
  print $ numStepCombinations 3 == 3
  print $ numStepCombinations 100 == 573147844013817084101

numStepCombinations :: Integer -> Integer
numStepCombinations n
  | n < 1 = error "Invalid input. Input must be greater than 0"
  | otherwise = helper 1 2 n
 where
  helper :: Integer -> Integer -> Integer -> Integer -- x and y stands for holding the last two numbers from the fibonacci sequence that we are currently reading
  helper x y 1 = x
  helper x y 2 = y
  helper x y stepsRrquired = helper y (x + y) (stepsRrquired - 1)

