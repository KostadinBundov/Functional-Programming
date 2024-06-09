import Data.List
import Data.Char

main ::IO()
main = do
    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)

checkNumber :: Integer -> (Int, Int)
checkNumber n = foldl (\(evenSum, oddSum) (index, digit) -> if even index then (evenSum + digit, oddSum) else (evenSum, oddSum + digit)) (0, 0) $ zip [0..] (map digitToInt $ show n)