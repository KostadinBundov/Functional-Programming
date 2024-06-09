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