main :: IO()
main = do
    print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 5 4.25 == 4.34
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75    == 4.95
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75          == 5.14
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0             == 2
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    == 5.05
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       == 5.25
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50       == 4.84

roundedResult :: Double -> Double
roundedResult n
 | n < 2 = 2
 | otherwise = (fromIntegral $ round $ n * 100) / 100

finalGrade :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
finalGrade d1 d2 d3 kz1 kz2 kt1 kt2 iz it = roundedResult o
 where
    d = (d1 + d2 + d3) / 3
    kt = (kt1 + kt2) / 2
    kz = (kz1 + kz2) / 2
    gradeIZ
     | kz >= 4.50 && kz1 > 4 && kz2 >= 4 = kz
     | otherwise = iz
    gradeIT 
     | kt >= 4.50 && kt1 > 4 && kt2 >= 4 = kt
     | otherwise = it
    tk = 1/4 * d + 3/8 * kt + 3/8 * kz
    o = 1/2 * tk + 1/4 * gradeIZ + 1/4 * gradeIT