main :: IO()

main = do
    
    print $ getAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] == 6

    -- my test
    print $ getAverage [(Temp 1 10), (Temp 6 12), (Temp 11 14), (Temp 16 7), (Temp 21 3), (Temp 26 4), (Temp 31 1)] == 16


getTemp :: Measuring -> Float
getTemp (Temp _ temp) = temp

getDay :: Measuring -> Int
getDay (Temp day _) = day

getAverage :: [Measuring] -> Int
getAverage temps = getDay $ foldr1 (\ t1 t2 -> if abs (getTemp t1 - averageTemp) <= abs (getTemp t2 - averageTemp) then t1 else t2) temps
 where
    averageTemp = sum / fromIntegral (length temps)
    sum = foldl (\ acc (Temp _ degrees) -> acc + degrees) 0 temps 

data Measuring = Temp Int Float