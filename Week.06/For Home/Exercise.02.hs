main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83]

type Cylinder = (Double, Double)

getVolumes :: [Cylinder] -> [Double]
getVolumes arr = map (\ x -> calculateVolume x) arr

calculateVolume :: Cylinder -> Double
calculateVolume (radius, height) = roundOneDigit $ pi * radius**2 * height

roundOneDigit :: Double -> Double
roundOneDigit = (/100) . fromIntegral . round . (100*)