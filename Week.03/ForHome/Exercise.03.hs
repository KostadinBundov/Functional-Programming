main :: IO()
main = do
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 == -1.047619047619048
    print $ calcSeriesSum 1 4 == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
    print $ calcSeriesSum 1 6 == -1.0762718762718764

calcSeriesSum :: Double -> Double -> Double
calcSeriesSum x n = helper 1 (-2)
 where
    helper :: Double -> Double -> Double
    helper currPower result
     | currPower > n = result
     | otherwise = helper (currPower + 1) (result + calc)
      where
        calc = getSign currPower * (2**(currPower + 1) * x**currPower) / getDenominator currPower 1

        getSign :: Double -> Double
        getSign power
         | mod (round power) 2 == 0 = -1
         | otherwise = 1

        getDenominator :: Double -> Double -> Double
        getDenominator 0 result = result
        getDenominator power result = result * getDenominator (power - 1) (result + 2)
