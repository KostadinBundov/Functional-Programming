main :: IO()
main = do
    print $ canCarry 5 15 3 == "Yes"
    print $ canCarry 1 5 4 == "Yes"
    print $ canCarry 13 25 2 == "No"
    print $ canCarry 24 104.44 21.12 == "No"
    print $ canCarry 51 34.75 19.852 == "No"
    print $ canCarry 42 95.11 0.51 == "Yes"
    print $ canCarry 5 20.50 5.28 == "No" -- my test

canCarry :: Int -> Double -> Double -> String
canCarry c k w
 | c < 0 = error "Number of products must be none negative"
 | k < 0 = error "Kilograms must be none negative"
 | w < 0 = error "Item weigh must be none negative"
 | fromIntegral c * w <= k = "Yes"
 | otherwise = "No"