import Data.List

main :: IO()
main = do
    print $ (myLambda (\ x -> x)) 5 == 5
    print $ (myLambda (\ x -> x)) "Hello" == "Hello"
    print $ (myLambda (+1)) 5 == 6

    print $ (negatePred (\x -> mod x 2 == 0)) 2 == False

    print $ (compose (\x -> x - 5) (\y -> y + 25)) 5 == 25
    print $ (compose group sort) "Hello World" == [" ","H","W","d","e","lll","oo","r"]

    print $ (partiallyApply (\x y -> 10 * x + y) 5) 10 == 60

partiallyApply :: (a -> a-> a) -> a -> (a -> a)
partiallyApply f x = (\ y -> f x y)

myLambda :: (a -> a) -> (a -> a)
myLambda f = (\ x -> f x)

negatePred :: (Num a) => (a -> Bool) -> (a -> Bool)
negatePred pred = (\ n -> not $ pred n)

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = (\ n -> f $ g n)