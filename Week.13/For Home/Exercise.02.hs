import Data.Char
import Data.List

main :: IO()
main = do
    print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
    -- in the example there is 1 absent and 3 lates so with more than two lates we should return false, not true
    print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == False
    print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False


cP = canPass (1,2)

type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)
data Attendance = Absent | Late | Present
 deriving (Eq, Show)
type StudentRecord = [Attendance]


canPass :: Criterion -> (StudentRecord -> Bool)
canPass (absents, lates) = (\studPresent -> (getCount Absent studPresent) <= absents && (getCount Late studPresent) <= lates)

getCount :: Attendance -> StudentRecord -> Int
getCount searched = length . filter (== searched)

-- getCount :: Attendance -> StudentRecord -> Int
-- getCount searched records = length $ filter(\x -> x == searched) records