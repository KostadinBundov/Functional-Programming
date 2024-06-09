main :: IO()

main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"

    -- my test
    print $ highestCapital [ (Country "Italy" "Rome" [City "Rome" 21 15, City "Milan" 122 13]), (Country "Switzerland" "Bern" [City "Bern" 540 12, City "Zurich" 408 14]), (Country "Spain" "Madrid" [City "Madrid" 667 16, City "Barcelona" 12 17])] == "Spain"

highestCapital :: [Country] -> Name
highestCapital countries = countryName $ foldl1 (\ c1 c2 -> if capitalElevation c1 > capitalElevation c2 then c1 else c2) countries

getCapitalFromCities :: Country -> City
getCapitalFromCities (Country name capitalName cities) = head $ filter (\ (City name _ _) ->  name == capitalName) cities

countryName :: Country -> Name
countryName (Country name _ _) = name

getElevation :: City -> Elevation
getElevation (City _ elevation _) = elevation

capitalElevation :: Country -> Elevation
capitalElevation country = getElevation $ getCapitalFromCities country 

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]