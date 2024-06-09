main :: IO()
main = do
    print $ getColdestCountryCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"
    print $ getColdestCountryCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 (-16)), (City "Plovdiv" 120 34), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 (-15)), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"
    print $ getColdestCountryCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 (-16)), (City "Plovdiv" 120 34), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 (20)), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria" -- my test

type Name = String
type Capital = Name 
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

getColdestCountryCapital :: [Country] -> Name
getColdestCountryCapital countries = let
    countryTemperatures = map (\(Country name _ cities) -> (name, map (\ (City _ _ temperature) -> temperature) cities)) countries
    getAverageTemperatures = map (\ (name, temperatures) -> (name, sum temperatures / fromIntegral (length temperatures))) countryTemperatures
    getColdestCountry = foldl1 (\ (name1, temp1) (name2, temp2) -> if temp1 <= temp2 then (name1, temp1) else (name2, temp2)) getAverageTemperatures
    in 
    fst getColdestCountry