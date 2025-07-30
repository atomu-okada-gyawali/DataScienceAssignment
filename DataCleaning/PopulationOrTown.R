library(tidyverse)
library(dplyr)

#Loading all data sets
HousePrices2021 = read_csv("assignments/Obtained Data/Housing_Price/HousePrices2021.csv")
HousePrices2022 = read_csv("assignments/Obtained Data/Housing_Price/HousePrices2022.csv")
HousePrices2023 = read_csv("assignments/Obtained Data/Housing_Price/HousePrices2023.csv")
HousePrices2024 = read_csv("assignments/Obtained Data/Housing_Price/HousePrices2024.csv")
PopulationData = read_csv("assignments/Obtained Data/Population/Population2011.csv")
class(HousePrices2021$Date_of_transfer)
#Renaming the columns
colnames(HousePrices2021)<-c("ID","Price","Date_of_transfer","Postcode","Property_Type","Old/New","Duration","PAON","SAON","Street","Locality","Town/City","District","County","PPD_Category_Type","Record_Status")
colnames(HousePrices2022)<-c("ID","Price","Date_of_transfer","Postcode","Property_Type","Old/New","Duration","PAON","SAON","Street","Locality","Town/City","District","County","PPD_Category_Type","Record_Status")
colnames(HousePrices2023)<-c("ID","Price","Date_of_transfer","Postcode","Property_Type","Old/New","Duration","PAON","SAON","Street","Locality","Town/City","District","County","PPD_Category_Type","Record_Status")
colnames(HousePrices2024)<-c("ID","Price","Date_of_transfer","Postcode","Property_Type","Old/New","Duration","PAON","SAON","Street","Locality","Town/City","District","County","PPD_Category_Type","Record_Status")

#deriving the shortPostcode by substringing first 4 letters from Postcode and mutating population data from 2012 to 2024
PopulationData = PopulationData %>%  
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  group_by(shortPostcode) %>%
  summarise_at(vars(Population), list(Population2011 = sum)) %>% 
  mutate(Population2012 = 1.00695353132322269 * Population2011) %>%
  mutate(Population2013 = 1.00669740535540783 * Population2012) %>%
  mutate(Population2014 = 1.00736463978721671 * Population2013) %>%
  mutate(Population2015 = 1.00792367505802859 * Population2014) %>%
  mutate(Population2016 = 1.00757874492811929 * Population2015) %>%
  mutate(Population2017 = 1.00679374473924223 * Population2016) %>%
  mutate(Population2018 = 1.00605929132212552 * Population2017) %>%
  mutate(Population2019 = 1.00561255390388033 * Population2018) %>%
  mutate(Population2020 = 1.00561255390388033 * Population2019) %>%
  mutate(Population2021 = 1.005425 * Population2020) %>%
  mutate(Population2022 = 1.004920 * Population2021) %>%
  mutate(Population2023 = 1.004510 * Population2022) %>%
  mutate(Population2024 = 1.004220 * Population2023) 
#making Population data by selecting population count from 2020 to 2024
PopulationData %>% 
  select(shortPostcode,Population2020,Population2021,Population2022,Population2023,Population2024)
#binding houseprices data
HousePrices =HousePrices2021 %>% 
  add_row(HousePrices2022) %>% 
  add_row(HousePrices2023) %>% 
  add_row(HousePrices2024)
#filtering only rows with county value = "SOUTH YORKSHIRE" OR "WEST YORKSHIRE",derviving shortPostcode, 
Towns = HousePrices %>%
  select(-"SAON",-"Locality") %>% 
  filter(County=="SOUTH YORKSHIRE"|County=="WEST YORKSHIRE") %>% 
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  left_join(PopulationData,by = "shortPostcode") %>% 
  select(shortPostcode,"Town/City",District,County,Population2020,Population2021,Population2022,Population2023,Population2024) %>% 
  group_by(shortPostcode) %>% 
  filter(row_number()==1) %>% 
  arrange(County)

write.csv(Towns, "assignments/Cleaned Data/Towns.csv")

