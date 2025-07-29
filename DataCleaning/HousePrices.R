library(tidyverse)
library(janitor)

#Loading houseprices data from different years
HousePrices2021 = read_csv("assignments/Obtained Data/Housing_Price/HousePrices2021.csv")
HousePrices2022 = read_csv("assignments/Obtained Data/Housing_Price/HousePrices2022.csv")
HousePrices2023 = read_csv("assignments/Obtained Data/Housing_Price/HousePrices2023.csv")
HousePrices2024 = read_csv("assignments/Obtained Data/Housing_Price/HousePrices2024.csv")
#Standardizing column names
colnames(HousePrices2021)<-c("ID","Price","Date_of_transfer","Postcode","Property_Type","Old/New","Duration","PAON","SAON","Street","Locality","Town/City","District","County","PPD_Category_Type","Record_Status")
colnames(HousePrices2022)<-c("ID","Price","Date_of_transfer","Postcode","Property_Type","Old/New","Duration","PAON","SAON","Street","Locality","Town/City","District","County","PPD_Category_Type","Record_Status")
colnames(HousePrices2023)<-c("ID","Price","Date_of_transfer","Postcode","Property_Type","Old/New","Duration","PAON","SAON","Street","Locality","Town/City","District","County","PPD_Category_Type","Record_Status")
colnames(HousePrices2024)<-c("ID","Price","Date_of_transfer","Postcode","Property_Type","Old/New","Duration","PAON","SAON","Street","Locality","Town/City","District","County","PPD_Category_Type","Record_Status")
#Load cleaned towns data
towns = read.csv("assignments/Cleaned Data/Towns.csv")
#binding all houseprice data
HousePrices =HousePrices2021 %>% 
  add_row(HousePrices2022) %>% 
  add_row(HousePrices2023) %>% 
  add_row(HousePrices2024) 
#filtering houseprice for south yorkshire and west yorkshire, deriving Year from date of transfer and droping null values
cleanHousePrices = HousePrices %>%
  filter(County=="SOUTH YORKSHIRE"|County=="WEST YORKSHIRE") %>% 
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  mutate(Year=substring(Date_of_transfer,1,4)) %>%
  arrange(County) %>% 
  select(Postcode,shortPostcode,Price,Year)

write.csv(cleanHousePrices, "assignments/Cleaned Data/cleanHousePrices.csv")
