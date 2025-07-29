library(tidyverse)

# Load previously cleaned towns with shortPostcode and County
towns <- read_csv("assignments/Cleaned Data/Towns.csv") %>%
  select(shortPostcode, County,District)

# === 1. Clean england_ks4final.csv ===
ks4final21 <- read_csv("assignments/Obtained Data/Performance/2021-2022/england_ks4final.csv")
ks4final22 <- read_csv("assignments/Obtained Data/Performance/2022-2023/england_ks4final.csv")
ks4final23 <- read_csv("assignments/Obtained Data/Performance/2023-2024/england_ks4final.csv")

ks4final21_selected = ks4final21 %>% select(SCHNAME,PCODE,LEA,ATT8SCR,P8MEA) %>% mutate(Year="2021")
ks4final22_selected = ks4final22 %>% select(SCHNAME,PCODE,LEA,ATT8SCR,P8MEA)%>% mutate(Year="2022")
ks4final23_selected = ks4final23 %>% select(SCHNAME,PCODE,LEA,ATT8SCR,P8MEA)%>% mutate(Year="2023")

ks4final_all <- bind_rows(ks4final21_selected, ks4final22_selected, ks4final23_selected)%>%
  mutate(shortPostcode = str_trim(substr(PCODE, 1, 4)),Attainment8 = as.numeric(ATT8SCR),Progress8 = as.numeric(P8MEA)) 

clean_ks4final <- ks4final_all %>%
  filter(!is.na(Attainment8)) %>% 
  left_join(towns, by = "shortPostcode") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  select(
    SchoolName = SCHNAME,
    Postcode = PCODE,
    shortPostcode,
    Attainment8 ,
    Progress8 ,
    Year
  ) 


write_csv(clean_ks4final, "assignments/Cleaned Data/cleanKS4Final.csv")
