library(tidyverse)

raw <- read.csv("Data/ALL ADDRESSES.csv")

regexpr <- "(?<=[,[:space:]])Apt[\\.[:space:]]*#{0,1}[A-z0-9]+|[,[:space:]]*SUITE[[:space:]]+[0-9A-z]+|#\\s*[0-9A-z]+|ste\\.*\\s+[0-9A-z\\.-]+"

address_info <- raw %>% group_by(System.Name.ID) %>%
  unique() %>%
  ungroup() %>%
  select(Full.Address) %>% 
  filter(Full.Address != "", !grepl("P[\\.[:space:]]*O[\\.[:space:]]+Box", Full.Address, ignore.case = TRUE)) %>% 
  mutate(Full.Address = gsub(regexpr, "", Full.Address, ignore.case = TRUE, perl = TRUE))


#### Remove suite numbers

write.csv(address_info, "Data/addresses_full_0112.csv", row.names = FALSE)

########## Data Cleanup for Proper Format ########


##### 
39.78373
-100.4459

address_locs <- read_csv("Data/Geocoded_Addresses_1.txt")

address_locs <- address_locs %>% filter(latitude > 40, longitude < -101) %>% select(-color)

write.csv(address_locs, "Data/addresses_coords.csv", row.names = FALSE)

