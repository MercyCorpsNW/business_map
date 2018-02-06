library(tidyverse)
library(readxl)

####RAW FILE FROM VISTASHARE####
raw <- read.csv("Data/all_addresses_012218.csv")


regexpr <- "(?<=[,[:space:]])Apt[\\.[:space:]]*#{0,1}[A-z0-9]+|[,[:space:]]*SUITE[[:space:]]+[0-9A-z]+|#\\s*[0-9A-z]+|ste\\.*\\s+[0-9A-z\\.-]+"

#eliminate duplicate rows and perform various regex substitution and filtering
address_info <- raw %>% group_by(System.Name.ID) %>%
  unique() %>%                                                                              
  ungroup() %>% 
  mutate(Full.Address = gsub(regexpr, "", Full.Address, ignore.case = TRUE, perl = TRUE)) %>% 
  filter(Full.Address != "", 
         !grepl("P[\\.[:space:]]*O[\\.[:space:]]+Box", Full.Address, ignore.case = TRUE), 
         !grepl("^[A-Z]|^\\n", Full.Address, ignore.case = TRUE),
         grepl("[0-9]", Full.Address, ignore.case = TRUE),
         grepl("[A-Z]", Full.Address, ignore.case = TRUE)) %>%  
  mutate(Full.Address = gsub("\\n", ", ", Full.Address, ignore.case = TRUE, perl = TRUE)) %>%
  rename(full_name = ï..Full.Name.Last.First.Mdl)
  


#### Remove suite numbers

write.csv(address_info, "Data/addresses_metadata_012218.csv", row.names = FALSE)

########## Data Cleanup for Proper Format ########


###### POST(AFTER ARCGIS) PROCESSING ######

allfields <- read_excel("C:/Users/Daniel/Google Drive/MercyCorps/MCNW/Mapping/Data/output_table_012218.xls")

trimmed <- allfields %>% select(LongLabel, Region, Subregion, Nbrhd, X, Y, DisplayX, DisplayY, 70:83) %>%
           filter(Region %in% c("Oregon", "Washington"))

write.csv(trimmed, "Data/coords_metadata_012218.csv")


###### IDA RECIPIENT DATASET ####

IDA <- read.csv("Data/IDA_join_cols_0202.csv")
biz_locs <- read.csv("Data/coords_metadata_012218.csv")

IDA_coords <- IDA %>% left_join(biz_locs, by = c("Related.System.Name.ID" = "System_Nam")) %>%
  filter(!is.na(DisplayX)) %>%
  distinct(System.Name.ID, .keep_all = TRUE) %>%
  select(-Alternate_, - business.of..as.partner..Related.Full.Address)

write.csv(IDA_coords, "Data/IDA_and_loanapp_locs.csv")

address_locs <- read_csv("Data/Geocoded_Addresses_1.txt")

address_locs <- address_locs %>% filter(latitude > 40, longitude < -101) %>% select(-color)

write.csv(address_locs, "Data/addresses_coords.csv", row.names = FALSE)

####### JOIN MICROTEST DATA TO BIZ DATA #######

microtest <- read.csv("Data/workbook_allcols.csv")

trimmed <- trimmed %>% left_join("workbook_allcols.csv")

#######



#39.78373
#-100.4459
