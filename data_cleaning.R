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
  

write.csv(address_info, "Data/addresses_metadata_012218.csv", row.names = FALSE)

#######################################################################
####THE ABOVE FILE IS RUN THROUGH A GEOCODING SERVICE AT THIS POINT####
#######################################################################

###########################################
###### POST(AFTER ARCGIS) PROCESSING ######
###########################################

##INITIAL OUTPUT FROM ARCGIS##
##THIS SHOULD INCLUDE LAT-LONG, OTHER GEODATA, AND A UNIQUE ID FOR EACH BUSINESS###
allfields <- read_excel("C:/Users/Daniel/Google Drive/MercyCorps/MCNW/Mapping/Data/output_table_012218.xls")

#### LIMIT TO OREGON AND WASHINGTON ####
trimmed <- allfields %>% select(LongLabel, Region, Subregion, Nbrhd, X, Y, DisplayX, DisplayY, 70:83) %>%
           filter(Region %in% c("Oregon", "Washington"))

#write a csv file which can be used in powerBI
write.csv(trimmed, "Data/coords_metadata_012218.csv")

###########################################
###########################################
###########################################


##################################################################
####Create file with business locations and total loan amounts####
##################################################################

loan_amounts <- read_csv("Data/biz_loan_amounts.csv")

loan_amounts <- loan_amounts %>% filter(!is.na(`Loan Amount`)) %>%
  distinct(Loan.ID.Number, .keep_all = TRUE) %>%
  group_by(`System Name ID`) %>%
  summarise(total_loans = sum(`Loan Amount`))

biz_locs <- read.csv("Data/coords_metadata_012218.csv")

biz_locs <- biz_locs %>% left_join(loan_amounts, by = c("System_Nam"= "System Name ID")) %>% View()
  select(-c(14:23))

write.csv(biz_locs, "Data/coords_loansums_0214.csv")

###################################
###### IDA RECIPIENT DATASET ######
###################################

IDA <- read.csv("Data/IDA_join_cols_0214.csv")
biz_locs <- read.csv("Data/coords_loansums_0214.csv")

IDA_coords <- IDA %>% left_join(biz_locs, by = c("Related.System.Name.ID" = "System_Nam")) %>% 
  filter(!is.na(DisplayX)) %>%
  distinct(System.Name.ID, .keep_all = TRUE) 

write.csv(IDA_coords, "Data/IDA_and_loanapp_locs.csv")

#39.78373
#-100.4459
