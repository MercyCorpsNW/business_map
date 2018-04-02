library(tidyverse)
library(readxl)
library(fuzzjoin
        )

####RAW FILE FROM VISTASHARE####
raw <- read_csv("Data/all_addresses_0226.csv")
colnames(raw) <- gsub(" ", "_", colnames(raw))


regexpr <- "(?<=[,[:space:]])Apt[\\.[:space:]]*#{0,1}[A-z0-9]+|[,[:space:]]*SUITE[[:space:]]+[0-9A-z]+|#\\s*[0-9A-z]+|ste\\.*\\s+[0-9A-z\\.-]+"

#eliminate duplicate rows and perform various regex substitution and filtering
address_info <- raw %>% group_by(System_Name_ID) %>%
  unique() %>%                                                                              
  ungroup() %>% 
  mutate(Full_Address = gsub(regexpr, "", Full_Address, ignore.case = TRUE, perl = TRUE)) %>% 
  filter(Full_Address != "", 
         !grepl("P[\\.[:space:]]*O[\\.[:space:]]+Box", Full_Address, ignore.case = TRUE), 
         !grepl("^[A-Z]|^\\n", Full_Address, ignore.case = TRUE),
         grepl("[0-9]", Full_Address, ignore.case = TRUE),
         grepl("[A-Z]", Full_Address, ignore.case = TRUE)) %>%  
  mutate(Full_Address = gsub("\\n", ", ", Full_Address, ignore.case = TRUE, perl = TRUE)) %>%
  rename(full_name = Full_Name_Last_First_Mdl)

####Keep bad addresses to preserve relevant metadata####
address_info <- raw %>% group_by(System_Name_ID) %>%
  distinct(System_Name_ID, .keep_all = TRUE) %>%                                                                              
  ungroup() %>% 
  mutate(Full_Address = gsub(regexpr, "", Full_Address, ignore.case = TRUE, perl = TRUE)) %>% 
  mutate(Full_Address = gsub("\\n", ", ", Full_Address, ignore.case = TRUE, perl = TRUE)) %>%
  mutate(Full_Address = ifelse(grepl("same as|home based|home-based", Full_Address, ignore.case = TRUE), Related_Full_Address, Full_Address)) %>%
  rename(full_name = Full_Name_Last_First_Mdl)
  

write_csv(address_info, "Data/addresses_metadata_0226.csv")

#######################################################################
####THE ABOVE FILE IS RUN THROUGH A GEOCODING SERVICE AT THIS POINT####
#######################################################################

###########################################
###### POST(AFTER ARCGIS) PROCESSING ######
###########################################

##INITIAL OUTPUT FROM ARCGIS##
##THIS SHOULD INCLUDE LAT-LONG, OTHER GEODATA, AND A UNIQUE ID FOR EACH BUSINESS###
allfields <- read_csv("Data/Export_Output_TableToExcel.csv")

#### LIMIT TO OREGON AND WASHINGTON ####
trimmed <- allfields %>% select(LongLabel, Region, Subregion, Nbrhd, X, Y, DisplayX, DisplayY, 61:70)
#write a csv file which can be used in powerBI
write_csv(trimmed, "Data/coords_metadata_0226.csv")

###########################################
###########################################
###########################################


##################################################################
####Create file with business locations and total loan amounts####
##################################################################

loan_amounts <- read_csv("Data/biz_loan_amounts.csv")
colnames(loan_amounts) <- gsub(" ", "_", colnames(loan_amounts))

loan_amounts <- loan_amounts %>% filter(!is.na(Loan_Amount)) %>%
  distinct(Loan_ID_Number, .keep_all = TRUE) %>%
  group_by(System_Name_ID) %>%
  summarise(total_loans = sum(Loan_Amount)) %>%
  mutate(System_Name_ID=as.character(System_Name_ID))

biz_locs <- read_csv("Data/coords_metadata_0226.csv") %>% 
  mutate(System_Name_ID = as.character(System_Name_ID))

biz_locs <- biz_locs %>% left_join(loan_amounts)

###################################
###### IDA RECIPIENT DATASET ######
###################################

IDA <- read_csv("Data/IDA_join_cols_0221.csv")
colnames(IDA) <- gsub(" ", "_", colnames(IDA))

IDA_biz_totals <- IDA %>% 
  filter(!is.na(IDA_Account_Lifetime_Summary_Match_Earned)) %>%
  distinct(IDA_Account_ID, .keep_all = TRUE) %>%
  group_by(Related_System_Name_ID) %>%
  summarise(total_ida = sum(IDA_Account_Lifetime_Summary_Match_Earned))

biz_locs <- biz_locs %>% left_join(IDA_biz_totals, by = c("System_Name_ID"= "Related_System_Name_ID"))

#####WRITE TO DISK AFTER ATTACHING LOAN AND IDA TOTALS#####

write_csv(biz_locs, "Data/coords_ida_loan_sums_0302.csv")

##########LEFT JOIN TO IDA IF DESIRED###########

IDA_coords <- IDA %>% left_join(biz_locs, by = c("Related System Name ID" = "System_Nam")) %>% 
  filter(!is.na(DisplayX)) %>%
  distinct(System.Name.ID, .keep_all = TRUE) 

write.csv(IDA_coords, "Data/IDA_and_loanapp_locs.csv")

####### Non-Biz-Metadata ######

roles <- read_csv("Data/nonbiz_metadata_0227.csv")
colnames(roles) <- gsub(" ", "_", colnames(roles))

roles <- roles %>% group_by(System_Name_ID) %>%
  select(-Role, -IDA_Account_Lifetime_Summary_Match_Earned) %>%
  distinct() %>%
  mutate(`3._Foundations_I_Intake.Hud_Income_Level` = ifelse(is.na(`3._Foundations_I_Intake.Hud_Income_Level`), `1._SBA_Client_Profile.Hud_Income_Level`, `3._Foundations_I_Intake.Hud_Income_Level`),
         Ethnicity = ifelse(is.na(Ethnicity), `1._SBA_Client_Profile.Race_(Sba)`, Ethnicity)) %>%
  mutate(Ethnicity = ifelse(Ethnicity == "American Indian/Alaska Native", "American Indian/Alaskan Native", Ethnicity),
         Ethnicity = ifelse(Ethnicity == "Black or African American", "Black / African American", Ethnicity),
         Ethnicity = ifelse(Ethnicity == "Other multiple race", "Other/Multiple", Ethnicity),
         Ethnicity = ifelse(Ethnicity == "Chose not to respond", NA, Ethnicity)
         )
          
roles <- roles %>% mutate(Related_System_Name_ID = paste(Related_System_Name_ID, collapse = ", ")) %>% distinct()


#Join Region Data to Role Data
demo_with_biz_region <- regex_left_join(roles, biz_locs %>% select(System_Name_ID, Subregion), by = c("Related_System_Name_ID" = "System_Name_ID"))

demo_with_biz_region_anon <- demo_with_biz_region %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-Full_Name_Last_First_Mdl, -System_Name_ID.x, -System_Name_ID.y, -Related_System_Name_ID)

write_csv(roles, na = "", "Data/demographics_distinct_0227.csv")
write_csv(demo_with_biz_region_anon, "Data/demographics_anonymous_0301.csv")
#39.78373
#-100.4459
