library(tidyverse)

nonbiz_summarized <- read_csv("Data/nonbiz_metadata_summarized_0305.csv") 
colnames(nonbiz_summarized) <- gsub(" ", "_", colnames(nonbiz_summarized))

nonbiz_summarized <- nonbiz_summarized %>% mutate(IDA_Account_Lifetime_Summary_Match_Earned = as.numeric(IDA_Account_Lifetime_Summary_Match_Earned))

sum(nonbiz_summarized$IDA_Account_Lifetime_Summary_Match_Earned, na.rm = TRUE)

