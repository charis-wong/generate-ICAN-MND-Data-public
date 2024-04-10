#clean data - note some of the regex drug names do not refer to valid drugs. we remove those regex drug names

source("R/configure.R")

library(dplyr)
library(googlesheets4)


druglist <- googlesheets4::read_sheet(ICANGooglesheetId, sheet = "AllDrugsList")
druglist$RegexDrug <- as.character(druglist$RegexDrug)

#first identify the invalid regex drug names - likely to be matched to high number of drugs in drug screening library
regexList <- druglist %>% 
  mutate(row = row_number())%>%
  group_by(RegexDrug)%>%
  summarise(n = length(row))%>%
  arrange(desc(n))


invalidDrugNames <- c("Sulfate ion", 
                      "Acetate",
                      "Chloride ion",
                      "Phosphate ion",
                      "Potassium",
                      "Ethanol",
                      "Dextrose, unspecified form",
                      "Succinic acid",
                      "D-glucose",
                      "Pea",
                      "Pear"
                      )

druglist <- druglist %>% 
  mutate(RegexDrug = ifelse(RegexDrug %in% invalidDrugNames, NA, RegexDrug))

druglist1 <- druglist %>%
  mutate(TotalnPub = ifelse(is.na(RegexDrug), 0, TotalnPub),
         AD = ifelse(is.na(RegexDrug), 0, AD),
         FTD = ifelse(is.na(RegexDrug), 0, FTD),
         HD = ifelse(is.na(RegexDrug), 0, HD),
         MND = ifelse(is.na(RegexDrug), 0, MND),
         MS = ifelse(is.na(RegexDrug), 0, MS),
         PD = ifelse(is.na(RegexDrug), 0, PD),
         relisyr = ifelse(is.na(RegexDrug), FALSE, relisyr),
         relisyrMeetsLogic = ifelse(is.na(RegexDrug), FALSE, relisyrMeetsLogic),
         relisyrPrioritised = ifelse(is.na(RegexDrug), FALSE, relisyrPrioritised),
         nTrials = ifelse(is.na(RegexDrug), NA, nTrials),
         TrialIDs = ifelse(is.na(RegexDrug), NA, TrialIDs)
         )

druglist1 <- druglist1 %>%
  select(-row)%>%
  unique()
  
write.csv(druglist1,  paste0("output/",Sys.Date(), "allDrugsList.csv"), row.names = FALSE)
write_sheet(druglist1, ICANGooglesheetId, sheet = "AllDrugsList")
