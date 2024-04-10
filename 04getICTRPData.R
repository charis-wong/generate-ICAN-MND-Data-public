#get clinical trials data from ICTRP - no API available. Get csv for MND/ALS trials manually by going to https://trialsearch.who.int/Default.aspx and search  amyotrophic lateral sclerosis OR motor neuron disease then click Export to CSV
#upload CSV to pomegranate and read in. 

library(dplyr)
library(googlesheets4)
library(stringr)


source('R/dictionaryValidationHelpers.R')
source("R/configure.R")


ICTRPresults <- read.csv("data/2022-06-27IctrpResults.csv")
ICTRPresults <- ICTRPresults %>% mutate(across(everything(), as.character))

ICTRPresults1 <-ICTRPresults %>%
  filter(Study.type == "Interventional")%>%
   mutate(Drugs = ifelse(grepl("Procedure:", Intervention)|
                           grepl("Device:", Intervention)|
                           grepl("Diagnostic Test:", Intervention)|
                           grepl("Radiation:", Intervention), NA, Intervention))

ICTRPresults1<-ICTRPresults1 %>%
  filter(!is.na(Drugs))%>%
  mutate(id = row_number())
CTDrugs <- ICTRPresults1%>% select(TrialID, id, Intervention)

drugBankDictionary <- googlesheets4::read_sheet(drugDictionarySheetId, sheet="drugBankDictionary")  


myDrug <- CTDrugs %>% select(id, Name = Intervention)

minimumIncludeFrequencies <- 6
maximumExcludeFrequencies <- 1

results1 <- ExtractDrug(myDrug, dictionaryName =  as.data.frame(drugBankDictionary), idColumn = "id", textSearchingHeaders = c("Name"), minimumIncludeFrequency = 6,  maximumExcludeFrequency = 1, varnames = c("Drug", "DrugFrequency"), groupAnnotation=F, ignoreCase=T)

CTDrugs1 <- merge(myDrug, results1)
CTDrugs2<-merge(CTDrugs1, ICTRPresults1, by = "id", all = TRUE)

CTDrugs2 <- CTDrugs2 %>%
  relocate(Intervention, .before = "Drug")


CTList <- CTDrugs2 %>% 
  filter(!is.na(Drug))%>%
  group_by(Drug)%>%
  summarise(nTrials = length(unique(TrialID)),
            TrialIDs = paste(TrialID, collapse = ", "))

write.csv(CTList, paste0("output/", Sys.Date(), "ICTRPClinicalTrialsList.csv"), row.names = FALSE)
write_sheet(CTList, ICANGooglesheetId, sheet = "ICTRPList")


# CTList <- read_sheet(ICANGooglesheetId, sheet = "ICTRPList")
# allDrugsList <- read_sheet(ICANGooglesheetId, sheet = "AllDrugsList")
# CTList1 <- CTList %>% rename(RegexDrug = Drug )
# allDrugsList1 <- left_join(allDrugsList, CTList1, by = "RegexDrug")
# write_sheet(allDrugsList1, ICANGooglesheetId, sheet = "AllDrugsList")
# write.csv(allDrugsList1, paste0("output/",Sys.Date(), "allDrugsList.csv"), row.names = FALSE)


# 
# allDrugsList <- googlesheets4::read_sheet(ICANGooglesheetId, sheet = "withMX1Data")
# allDrugsList1 <- left_join(allDrugsList, CTList, by = "Drug")
# write_sheet(allDrugsList1, ICANGooglesheetId, sheet = "withMX1Data")
# write.csv(allDrugsList1, paste0("output/",Sys.Date(), "allDrugsList.csv"), row.names = FALSE)
