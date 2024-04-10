library(dplyr)
library(googlesheets4)
library(stringr)
library(rvest)
library(xml2)
source('R/dictionaryValidationHelpers.R')
source("R/configure.R")

bnfDrugs <-read_html("https://bnf.nice.org.uk/drugs/")
# write_xml(bnfDrugs, paste0("data/bnfDrugs", Sys.Date(), ".xml"))

nodes <- bnfDrugs %>% html_node("body") 
druglist <- nodes %>%html_nodes("ol")
druglist <-druglist[3:29]
drugs <- druglist %>% html_nodes("li")
drugtable<-drugs%>%html_nodes("a")%>%html_text2()%>%as.data.frame()
drugpages<- druglist %>%html_nodes("a")%>%html_attr("href")%>%as.data.frame()
drugtable <- cbind(drugtable, drugpages)
names(drugtable) <- c("Drug", "Link")
drugtable <- drugtable %>% mutate(across(everything(), as.character))

for (i in 1:nrow(drugtable)){
  drugdata<-drugtable[i, ]
  link<-drugdata$Link
  url<-paste0("https://bnf.nice.org.uk", link)
  data <- read_html(url)
  section <- data %>% html_elements("section")
  arialabels <- section%>%html_attr("aria-labelledby")
  labelDf<-data.frame(labels = arialabels)%>%mutate(id = row_number())
  if(any(grepl("medicinal-form", (labelDf$labels)))){
    row<-filter(labelDf, labels== "medicinal-form" | labels == "medicinal-forms")$id
    data <- section[[row]]
    medicinalForms <- data %>% html_elements("li")%>%html_text2()%>%list()
    specialOrderForms<-data%>%html_elements("p")%>% html_text2() %>%tolower()%>%str_subset("special\\Worder")
    specialOrderForms <- str_replace(specialOrderForms, "forms available from special\\Worder manufacturers include: ", "")
    specialOrderForms <- str_split(specialOrderForms, ", ", simplify = FALSE)}
  
  newdrugtable <- drugdata
  newdrugtable$MedicinalForms <- medicinalForms
  if(length(specialOrderForms) == 0)   newdrugtable$SpecialOrderForms = "NA" else newdrugtable$SpecialOrderForms <-specialOrderForms
  
  if(!exists("drugTableOutput")){drugTableOutput <- newdrugtable} else {drugTableOutput <- rbind(drugTableOutput, newdrugtable)}

}


#match bnf names to drug dictionary names to then match with names in alldruglist
BNFRegexTable <- drugTableOutput %>% rename(BNFDrug = Drug)%>%mutate(id = row_number())

drugBankDictionary <- googlesheets4::read_sheet(drugDictionarySheetId, sheet="drugBankDictionary")
myDrugs <- ExtractDrug(BNFRegexTable, 
                       dictionaryName = as.data.frame(drugBankDictionary), 
                       idColumn = "id", 
                       textSearchingHeaders = "BNFDrug", 
                       minimumIncludeFrequency = 9,  
                       maximumExcludeFrequency = 1, 
                       varnames = c("Drug", "DrugFrequency"), 
                       groupAnnotation=F, ignoreCase = T)

BNFDrugTableOutput <- BNFRegexTable %>%
  left_join(myDrugs)

#check names and filter out erroneous names
# listToCheck <- BNFDrugTableOutput%>%select(Drug) %>% unique()

BNFDrugTableOutput <- BNFDrugTableOutput%>%
  filter(Drug != "Rabbit"&
           Drug != "Acetate" &
           Drug != "Chloride ion"&
           Drug != "Aluminium" &
           Drug != "Bean" &
           Drug != "Carbonate ion" &
           Drug != "Coconut" &
           Drug != "Zinc")

BNFDrugTableOutput <- BNFDrugTableOutput%>%
  select(-id, -DrugFrequency)

BNFDrugTableOutput <-BNFDrugTableOutput%>%
  mutate(MedicinalForms = ifelse(MedicinalForms == "character(0)", "NA", MedicinalForms))

MedFormsList <-BNFDrugTableOutput%>%
  filter(MedicinalForms != "NA") %>%
  select(MedicinalForms) %>%
  unlist()%>%
  unique()%>%
  as.data.frame()

SOFormsList <- BNFDrugTableOutput%>%
  filter(SpecialOrderForms != "NA") %>%
  select(SpecialOrderForms) %>%
  unlist()%>%
  as.data.frame()

names(SOFormsList) <- "SOForms"

SOFormsList <- SOFormsList %>%
  mutate(SOForms = gsub("\\.", "", SOForms))%>%unique()

BNFDrugTableOutput <- BNFDrugTableOutput %>%
  mutate(oralLiquidForm = ifelse(grepl("Oral solution", MedicinalForms)|
                                   grepl("Oral suspension", MedicinalForms)|
                                   grepl("Oral drops", MedicinalForms)|
                                   grepl("Liquid", MedicinalForms),
                                 TRUE, FALSE))%>%
  mutate(OtherOralDysphagiaFriendlyForms = ifelse(grepl("Effervescent tablet", MedicinalForms)|
                                                    grepl("Dispersible tablet", MedicinalForms)|
                                                    grepl("Orodispersible tablet", MedicinalForms)|
                                                    grepl("Soluble tablet", MedicinalForms)|
                                                    grepl("Oral gel", MedicinalForms)|
                                                    grepl("Oromucosal gel", MedicinalForms)|
                                                    grepl("Oromucosal solution", MedicinalForms)|
                                                    grepl("Orodispersible film", MedicinalForms),
                                                  TRUE, FALSE))


BNFDrugTableOutput <- BNFDrugTableOutput %>%
  mutate(SOoralLiquidForm = ifelse(grepl("oral suspension", SpecialOrderForms)|
                                     grepl("oral solution", SpecialOrderForms),
                                 TRUE, FALSE))%>%
  mutate(SOOtherOralDysphagiaFriendlyForms = ifelse(grepl("effervescent tablet", SpecialOrderForms)|
                                                    grepl("granules", MedicinalForms)|
                                                    grepl("orodispersible tablet", MedicinalForms)|
                                                    grepl("oral drops", MedicinalForms)|
                                                    grepl("dispersible tablet", MedicinalForms)|
                                                    grepl("gel", MedicinalForms)|
                                                    grepl("oromucosal solution", MedicinalForms)|
                                                    grepl("Orodispersible film", MedicinalForms),
                                                  TRUE, FALSE))


# Write out results
BNFDrugTableOutput1 <- BNFDrugTableOutput %>% mutate(across(everything(), as.character))

write.csv(BNFDrugTableOutput1, paste0("output/bnf/", Sys.Date(), "BNFData.csv"), row.names = FALSE)
write_sheet(BNFDrugTableOutput1, ICANGooglesheetId, sheet = "BNFData")

