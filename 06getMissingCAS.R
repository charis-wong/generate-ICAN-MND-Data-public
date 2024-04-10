library(dplyr)
library(googlesheets4)
library(rvest)
library(xml2)
library(httr)
library(jsonlite)

source("R/configure.R")

allDrugsList <- read_sheet(ICANGooglesheetId, sheet = "AllDrugsList")

allDrugsList1 <- allDrugsList%>%
  mutate(row = row_number())%>%
  mutate(across(everything(), as.character))

getCAS<-function(drug){
  base<- "commonchemistry.cas.org/api"
  drugname<- tolower(drug)
  drugname <-gsub(" ", "%20", drugname)
  call1<-paste0(base, "/search?q=", drugname, "&size=1")
  call1<-URLencode(call1)
  get_CAS<-GET(call1)
  
  get_CAS_text<-content(get_CAS, "text")
  
  get_CAS_json <- fromJSON(get_CAS_text, flatten = "TRUE")
  
  CAS<-get_CAS_json$results
  CAS<-ifelse(length(CAS) == 0, NA, CAS$rn)
  print(CAS)
}

drugsWithCAS <-allDrugsList1%>%
  filter(!is.na(CAS))

drugsMissingCAS <- allDrugsList1 %>%
  filter(is.na(CAS))

drugsMissingCAS$CAS<-sapply(as.factor(drugsMissingCAS$Name), getCAS)

drugsMissingCAS1 <- drugsMissingCAS %>% 
  filter(!is.na(CAS))

drugsMissingCAS2 <- drugsMissingCAS %>%
  filter(is.na(CAS))

#For hits where CAS not identified via commonchemistry, try chemIDplus


getCASFromChemIDplus <-function(drug){
  base<-"https://chem.nlm.nih.gov/api/data/name/equals/"
  drugname<- tolower(drug)
  drugname <-gsub(" ", "%20", drugname)
  url1<-paste0(base, drugname, "?data=summary&format=xml")
  xmldata<-GET(url1)
  
  if (xmldata$status_code !=200) { 
    CAS<-NA} else{ 
      data<-read_xml(url1)
      CAS <- data %>%html_node("rn")%>%html_text()}
  return(CAS)
}

drugsMissingCAS2$CAS <- sapply(drugsMissingCAS2$Name, getCASFromChemIDplus)

allDrugsList2 <- bind_rows(drugsWithCAS, 
                           drugsMissingCAS1,
                           drugsMissingCAS2
                           )

allDrugsList2$row <- as.numeric(allDrugsList2$row)
allDrugsList2 <- allDrugsList2 %>%
  arrange(row)%>%
  select(-row)

write.csv(allDrugsList2, paste0("output/", Sys.Date(), "allDrugsList.csv" ), row.names = FALSE)
write_sheet(allDrugsList2, ICANGooglesheetId, sheet = "AllDrugsList")
