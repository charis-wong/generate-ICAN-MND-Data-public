source('R/dictionaryValidationHelpers.R')
source("R/configure.R")
library(AutoAnnotation)
library(DBI)
library(dplyr)
library(googlesheets4)
library(purrr)
library(readxl)
library(RMySQL)
library(rvest)
library(tibble)
library(tidyr)
library(xml2)

#read in drugbank dictionary and drug CAS numbers-----

drugBankDictionary <- googlesheets4::read_sheet(drugDictionarySheetId, sheet="drugBankDictionary")  
drugVocabulary<-googlesheets4::read_sheet(drugDictionarySheetId, sheet="drugVocabulary", col_types = c("ccccccc"))  
drugCAS<-drugVocabulary%>%select("Common name","CAS")
drugCAS$CAS<-as.character(drugCAS$CAS)
colnames(drugCAS)<-c("Name", "CAS")
drugBankDictionary_CAS<-left_join(drugBankDictionary, drugCAS, by = "Name")

#read drug lists of all categories-----
###### Drug Screening Library ----
drugScreenLibBBB<-read.csv("data/drugLibrary_AdmetSARBBB2021-08-18.csv")%>%rename(p_CNSPenetrance = probability)
drugLibRaw <- read.csv("data/DrugLibrary.csv")
noCNSPenetranceData <- drugScreenLibBBB%>% filter(is.na(CNSPenetrance))
noCNSPenetranceData <- drugLibRaw %>% filter(Compound_Id %in% noCNSPenetranceData$Compound_Id)
#check CNS Penetrance on admetSar1 using SMILES

drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0002605", ]$CNSPenetrance <- "+"
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0002605", ]$p_CNSPenetrance <- 0.940

drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0001577", ]$CNSPenetrance <- "-"
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0001577", ]$p_CNSPenetrance <- 0.945

drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0003030", ]$CNSPenetrance <- "-"
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0003030", ]$p_CNSPenetrance <- 0.919

# invalid SMILES for MND0004939, MND0000745, MND0001288 - use CAS to get canonical SMILES from pubchem, then reattempt on admetsar1
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0004939", ]$SMILES <- "[CH3-].CC1=CC2=C(C=C1C)N(C=N2)C3C(C(C(O3)CO)OP(=O)([O-])OC(C)CNC(=O)CCC4(C(C5C6(C(C(C(=C(C7=NC(=CC8=NC(=C(C4=N5)C)C(C8(C)C)CCC(=O)N)C(C7(C)CC(=O)N)CCC(=O)N)C)[N-]6)CCC(=O)N)(C)CC(=O)N)C)CC(=O)N)C)O.[Co+3]"
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0004939", ]$CNSPenetrance <- "-"
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0004939", ]$p_CNSPenetrance <- 0.7375


drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0000745", ]$SMILES <- "CC1CC=CC=CC(C(CC(C(C(C(CC(=O)O1)O)OC)OC2C(C(C(C(O2)C)OC3CC(C(C(O3)C)O)(C)O)N(C)C)O)CC=O)C)OC4CCC(C(O4)C)N(C)C"
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0000745", ]$CNSPenetrance <- "-"
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0000745", ]$p_CNSPenetrance <- 0.989


drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0001288", ]$SMILES <- "CC(C)CC(C(=O)NC(C)C(=O)NC(C(C)C)C(=O)NC(C(C)C)C(=O)NC(C(C)C)C(=O)NC(CC1=CNC2=CC=CC=C21)C(=O)NC(CC(C)C)C(=O)NC(CC3=CNC4=CC=CC=C43)C(=O)NC(CC(C)C)C(=O)NC(CC5=CNC6=CC=CC=C65)C(=O)NC(CC(C)C)C(=O)NC(CC7=CNC8=CC=CC=C87)C(=O)NCCO)NC(=O)C(C)NC(=O)CNC(=O)C(C(C)C)NC=O"
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0001288", ]$CNSPenetrance <- "-"
drugScreenLibBBB[drugScreenLibBBB$Compound_Id == "MND0001288", ]$p_CNSPenetrance <- 0.791

formatCAS <- function(CAS){
  CAS <-gsub("/", "-", CAS)
  CAS <-as.character(CAS)
  return(CAS)  
}
drugScreenLibBBB$CAS <- formatCAS(drugScreenLibBBB$CAS)


##### relisyr ----
# get relisyr list and generate drug disease table
relisyrStudies <-   RMySQL::dbReadTable(mySQLCon , "ReLiSyRClinicalStudies")
entityOfInterest <- googlesheets4::read_sheet(googleSheetId, sheet="entityOfInterest")
diseaseOfInterest <- entityOfInterest[entityOfInterest$Type == "diseaseOfInterest", ]$Item
drugOfInterest <- entityOfInterest[entityOfInterest$Type == "drugOfInterest", ]$Item

relisyrDrugsDiseaseList <- relisyrStudies
relisyrDrugsDiseaseList$Disease <- factor(relisyrDrugsDiseaseList$Disease, levels = diseaseOfInterest)
relisyrDrugDiseaseCrossTable <- as.data.frame.matrix(table(relisyrDrugsDiseaseList[, c("Drug", "Disease")]))
relisyrDrugDiseaseCrossTable <-rownames_to_column(relisyrDrugDiseaseCrossTable, var="Drug")

relisyrDrugDiseaseCrossTable <- relisyrDrugDiseaseCrossTable %>%
  mutate(relisyr = TRUE)

relisyrOtherDiseasesofInterest <- relisyrDrugDiseaseCrossTable%>%
  select(Drug, AD, FTD, HD, MS, PD)

relisyrOtherDiseasesofInterest$countDiseases<-rowSums(relisyrOtherDiseasesofInterest>0)-1
relisyrDrugDiseaseCrossTable <- left_join(relisyrDrugDiseaseCrossTable, 
                                          select(relisyrOtherDiseasesofInterest, c("Drug", "countDiseases")),
                                          by = "Drug")
relisyrDrugDiseaseCrossTable <- relisyrDrugDiseaseCrossTable %>%
  mutate(relisyrMeetsLogic = ifelse(MND>0 | countDiseases >1, TRUE, FALSE))%>%
  mutate(relisyrPrioritised = ifelse(tolower(Drug) %in% tolower(drugOfInterest), TRUE, FALSE))%>%
  select(-countDiseases)


#to merge relisyr and above CAS list, use regex/drug dictionary to  match drugs with similar but not identical preparations (i.e. different cas numbers)
myDrug <- as.data.frame(unique(drugScreenLibBBB$Name))
names(myDrug) <- "Name"
myDrug$id <- seq_along(myDrug$Name)

minimumIncludeFrequencies <- 6
maximumExcludeFrequencies <- 1

results1 <- ExtractDrug(myDrug, dictionaryName =  as.data.frame(drugBankDictionary), idColumn = "id", textSearchingHeaders = c("Name"), minimumIncludeFrequency = 6,  maximumExcludeFrequency = 1, varnames = c("Drug", "DrugFrequency"), groupAnnotation=F, ignoreCase=T)


drugLibrary <- merge(myDrug, results1)
drugLibrary1<-merge(drugLibrary, drugScreenLibBBB, by = "Name", all = TRUE)


relisyr_DL_list<-merge(relisyrDrugDiseaseCrossTable, drugLibrary1, by = c("Drug"), all = TRUE)%>%
  select(-c("id", "DrugFrequency"))

relisyr_DL_list$CAS<-replace_na(relisyr_DL_list$CAS, "NA")

calculateTotalnPub <- function(druglist){
  data <- druglist %>% select(AD,FTD,HD,MND,MS,PD)
  TotalnPub <- rowSums(data)
  return(TotalnPub)
}

relisyr_DL_list$TotalnPub <- calculateTotalnPub(relisyr_DL_list)

##### wgcna predictions ----

wgcna <- read.csv("data/CompoundList-inSilicoPrediction_MND_CAS.csv")
wgcna <- wgcna %>% rename(CAS = Compound.CAS, Name = CompoundName)
wgcna$CAS <- formatCAS(wgcna$CAS)
wgcna <- wgcna%>%group_by(CAS, Name)%>%
  summarise(Gene = paste(Gene, collapse = "; "))%>%
  mutate(WGCNA = TRUE)
mergedList<-merge(relisyr_DL_list, wgcna, by = c("CAS","Name"), all=TRUE)


wgcnaBBB <- read.csv("data/2022-08-17_WGCNA_BBB.csv")
wgcnaBBB <- wgcnaBBB %>%
  select(CAS = Compound.CAS, 
         Name1 = Name,
         SMILES1 = SMILES,
         CNSPenetrance1 = CNSPenetrance,
         p_CNSPenetrance1 = probability)
wgcnaBBB<-wgcnaBBB%>% replace_na(list(1:5, "NA"))
wgcnaBBB$CAS <- formatCAS(wgcnaBBB$CAS)

mergedList <- left_join(mergedList, wgcnaBBB, by = "CAS", all =TRUE)
mergedList <- mergedList %>%
  mutate(Name = ifelse(Name == "", Name1, Name),
         SMILES = ifelse(SMILES == ""|is.na(SMILES), SMILES1, SMILES),
         CNSPenetrance = ifelse(CNSPenetrance == ""|is.na(CNSPenetrance), CNSPenetrance1, CNSPenetrance),
         p_CNSPenetrance = ifelse(p_CNSPenetrance ==""|is.na(p_CNSPenetrance), p_CNSPenetrance1, p_CNSPenetrance))

mergedList <- mergedList %>%
  select(-Name1, -SMILES1, -CNSPenetrance1, -p_CNSPenetrance1)

mergedList <- mergedList%>%
  mutate(CNSPenetrance = ifelse(CNSPenetrance == "+", TRUE, ifelse(CNSPenetrance == "-", FALSE, NA)))


mergedList<-mergedList%>%
  modify_if(is.logical, replace_na, FALSE)%>%
  modify_if(is.integer, replace_na, 0L)


#getChemblData

mergedList1 <- mergedList%>% mutate(across(everything(), as.character))
mergedList1 <- mergedList1 %>% mutate(row = row_number())
mergedList1 <- mergedList1 %>% mutate(Name = ifelse( (is.na(Name) | Name == "")& !is.na(Drug), Drug, Name ))



for (i in 1:nrow(mergedList1)){
  drugi<-mergedList1[i, ]
  drug<- drugi$Name%>%tolower()
  drug <-gsub(" ", "%20", drug)
  row<-i
  url <- paste0("https://www.ebi.ac.uk/chembl/api/data/molecule?pref_name__iexact=", drug)
  
  data <- read_html(url)
  molecules <- data %>%html_node("molecules")%>%html_children()
  if(length(molecules)==0){
    url <- paste0("https://www.ebi.ac.uk/chembl/api/data/molecule?molecule_synonyms__molecule_synonym__iexact=", drug)
    data <- read_html(url)
    molecules <- data %>%html_node("molecules")%>%html_children()
  }
  if(length(molecules)>0){
    for(n in 1:length(molecules)){
      data<-molecules[n]
      name <- data %>%html_nodes("pref_name")%>%html_text()%>%paste(collapse="; ")
      chemblIDs <- data %>%html_nodes("molecule_chembl_id")%>%html_text()%>%unique()%>%paste(collapse="; ")
      synonyms <- data %>%html_nodes("molecule_synonym")%>%html_text()%>%unique()%>%paste(collapse="; ")
      SMILES <- data %>% html_node("canonical_smiles")%>%html_text()%>%paste(collapse="; ")  
      mol_formula<-data%>%html_node("full_molformula")%>%html_text()%>%paste(collapse="; ")
      mol_weight<-data%>%html_node("full_mwt")%>%html_text()%>%paste(collapse="; ")
      mol_inchi<- data%>%html_node("standard_inchi")%>%html_text()%>%paste(collapse="; ")
      mol_type<-data%>%html_node("molecule_type")%>%html_text()%>%paste(collapse="; ")
      natural_product <- data %>%html_node("natural_product")%>%html_text()%>%paste(collapse="; ")
      natural_product <- ifelse(natural_product =="", FALSE, TRUE)%>%paste(collapse="; ")
      oral <- data %>%html_node("oral")%>%html_text()
      oral <- ifelse(oral =="", FALSE, TRUE)%>%paste(collapse="; ")
      class <- data%>%html_node("usan_stem_definition")%>%html_text()%>%paste(collapse="; ")
      ro5Violations <- data%>%html_node("num_ro5_violations")%>%html_text()
      ro5 <- ifelse(ro5Violations == "", TRUE, FALSE)%>%paste(collapse="; ")
      ro3_pass <- data %>%html_node("ro3_pass")%>%html_text()
      ro3 <- ifelse(ro3_pass == "Y", TRUE, FALSE)%>%paste(collapse="; ")
      max_phase <- data %>% html_node("max_phase")%>%html_text()%>%paste(collapse="; ")
      availability_type <- data %>%html_node("availability_type")%>%html_text()
      df<- data.frame(row, name, chemblIDs, synonyms, SMILES, mol_formula, mol_weight, mol_type, natural_product, class, ro5, ro3, max_phase, availability_type, oral)
      
    }
    
    if(!exists("outputDF")) outputDF <- df else outputDF<-rbind(outputDF, df)
  }}


outputDF <- outputDF%>%
  rename(chemblName = name,
         chemblSMILES = SMILES)


newDrugList <- left_join(mergedList1, outputDF, by = "row")
newDrugList <- newDrugList%>%select(-row, -chemblName)

#ADD drug screen results
MX1<- read_excel("data/Formatted_MX1_AstroScreen.xlsx", sheet = "MX1_AstroScreen")
MX1_DRCList <- read_excel("data/Formatted_MX1_AstroScreen.xlsx", sheet = "MX1_DRCList")
MX1_DRCStatus <- MX1_DRCList%>%
  select(Compound_Id, DRCStatus = Status)
MX1 <- left_join(MX1, MX1_DRCStatus, by= "Compound_Id")
MX1confirmation <- read_excel("data/Formatted_MX1_AstroScreen.xlsx", sheet = "MX1_ConfirmedHits")
astrocyteMCB<-read.csv("data/MCB_retest_results[1].csv")
proteinAggregation<-read.csv("data/HitList_ProteinAggregationAssay_drc-retests-activity.csv")

astrocyteMCB <- astrocyteMCB %>% rename(Compound_Id = Compound_ID)

astrocyteMCB1<-astrocyteMCB%>%
  group_by(Compound_Id)%>%
  filter(z.score_fluorescence >1.96)

astrocyteMCB1 <- astrocyteMCB1 %>%
  filter(z.score_confluence > -1.96)%>%
  select (Compound_Id)%>%
  unique()

astrocyteMCB <-astrocyteMCB %>% select(Compound_Id)%>%unique()

proteinAggregation<-proteinAggregation%>%select(Compound_Id)

MX1 <- MX1%>%
  mutate(MX1_ModHits = ifelse(IntegratedIntensity_normalized_Zscore< -1.96 & 
                                IntegratedIntensity_normalized_Zscore> -3 &
                                NucleiCount_Zscore> -1.96 , TRUE, FALSE))%>%
  mutate(MX1_StrongHits = ifelse(IntegratedIntensity_normalized_Zscore< -3 & NucleiCount_Zscore> -1.96 , TRUE, FALSE))


MX1 <- MX1 %>% select(Compound_Id,
                      MX1_ModHits, 
                      MX1_StrongHits, 
                      MX1_DRCStatus = DRCStatus)

MX1confirmation <- MX1confirmation %>% 
  select(Compound_Id, 
         MX1ConfZScore = IntegratedIntensity_normalized_Zscore,
         MX1ConfNucleiCountZScore = NucleiCount_Zscore)



newDrugList1 <- newDrugList%>%
  mutate(MCBPositiveHit = ifelse(Compound_Id %in% astrocyteMCB1$Compound_Id, TRUE, FALSE),
         astrocyteMCB = ifelse(Compound_Id %in% astrocyteMCB$Compound_Id, TRUE, FALSE),
         proteinAggregation = ifelse(Compound_Id %in% proteinAggregation$Compound_Id, TRUE, FALSE))


newDrugList1 <- left_join(newDrugList1, MX1, by = "Compound_Id")
newDrugList1 <- left_join(newDrugList1, MX1confirmation, by = "Compound_Id")

newDrugList1<- newDrugList1 %>% 
  mutate(MX1_ModHits = ifelse(MX1_ModHits != TRUE, FALSE, TRUE),
         MX1_StrongHits = ifelse(MX1_StrongHits != TRUE, FALSE, TRUE))

newDrugList1 <- newDrugList1 %>%
  mutate(MX1_DRC1 = ifelse(MX1_DRCStatus == "DRCList", TRUE, FALSE),
         MX1_DRC_all = ifelse(!is.na(MX1_DRCStatus), TRUE, FALSE))

newDrugList1 <- newDrugList1 %>% 
  mutate(MX1_ModHits = ifelse(is.na(MX1_ModHits)|MX1_ModHits=="", FALSE, MX1_ModHits),
         MX1_StrongHits = ifelse(is.na(MX1_StrongHits|MX1_StrongHits == ""), FALSE, MX1_StrongHits),
         MX1_DRC1 = ifelse(is.na(MX1_DRC1)|MX1_DRC1=="", FALSE, MX1_DRC1),
         MX1_DRC_all = ifelse(is.na(MX1_DRC_all)|MX1_DRC_all == "", FALSE, MX1_DRC_all)
  )


newDrugList1 <- newDrugList1%>% mutate(drugScreenLib = ifelse(!is.na(Compound_Id), TRUE, FALSE))
newDrugList1 <- newDrugList1 %>% unique()

newDrugList1 <- left_join(newDrugList1, CTList, by = "Drug")

write_sheet(newDrugList1, ICANGooglesheetId, sheet = "withMX1Data")
write.csv(newDrugList1, paste0("output/",Sys.Date(), "allDrugsList.csv"), row.names = FALSE)


#


#
# mergedList2 <- mergedList1 %>% mutate(Name = ifelse( (is.na(Name) | Name == "")& !is.na(Drug), Drug, Name ))
# 
# mergedList2ToGetData <- mergedList2 %>% filter(!row %in% outputDF$row & !is.na(Name) & Name != "" & !is.na(Drug) & Drug != "")
# mergedList2ToGetData <- left_join(mergedList2ToGetData, select(mergedList1, row, Name1 = Name), by = "row")
# mergedList2ToGetData <- mergedList2ToGetData%>%
#   relocate(Name1, .after = Name)%>%
#   mutate(toGetData = ifelse(Name %in% Name1, F, T))%>%
#   filter(toGetData == TRUE)
# 
# mergedList2ToGetData <- mergedList2ToGetData%>%
#   select(-toGetData, - Name1)
# 
# 
# for (i in 1:nrow(mergedList2ToGetData)){
#   drugi<-mergedList2ToGetData[i, ]
#   drug<- drugi$Name%>%tolower()
#   drug <-gsub(" ", "%20", drug)
#   row<-drugi$row
#   url <- paste0("https://www.ebi.ac.uk/chembl/api/data/molecule?pref_name__iexact=", drug)
#   
#   data <- read_html(url)
#   molecules <- data %>%html_node("molecules")%>%html_children()
#   if(length(molecules)==0){
#     url <- paste0("https://www.ebi.ac.uk/chembl/api/data/molecule?molecule_synonyms__molecule_synonym__iexact=", drug)
#     data <- read_html(url)
#     molecules <- data %>%html_node("molecules")%>%html_children()
#   }
#   if(length(molecules)>0){
#     for(n in 1:length(molecules)){
#       data<-molecules[n]
#       name <- data %>%html_nodes("pref_name")%>%html_text()%>%paste(collapse="; ")
#       chemblIDs <- data %>%html_nodes("molecule_chembl_id")%>%html_text()%>%unique()%>%paste(collapse="; ")
#       synonyms <- data %>%html_nodes("molecule_synonym")%>%html_text()%>%unique()%>%paste(collapse="; ")
#       SMILES <- data %>% html_node("canonical_smiles")%>%html_text()%>%paste(collapse="; ")  
#       mol_formula<-data%>%html_node("full_molformula")%>%html_text()%>%paste(collapse="; ")
#       mol_weight<-data%>%html_node("full_mwt")%>%html_text()%>%paste(collapse="; ")
#       mol_inchi<- data%>%html_node("standard_inchi")%>%html_text()%>%paste(collapse="; ")
#       mol_type<-data%>%html_node("molecule_type")%>%html_text()%>%paste(collapse="; ")
#       natural_product <- data %>%html_node("natural_product")%>%html_text()%>%paste(collapse="; ")
#       natural_product <- ifelse(natural_product =="", FALSE, TRUE)%>%paste(collapse="; ")
#       oral <- data %>%html_node("oral")%>%html_text()
#       oral <- ifelse(oral =="", FALSE, TRUE)%>%paste(collapse="; ")
#       class <- data%>%html_node("usan_stem_definition")%>%html_text()%>%paste(collapse="; ")
#       ro5Violations <- data%>%html_node("num_ro5_violations")%>%html_text()
#       ro5 <- ifelse(ro5Violations == "", TRUE, FALSE)%>%paste(collapse="; ")
#       ro3_pass <- data %>%html_node("ro3_pass")%>%html_text()
#       ro3 <- ifelse(ro3_pass == "Y", TRUE, FALSE)%>%paste(collapse="; ")
#       max_phase <- data %>% html_node("max_phase")%>%html_text()%>%paste(collapse="; ")
#       availability_type <- data %>%html_node("availability_type")%>%html_text()
#       df<- data.frame(row, name, chemblIDs, synonyms, SMILES, mol_formula, mol_weight, mol_type, natural_product, class, ro5, ro3, max_phase, availability_type, oral)
#       
#     }
#     
#     if(!exists("outputDF")) outputDF <- df else outputDF<-rbind(outputDF, df)
#   }}
