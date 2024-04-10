# RUN ON CW MACBOOK, REQUIRES RSelenium and Docker#

library("RSelenium")
library(htmltools)
library(rvest)
library(dplyr)
library(xml2)

#Ensure Docker Desktop app running with running session. 
#Otherwise, install Docker Desktop and use code below (if MacOS/Linux; otherwise try shell() instead of system(); 
#port number might need to be changed) to initiate Docker session

#system('docker run -d -p 4445:4444 selenium/standalone-chrome')

#port number and browser dependent on OS
rD <- rsDriver(browser="chrome", port=4445L, verbose=F)
remDr <- rD[["client"]]
remDr$navigate("http://lmmd.ecust.edu.cn/admetsar2/")

#check correct url loaded
remDr$getCurrentUrl()

#for single drug-------

#load drug SMILES; example using 1st drug in drugLibrary
drugSMILES <- read.csv("DrugLibrary.csv")
testSMILES1 <- drugSMILES$SMILES[1]

#enter SMILES into input box
remDr$findElement(using = "name", value = "smiles")$sendKeysToElement(list(testSMILES1))

#click on predict button
remDr$findElement(using = "name", value = "predict_compound")$clickElement()

# give the page time to fully load
Sys.sleep(15) 
#check that page has loaded - should return a /result/?tid= [query id] in url
remDr$getCurrentUrl()

#get html data
html <- remDr$getPageSource()[[1]]

#parse html data to get table, then identify BBB data, make into BBB data frame
admetSARdata <- read_html(html)%>%
  html_table(fill = TRUE)

BBB<-admetSARdata[[5]]

names(BBB)<- c("ADMETPredictedProfile", "value", "probability")

BBB<- BBB%>%
  filter(ADMETPredictedProfile == "Blood Brain Barrier")%>%
  mutate(SMILES = testSMILES1)%>%
  select(-ADMETPredictedProfile)%>%
  relocate(SMILES, .before = "value")%>%
  rename(CNSPenetrance = value)


#loop for multiple drug SMILES--------------
#load list of drug SMILES
SMILES <- drugSMILES$SMILES

#create empty output data frame
BBBdf<- data.frame(SMILES = c(),
                   CNSPenetrance = c(),
                   probability = c())

for (i in SMILES){
  remDr$navigate("http://lmmd.ecust.edu.cn/admetsar2/")
  remDr$findElement(using = "name", value = "smiles")$sendKeysToElement(list(i))
  remDr$findElement(using = "name", value = "predict_compound")$clickElement()
  Sys.sleep(15) 
  html <- remDr$getPageSource()[[1]]
  admetSARdata <- read_html(html)%>%
    html_table(fill = TRUE)
  if(length(admetSARdata)<5) next
  else{
    BBB<-admetSARdata[[5]]
    names(BBB)<- c("ADMETPredictedProfile", "value", "probability")
    BBB<- BBB%>%
      filter(ADMETPredictedProfile == "Blood Brain Barrier")%>%
      mutate(SMILES = i)%>%
      select(-ADMETPredictedProfile)%>%
      relocate(SMILES, .before = "value")%>%
      rename(CNSPenetrance = value)
    BBBdf<- rbind(BBBdf, BBB)
  }
}


drugLibraryWithBBB <- left_join(drugLibrary, BBBdf, by = "SMILES")%>%unique()

write.csv(drugLibraryWithBBB, paste0("drugLibrary_AdmetSARBBB", Sys.Date(), ".csv"), row.names = FALSE)


