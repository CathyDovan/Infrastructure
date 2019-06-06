###########SETUP ENV##############
#install.packages("e1071", repos = "https://cran.rstudio.com")
#install.packages("gmodels")
#install.packages("caret")
#install.packages("pdftools")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("tidytext")
#install.packages("dplyr")
#install.packages("class")
#install.packages("stringr")
#install.packages("openxlsx")
library(tm)
library(dplyr)
library(tidyverse)
library(tidytext)
library(reshape2)
library(class)
library(e1071)
library(gmodels)
library(caret)
library(stringr)
library(openxlsx)

###########Apply to new Data############
#Load and format data into DTM
#read csv
#pims_raw <- read.csv("http://35.183.198.35/docs/pims_data.csv", stringsAsFactors = FALSE)
pims_raw<-read.csv(input$file1$datapath)
names(pims_raw)<-str_replace_all(names(pims_raw), c(" " = "." , "," = "" ))
pims_raw<-pims_raw %>% 
  rename(Std.Cat=Standardized.Category,Title=Title..EN.,Descr=Description..EN.)
pims_raw$INFEA.Multiplier.Category<-NA
str(pims_raw)

#convert categories to factors (maybe)
pims_raw$INFEA.Multiplier.Category <-factor(pims_raw$INFEA.Multiplier.Category)
#str(pims_raw$INFEA.Multiplier.Category)

#combine columns (Std.Cat, Title, Descr.)
pims_raw$text <- paste(pims_raw$Std.Cat,pims_raw$Title,pims_raw$Descr)
#str(pims_raw$text)

#Data Prep
#create corpus
pims_corpus<-VCorpus(VectorSource(pims_raw$text))
#print(proj_corpus)
#inspect(proj_corpus[1:5])

#clean corpus
#Removing specific words
stopwords <- read.csv("http://35.183.198.35/docs/stopwords.csv",header=FALSE)
stopwords <- as.character(stopwords$V1)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c(stopwords,stopwords("en"))) 
  return(corpus)
}

pims.corpus.clean <- clean_corpus(pims_corpus)
#inspect(pims.corpus.clean[1:5])

#Create Doctument Term Matrix (DTM)
pims.dtm <- DocumentTermMatrix(pims.corpus.clean)
#check DTM
#inspect(proj.dtm[1:20,20:30])
#pims.dtm

#Find frequent terms
pims_FT<-findFreqTerms(pims.dtm,10)
#pims_FT[1:100]

pims.dtm.ft<-pims.dtm[,pims_FT]

#Change numerical values to Yes/No in DTM
yes.no <- function(x){
  y <- ifelse(x>0,1,0)
  y <- factor(y,levels=c(0,1),labels=c("No","Yes"))
  return(y)
}
pims_input<-apply(pims.dtm.ft,2,yes.no)

#load model
project_classifier<-readRDS("./project_classifier.rds")
#print(project_classifier)

#classify pims data
pims_classify<-predict(project_classifier,newdata=pims_input)

#combine prediction in dataset
pims_output<-pims_raw
pims_output$INFEA.Multiplier.Category<-pims_classify

#apply multiplier effect
#load multiplier table and rename columns
multiplier <- read.csv("http://35.183.198.35/docs/infea_multiplier_tool_2015.csv", header = TRUE)
multiplier <- multiplier %>% 
  rename(Region=geo,INFEA.Multiplier.Category=Asset)

# convert the multiplier sheet into a wide format so that multiplier types are in individual columns
multiplierspread <- reshape(multiplier, idvar = c('year','Region','INFEA.Multiplier.Category'), direction = 'wide', timevar = 'type')

#merge multiplier and pims_output
combined <- merge(pims_output, multiplierspread, all.x = TRUE)

#convert costs from factors to numbers for calculation
combined$Total.Eligible.Costs <- as.numeric(as.character(gsub('\\$|,', '',combined$Total.Eligible.Costs)))
combined$Program.Contribution <- as.numeric(as.character(gsub("[$,]","",combined$Program.Contribution)))

# Calculate all of the multiplier output fields
combined$Direct.Jobs.Eligible <- combined$Total.Eligible.Costs / 1000 * combined$value.EMPL_DIR
combined$Total.Jobs.Eligible <- combined$Total.Eligible.Costs / 1000 * combined$value.EMPLT
combined$Indirect.Jobs.Eligible <- combined$Total.Jobs.Eligible - combined$Direct.Jobs.Eligible
combined$Direct.Value.Added.Eligible <- combined$Total.Eligible.Costs  * combined$value.GDPD
combined$Total.Value.Added.Eligible <- combined$Total.Eligible.Costs * combined$value.GDPT
combined$Indirect.Value.Added.Eligible <-combined$Total.Value.Added.Eligible - combined$Direct.Value.Added.Eligible
combined$Direct.Compensation.Eligible <- combined$Total.Eligible.Costs * combined$value.WSLID
combined$Total.Compensation.Eligible <- combined$Total.Eligible.Costs * combined$value.WSLIT
combined$Indirect.Compensation.Eligible <- combined$Total.Compensation.Eligible - combined$Direct.Compensation.Eligible
combined$Imports.Eligible <- combined$Total.Eligible.Costs * combined$value.IMPT
combined$Taxes.Eligible <- combined$Total.Eligible.Costs * combined$value.TAXFND

combined$Direct.Jobs.Contribution <- combined$Program.Contribution / 1000 * combined$value.EMPL_DIR
combined$Total.Jobs.Contribution <- combined$Program.Contribution / 1000 * combined$value.EMPLT
combined$Indirect.Jobs.Contribution <- combined$Total.Jobs.Contribution - combined$Direct.Jobs.Contribution
combined$Direct.Value.Added.Contribution <- combined$Program.Contribution * combined$value.GDPD
combined$Total.Value.Added.Contribution <- combined$Program.Contribution * combined$value.GDPT
combined$Indirect.Value.Added.Contribution <- combined$Total.Value.Added.Contribution - combined$Direct.Value.Added.Contribution
combined$Direct.Compensation.Contribution <- combined$Program.Contribution * combined$value.WSLID
combined$Total.Compensation.Contribution <- combined$Program.Contribution * combined$value.WSLIT
combined$Indirect.Compensation.Contribution <- combined$Total.Compensation.Contribution - combined$Direct.Compensation.Contribution
combined$Imports.Contribution <- combined$Program.Contribution * combined$value.IMPT
combined$Taxes.Contribution <- combined$Program.Contribution * combined$value.TAXFND

#Format the output columns
combined$Direct.Jobs.Eligible <- format(round(combined$Direct.Jobs.Eligible,0),nsmall=0,big.mark=",")
combined$Total.Jobs.Eligible <- format(round(combined$Total.Jobs.Eligible,0),nsmall=0,big.mark=",")
combined$Indirect.Jobs.Eligible <- format(round(combined$Indirect.Jobs.Eligible,0),nsmall=0,big.mark=",")
combined$Direct.Value.Added.Eligible <- gsub("[ ]","",paste("$",format(round(combined$Direct.Value.Added.Eligible,0),big.mark=","),sep=""))
combined$Total.Value.Added.Eligible <-  gsub("[ ]","",paste("$",format(round(combined$Total.Value.Added.Eligible,0),nsmall=0,big.mark=","),sep=""))
combined$Indirect.Value.Added.Eligible <- gsub("[ ]","",paste("$",format(round(combined$Indirect.Value.Added.Eligible,0),nsmall=0,big.mark=","),sep=""))
combined$Direct.Compensation.Eligible <- gsub("[ ]","",paste("$",format(round(combined$Direct.Compensation.Eligible,0),nsmall=0,big.mark=","),sep=""))
combined$Total.Compensation.Eligible <- gsub("[ ]","",paste("$",format(round(combined$Total.Compensation.Eligible,0),nsmall=0,big.mark=","),sep=""))
combined$Indirect.Compensation.Eligible <- gsub("[ ]","",paste("$",format(round(combined$Indirect.Compensation.Eligible,0),nsmall=0,big.mark=","),sep=""))
combined$Imports.Eligible <- gsub("[ ]","",paste("$",format(round(combined$Imports.Eligible,0),nsmall=0,big.mark=","),sep=""))
combined$Taxes.Eligible <- gsub("[ ]","",paste("$",format(round(combined$Taxes.Eligible,0),nsmall=0,big.mark=","),sep=""))

combined$Direct.Jobs.Contribution <- format(round(combined$Direct.Jobs.Contribution,0),nsmall=0,big.mark=",")
combined$Total.Jobs.Contribution <- format(round(combined$Total.Jobs.Contribution,0),nsmall=0,big.mark=",")
combined$Indirect.Jobs.Contribution <- format(round(combined$Indirect.Jobs.Contribution,0),nsmall=0,big.mark=",")
combined$Direct.Value.Added.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Direct.Value.Added.Contribution,0),nsmall=0,big.mark=","),sep=""))
combined$Total.Value.Added.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Total.Value.Added.Contribution,0),nsmall=0,big.mark=","),sep=""))
combined$Indirect.Value.Added.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Indirect.Value.Added.Contribution,0),nsmall=0,big.mark=","),sep=""))
combined$Direct.Compensation.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Direct.Compensation.Contribution,0),nsmall=0,big.mark=","),sep=""))
combined$Total.Compensation.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Total.Compensation.Contribution,0),nsmall=0,big.mark=","),sep=""))
combined$Indirect.Compensation.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Indirect.Compensation.Contribution,0),nsmall=0,big.mark=","),sep=""))
combined$Imports.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Imports.Contribution,0),nsmall=0,big.mark=","),sep=""))
combined$Taxes.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Taxes.Contribution,0),nsmall=0,big.mark=","),sep=""))

combined$Total.Eligible.Costs <- gsub("[ ]","",paste("$",format(round(combined$Total.Eligible.Costs,0),nsmall=0,big.mark=","),sep=""))
combined$Program.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Program.Contribution,0),nsmall=0,big.mark=","),sep=""))

#Remove multiplier value fields from the output and move Project number to the first column
combined <- combined %>%
  select("ï..Project..",everything()) %>%
  select(-one_of("year","value.GDPD","value.EMPL_DIR","value.EMPLT","value.GDPT","value.WSLID","value.WSLIT","value.IMPT","value.TAXFND","value.IMP_DIR","value.IMP_INI"))

#export dataset as csv
write.csv(combined,"output_pims.csv")

