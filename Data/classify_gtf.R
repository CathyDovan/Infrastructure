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
#library(gmodels)
#library(caret)
library(stringr)
#library(openxlsx)

###########Apply to new Data############
#Load and format data into DTM
#read csv
gtf_raw <- read.csv("http://35.183.198.35/docs/gtf_data.csv", stringsAsFactors = FALSE)
#gtf_raw<-read.csv(input$file1$datapath)
names(gtf_raw)<-str_replace_all(names(gtf_raw), c(" " = "." , "," = "" ))
gtf_raw<-gtf_raw %>% 
  rename(Std.Cat=Standardized.Category,Title=Project.Title,Descr=Project.Description)
gtf_raw$INFEA.Multiplier.Category<-NA
#str(gtf_raw)

#convert categories to factors (maybe)
gtf_raw$INFEA.Multiplier.Category <-factor(gtf_raw$INFEA.Multiplier.Category)
#str(gtf_raw$INFEA.Multiplier.Category)

#combine columns (Std.Cat, Title, Descr.)
gtf_raw$text <- paste(gtf_raw$Std.Cat,gtf_raw$Title,gtf_raw$Descr)
#str(gtf_raw$text)

#Data Prep
#create corpus
gtf_corpus<-VCorpus(VectorSource(gtf_raw$text))
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

gtf.corpus.clean <- clean_corpus(gtf_corpus)
#inspect(gtf.corpus.clean[1:5])

#Create Doctument Term Matrix (DTM)
gtf.dtm <- DocumentTermMatrix(gtf.corpus.clean)
#check DTM
#inspect(proj.dtm[1:20,20:30])
#gtf.dtm

#Find frequent terms
gtf_FT<-findFreqTerms(gtf.dtm,10)
#gtf_FT[1:100]

gtf.dtm.ft<-gtf.dtm[,gtf_FT]

#Change numerical values to Yes/No in DTM
yes.no <- function(x){
  y <- ifelse(x>0,1,0)
  y <- factor(y,levels=c(0,1),labels=c("No","Yes"))
  return(y)
}
gtf_input<-apply(gtf.dtm.ft,2,yes.no)

#load model
project_classifier<-readRDS("./project_classifier.rds")
#print(project_classifier)

#classify gtf data
gtf_classify<-predict(project_classifier,newdata=gtf_input)

#combine prediction in dataset
gtf_output<-gtf_raw
gtf_output$INFEA.Multiplier.Category<-gtf_classify

#apply multiplier effect
#load multiplier table and rename columns
multiplier <- read.csv("http://35.183.198.35/docs/infea_multiplier_tool_2015.csv", header = TRUE)
multiplier <- multiplier %>% 
  rename(Region=geo,INFEA.Multiplier.Category=Asset)

# convert the multiplier sheet into a wide format so that multiplier types are in individual columns
multiplierspread <- reshape(multiplier, idvar = c('year','Region','INFEA.Multiplier.Category'), direction = 'wide', timevar = 'type')

#reformat gtf output for geography column
gtf_output$Region<-ifelse(gtf_output$Province.Territory=="Alberta","AB", 
                  ifelse(gtf_output$Province.Territory=="British Columbia","BC",
                  ifelse(gtf_output$Province.Territory=="Manitoba","MB",
                  ifelse(gtf_output$Province.Territory=="Saskatchewan","SK",
                  ifelse(gtf_output$Province.Territory=="Ontario","ON",
                  ifelse(gtf_output$Province.Territory=="Quebec","QC",
                  ifelse(gtf_output$Province.Territory=="New Brunswick","NB",
                  ifelse(gtf_output$Province.Territory=="Nova Scotia","NS",
                  ifelse(gtf_output$Province.Territory=="Newfoundland and Labrador","NL",
                  ifelse(gtf_output$Province.Territory=="Prince Edward Island","PE",
                  ifelse(gtf_output$Province.Territory=="Yukon","YT",
                  ifelse(gtf_output$Province.Territory=="Northwest Territories","NT",
                  ifelse(gtf_output$Province.Territory=="Nunavut","NU"," ")))))))))))))

gtf_output <- gtf_output %>% 
  rename(Total.Project.Costs=Total.Project.Cost)


#merge multiplier and gtf_output
combined <- merge(gtf_output, multiplierspread, all.x = TRUE)

#convert costs from factors to numbers for calculation
combined$Total.Project.Costs <- as.numeric(as.character(gsub('\\$|,', '',combined$Total.Project.Costs)))
combined$Total.Program.Contribution <- as.numeric(as.character(gsub("[$,]","",combined$Total.Program.Contribution)))

# Calculate all of the multiplier output fields
combined$Direct.Jobs.Eligible <- combined$Total.Project.Costs / 1000 * combined$value.EMPL_DIR
combined$Total.Jobs.Eligible <- combined$Total.Project.Costs / 1000 * combined$value.EMPLT
combined$Indirect.Jobs.Eligible <- combined$Total.Jobs.Eligible - combined$Direct.Jobs.Eligible
combined$Direct.Value.Added.Eligible <- combined$Total.Project.Costs  * combined$value.GDPD
combined$Total.Value.Added.Eligible <- combined$Total.Project.Costs * combined$value.GDPT
combined$Indirect.Value.Added.Eligible <-combined$Total.Value.Added.Eligible - combined$Direct.Value.Added.Eligible
combined$Direct.Compensation.Eligible <- combined$Total.Project.Costs * combined$value.WSLID
combined$Total.Compensation.Eligible <- combined$Total.Project.Costs * combined$value.WSLIT
combined$Indirect.Compensation.Eligible <- combined$Total.Compensation.Eligible - combined$Direct.Compensation.Eligible
combined$Imports.Eligible <- combined$Total.Project.Costs * combined$value.IMPT
combined$Taxes.Eligible <- combined$Total.Project.Costs * combined$value.TAXFND

combined$Direct.Jobs.Contribution <- combined$Total.Program.Contribution / 1000 * combined$value.EMPL_DIR
combined$Total.Jobs.Contribution <- combined$Total.Program.Contribution / 1000 * combined$value.EMPLT
combined$Indirect.Jobs.Contribution <- combined$Total.Jobs.Contribution - combined$Direct.Jobs.Contribution
combined$Direct.Value.Added.Contribution <- combined$Total.Program.Contribution * combined$value.GDPD
combined$Total.Value.Added.Contribution <- combined$Total.Program.Contribution * combined$value.GDPT
combined$Indirect.Value.Added.Contribution <- combined$Total.Value.Added.Contribution - combined$Direct.Value.Added.Contribution
combined$Direct.Compensation.Contribution <- combined$Total.Program.Contribution * combined$value.WSLID
combined$Total.Compensation.Contribution <- combined$Total.Program.Contribution * combined$value.WSLIT
combined$Indirect.Compensation.Contribution <- combined$Total.Compensation.Contribution - combined$Direct.Compensation.Contribution
combined$Imports.Contribution <- combined$Total.Program.Contribution * combined$value.IMPT
combined$Taxes.Contribution <- combined$Total.Program.Contribution * combined$value.TAXFND

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

combined$Total.Project.Costs <- gsub("[ ]","",paste("$",format(round(combined$Total.Project.Cost,0),nsmall=0,big.mark=","),sep=""))
combined$Total.Program.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Total.Program.Contribution,0),nsmall=0,big.mark=","),sep=""))

#Remove multiplier value fields from the output and move Project number to the first column
combined <- combined %>%
  select("Project..",everything()) %>%
  select(-one_of("year","value.GDPD","value.EMPL_DIR","value.EMPLT","value.GDPT","value.WSLID","value.WSLIT","value.IMPT","value.TAXFND","value.IMP_DIR","value.IMP_INI","text"))

#export dataset as csv
write.csv(combined,"output_gtf.csv")

