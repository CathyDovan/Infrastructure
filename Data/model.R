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
library(pdftools)
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

###########START HERE##############
#Naive-bayes Model - Gaussian Dist'n

#read csv and reformat names of required columns
proj_raw <- read.csv("http://35.183.198.35/docs/training_data.csv", stringsAsFactors = FALSE)
names(proj_raw)<-str_replace_all(names(proj_raw), c(" " = "." , "," = "" ))
proj_raw<-proj_raw %>% 
  rename(Std.Cat=Standardized.Category,Title=Title..EN.,Descr=Description..EN.,INFEA.Multiplier.Category=INFEA.Multiplier )
#str(proj_raw)

#delete all rows without INFEA multiplier(for training)
proj_raw<- proj_raw[!(is.na(proj_raw$INFEA.Multiplier.Category) | proj_raw$INFEA.Multiplier.Category==""), ]


#convert categories to factors
proj_raw$INFEA.Multiplier.Category <-factor(proj_raw$INFEA.Multiplier.Category)
#str(proj_raw$INFEA.Multiplier.Category)

#table(proj_raw$INFEA.Multiplier.Category)

#combine columns (Std.Cat, Title, Descr.)
proj_raw$text <- paste(proj_raw$Std.Cat,proj_raw$Title,proj_raw$Descr)
str(proj_raw$text)

#Data Prep
#create corpus
proj_corpus<-VCorpus(VectorSource(proj_raw$text))
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

proj.corpus.clean <- clean_corpus(proj_corpus)
#inspect(proj.corpus.clean[1:5])

#Create Doctument Term Matrix (DTM)
proj.dtm <- DocumentTermMatrix(proj.corpus.clean)
#check DTM
#inspect(proj.dtm[1:20,20:30])
#proj.dtm

#Create Training and Testing Datasets
#set training sample at 70% of dataset
ind = sample(1:nrow(proj_raw), size=0.7*nrow(proj_raw))

#separate categories in training and testing datasets(for ease of use later on)
proj.train.labels <- proj_raw[ind,]$INFEA.Multiplier.Category
proj.test.labels <- proj_raw[-ind,]$INFEA.Multiplier.Category

#check label distribution (is the sample balanced and representative?)
#prop.table(sort(table(proj.train.labels)))
#prop.table(sort(table(proj.test.labels)))
#prop.table(sort(table(proj_raw$INFEA.Multiplier.Category)))

#create training and testing datasets
proj.DTM.train<- proj.dtm[ind,]
proj.DTM.test<- proj.dtm[-ind,]

#create corpus versions of training and testing matrices
#proj.corpus.clean.train<-proj.corpus.clean[ind]
#proj.corpus.clean.test<-proj.corpus.clean[-ind]

#select freq terms for each category
a_oil.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Other oil and gas engineering construction")
a_hospital.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Hospitals, health centres, clinics, nursing homes and other health care buildings")
a_turbine.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Turbines and turbine generator set units")
a_school.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Schools, colleges, universities and other educational buildings")
a_electric.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Electric power engineering construction")
a_communication.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Communication engineering construction")
a_marine.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Marine engineering construction")
a_other_eng.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Other engineering construction")
a_bus.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Buses")
a_inst_bldg.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Other institutional buildings")
a_comm_bldg.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Other commercial buildings")
a_transport.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Other transportation construction")
a_water.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Waterworks engineering construction")
a_sewer.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Sewage engineering construction")
a_road.train = subset(proj_raw[ind,], INFEA.Multiplier.Category == "Highway, roads, streets, bridges and overpasses")

#identify frequent terms in DTM, terms that appear at least 10 times
Freq.Terms<-findFreqTerms(proj.DTM.train,10)
#length(Freq.Terms)
#Freq.Terms[1:100]

#identify frequent terms in training and testing data
proj.dtm.freq.terms.train<-proj.DTM.train[,Freq.Terms]
proj.dtm.freq.terms.test<-proj.DTM.test[,Freq.Terms]

#inspect(proj.dtm.freq.terms.train[1:20,20:30])
#inspect(proj.dtm.freq.terms.test[1:20,20:30])

#convert all 0/1 to No/Yes for Classifier Model (model cannot read numerical input)
yes.no <- function(x){
  y <- ifelse(x>0,1,0)
  y <- factor(y,levels=c(0,1),labels=c("No","Yes"))
  return(y)
}

proj.train <- apply(proj.dtm.freq.terms.train,2,yes.no)
proj.test <- apply(proj.dtm.freq.terms.test,2,yes.no)
#head(proj.train)
#head(proj.test)


#Classifier Model

#create final model
set.seed(7)
proj.class.NB<-naiveBayes(proj.train,proj.train.labels,laplace=1,CV=10)

#save model to disk
saveRDS(proj.class.NB,"./project_classifier.rds")
