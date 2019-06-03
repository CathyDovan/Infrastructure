

library(dplyr)
library(tidyverse)


###### bring in the multiplier table and raw project data
multiplier <- read.csv("infea_multiplier_tool_2015.csv", header = TRUE)

data <- read.csv("sample_output2.csv",header=TRUE)


## convert the "geo" column to "Region" to match the raw data and INFEA.Multiplier.Category


colnames(multiplier)[colnames(multiplier)=="geo"] <- "Region"
colnames(multiplier)[colnames(multiplier)=="Asset"] <- "INFEA.Multiplier.Category"


## convert the multiplier sheet into a wide format so that multiplier types are in individual columns
multiplierspread <- reshape(multiplier, idvar = c('year','Region','INFEA.Multiplier.Category'), direction = 'wide', timevar = 'type')



combined <- merge(data, multiplierspread, all.x = TRUE)



##convert costs from factors to numbers for calculation

combined$Total.Eligible.Costs <- as.numeric(as.character(gsub("[$,]","",combined$Total.Eligible.Costs)))

combined$Program.Contribution <- as.numeric(as.character(gsub("[$,]","",combined$Program.Contribution)))


#### Calculate all of the multiplier output fields
combined$Direct.Jobs.Eligible <- combined$Total.Eligible.Costs / 1000 * combined$value.EMPL_DIR
combined$Total.Jobs.Eligible <- combined$Total.Eligible.Costs / 1000 * combined$value.EMPLT
combined$Indirect.Jobs.Eligible <- combined$Total.Jobs.Eligible - combined$Direct.Jobs.Eligible
combined$Direct.Value.Added.Eligible <- combined$Total.Eligible.Costs  * combined$value.GDPD
combined$Total.Value.Added.Eligible <- combined$Total.Eligible.Costs * combined$value.GDPT
combined$Indirect.Value.Added.Eligible <-combined$Total.Value.Added.Eligible - combined$Direct.Value.Added.Eligible
combined$Direct.Compensation.Eligible <- combined$Total.Eligible.Costs * combined$value.WSLID
combined$Total.Compensation.Eligible <- combined$Total.Eligible.Costs * combined$value.WSLIT
combined$Indirect.Compensation.Eligible <- combined$Total.Compensation.Eligible - combined$Direct.Compensation.Eligible
combined$Imports.Eligible <- combined$Total.Eligible.Cost * combined$value.IMPT
combined$Taxes.Eligible <- combined$Total.Eligible.Cost * combined$value.TAXFND

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


####Format the output columns


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

combined$Total.Eligible.Costs <- gsub("[ ]","",paste("$",format(round(combined$Total.Eligible.Cost,0),nsmall=0,big.mark=","),sep=""))
combined$Program.Contribution <- gsub("[ ]","",paste("$",format(round(combined$Program.Contribution,0),nsmall=0,big.mark=","),sep=""))

combined <- combined %>%
  select("Project.Num",everything()) %>%
  select(-one_of("year","value.GDPD","value.EMPL_DIR","value.EMPLT","value.GDPT","value.WSLID","value.WSLIT","value.IMPT","value.TAXFND","value.IMP_DIR","value.IMP_INI"))


write_csv(combined, "Output.csv", append = FALSE, quote_escape = "double")
