rm(list = ls())
library(tidyverse)
library(stringr)

#Map data set, which has the long and lat
map.data<- read.csv("C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/mapdata.csv", header = TRUE, stringsAsFactors = F)

#Project data since 2002, which has project number
project.data<- read.csv("C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Project List for policy.csv", header=TRUE, stringsAsFactors = F)

###################################################################################
#Data Cleaning for Matching
#
# matched using (project name/title) and (estimated total cost/total eligible cost)
###################################################################################

#Filter for only Infrastructure Canada Projects in map.data
map.data<- map.data%>%
  filter(Delivery.Department.Agency=="Infrastructure Canada")%>%
  mutate(Estimated.Total.Cost = Estimated.Total.Cost....)

class(map.data$Project.Name)
class(map.data$Estimated.Total.Cost)

#Retaining only numbers to make matching easier in project.data
project.data$Total.Eligible.Cost<- gsub('\\D+','', project.data$Total.Eligible.Cost)
project.data$Program.Contribution<- gsub('\\D+','', project.data$Program.Contribution)


########################################################
#Merging, inner join which only joins rows that match
#lacks asset categories
########################################################
merged.data<- inner_join(map.data, project.data, by=c("Project.Name"="Title..EN.", "Estimated.Total.Cost"="Total.Eligible.Cost"))

#Checking matching
#Check1<- project.data%>%
#  filter(Project.. %in% c("50200", "48500", "52029", "52000", "49000", "46415",
#                          "46578", "46613", "46839"))
#titles<- Check1$Title..EN.
#class(titles)

#Check2<- map.data%>%
#  filter(Project.Name %in% titles)

#Check3<- Check2%>%
#  filter(Estimated.Total.Cost %in% c("5000", "101100", "89636000", "315000", "50000",
#                                  "5962400", "21575354", "9500000", "2843551"))


#Exporting a csv file 
#write.csv(c.merged.data,"C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Sample output/sample_output1.csv", row.names = FALSE)

################################################################
#Matching with multiplyer CWWF and PTIF to get asset categories
#
# matching with project number
#################################################################
cwwf.data<- read.csv("C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Multiplyer/CWWF.csv", header = TRUE, stringsAsFactors = F)
ptif.data<- read.csv("C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Multiplyer/PTIF.csv", header = TRUE, stringsAsFactors = F)

#Both cwwf and ptif, need to make sure the columns are the same
names(cwwf.data)
names(ptif.data)

s.cwwf.data<- cwwf.data%>%
  mutate(Asset.Category = NAPCS.Asset.Category,
         Construction.Start.Date = Construction.Start.Date..Actual.or.Forecast.,
         Construction.End.Date = Construction.End.Date..Actual.or.Forecast.)%>%
  select(Project.Number, Title, Program.abbr., Asset.Category, INFEA.Multiplier.Category,
         Construction.Start.Date, Construction.End.Date
         )

s.ptif.data<- ptif.data%>%
  mutate(Asset.Category = INFEA.asset.category,
         INFEA.Multiplier.Category = INFEA.multiplier.category
         )%>%
  select(Project.Number, Title, Program.abbr., Asset.Category, INFEA.Multiplier.Category,
         Construction.Start.Date, Construction.End.Date
  )

both.data<- rbind(s.cwwf.data,s.ptif.data)

#Making sure that the project numers are integers so it is easier to match
both.data$Project.Number<- as.integer(both.data$Project.Number)
class(both.data$Project.Number)
class(merged.data$Project..)


##Matching
#cwwf.merged.data<- inner_join(c.merged.data, cwwf.data, by=c("Project.."="Project.Number"))
#ptif.merged.data<- inner_join(c.merged.data, ptif.data, by=c("Project.."="Project.Number"))
both.merged.data<- inner_join(merged.data, both.data, by=c("Project.." = "Project.Number"))

#Selecting relevant variables
c.both.merged.data<- both.merged.data%>%
  mutate(Project.Num= Project.., 
         Program= Program.abbr.,
         Total.Eligible.Costs = Estimated.Total.Cost,
         Approval.Year = substring(Approval.Date,1,4))%>%
  select(
    Project.Num, Project.Name, Program, Region, Asset.Category, INFEA.Multiplier.Category,
    Total.Eligible.Costs, Program.Contribution, Construction.Start.Date, Construction.End.Date,
    Approval.Year,Longitude, Latitude
  )

data<- c.both.merged.data

#Exporting a csv file
write.csv(data,"C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Sample output/MapProjectCwwfPtif.csv", row.names = FALSE)

#Removing all objects except
rm(list=setdiff(ls(), c("merged.data","data")))


########################
#Roll-up by Province
########################
by.province<- data%>%
  group_by(Region)%>%
  summarise(total.count=n(),
            sum(as.numeric(Total.Eligible.Costs), na.rm = TRUE),
            avg.Total.Eligible.Costs= mean(as.numeric(Total.Eligible.Costs), na.rm=TRUE),
            sum(as.numeric(Program.Contribution), na.rm = TRUE),
            avg.Program.Contribution= mean(as.numeric(Program.Contribution), na.rm=TRUE)
  )


#Exporting a csv file
write.csv(by.province,"C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Sample output/byprov_merged_data.csv", row.names = FALSE)


#################
#Ontario only
#################
ontario<- data%>%
  filter(Region=="ON")

write.csv(ontario, "C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Sample output/ontario.csv", row.names = FALSE)

#################
#Alberta
#################
alberta<- data%>%
  filter(Region=="AB")

write.csv(alberta, "C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Sample output/alberta.csv", row.names = FALSE)

##########
#ON and AB
##########
on.ab<- rbind(ontario, alberta)
scales::dollar(sum(as.numeric(on.ab$Program.Contribution)))
scales::dollar(sum(as.numeric(on.ab$Total.Eligible.Costs)))


write.csv(on.ab, "C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Sample output/on_ab.csv", row.names = FALSE)


#####
#2016
#####
y.2016<- data%>%
  filter(Approval.Year == "2016")
write.csv(y.2016, "C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Sample output/y_2016.csv", row.names = FALSE)


y.2016_2017<- data%>%
  filter(Approval.Year == "2016"|Approval.Year=="2017")
write.csv(y.2016_2017, "C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Sample output/y_2016-2017.csv", row.names = FALSE)

y.2016_2018<- data%>%
  filter(Approval.Year == "2016"|Approval.Year=="2017"|Approval.Year=="2018")
write.csv(y.2016_2018, "C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Matching datasets/Sample output/y_2016-2018.csv", row.names = FALSE)

y.2016_2019<- data%>%
  filter(Approval.Year == "2016"|Approval.Year=="2017"|Approval.Year=="2018"|Approval.Year=="2019")
