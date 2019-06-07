rm(list=ls())
library(tidyverse)
library(ggmap)

#Load input data
input.data<- read.csv("C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Geocoding Cities/Gas Tax Fund/gtf_data.csv", header = T, stringsAsFactors = F)
names(input.data)

#Identify city and province variables
city<- input.data$Location
city<- gsub("&", "and", city)

province<- input.data$Province.Territory

#Creating a location variable which combines city and province
input.data$glocation<- paste("Canada,", province,",", city)


#Google API
register_google(key="AIzaSyA8WbqUM0ZmgCKzzgfPKfS3ArKIPa_MxVk")
getOption("ggmap") #summarises the Google credentials to check how you are connected

longlat<- geocode(input.data$glocation)

#Output data
output.data<- input.data%>%
  mutate(Longitude = longlat$lon, Latitude = longlat$lat)

#Exporting new csv
write.csv(output.data, "C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Geocoding Cities/Gas Tax Fund/gtf_data_longlat.csv", row.names = FALSE)

###########
#Roll-up
#########
names(output.data)

by.province<- output.data%>%
  group_by(Province.Territory)%>%
  summarise(total.projects=n(),
            total.cost = sum(as.numeric(Total.Project.Cost), na.rm = TRUE)
  )


output.data$Total.Project.Cost<- gsub('\\D+','', output.data$Total.Project.Cost)

test<- as.numeric(output.data$Total.Project.Cost)

write.csv(by.province, "C:/Users/Dragonfly/Desktop/Digital Academy/Practicum/Geocoding Cities/Gas Tax Fund/gtf_data_province.csv", row.names = FALSE)

