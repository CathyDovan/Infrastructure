# Insights into Infrastructure Investments
A tool to:
- first, classify semi-structured Infrastructure Canada project data using Infrastructure Economic Account (INFEA) categories; 
- second, to apply established modeling to derive economic benefits from projects (e.g. jobs and contribution to Gross Domestic Product); and,
- third, to layer this information onto a map showing location and value of projects, along with the derived economic benefits.

## Motivation
The team wanted to answer the question: "How might we improve our ability to report on past federal infrastructure investment in order to tell our (Infrastructure Canada's) story to Canadians?"

## Setup
1. Install R version 3.5.2 (2018-12-20) [suggestion to use RStudio](https://www.rstudio.com/).

2. Clone Git repository to your computer:
   - using the clone feature and [URL](https://github.com/CathyDovan/Infrastructure.git)
   - downloading the repository and extracting all files from .zip folder onto your local drive

## Table of Contents (Repository Structure)
### 1. Data Folder
The data folder contains the application and all snippets of code used to create the classifier and multiplier.
   - *Multiplier Folder* - contains the code for the classifier/multiplier app
   - *GeoCode gtf.R* -  code to generate Latitude and Longitude coordinates for GTF projects, based on city
   - *Matching - v2 - cwwf and ptif.R*	- code to match Latitude and Longitude coordinates for PTIF and CWWF projects, based on existing INFC map
   - *MultiplierScript.R* - code to automate the running of the INFEA multiplier (part of the app)
   - *Test.txt* - to delete
   - *classify_PIMS.R*	- code to classify PIMS data into INFEA categories (part of the app)
   - *classify_gtf.R* - code to classify GTF data into INFEA categories (part of the app)
   - *model.R* - code to train a Naive Bayes classification model using 12996 PIMS projects (excludes all Capacity Building projects)
   - *project_classifier.rds* - stored Naive Bayes classifier model used in classify_gtf and classify_pims scripts (part of app)
### 2. Map


## Preparing project data for the app
The app and map both use a selection of columns available in each dataset (PIMS and GTF). In order for the app to work, these columns must be named as follows before loading the data in the app.

**GTF Data**
- Province/Territory
- Project Title
- Project Description
- Location
- Standardized Category
- Total Project Cost
- Total Project Contribution

**PIMS Data**
- Region
- Title (EN)
- Description (EN)
- Location
- Standardized Category
- Total Eligible Costs
- Project Contribution
- Actual Construction Start Date
- Actual Construction End Date
- Forecasted Construction Start Date
- Forecasted Construction End Date


## Using the app
1. Open R (or RStudio)
2. Click on File > Open Script
3. Navigate to where you cloned the GitHub repository (as unzipped files) and navigate through Infrastructure > Data > Multiplier. Select the file **app.R**
4. Click the **Run App** button in the R interface:
![](images/RunApp.PNG?raw=true "RunApp")

5. Select the list of projects you wish to classify or to which to apply the multiplier by using the **Browse** option:
![](images/OpenApp.png?raw=true "OpenApp")

6. Choose the function you wish to perform on your data using one of three option buttons below:
![](images/FunctionOptions.png?raw=true "Functions")

Once the app has finished processing your request, it will provide you with a link to where the output is saved, in the same directory as the GitHub repository.

## Using the map
#### Preparing your data for the map
In order to map the data, you will need to use another R program to attach Longitude and Latitude coordinates to each project. Note that coordinates are calculated using the *Location* and *Province/Terrority* for GTF and *Region* and *Location* for PIMS data.

1. Open the R Script **GeoCode gtf.R**
2. Modify the following lines of code to read the app output file and write the same output files with map coordinates 
   ```R
   #Load input data
   input.data<- read.csv("PATH/gtf_data.csv", header = T, stringsAsFactors = F)
   ...
   #Exporting new csv
   write.csv(output.data, "PATH/gtf_data_longlat.csv", row.names = FALSE)
   ...
   write.csv(by.province, "PATH/gtf_data_province.csv", row.names = FALSE)
   ```
3. Highlight the full block of code in the R editor and click **Run**
![](images/RunCoordGenerator.png?raw=true "GenerateCoordinates")

#### Viewing your datafile on the map








