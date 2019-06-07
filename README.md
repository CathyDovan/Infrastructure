# Insights into Infrastructure Investments
A tool to:
- first, classify semi-structured Infrastructure Canada project data using Infrastructure Economic Account (INFEA) categories; 
- second, to apply established modeling to derive economic benefits from projects (e.g. jobs and contribution to Gross Domestic Product); and,
- third, to layer this information onto a map showing location and value of projects, along with the derived economic benefits.

## Motivation
The team wanted to answer the question: "How might we improve our ability to report on past federal infrastructure investment in order to tell our (Infrastructure Canada's) story to Canadians?"

## Setup and Installation
### Setup
1. Install R version 3.5.2 (2018-12-20) [suggestion to use RStudio](https://www.rstudio.com/).

2. Clone Git repository to your computer:
- using the clone feature and [URL](https://github.com/CathyDovan/Infrastructure.git)
- downloading the repository and extracting all files from .zip folder onto your local drive

## Table of Contents (Repository Structure)
### 1. Data Folder
The data folder contains the application and all snippets of code used to create the classifier and multiplier.
- *Multiplier Folder* - contains the code for the classifier/multiplier app. 
- *GeoCode gtf.R* -  code to generate Latitude and Longitude coordinates for GTF projects, based on city
- *Matching - v2 - cwwf and ptif.R*	- code to match Latitude and Longitude coordinates for PTIF and CWWF projects, based on existing INFC map
- *MultiplierScript.R* - code to automate the running of the INFEA multiplier
- *Test.txt* - to delete
- *classify_PIMS.R*	- code to classify PIMS data into INFEA categories
- *classify_gtf.R* - code to classify GTF data into INFEA categories
- *model.R	NB* - code to train a Naive Bayes classification model using 12996 PIMS projects (excluding all Capacity Building projects)
- *project_classifier.rds* - stored Naive Bayes classifier model used in classify_gtf and classify_pims scripts




