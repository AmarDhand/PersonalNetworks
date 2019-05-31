################################################################################
# PROJECT: PersonalNetworks 
# PURPOSE: Import raw data in REDCap form and convert it to R
# DIR:     "~/Desktop/PersonalNetworks"
# INPUTS:  Fake data created in REDCap ("20180806_PersonalNetwork_data.csv") 
#          Can be replaced with real data of participants.
# OUTPUTS: A temp.rda file that will become the input for part 3 of the code 
# AUTHORS: Abby Halm, Nuzulul Kurniansyah, Amar Dhand
# CREATED: 08/01/18
# LATEST:  08/14/18
# PSERIES: NA
# NSERIES: 20180807_PersonalNetworks_cleandata2_code.R
# NOTES:   Step 1 of 2 parts of the code. 
#          Code works on raw .csv outputs from Redcap, no processing required
# ##############################################################################

#Empties Global Environment cache
rm(list = ls())

#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd("~/Desktop/London Project")

#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each library()
library(tidyverse)

#Read in data
#Imports data and assigns it to variable "sample_data"
sample_data <- read.csv("ToolForVisuallyMappi_DATA_2019-05-31_2140.csv", 
	stringsAsFactors = FALSE)
#Stores "sample_data" as a table data frame for easier reading
sample_data <- tbl_df(sample_data)

#Check if REDCap has changed study_id to record_id, replace if so
colnames(sample_data)[colnames(sample_data) == "record_id"] <- "study_id"

##The remaining code sets variable types and assigns levels to categorical
#  variables. We given a detailed annotation of this process for the variable 
#  "sex" below. Subsequent variables follow the same pattern. 

#Demographics of Central Person (Ego)
#ego's sex, stored as variable "sex", is made into a factor 
sample_data$sex <- factor(sample_data$sex, levels = c("0", "1", "2"))
#assigns levels to variable "sex"
levels(sample_data$sex) <- c("Female","Male","Other")

sample_data$edu <- factor(sample_data$edu, 
	levels = c("1,", "2", "3", "4", "5", "6", "88"))
levels(sample_data$edu) <- c("Some high school or less", "High school grad", 
	"Some college", "Associate degree", "Bachelor's degree", "Graduate degree",
	"Prefer not to answer")

sample_data$employment <- factor(sample_data$employment, 
	levels = c("1", "2", "3", "0", "4", "5", "6", "7"))
levels(sample_data$employment) <- c("Employed for wages", "Self-employed",
	"Out of work and looking for work", 
	"Out of work but not currently looking for work", "Student", "Retired", 
	"Unable to work", "Prefer not to answer")

sample_data$occupation <- factor(sample_data$occupation, levels = c("1", "2", 
	"3", "4", "5", "6", "7", "8", "9", "10", "77"))
#  note that for participants who select "0", "4", "5", "6", or "7" for variable
#  "employment", the value for "occupation" will be NA
levels(sample_data$occupation) <- c("Executive, manager", 
	"Sales or clerical worker", "Mechanic, electrician, skilled worker", 
	"Machine operator, inspector, bus/cab driver", "Service worker", 
	"Professional", "Business owner", "Laborer, unskilled worker", "Farming", 
	"Military", "Other")

sample_data$income <- factor(sample_data$income, levels = c("1", "2", "3", "4",
	"5"))
levels(sample_data$income) <- c("less than $5,000", "$5,000 to $49,000", 
	"$50,000 to $169,000", "$170,000 to $490,000", "more than $500,000")

sample_data$married <- factor(sample_data$married, levels = c("0", "1"))
levels(sample_data$married) <- c("Not married", "Married")

sample_data$live_alone <- factor(sample_data$live_alone, levels = c("0", "1"))
levels(sample_data$live_alone) <- c("No", "Yes")

#Ego's race
#Due to multiple choice, code below organizes particpant's choices
#  into race1 and race2. If the participant only chooses 1 race, the value for 
#  "race2" will be NA
r <- sample_data %>% select(study_id, race___1:race___88)
colnames(r) <- c("study_id", "Black", "White", "American_Indian", "Asian",
	"Hawaiian", "Other", "Unknown")
#creates variable, "race1", that contains the first race a participant chooses
#  if the participant selects multiple races, then "race1" variable represents
#  the race that appears first in the list of choices, and does NOT denote any 
#  ordering assigned by the participant 
race1 <- r %>% gather(race, count, -study_id) %>% filter(count == 1) %>% 
	arrange(study_id) %>% select(-count) %>% group_by(study_id) %>% slice(1) %>% 
	data.frame()
#creates variable, "race2", that contains the second race a participant chooses
#  if the participant selects multiple races, then "race2" variable represents
#  the race that appears second in the list of choices, and does NOT denote any 
#  ordering assigned by the participant 
race2 <- r %>% gather(race, count, -study_id) %>% filter(count == 1) %>% 
	arrange(study_id) %>% select(-count) %>% group_by(study_id) %>% slice(2) %>% 
	data.frame()

#creates a table that combines "race1" and "race2" by study_id
race <- left_join(race1, race2, by = 'study_id')
colnames(race) <- c("study_id", "race1", "race2")
#adds "race" table onto "sample_data", thus adding variables "race1" and "race2"
#  to the original data frame, containing all variables
sample_data <- left_join(sample_data, race, by = "study_id") %>% 
	select(-race___1:-race___88)

#Ego health habits: 
#Again, setting variable types and assinging levels to categorical variables
#  see code annotations for "sex" variable for a more detailed description of 
#  each line of code 
sample_data$alcohol <- factor(sample_data$alcohol, levels = c("0", "1", "9"))
levels(sample_data$alcohol) <- c("No", "Yes", "I do not drink heavily")

sample_data$smoke <- factor(sample_data$smoke, levels = c("0", "1",
	"9"))
levels(sample_data$smoke) <- c("No", "Yes", "I do not smoke")

sample_data$exercise <- factor(sample_data$exercise, levels = c("0", "1"))
levels(sample_data$exercise) <- c("No", "Yes")

sample_data$diet <- factor(sample_data$diet, levels = c("0", "1"))
levels(sample_data$diet) <- c("No", "Yes")

#Ego Health problems organized into columns
#The code below organizes the Ego's Health Problems (in which the participant
#  can select multiple choices) into columns. 
#same code as for "race" variable
h <- sample_data %>% select(study_id, health___1:health___0)
colnames(h) <- c("study_id", "General", "Pain", "Cognitive_MentalHealth",
	"Cardiac", "NoProblems")
#creates variable, "health_prob1", that contains the first health problem a 
#  participant chooses if the participant selects multiple health problems, 
#  then "health_prob1" variable represents the health problem that appears first
#  in the list of choices on REDCap, and does NOT denote any ordering 
#  assigned by the participant 
#The same code is then used to create variables for any second, third, or fourth
#  health problems the participant chooses.
health_prob1 <- h %>% gather(health_prob, count, -study_id) %>% 
	filter(count == 1) %>% arrange(study_id) %>% select(-count) %>% 
	group_by(study_id) %>% slice(1) %>% data.frame()
health_prob2 <- h %>% gather(health_prob, count, -study_id) %>% 
	filter(count == 1) %>% arrange(study_id) %>% select(-count) %>% 
	group_by(study_id) %>% slice(2) %>% data.frame()
health_prob3 <- h %>% gather(health_prob, count, -study_id) %>% 
	filter(count == 1) %>% arrange(study_id) %>% select(-count) %>% 
	group_by(study_id) %>% slice(3) %>% data.frame()
health_prob4 <- h %>% gather(health_prob, count, -study_id) %>% 
	filter(count == 1) %>% arrange(study_id) %>% select(-count) %>% 
	group_by(study_id) %>% slice(4) %>% data.frame()
health_problems <- left_join(health_prob1, health_prob2, by = 'study_id')
health_problems <- left_join(health_problems, health_prob3, by = 'study_id')
health_problems <- left_join(health_problems, health_prob4, by = 'study_id')
colnames(health_problems) <- c("study_id", "health_problem1", "health_problem2",
	"health_problem3", "health_problem4")
sample_data <- left_join(sample_data, health_problems, by = "study_id") %>% 
	select(-health___1:-health___0)

##Calculate total network size. Defined as all unique names entered in name
#generator boxes and extra boxes provided.
sample_data.df <- data.frame(sample_data) 
datalist = list()

#Calculate network size by summing all selected names. This is simplified from
#  previous version as we have no other names to deal with.
network_size <- sample_data %>% select(name_1:name_15) %>% apply(1,sum)

#merge network_size and remove other size variables to reduce confusion
sample_data <- cbind(sample_data, network_size) %>% select(-size, -first)

#create temp file of data frame with all changes made in code
save(sample_data, file = "temp.rda")
