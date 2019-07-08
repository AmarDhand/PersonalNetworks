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
setwd("~/Desktop/PersonalNetworks-master")

#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each library()
library(tidyverse)

#Read in data
#Imports data and assigns it to variable "sample_data"
sample_data <- read.csv("20180807_PersonalNetwork_data.csv", 
	stringsAsFactors = FALSE)
#Stores "sample_data" as a table data frame for easier reading
sample_data <- tbl_df(sample_data)

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
r <- sample_data %>% select(record_id, race___1:race___88)
colnames(r) <- c("record_id", "Black", "White", "American_Indian", "Asian",
	"Hawaiian", "Other", "Unknown")
#creates variable, "race1", that contains the first race a participant chooses
#  if the participant selects multiple races, then "race1" variable represents
#  the race that appears first in the list of choices, and does NOT denote any 
#  ordering assigned by the participant 
race1 <- r %>% gather(race, count, -record_id) %>% filter(count == 1) %>% 
	arrange(record_id) %>% select(-count) %>% group_by(record_id) %>% slice(1) %>% 
	data.frame()
#creates variable, "race2", that contains the second race a participant chooses
#  if the participant selects multiple races, then "race2" variable represents
#  the race that appears second in the list of choices, and does NOT denote any 
#  ordering assigned by the participant 
race2 <- r %>% gather(race, count, -record_id) %>% filter(count == 1) %>% 
	arrange(record_id) %>% select(-count) %>% group_by(record_id) %>% slice(2) %>% 
	data.frame()

#creates a table that combines "race1" and "race2" by record_id
race <- left_join(race1, race2, by = 'record_id')
colnames(race) <- c("record_id", "race1", "race2")
#adds "race" table onto "sample_data", thus adding variables "race1" and "race2"
#  to the original data frame, containing all variables
sample_data <- left_join(sample_data, race, by = "record_id") %>% 
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
h <- sample_data %>% select(record_id, health___1:health___0)
colnames(h) <- c("record_id", "General", "Pain", "Cognitive_MentalHealth",
	"Cardiac", "NoProblems")
#creates variable, "health_prob1", that contains the first health problem a 
#  participant chooses if the participant selects multiple health problems, 
#  then "health_prob1" variable represents the health problem that appears first
#  in the list of choices on REDCap, and does NOT denote any ordering 
#  assigned by the participant 
#The same code is then used to create variables for any second, third, or fourth
#  health problems the participant chooses.
health_prob1 <- h %>% gather(health_prob, count, -record_id) %>% 
	filter(count == 1) %>% arrange(record_id) %>% select(-count) %>% 
	group_by(record_id) %>% slice(1) %>% data.frame()
health_prob2 <- h %>% gather(health_prob, count, -record_id) %>% 
	filter(count == 1) %>% arrange(record_id) %>% select(-count) %>% 
	group_by(record_id) %>% slice(2) %>% data.frame()
health_prob3 <- h %>% gather(health_prob, count, -record_id) %>% 
	filter(count == 1) %>% arrange(record_id) %>% select(-count) %>% 
	group_by(record_id) %>% slice(3) %>% data.frame()
health_prob4 <- h %>% gather(health_prob, count, -record_id) %>% 
	filter(count == 1) %>% arrange(record_id) %>% select(-count) %>% 
	group_by(record_id) %>% slice(4) %>% data.frame()
health_problems <- left_join(health_prob1, health_prob2, by = 'record_id')
health_problems <- left_join(health_problems, health_prob3, by = 'record_id')
health_problems <- left_join(health_problems, health_prob4, by = 'record_id')
colnames(health_problems) <- c("record_id", "health_problem1", "health_problem2",
	"health_problem3", "health_problem4")
sample_data <- left_join(sample_data, health_problems, by = "record_id") %>% 
	select(-health___1:-health___0)

##Calculate total network size. Defined as all unique names entered in name
#generator boxes and extra boxes provided.
sample_data.df <- data.frame(sample_data) 
datalist = list()

calculate_size <- function(x) {
  ##########
  # Function: Creates a network_size variable that takes into account any names 
  #           written in the extra names boxes
  # Inputs: x = Variable that stores the dataset
  # Ouputs: network_size variable for each ID 
    ##########
  #first select all names put in the first 15 columns 
  names_first_15 <- sample_data %>% select(name1, name2, name3, name4, name5, 
  	name6, name7, name8, name9, name10, name11, name12, name13, name14, name15)
  
  #next, select the names for id x
  names_first_15 <- names_first_15[x, ]
  
  #create data frame and transpose it to make it easier to manage 
  names_first_15 <- as.data.frame(t(names_first_15))	
  
  #change the column name
  colnames(names_first_15) <- c("Names")
  	
  #select the keep/remove designation, stored as variables "name_1" to "name_15"
  #  for each of the first 15 names 
  keep_names <- sample_data %>% select(name_1:name_15)
  keep_names <- keep_names[x, ]
  
  #change colnames to numbers 1:15, so that it is easier to do rbind
  colnames(keep_names) <- c(1:15)
  
  #input the data into a data frame and transpose it
  keep_names <- data.frame(t(keep_names))
  
  #change the name of the column to "Value"
  colnames(keep_names) = "Value"
  
  #combine "names_first_15" (the first 15 names entered) and "keep_names" (the 
  #  keep/remove designation for each of the first 15 names) using cbind function
  names_combined <- cbind(names_first_15, keep_names)
  
  #remove any row that contain NA in names_combined
  names_combined <- names_combined[complete.cases(names_combined), ]
  
  #split names_combined into names designated as "keep" (Value = 1) and 
  #  names designated as "remove" (Value = 0)
  names_combined_keep <- split(names_combined, names_combined$Value == 1)
  
  # Select only the names designated as $`TRUE` ("keep")
  names_combined_keep <- names_combined_keep$`TRUE`
  
  #Change all characters into Uppercase
  names_combined_keep <- toupper(names_combined_keep$Names)
  
  #Remove any spaces 
  names_combined_keep <- gsub(" ", "", names_combined_keep)
  
  #Make names_combined_keep into a data frame to make it easier to manage
  names_combined_keep <- data.frame(names_combined_keep)
  colnames(names_combined_keep) <- "Names"
  
  #Now, take all of the names from the 3 extra names boxes 
  #  Strsplit : split names based on coma saparated value and change them into 
  #  characters. 
  names_box1 <- strsplit(as.character(sample_data$more_names_1)[x], 
  	split = ",")
  names_box2 <- strsplit(as.character(sample_data$more_names_2)[x], 
  	split = ",")
  names_box3 <- strsplit(as.character(sample_data$more_names_3)[x], 
  	split = ",")
  
  #Unlist names_box1:names_box3 and create a vector of names for each extra names 
  #  box
  names_box1 <- as.vector(unlist(names_box1, use.names = FALSE))
  names_box2 <- as.vector(unlist(names_box2, use.names = FALSE))
  names_box3 <- as.vector(unlist(names_box3, use.names = FALSE))
  
  #combine the 3 extra names vectors into list so that we can combine 
  #  names_box1:3 into one vector
  names_box <- list(names_box1, names_box2, names_box3)
  
  #make the names_box list into a vector
  names_box <- Reduce(c, names_box)
  
  #remove "NA" in names_box
  names_box <- names_box[!is.na(names_box)]
  
  #Remove Spaces in names_box
  names_box <- gsub(" ", "", names_box)
  
  #Change all character in names_box to uppercase 
  names_box <- toupper(names_box)
  
  #Remove duplicates values in names_box vector
  names_box <- unique(names_box)
  
  #makes names_box into a data frame and change the column name to "Names" 
  #  to make it easier to merge with names_combined_keep
  names_box <- data.frame(names_box)
  colnames(names_box) <- "Names"
  
  # Merge unique names from boxes with unique names of first 15 and 
  #remove duplicates between them 
  #  Keep this order. Placing names_combined_keep first preserves any duplicate 
  #  names that both designated as "keep" by the participant 
  names_network <- merge(names_combined_keep,names_box,by = c("Names"), all = TRUE)
  
  # convert names_network into a vector
  names_network <- as.vector(t(names_network))
  
  #calculate the total network size
  total_size <- length(names_network)
  return(total_size)
}

#apply 'calculate_size' function to all study IDs
network_size <- unlist(lapply(1:nrow(sample_data), calculate_size))

#merge network_size and remove other size variables to reduce confusion
sample_data <- cbind(sample_data, network_size) %>% select(-size, -first)

#create temp file of data frame with all changes made in code
save(sample_data, file = "temp.rda")
