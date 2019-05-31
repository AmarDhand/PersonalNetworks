################################################################################
# PROJECT: PersonalNetworks
# PURPOSE: To create network metrics from REDCap data
# DIR:     "~/Desktop/PersonalNetworks"
# INPUTS:  A temp.rda file that was the output of part 2 of the code. This file 
#          contains the REDCap data that was imported and made into R form 
#          in part 2 of the code.
# OUTPUTS: 1): A .csv file called "Data_Unfiltered" that contains all variables
#					 including new network metrics.
#          2): A .csv file that has cleaned and parsed the original dataset down 
#          to 39 variables for further network and regression analyses. 
# AUTHORS: Abby Halm, Nuzulul Kurniansyah, Amar Dhand
# CREATED: 08/02/18
# LATEST:  08/30/18
# PSERIES: 20180807_PersonalNetworks_cleandata1_code.R
# NSERIES: NA
# NOTES:   Step 2 of 2 parts of the code, code requires output from part 1
################################################################################

#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd("~/Desktop/London Project")

#Detatches all packages from current iteration of R, most packages interfere with this code
detach_all_packages <- function() {
  ##########
  # Function: Detatches all attatched packages from current instance of R
  # Inputs: none, just call the function
  # Ouputs: none
  # Credit to mjaniec on stack overflow for function logic
  ##########  
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices",
                      "package:utils", "package:datasets",
                      "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",
                                                  search())) == 1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list) > 0)  for (package in package.list) detach(package,
      character.only = TRUE)
}
detach_all_packages()

#Empties Global Environment cache
rm(list = ls())

#Importing packages. If not yet installed, packages can be installed by going to:
#Tools -> Install Packages, then enter their exact names from within each 
#library()
library(tidyverse) # For data management
library(igraph) # To transform and analyze network data
#Although not supposed to load here, the functions below auto-loads the 
#following. If not already, make sure to install these packages as well.
#  egonet
#  sna
#  statnet.common
#  network

#Load temp file created from 2nd part of code
load("temp.rda")
#In case study id's are out of order, this will reorder them to ensure that functions
#  can preserve order and output information correctly.
sample_data <- sample_data[order(sample_data$study_id), ]

#Select study_id and each relationship tie
shape <- sample_data %>% select(study_id, tie1:a_tie105) %>%
  group_by(study_id) %>% slice(1)

#Making a function that will create a blank matrix, then fill it with ties 
#from one study_id, then assign EGO and alter number to the matrix
make_matrix <- function(x) {
##########
# Function: Creates an NA-stripped matrix from a single row dataset
# Inputs: x = Variable that stores the dataset
# Ouputs: matrix "mat", the matrix will be stripped of people which have zero ties,
#           the matrix will also turn diagonal NA's (and mistaken NA's) into 0's
##########
ties <- as.integer(shape[x, -1]) #select row for ID x
mat <- matrix(NA, 16, 16) #create empty 16 x 16 matrix, with all values = NA
mat[lower.tri(mat)] <- ties #fill lower triangle of matrix with ties
mat <- t(mat) #transpose the matrix
mat[lower.tri(mat)] <- ties #fill the lower triangle of matrix with ties, making 
#                            the matrix redundant
colnames(mat) <- rownames(mat) <- c("EGO", "1", "2", "3", "4", "5", "6", "7", 
  "8", "9", "10", "11", "12", "13", "14", "15") #assign EGO and alter number 
#                                                mannually into column and row 
#                                                names of matrix
mat <- mat[(!colSums(mat, 1) == 0), (!colSums(mat, 1) == 0)] #remove NA values
diag(mat) <- 0 #assign diagonal, which represents an individual's tie with 
#               him or herself equal to 0
return(mat)
}

# Build Matrix for All Participants
#Using code developed by Amar Dhand, MD DPHIL (06/16), built a 
#loop that applies the matrix function to every row. 100+ lines of code to 2!
z <- array(1:nrow(shape)) # An array of the number of rows
mats <- lapply(z, make_matrix) # Applies the matrix function to every study ID

#Transform to igraph objects with removal of ego

#First, identify the very small networks that need to be analyzed separately
#The code for calculating network metrics in the remainder of this script
#breaks for networks that contain less than 2 alters. Any networks in the dataset
#that contain less thab 2 alters (network size of less than 3, including ego) 
#should be investigated further.
rowsize <- unlist(lapply(mats, nrow))
rowsize
#create subset list with small networks (less than 3 individuals, including ego)
small <- mats[rowsize < 3]
small

#subset of list without small networks. All network metric calculations below
#will be made using this subset of networks containing more 2 or more alters 
mats <- mats[!rowsize < 3]

#creates igraph object with REMOVAL of Ego
make_graph <- function (x) {
##########
# Function: Creates an NA-stripped matrix from a single row dataset
# Inputs: x = Variable that stores the dataset
# Ouputs: matrix "mat", the matrix will be stripped of people which have zero ties,
#           the matrix will also turn diagonal NA's (and mistaken NA's) into 0's
##########  
	graph.adjacency(x[-1, -1], mode = "undirected", weighted = TRUE)
}
graphs <- lapply(mats, make_graph) #creation of graph objects
names(graphs) <- names(mats) #carry over patient id labels

# Calculate maximum, avg degree, and density of alters after removal of Ego
max_degree <- unlist(lapply(graphs, function(x){max(degree(x))}))

mean_degree <- unlist(lapply(graphs, function(x){mean(degree(x))}))

density <- unlist(lapply(graphs, function(x){graph.density(x)}))

#*Important to note: The above is calculated WITHOUT THE EGO, whereas below is 
#  calculated WITH THE EGO.*
#Calculate constraint and effective size (Burt, 1992)
library(egonet) # careful of interference with igraph functions. Do not load 
#                 before calculating degree and density without EGO (above)

#Calculate constraint with EGO
constraint <- unlist(lapply(mats, function(x){as.numeric(index.egonet(x, 
  index = list("constraint")))}))

#Calculate effective size with EGO
effsize <- unlist(lapply(mats, function(x){as.numeric(index.egonet(x, 
  index = list("effsize")))}))

#Create data frame, called "structure," that includes maximum and average degree, 
#  density, and constraint 
#  also include constraintInt here as well which is constraint x 100.
structure <- data.frame(max_degree, mean_degree, density, constraint,
  constraintInt = constraint * 100, effsize)

#Add study_id to "structure" data frame, so that we can add it to the original 
#  data set
study_id <- shape$study_id[!rowsize < 3] #study id without small networks
structure <- cbind(study_id = study_id, structure, stringsAsFactors = FALSE)

#Add the "structure" data frame to the original data set by matching "study_id"
master.pre <- left_join(sample_data, structure, by = c("study_id")) 

#Clean, remove unecessary variables
rm(list = setdiff(ls(), c("master.pre", "raw", "wd", "z")))

#Create variable, z, which is an arry of unique study IDs
z <- array(1:length(unique(master.pre$study_id)))

#Create new compositional variables: 
#Calculate age SD
#Select study_id, and the age of each name, from name 1 to 15
age <- master.pre %>% select(study_id, name1age:name15age) %>% 
  group_by(study_id) %>% slice(1)
#Calculate age SD
age_sd <- apply(age[, -1], 1, sd, na.rm = TRUE) 

#Sex IQV--Two versions here just to use for later code.
sex <- master.pre %>% select(study_id, name1sex:name15sex) %>% 
  group_by(study_id) %>% slice(1)
sex1 <- master.pre %>% select(study_id, name1sex:name15sex) %>% 
	group_by(study_id) %>% slice(1)

sex1[sex1 == 2] <- NA #change "other" sex to NA for sex IQV calculation

sex_diversity <- function(x) {
##########
# Function: Calculates IQV for sex of alters, excluding those designated as "other"
# Inputs: x = Variable that stores the dataset
# Ouputs: IQV of the sex of alters for each study ID
########## 
  a <- sum(!is.na(sex1[x, 2:16])) #total number of alters
  b <- sum(sex1[x, 2:16], na.rm = TRUE) #number of men
  c <- a-b #number of women
  d <- (1 - ((b / a)^2 + (c / a)^2)) / (1 - (1 / 2)) 
  return (d)
}

#Apply sex_diversity function across all study IDs
IQVsex <- apply(z, 1, sex_diversity) #z from above is an array of unique Study IDs

#Cell totals
#Use the code below to determine number of cells occupied instead of "network size". 
#Better for proportions because we don't know what trait exists after 
#first 10 "kept" names
cell_totals <- function(x){ifelse(is.na(sex[x, -1]), 0, 1)}
cells_df <- cell_totals(z)
tot_cells <- apply(cells_df, 1, sum)

#Race IQV
race <- master.pre %>% select(study_id, name1race:name15race) %>% 
  group_by(study_id) %>% slice(1)

race[race == 77] <- NA #set "Other" values to NA for race IQV calculation
race[race == 99] <- NA #set "Don't know" values to NA for race IQV calculation

race_diversity <- function(x) {
##########
# Function: Calculates IQV for race of alters, excluding those designated as 
#           "Other" and "Don't Know:
# Inputs: x = Variable that stores the dataset
# Ouputs: IQV of the race of alters for each study ID
########## 
  a <- sum(!is.na(race[x, 2:11]))  #total number of alters
  b <- sum(race[x, 2:11] == 1, na.rm = TRUE)  #number of blacks
  c <- sum(race[x, 2:11] == 2, na.rm = TRUE)  #number of whites
  d <- sum(race[x, 2:11] == 3, na.rm = TRUE)  #number of american indian/alaska natives
  e <- sum(race[x, 2:11] == 4, na.rm = TRUE)  #number of asians
  f <- sum(race[x, 2:11] == 5, na.rm = TRUE)  #number of native hawaiian/
  #                                                       other pacific islanders
  d <- (1 - ((b / a)^2 + (c / a)^2 + (d / a)^2 + (e / a)^2 + (f / a)^2)) / (1 - (1 / 5))
  return(d)
}

#Apply race_diversity function to all Study IDs
IQVrace <- apply(z, 1, race_diversity)

#Contact frequency
#This variable looks at the frequency of contact with each alter. 
#It is treated as a categorical variable. 
#The code first dichotomizes this variable to weak and strong and ties and
#  then calculates a proportion of weak ties.

freq <- master.pre %>% select(study_id, name1speak:name15speak) %>%
  group_by(study_id) %>% slice(1)

freq[freq == 99] <- NA #Set "Don't know" to NA for calculating the proportion of 
#                     weak ties

#Weak ties defined as interacting monthly or less often.
#In prior paper (Dhand et al., 2018), weak defined as weekly, monthly, or less often.
#Isolating >3 = those who the ego interacts with monthly or less often 


weak <- function(x){
  ##########
  # Function: Calculates proportion of weak ties based on frequency of contact
  #           for each study ID
  # Inputs: x = Variable that stores the dataset
  # Ouputs: Proportion of weak ties based on frequency of contact for each Study ID
  ##########  
  ifelse(freq[x, -1] > 2, 1, 0)
  } 

#Create a df with only frequency of contact columns for all rows
weak_df <- weak(z)
#Collapse by summing number of weak ties
weak_freq <- apply(weak_df, 1, sum, na.rm = TRUE) 
#Calculate proportion of weak ties
weak_freq_prop <- weak_freq / tot_cells 

#Add new variables, created above, to data object, "master.pre"
study_id <- unique(master.pre$study_id)
df <- data.frame(study_id, age_sd, IQVsex, IQVrace, weak_freq_prop, 
	stringsAsFactors = FALSE)
master.pre <- left_join(master.pre, df, by = c("study_id"))

#Clean, remove unnecessary variables
rm(list = setdiff(ls(), c("master.pre", "z", "tot_cells")))

#checker fucntion used in multiple proportions
checker <- function(x){
  ##########
  # Function: Checks a vector of integers to see if it contains any 1's, does not
  #   return warnings like the any() function normally does
  # Inputs: x = vector of integers
  # Ouputs: logical, TRUE or FALSE
  ##########  
  ifelse(any(x %in% 1), TRUE, FALSE)
}

#Contact duration
#This variable is for number of years of contact. 
#It's another way to describe the links. Later, one can substitute this into the 
#matrices (at least for ego-alter ties) and see if it improves the description. 
#This can be done with frequency, duration, and distance.

dur <- master.pre %>% select(study_id, name1length:name15length) %>%
  group_by(study_id) %>% slice(1)

dur[dur == 99] <- NA #Code "Don't know" as NA

#Isolating < 3 = those alters who the ego has known for less than 6 years
weak <- function(x){
  ##########
  # Function: Calculates proportion of weak ties based on number of years of contact
  #           for each study ID
  # Inputs: x = Variable that stores the dataset
  # Ouputs: Proportion of weak ties based on years of contact for each STUDY ID
  ##########  
  ifelse(dur[x, - 1] < 3, 1, 0)
  }

#Create a df with only years of contact columns for all rows
weak_df <- weak(z)
#Collapse by summing number of weak ties
weak_dur <- apply(weak_df, 1, sum, na.rm = TRUE) 
#Calculate proportion of weak ties
weak_dur_prop <- weak_dur / tot_cells 

# Distance of alters to ego
#This variable is another way to characterize the tie--by proximity. 
#  This could also be substituted into the matrices later on 
#  (for ego-alter ties only). Here, I dicotomize at the 16 miles, 
#  so far is >15 miles.

dist <- master.pre %>% select(study_id, name1dist:name15dist) %>%
  group_by(study_id) %>% slice(1)
#Isolating <3 = those who live farther than 15 miles away 


far <- function(x){
  ##########
  # Function: Calculates proportion of alters who live further than 15 miles away
  #           for each study ID
  # Inputs: x = Variable that stores the dataset
  # Ouputs: Proportion alters who live further than 15 miles away for each Study ID
  ##########  
  ifelse(dist[x, -1] < 3, 1, 0)
  }

#Create a df with only distance columns for all rows
far_df <- far(z)
#Collapse by summing number of alters who live >15 miles away 
far_dist <- apply(far_df, 1, sum, na.rm = TRUE) 
#Calculate proportion of alters who live >15 miles away
far_dist_prop <- far_dist / tot_cells 
#divide by total number of alters

#Proportion kin 
roles <- master.pre %>% select(study_id, name1relat___1:name15relat___77) %>%
  group_by(study_id) %>% slice(1) %>% ungroup() %>% select(-study_id)

#Isolate all kin entries which are choices 1 through 2
spouse_family <- grepl("___1", names(roles))|grepl("___2", names(roles))
#Function to select spouse or family members (kin)

kin <- function(x){
  ##########
  # Function: Calculates proportion of alters who are kin for each Study ID
  # Inputs: x = Variable that stores the dataset
  # Ouputs: Proportion of alters who are kin each Study ID
  ##########  
  ifelse(roles[x, spouse_family] == 1, 1, 0)
  }

#Creates a blank vector to store the number of ties with family members
kin_prob_num <- c()

kin_df <- as.data.frame(t(kin(z)))
#Creates a new variable ties to sort each family member by its associated tie
kin_df$ties <- read.fwf(textConnection(rownames(kin_df)),
                                widths = 11)

for(i in 1:(length(kin_df) - 1)){
  #This for loop goes through each survey and checks to see if each tie has a family
  #  member. The loop uses the by() function's factor system to organise each set
  #  of family members by their previosly assigned tie identifier. Then it runs
  #  the created checker function.
  kin_prob_num[i] <- sum(by(kin_df[, i],
                               kin_df$ties, checker) * 1)
}

#Calculate proportion of kin 
kin_prop <- kin_prob_num / tot_cells

#Proportion of negative ties
negative_all <- master.pre %>% select(study_id, name1neg:name15neg) %>%
  group_by(study_id) %>% slice(1) %>% ungroup() %>% select(-study_id)

negative <- apply(negative_all, 1, sum, na.rm = TRUE)
neg_prop <- negative / tot_cells

#Add new variables to data object
study_id <- unique(master.pre$study_id)
df <- data.frame(study_id, kin_prop, neg_prop, weak_dur_prop, far_dist_prop, 
  stringsAsFactors = FALSE)

master.pre <- left_join(master.pre, df, by = c("study_id"))

#Health habits of network members
#These variables examine the smoking, drinking, diet, and exercise 
#  behaviors of the alters in the network.

#Smoking
smoking_all <- master.pre %>% select(study_id, name1smoke:name15smoke) %>%
  group_by(study_id) %>% slice(1) %>% ungroup() %>% select(-study_id)

#Isolating 0 = Not cut down on smoking and 1 = Cut down on smoking
smokers <- function(x){
  ##########
  # Function: Calculates proportion of alters who have not cut down on smoking
  #           for each Study ID
  # Inputs: x = Variable that stores the dataset
  # Ouputs: Proportion of alters who have not cut down on smoking
  ##########  
  
  #First value is what is checked, second value is what the checked value is replaced with,
  #  third value is what the non-matching values are replaced with.
  yes_answers <- ifelse(smoking_all[x, ] == 1, 1, 0)
  no_answers <- ifelse(smoking_all[x, ] == 0, 1, 0)
  return(no_answers + yes_answers)
  } 

#Create a df with only smoking columns for all rows
smokers_df <- smokers(z)
#Collapse by summing total number of alters who have not cut back on smoking
smoking <- apply(smokers_df, 1, sum, na.rm = TRUE)
#Proportion of those who have not cut down on smoking
smoking_prop <- smoking / tot_cells

#Heavy Alcohol
alcohol_all <- master.pre %>% select(study_id, name1alcohol:name15alcohol) %>%
  group_by(study_id) %>% slice(1) %>% ungroup() %>% select(-study_id)

#Isolating 0 = Not cut down on heavy drinking, 1 = cut down on heavy drinking
drinkers <- function(x){
  ##########
  # Function: Calculates proportion of alters who have not cut down on heavy 
  #           drinking for each Study ID
  # Inputs: x = Variable that stores the dataset
  # Ouputs: Proportion of alters who have not cut down on heavy drinking
  ##########
  yes_answers <- ifelse(alcohol_all[x, ] == 1, 1, 0)
  no_answers <- ifelse(alcohol_all[x, ] == 0, 1, 0)
  return(no_answers + yes_answers)
  }

#Create a df with only alcohol columns for all rows
drinkers_df <- drinkers(z)
#Collapse by summing total number of alters who have not cut down on heavy drinking
drinking <- apply(drinkers_df, 1, sum, na.rm = TRUE)
#Proportion of total number of alters who have not cut down on heavy drinking
drinking_prop <- drinking / tot_cells

#No exercise
exercise_all <- master.pre %>% select(study_id, name1exer:name15exer) %>%
	group_by(study_id) %>% slice(1) %>% ungroup() %>% select(-study_id)

#Isolating 0=Does not exercise at least 3-4 times per week

no_exercisers <- function(x){
  ##########
  # Function: Calculates proportion of alters who do not exercise 3-4 times per 
  #           week for each Study ID
  # Inputs: x = Variable that stores the dataset
  # Ouputs: Proportion of alters who do not exercise 3-4 times per week for each
  #         Study ID
  ########## 
  ifelse(exercise_all[x, ] == 0, 1, 0)
  }

#Create a df with only exercise columns for all rows
no_exercisers_df <- no_exercisers(z)
#Collapse by summing total number of alters who do not exercise 3-4 times per week 
no_exercise <- apply(no_exercisers_df, 1, sum, na.rm = TRUE)
#Proportion of alters who do not exercise 3-4 times per week
no_exercise_prop <- no_exercise / tot_cells

#Bad diet: 
diet_all <- master.pre %>% select(study_id, name1diet:name15diet) %>%
  group_by(study_id) %>% slice(1) %>% ungroup() %>% select(-study_id)

#Isolating those who have a unhealthy diet

bad_diet <- function(x){
  ##########
  # Function: Calculates proportion of alters who have a bad diet for each study ID
  # Inputs: x = Variable that stores the dataset
  # Ouputs: Proportion of alters who have a bad diet for each Study ID
  ##########  
  ifelse(diet_all[x,] == 0, 1, 0)
  }

#Create a df with only diet columns for all rows
bad_diet_df <- bad_diet(z)
#Collapse by summing total number of alters who have a bad diet
bad_diet <- apply(bad_diet_df, 1, sum, na.rm = TRUE)
#Proprotion of alters who have a bad diet
bad_diet_prop <- bad_diet / tot_cells

#Health problems: 
health_all <- master.pre %>% 
	select(study_id, name1health___1:name15health___99) %>%
	group_by(study_id) %>% slice(1) %>% ungroup() %>% select(-study_id)

#Isolating those who have health problems ("___1", "___2","___3",or "___4")
#  Health_all[1, !not] identifies all columns except ___0 and ___99
not <- grepl("___0", names(health_all))|grepl("___99", names(health_all))
#Creates a blank vector to store the number of ties with health problems
health_prob_num <- c()

health_prob <- function(x){
  ##########
  # Function: Calculates proportion of alters who have health problems for each 
  #           Study ID
  # Inputs: x = Variable that stores the dataset
  # Ouputs: Proportion of alters who have health problems for each Study ID
  ##########  
  ifelse(health_all[x, !not] == 1, 1, 0)
  }

#Create a df with only health problem columns for all rows
health_prob_df <- as.data.frame(t(health_prob(z)))
#Creates a new variable ties to sort each health problem by its associated tie
health_prob_df$ties <- read.fwf(textConnection(rownames(health_prob_df)),
                                widths = 11)

for(i in 1:(length(health_prob_df) - 1)){
  #This for loop goes through each survey and checks to see if each tie has a health
  #  problem. The loop uses the by() function's factor system to organise each set
  #  of health problems by their previosly assigned tie identifier. Then it runs
  #  the created checker function.
  health_prob_num[i] <- sum(by(health_prob_df[, i],
                               health_prob_df$ties, checker) * 1)
}
#Proportion of alters with health problems
health_prob_prop <- health_prob_num / tot_cells

#Add new variables to data object, "master.pre"
study_id <- unique(master.pre$study_id)
df <- data.frame(study_id, smoking_prop, drinking_prop, no_exercise_prop, 
	bad_diet_prop, health_prob_prop, stringsAsFactors = FALSE)

master <- left_join(master.pre, df, by = c("study_id"))

write.csv(master, file = "Data_Unfiltered.csv")

#clean, remove unnecessary variables
rm(list=setdiff(ls(), c("master")))

#Create parsed version for analysis
final_table <- master %>% select(
	#key identifiers
	study_id, 
	#demographics
	sex, race1, race2, education = edu, employment, occupation, income,
	married, live_alone,
	#health habits and problems
	alcohol, smoke, exercise, healthy_diet = diet, 
	health_problem1:health_problem4,
	#structure network variables
	network_size, density, constraint = constraintInt, effsize, max_degree, 
	mean_degree, 
	#composition network variables
	kin_prop, age_sd, IQVsex, IQVrace, weak_freq_prop, weak_dur_prop, 
	far_dist_prop, drinking_prop, smoking_prop, no_exercise_prop, bad_diet_prop,
	health_prob_prop)

#Save parsed version as a .csv file
write.csv(final_table, file = "Clean_Data.csv")

