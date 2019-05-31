################################################################################
# PROJECT: PersonalNetworks 
# PURPOSE: Clean up alter-centric data for easier data analysis
# DIR:     "~/Desktop/PersonalNetworks"
# INPUTS:  Fake data created in REDCap ("20180806_PersonalNetwork_data.csv") 
#          Can be replaced with real data of participants.
# OUTPUTS: An alter_data.rda file that contains data on Alters of Ego's network
# AUTHORS: Liam McCafferty, Amar Dhand
# CREATED: 10/29/18
# LATEST:  05/07/19
# PSERIES: NA
# NSERIES: NA
# NOTES:   Output variable is named "alter_frame".
#          This code is still in beta, please email w/ suggestions or questions
#            lmccafferty@bwh.harvard.edu
###############################################################################

#Empties Global Environment cache
rm(list = ls())

#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd("~/Desktop/London Project")

#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each
#  library()
library(tidyverse)

#Read in data
#Imports data and assigns it to variable "sample_data"
sample_data <- read.csv("ToolForVisuallyMappi_DATA_2019-05-31_2140.csv", 
                        stringsAsFactors = FALSE)
#Stores "sample_data" as a table data frame for easier reading
sample_data <- tbl_df(sample_data)

#Selecting the name_# keep/remove variables
alter_keeps <- sample_data %>% select(name_1:name_15)
alter_keeps <- cbind(sample_data$record_id, alter_keeps)

########################## Making the Dataframe ###############################

#Creating dataframe we will store all of our data in
alter_frame <- data.frame(record_id = NA, alter_ids = NA, alter_nums = NA)

#Function which creates dataframe containing alter info
alter_frame_maker <- function(alter_keeps){
  #This function is designed to take a single line from a selected set from the
  #alter keep/remove section. It then creates a dataframe that has the record_id
  #as its first variable, and the names of the vector as the values for "alter
  #names" the intention of this dataframe is to have it be added together with
  #all other dataframe of the data.
  
  #Saving the alter data
  workbench <- alter_keeps[1,2:16]
  #Saving the study id
  record_id <- alter_keeps[1,1]
  
  #Eliminating alters which are not marked as "keep", aka 1's.
  workbench <- names(workbench)[workbench == 1]
  #Limiting the number of alters to 10
  if(length(workbench) > 10){
    workbench <- workbench[1:10]
  }
  
  test_frame <- data.frame(record_id = rep(record_id, length(workbench)))
  test_frame$alter_ids <- sub("name_","name",workbench)
  test_frame$alter_nums <- as.integer(sub("name_","",workbench))
  
  
  return(test_frame)
}

#Appends alter info dataframes
for(i in 1:nrow(alter_keeps)){
  alter_frame <- rbind(alter_frame, alter_frame_maker(alter_keeps[i,]))
}
#Removes pre-set NA row from alter frame
alter_frame <- alter_frame[!is.na(alter_frame$record_id),]


########################## Important Functions ################################
# #Useful for accessing stuff:
# alter_set <- alter_frame[alter_frame$record_id == 1,]

info_finder <- function(alter_set,suffix){
  #Function which finds the data associated with alters at a certain set of
  #  numbers.
  #The var suffix has to be entered exactly to the name of the variable in the
  #  dataset that we are accessing. "Name" and the number are pre-provided. For
  #  example, accessing "name#" would be suffix = "", accessing "name#educ" would
  #  be suffix = "educ". Returns an unlisted vector.
  #Note that this function is only designed for a single study id, thus it
  #  should be used in a forloop or by function
  
  #Note only works on radial or text enter data types. Not checkbox. I will need
  #to do checkbox manually
  
  #Error check to make sure that there is only one study id being accessed.
  if(length(unique(alter_set$record_id)) != 1){
    stop("Too many study ID's in input, isolate them")
  }
  #Isolating study id and accessing sample_data
  record_id <- unique(alter_set$record_id)
  workbench <- sample_data[sample_data$record_id == record_id,]
  #Creating blank vector
  alter_info <- c()
  
  #Adding info from dataset to blank vector depending upon entered suffix variable
  for(i in 1:length(alter_set$alter_nums)){
    alter_info[i] <- unlist(workbench[paste("name",alter_set$alter_nums[i],suffix,sep = "")])
  }
  
  return(alter_info)
}

checkbox_finder <- function(alter_set, var_generic_pre, var_generic_post, checkbox_names){
  #Function which finds the data of a generic checkbox set of data, then outputs
  #the info as a list. Note that this function only works on a set of alter info
  #of one study id.
  
  #Inputs-
  # alter_set: a single record_id alter set from the base alter_frame
  # var_generic_pre: the generic version of the first variable in the generic
  #   set of checkbox variables. The format is to replace the alter identifer with
  #   an "X" and then ___# with the correct number. For example, the generic
  #   checkbox of name1health___1 would be nameXhealth___1.
  # var_generic_post: the generic version of the last variable in the generic
  #   set of checkbox variables.
  # checkbox_names: the list of values/names assigned to each variable. Make
  #   sure the list is in the exact same order as in your dataset.
  
  #Checks to see if only one study ID entered. Results in an error if not.
  if(length(unique(alter_set$record_id)) != 1){
    stop("Too many study ID's in input, isolate them")
  }
  
  #Isolating study id and accessing sample_data
  record_id <- unique(alter_set$record_id)
  workbench <- sample_data[sample_data$record_id == record_id,]
  
  #Create blank list to assign stuff to, need to add an NA to allow for assignment
  output_list <- list()
  output_list[length(checkbox_names)] <- NA
  
  for(i in 1:length(alter_set$alter_nums)){
    #Creating strings which will access the support type at the sepecified alter
    supp_first <- sub("X", alter_set$alter_nums[i], var_generic_pre)
    supp_last <- sub("X", alter_set$alter_nums[i], var_generic_post)
    
    #Accessing the supp data aligned with the correct data.
    supp_set <- unlist(select(workbench, supp_first:supp_last))
    #Adding names to the support set
    names(supp_set) <- checkbox_names
    #Eliminating support set which are not 1's using logicals, then using 1:6 we
    #set every data point afterwards to be an NA, so that NA's can be assigned
    #in place of the eliminated values while still respecting order and forcing
    #all "yes's" to the front
    add_this <- supp_set[as.logical(supp_set)][1:length(checkbox_names)]
    #Now that the logical has been applied, changing values to the names
    add_this <- names(add_this)
    
    #Assigning variables to their correct vectors
    for(j in 1:length(checkbox_names)){
      output_list[[j]][i] <- add_this[j]
    }
    
  }
  return(output_list)
}

checkbox_arranger <- function(alter_frame, var_generic_pre, var_generic_post, checkbox_names){
  #Function which takes a set of output lists from the checkbox_finder function,
  #then reorganises them into a list which is oriented on a per-variable rather
  #than per-record_id basis. The assumption that each index of the list can then
  #be input as a vector into the alter_frame dataset.
  #Inputs-
  # alter_frame: full alter frame.
  
  #Note: All other variables are the same as in checkbox_finder function, check it
  # for notation.
  
  #by checkbox_finder function through alter_frame. Groups by record_id. Output
  #is a list of lists
  checkbox_list <- by(alter_frame, alter_frame$record_id, checkbox_finder,
                  var_generic_pre = var_generic_pre,
                  var_generic_post = var_generic_post,
                  checkbox_names = checkbox_names)
  
  #Creating blank list, then adding a false element to the list a +1 the
  #possible number of variable entries, so there is an open "NULL" space for us
  #to enter vectors.
  output_list <- list()
  output_list[length(checkbox_names) + 1] <- "FALSE ELEMENT"
  
  #Iterates through each variable in each study id, entering it into the current
  #variable vectors by appending them.
  for(i in 1:length(checkbox_names)){
    for(j in 1:length(checkbox_list)){
      output_list[[i]] <- append(output_list[[i]], checkbox_list[[j]][[i]])
    }
  }
  
  return(output_list)
}


############################ Adding support data ##############################

alter_frame$support <- unlist(by(alter_frame, alter_frame$record_id,
                                 info_finder, suffix = "support"))
#As support data is technically a binary, though the values are 1 and NA rather
#than 1 and 0, I transform all NA's into 0's.
alter_frame$support[is.na(alter_frame$support)] <- 0

alter_frame$support <- factor(alter_frame$support, levels = c(1, 0))
levels(alter_frame$support) <- c("Supports me most often",
                                 "Does not support me most often")


############################ Adding support type ##############################
#Preloading values for the checkbox_arranger function. I have notated this
#checkbox set, however others are not notated. Reference this notation to
#understand others as the method is exactly the same accross all checkboxes

#varpre_[variable] and varpost_[variable] are the first and last "generic"
#versions of the variable. You replace the number indicated the name# of alter
#with an "X", and then the ___#'s represent the first and last variables of a
#checkbox set, as if you were using "select" (because the code does).
varpre_supp <- "nameXsupptype___1"
varpost_supp <- "nameXsupptype___0"
#checkbox_[variable] is a vector of strings which are the assigned names for
#each variable of the checkbox set, in order. Note: it must be in order and with
#all names represented, or the code might break/give incorrect answers.
checkbox_supp <- c("Emotional support", "Life advice", "Financial support",
                   "Physical or Health support", "Camaraderie", "None")

#Actual assignment. Check notation in function for info.
list_supp <- checkbox_arranger(alter_frame, varpre_supp, varpost_supp, checkbox_supp)

#Iterate through output list and assign it to a newly create unique variable
#based upon the number of possible variables. Don't worry, later on we remove
#all checkbox columns that have no entries.
for(i in 1:length(checkbox_supp)){
  alter_frame[paste("supp_type",i,sep = "")] <- factor(list_supp[[i]],
                                                       levels = checkbox_supp)
}


############################ Adding sex #######################################

alter_frame$sex <- unlist(by(alter_frame, alter_frame$record_id,
                             info_finder, suffix = "sex"))

alter_frame$sex <- factor(alter_frame$sex, levels = c(1,0,2))
levels(alter_frame$sex) <- c("Male","Female","Other")

############################ Adding neg #######################################
#Adding "Do people in your network have a negative influence on your health"
# or "neg" for short.

alter_frame$neg <- unlist(by(alter_frame, alter_frame$record_id,
                             info_finder, suffix = "neg"))

alter_frame$neg <- factor(alter_frame$neg, levels = c(1,0))
levels(alter_frame$neg) <- c("Negative Effect", "Positive/Neutral Effect")


############################ Adding race ######################################

alter_frame$race <- unlist(by(alter_frame, alter_frame$record_id,
                             info_finder, suffix = "race"))

alter_frame$race <- factor(alter_frame$race, levels = c(1,2,3,4,5,77,99))
levels(alter_frame$race) <- c("Black or African American", "White",
                              "American Indian/Alaska Native", "Asian",
                              "Native Hawaiian or Other Pacifc Islander",
                              "Other", "Don't know")


############################# Adding education ################################

alter_frame$educ <- unlist(by(alter_frame, alter_frame$record_id,
                              info_finder, suffix = "educ"))

alter_frame$educ <- factor(alter_frame$educ, levels = c(1, 2, 3, 4, 5, 6, 99))
levels(alter_frame$educ) <- c("Some high school or less", "High school grad",
                              "Some college", "Associate Degree",
                              "Bachelor's Degree", "Graduate Degree",
                              "Don't know")


############################# Adding Speak frequency ##########################

alter_frame$speak <- unlist(by(alter_frame, alter_frame$record_id,
                               info_finder, suffix = "speak"))

alter_frame$speak <- factor(alter_frame$speak, levels = c(1, 2, 3, 4, 99))
levels(alter_frame$speak) <- c("Daily", "Weekly", "Monthly",
                               "Less often", "Don't know")


############################# Adding length ###################################
#Adding length that the alter knows the ego

alter_frame$length <- unlist(by(alter_frame, alter_frame$record_id,
                                info_finder, suffix = "length"))

alter_frame$length <- factor(alter_frame$length, levels = c(1, 2, 3, 99))
levels(alter_frame$length) <- c("Less than three", "Three to six",
                                "More than six", "Don't know")


############################# Adding relat ####################################

varpre_relat <- "nameXrelat___1"
varpost_relat <- "nameXrelat___77"
checkbox_relat <- c("Spouse","Family","Friend","Advisor","Co-worker","Other")

list_relat <- checkbox_arranger(alter_frame, varpre_relat, varpost_relat, checkbox_relat)

for(i in 1:length(checkbox_relat)){
  alter_frame[paste("relat",i,sep = "")] <- factor(list_relat[[i]],
                                                  levels = checkbox_relat)
}

############################# Adding Age ######################################
#Does not need to be made into a factor as age in a straight integer

alter_frame$age <- unlist(by(alter_frame, alter_frame$record_id,
                             info_finder, suffix = "age"))

############################# Adding alcohol ##################################

alter_frame$alcohol <- unlist(by(alter_frame, alter_frame$record_id,
                                 info_finder, suffix = "alcohol"))

alter_frame$alcohol <- factor(alter_frame$alcohol, levels = c(1, 0, 9, 99))
levels(alter_frame$alcohol) <- c("Yes", "No", "Does not drink heavily",
                                 "Don't know")

############################# Adding smoke ####################################

alter_frame$smoke <- unlist(by(alter_frame, alter_frame$record_id,
                                 info_finder, suffix = "smoke"))

alter_frame$smoke <- factor(alter_frame$smoke, levels = c(1, 0, 9, 99))
levels(alter_frame$smoke) <- c("Yes", "No", "Does not smoke",
                                 "Don't know")

############################# Adding exercise #################################

alter_frame$exer <- unlist(by(alter_frame, alter_frame$record_id,
                              info_finder, suffix = "exer"))

alter_frame$exer <- factor(alter_frame$exer, levels = c(1, 0, 99))
levels(alter_frame$exer) <- c("Yes", "No", "Don't know")

############################# Adding diet #####################################

alter_frame$diet <- unlist(by(alter_frame, alter_frame$record_id,
                              info_finder, suffix = "diet"))

alter_frame$diet <- factor(alter_frame$diet, levels = c(1, 0, 99))
levels(alter_frame$diet) <- c("Yes", "No", "Don't know")

############################# Adding health ###################################

varpre_health <- "nameXhealth___1"
varpost_health <- "nameXhealth___99"
checkbox_health <- c("General Health", "Pain", "Cognitive/Mental Health",
                     "Cardiac", "No Health Problems", "Don't Know")

list_health <- checkbox_arranger(alter_frame, varpre_health, varpost_health, checkbox_health)

for(i in 1:length(checkbox_health)){
  alter_frame[paste("health",i,sep = "")] <- factor(list_health[[i]],
                                                    levels = checkbox_health)
}

############################# Adding distance #################################

alter_frame$dist <- unlist(by(alter_frame, alter_frame$record_id,
                              info_finder, suffix = "dist"))

alter_frame$dist <- factor(alter_frame$dist, levels = c(1, 2, 3, 4, 5))
levels(alter_frame$dist) <- c("Same house", "1-5 miles", "6-15 miles",
                              "16-50 miles", "50+ miles")


################### Stripping Unnessary Checkbox columns ######################

#Isolating logically the checkbox variables. "grepl" creates a logical vector,
#of the strings in the vector colnames(alter_frame), which contain "supp_type"
#or "race" or "relat" or "fbwith" or "health". Regex expressions recognize the
#"|" symbol as an or. So by putting in a string with "x|y" the function knows to
#check if either set is contained.
check_log <- grepl(c("supp_type|race|relat|fbwith|health"), colnames(alter_frame))
checkboxes <- alter_frame[,check_log]

#Checks each checkbox column for its unique values. If all of the values are
#NA's, and nothing else, then the name of the column is recorded in the vector
#na_columns.
na_columns <- c()
for(i in 1:ncol(checkboxes)){
  if(all(is.na(unique(checkboxes[,i])))){
    na_columns <- append(na_columns, names(checkboxes[i]))
  }
}

#Eliminates all NA only columns.
alter_frame <- alter_frame[,!(names(alter_frame) %in% na_columns)]

#Exporting data
save(alter_frame, file = "alter_data.rda")
