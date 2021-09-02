################################################################################
# PROJECT: PersonalNetworks 
# PURPOSE: Provide methods of analysis and generate a basic clean info table
# DIR:     "~/Desktop/PersonalNetworks"
# INPUTS:  Save/Load data "ego_data.rda" output from:
#                                     20190510_PersonalNetworks_egodata_code.R
#          Save/Load data "alter_data.rda" output from:
#                                     20190510_PersonalNetworks_alterdata_code.R
# OUTPUTS: A pre-processed set of data output csv "Clean_Data_2.csv"
# AUTHORS: Liam McCafferty, Amar Dhand
# CREATED: 03/06/2020
# LATEST:  
# PSERIES: 20190510_PersonalNetworks_egodata_code.R
#          20190510_PersonalNetworks_alterdata_code.R
# NSERIES: NA
# NOTES:   This code is still in beta, please email w/ suggestions or questions
#            lmccafferty@bwh.harvard.edu
################################################################################

rm(list = ls())
#Set working directory to current file location
#This code should already work, but if not do this:
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("~/Desktop/PersonalNetworks-master")

library(tidyverse)

load("alter_data.rda")
load("ego_data.rda")

########################### IQV or Diversity ###################################
#This statistic is the diversity of a group of individuals.
#This is the general equation for IQV
#
#      ∑(number in a category / total people ^ 2) /
#           (1 - (1 / number of categories))
#
#Common Example Statistics:
#     Race IQV; diversity of race w/in an individuals' network
#     Sex IQV; diversity of sex w/in an individuals' network
#
#Statistical notes:
# - As IQV scales with the number of possible categories
#  individuals can fall into, having categories in your data that very few
#  individuals fall into can cause abnormally low IQV levels. For example, if
#  you have 5 categories for pets, but 98% of individuals only own cats or dogs,
#  then you may end up with much lower IQV's than you would if the other 3 pet
#  categories were combined into one "other" category.
# - IQV is not a linear statistic unlike Proportion or Homophily
# - Our version of IQV does not work with checkbox statistics, or statistics in
#  which inviduals can have more than 1 possible category they fall into. There
#  may be better measures of diversity which can accomodate this state, but
#  unfortunately we do not have it.

iqv_calculator <- function(input_alters, possible_categories = NA){
	#Creating function which will be recursively used in calculations
	iqv_chunk <- function(cat_count, alter_total){
		sapply(cat_count,function(x){(x/alter_total)^2})
	}
	#Number of alters w/ logic for removing NA's
	alter_total <- sum(!is.na(input_alters))

	#Possible categories
	if(!any(is.na(possible_categories))){
		#If we have no factor but defined categories, then we use them
		possible_categories_count <- possible_categories %>% length()
	}else if(is.factor(input_alters)){
		#If we have factors we default use their levels
		possible_categories <- input_alters %>% levels()
		possible_categories_count <- possible_categories %>% length()
	}else if(any(is.na(possible_categories))){
		#If we have no factor, and if no categories we use uniques
		possible_categories <- input_alters %>% unique()
		possible_categories_count <- possible_categories %>% length()
	}
	
	#Each category count
	cat_count <- sapply(possible_categories, function(x){sum(input_alters %in% x)})
	
	#Calculationl, w/ logic to return NA rather than Inf if denominator is 0
	if(alter_total == 0){
		return(NA)
	}else if(all(is.na(input_alters))){
		return(NA)
	}else{
		return((1 - sum(iqv_chunk(cat_count, alter_total))) /
					 	(1 - (1/possible_categories_count) ))
	}
}

########################## Homophily/Homogeneity ###############################
#This statistic is the number of individuals w/in a network who are the same as
#  the ego/respondent of the survey. Technically the only difference between
#  homophily and proportion is the value we are comparing to, therefore they use
#  the same function but require different inputs
#
# Common Example Statistics
#     Racial homophily; do respondents surround themselves with members of the
#       same race or ethnicity
#     Professional Homophily; do respondents surround themselves w/ individuals
#       working in the same profession
#     Class homophily; do respondents surround themselves w/ indivdidausl of the
#       same social class/socioeconomic status
#
#Theory Notes: Although statistically Homophily and Homogeneity are calculated
#  the same way; they are seperate concepts. Homogeneity is simply an
#  observation of a network: the proportion of members of a network who are the
#  same as the ego/respondent. However, homophily contains within it an implicit
#  argument: that individuals will actively or passively surround themselves
#  with individuals who are like themselves. This pattern is well studied and
#  understood to exist within networks, but depending upon hypotheses and
#  evidence provided it may be a good idea to lean on the more conservative side
#  of things and refer to this statistic as Homogeneity.
#
#Statistical Notes:
# - For this statistic to work you must have data for the ego/respondent and
#   members of their network have the exact same possible categories for them to
#   fall into. This means that usage of this statistic requires careful planning
#   of matching ego and alter ?'s with each other, or processing either set of
#   data to best match the other. If this is not the case, then using other
#   statistics may be a good idea.
# - Unlike proportion, homophily as a statistic only needs to be represented by
#   a single variable as it references the categories of the ego/respondent.
# - Homophily is dependent upon having data from the ego/respondent. This means
#   depending upon data quality you may have a larger number of NA's than other
#   statistics.

############################### Proportion #####################################
#This statistic is pretty simple to understand, it is simply the number of
#  individuals with a certain value or set of values divided by the total number
#  of individuals.
#
# Common Example Statistics
#     Proportion Kin: Are respondents' networks predominantly family members, or
#       non-familial friends/associates
#     Proportion Unhealthy Ties: Do respondents' networks consist of individuals
#       w/ negative health habits (e.g. smoking, heavy drinking, unhealthy diet,
#       little to no exercise, etc.)
#
#Statistical Notes:
# - For a number of reasons proportion is a statistic needing much more
# customization to the study at hand. It may be a good idea to set up what
# questions we want to answer about networks and their composition beforehard.
# This way analysts are not bogged down in looking at every possible combination
# of factors.
# - Unlike IQV and Homophily, proportion is not collapsed into a single
# variable. Instead you may need to have a seperate proportion variable for
# every level or possible combination of levels. For example, if you have a
# cohort of dog owners, cat owners, and guinea pig owners; you may need to have
# prop_dog, prop_cat, and prop_guineapig to cover every value. You may be able
# to combine statistics (e.g. prop_nondog) but this is dependent upon what your
# study is trying to understand about the composition of the networks.
# - Continuing this line of thought; for variables which can contain multiple
# possible values it may be likely that proportions do not seem to line up in a
# easy to conceptualize manner. For example, if you have a cohort of dog owners,
# cat owners, and guinea pig owners; it may be possible to own both cats and
# dogs. This means that network w/ 3 people, two of whom have cats and dogs,
# while one has guinea pigs, will give these values: dogs - 0.66, cats - 0.66,
# guinea pigs - 0.33. Notice how these proportions add up to greater than 1.
# This can expand dramatically in wholistic analysis where individual
# proportions will be dramatically lower than their combined statistic.

prop_calculator <- function(input_alters, input_check, na.rm = TRUE){
	
	#Add in logic to check if number of values for each row/index depending upon location
	
	#Logic for vectors. Potential area for error as is.atomic somtimes picks up
	#  matrices. Hopefully they are filtered out by !is.matrix(), however if
	#  further areas occur this may be the point.
	if(is.atomic(input_alters) & !is.matrix(input_alters)){
		if(na.rm){
			alter_count <- sum(!is.na(input_alters))
		}else if(!na.rm){
			alter_count <- length(input_alters)
		}
		match_count <- sum(as.character(input_alters) %in% as.character(input_check))
		
	#Logic for dataframes or matrices. If input is a list it may be problematic so
	#  we return NA's otherwise.
	}else if(is.data.frame(input_alters) | is.matrix(input_alters)){
		if(na.rm){
			alter_count <- sum(!is.na(input_alters[,1]))
		}else if(!na.rm){
			alter_count <- nrow(input_alters)
		}
		
		apply(input_alters, 1, function(x){
			any(x %in% input_check)
		}) %>% sum(na.rm = TRUE) -> match_count
		
	#Error stop in case we don't trip either vector or dataframe/matrix
	#  statements. Hopefully this catches all use cases so that users will
	#  interpret what issue their input is having.
	}else{
		stop("input_alters is not falling into category of vector or dataframe/matrix")
	}
	
	#Logic checking if all alters are NA's. If this is the case returning a 0 would
	#  be innacurate as there is no data at all.
	if(alter_count == 0){
		return(NA)
	}else{
		#Return of the proportion
		return(match_count / alter_count)
	}
}

############################## Example Tables ##################################

# Simple dplyr Example #########################################################
#In this table I show how to do composition analysis using dplyr pipelines. This
#  method is quite clean and doesn't require any data transformation after
#  analysis. However, I've found that it can be a bit fiddly at times. For
#  example we don't use subset for extracting ego data in the "race homophily"
#  example. This is due to both datasets using the same name and values for
#  record_id, causing overlap where there shouldn't be. Its very likely I'm
#  simply inexperienced with the tools and there may be a elegant solution to
#  this issue, but it works in this case.

#I would advise using this method if you are planning on doing simple
#  proportion/IQV calculations and just want a usable dataframe without having
#  to fiddle w/ names and formatting

alter_frame %>%                #our input alter_frame
	mutate(health1 = health1 %>% #modifying data to show off NA's
				 	"levels<-"(c(levels(health1)[-6], NA))) %>%
	group_by(record_id) %>%
	summarize(
		#Example of IQV
		sex_iqv = iqv_calculator(sex),
		#Example of Homophily
		race_homophily =
			prop_calculator(race,
											sample_data[sample_data$record_id %in% record_id,]$race1),
		#Example of Single Category Proportion
		prop_white = prop_calculator(race,
																 c("White")),
		#Example of Multi Category Proportion
		prop_kin = prop_calculator(data.frame(relat1, relat2),
															 c("Spouse", "Family")),
		#Example of how IQV deals w/ NA's
		iqv_unhealthy_NA = iqv_calculator(health1),
		#Example of how proportion deals w/ NA's
		prop_healthy_noNA = prop_calculator(health1, "No Health Problems"),
		prop_healthy_yesNA = prop_calculator(health1, "No Health Problems",
																					 na.rm = FALSE)
	)

# Processed Ego dplyr Example ##################################################
#This pipeline shows another solution to using a dplyr pipeline. In this example
#  we create a second dataset of ego data w/ a different id name to allow for a
#  more readable subsetting of ego data.
sample_data_alt <- sample_data %>%
	rename(study_id = record_id)

alter_frame %>%                #our input alter_frame
	mutate(health1 = health1 %>% #modifying data to show off NA's
				 	"levels<-"(c(levels(health1)[-6], NA))) %>%
	group_by(record_id) %>%
	summarize(
		#Example of IQV
		sex_iqv = iqv_calculator(sex),
		#Example of Homophily
		race_homophily =
			prop_calculator(
				race,                                 # Alter Data
				sample_data_alt %>%                   #
					subset(study_id %in% record_id) %>% # Ego Data
					"$"("race1")                        #
			),
		#Example of Single Category Proportion
		prop_white =
			prop_calculator(race, c("Black or African American", "White")),
		#Example of Multi Category Proportion
		prop_kin = prop_calculator(data.frame(relat1, relat2),
															 c("Spouse", "Family")),
		#Example of how IQV deals w/ NA's
		iqv_unhealthy_NA = iqv_calculator(health1),
		#Example of how proportion deals w/ NA's
		prop_healthy_noNA = prop_calculator(health1, "No Health Problems"),
		prop_healthy_yesNA = prop_calculator(health1, "No Health Problems",
																				 na.rm = FALSE)
	)

# By() function Example ########################################################
#Example of calculating statistics using the function "by()" rather than the
#  dplyr pipeline. By() as a function is a lot like lapply/apply except it
#  applies a defined function by groups of data rather than by single rows or
#  columns. I find this method to be much more flexible than dplyr, but it does
#  require a level of post-processing to construct a usable dataframe.
#I would advise using this method if you are planning on doing analysis more
#  complex than simple proportions/IQV/homophily.
by(alter_frame %>%               #our input alter_frame
	 	mutate(health1 = health1 %>% #modifying data to show off NA's
	 				 	"levels<-"(c(levels(health1)[-6], NA))),
	 alter_frame$record_id, #the groups which alter frame is split by
	 function(input_frame){ #The actulal functions we apply
	 	ego_data <- sample_data %>%
	 		subset(record_id == unique(input_frame$record_id))
	
		c(#Establish the id to allow for joining using left_join()
			"record_id" = ego_data$record_id,
			#Example of IQV
			"sex_iqv" = iqv_calculator(input_frame$sex),
			#Example of Homophily
			"race_homophily" = prop_calculator(input_frame$race, ego_data$race1),
			#Example of Single Category Proportion
			"prop_white" = prop_calculator(input_frame$race, c("White")),
			#Example of Multi Category Proportion
			"prop_kin" = prop_calculator(input_frame %>% select(relat1, relat2),
																	 c("Spouse", "Family")),
			#Example of how IQV deals w/ NA's
			iqv_unhealthy_NA = iqv_calculator(input_frame$health1),
			#Example of how proportion deals w/ NA's
			prop_healthy_noNA = prop_calculator(input_frame$health1, "No Health Problems"),
			prop_healthy_yesNA = prop_calculator(input_frame$health1, "No Health Problems",
																					 na.rm = FALSE) ) %>%
			return()
	 }
) -> by_product

#This set of processing is required for the result of the by() functiont to be a
#  readable format. By's output is sort-of like a list but with additional
#  doodads, which many functions don't like dealing with.
matrix(data = unlist(by_product),
			 nrow = length(by_product),
			 ncol = unique(sapply(by_product, length)),
			 byrow = TRUE) %>%
	data.frame() %>%
	"colnames<-"(names(c(by_product[[1]])))

# Complex By() function Example ################################################

input_frame <- alter_frame %>% subset(record_id == 1)

by(alter_frame,           #our input alter_frame
	 alter_frame$record_id, #the groups which alter frame is split by
	 
	 function(input_frame){ #The actulal functions we apply
	 	
	 	ego_data <- sample_data %>%
	 		subset(record_id == unique(input_frame$record_id))
	 	
	 	c(#Establish the id to allow for joining using left_join()
	 		"record_id" = ego_data$record_id,
	 		
	 		#Cross-statistic analysis example. In this case we check to see if a tie
	 		#  who "supports the me (the respondent) most often" offers "emotional
	 		#  support" from the variables support and supp_type1:supp_type5
	 		#  respectively
	 		"most_emotional_support" =
	 			input_frame %>%
	 			select(support, supp_type1:supp_type5) %>%
	 			apply(1, function(x){
	 				return(any(x %in% "Supports me most often") &
	 							 	any(x %in% "Emotional support"))
	 			}) %>%
	 			sum() %>%
	 			"/"(sum(!is.na(input_frame$support) | !is.na(input_frame$supp_type1))),
	 		
	 		#Summation example. Sometimes certain statistics are determined by the
	 		#  presence of a sum of certain variables. Note that this analysis is just
	 		#  an example and not actually how support may work in networks.
	 		"provides_support" =
	 			input_frame %>%
	 			select(supp_type1:supp_type5) %>%
	 			apply(1, function(x){
	 				x %in% levels(input_frame$supp_type1)[-6]
	 			}) %>%
	 			as.data.frame() %>%
	 			sapply(sum) %>%
	 			">="(3) %>%
	 			sum() %>%
	 			"/"(sum(!is.na(input_frame$supp_type1)))
	 		
	 		) %>%
	 		return()
	 }
) -> by_product

matrix(data = unlist(by_product),
			 nrow = length(by_product),
			 ncol = unique(sapply(by_product, length)),
			 byrow = TRUE) %>%
	data.frame() %>%
	"colnames<-"(names(c(by_product[[1]])))

#################### Outputting Standard Clean_Data Set ########################

#Having shown off the possible ways our alter data can be analyzed, lets give
#  a basic set of variables we can use. Feel free to take this and customize it
#  to add additional variables you wish to analyze or change cutoffs for
#  different values

alter_frame %>%
	group_by(record_id) %>%
	summarize(
		kin_prop = prop_calculator(data.frame(relat1, relat2),
															 c("Spouse", "Family")),
		age_sd = age %>% sd(na.rm = TRUE),
		IQVsex = iqv_calculator(sex, c("Male", "Female")),
		IQVrace =  iqv_calculator(race, c("Black or African American", "White",
																			"American Indian/Alaska Native", "Asian",
																			"Native Hawaiian or Other Pacifc Islander")),
		weak_freq_prop = prop_calculator(speak, c("Monthly", "Less often")),
		weak_dur_prop = prop_calculator(length, c("Less than three", "Three to six")),
		far_dist_prop = prop_calculator(dist,
																		c("16-50 miles", "50+ miles")),
		drinking_prop = prop_calculator(alcohol, c("Yes", "No")),
		smoking_prop = prop_calculator(smoke, c("Yes", "No")),
		no_exercise_prop = prop_calculator(exer, c("No")),
		bad_diet_prop = prop_calculator(diet, c("No")),
		health_prob_prop = prop_calculator(health1,
																			 c("General Health", "Pain",
																			 	"Cognitive/Mental Health", "Cardiac"))
		) ->
	alter_table

sample_data %>% select(record_id, age, sex, race1, race2, edu, zip, employment,
											 occupation, income, married, live_alone, household_number,
											 alcohol, smoke, exercise, diet,
											 health_problem1:health_problem4, network_size, density,
											 constraintInt, effsize, max_degree, mean_degree) %>%
	mutate(record_id = record_id %>% as.character() %>% as.integer()) ->
	ego_table

output_table <- left_join(ego_table, alter_table)

output_table %>%
	rename(education = edu,
				 healthy_diet = diet,
				 constraint = constraintInt) ->
	output_table

write.csv(output_table, file = "Clean_Data_2.csv")
