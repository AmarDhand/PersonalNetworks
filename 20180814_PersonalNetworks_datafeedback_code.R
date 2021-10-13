# ##############################################################################
# PROJECT: PersonalNetworks 
# PURPOSE: Create a pdf for each entry in network survey, as well as a network 
#					 grid of all surveys. Each survey is individually named and identfied 
#					 by record_id
# DIR:     "~/Desktop/PersonalNetworks"
# INPUTS:  Pre-processed data from egodata and alterdata codes
# OUTPUTS: 1) A series of pdfs, each with the name "ID[record_id] Social Network 
#					 Image.pdf", each pdf contains a network graph w/ labels.
#          2) A pdf with the name "Social Network Grid.pdf" which contains a 
#					 montage of all created network graphs (w/out node labels) with 
#					 record_id's.
# AUTHOR:  Liam McCafferty, Meghan Hutch, Nuzulul Kurniansyah, Amar Dhand
# CREATED: 06/08/18
# LATEST:  09/22/21
# NOTES:   
# ##############################################################################

#This code does two different actions:
#Aim 1: First it constructs a labeled network graph PDF for each row/ID. This graph
#  is colored, contains the names of each network member, and has a second page
#  which lists some important statistics in a table form.
#Aim 2: Second it makes a montage of these network graphs, each labeled with their
#  constituent ID.

#In case you need to make changes this should help where to look:
# | Function       |  What it does                 | Dependencies  | Aim Usage |
# | -------------------------------------------------------------------------- |
# | make_base_mat  | constructs adjacency matrix   | none          | 1 & 2     |
# | make_net_array | makes list of network graphs  | make_base_mat | 2         |
# | make_image     | makes one fancy network graph | make_base_mat | 1         |
# | prop_calculator| calculates alter proportions  | none          | 1         |
# | iqv_calculator | calculates alter diversity    | none          | 1         |
# | make_table     | makes statistics table        | prop/iqv_calc | 1         |
# | -------------------------------------------------------------------------- |

# Empties Global Environment cache
rm(list = ls())

#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each library()
#  Otherwise this set of code should automatically install the required packages.

ifelse("tidyverse" %in% rownames(installed.packages()), "installed", install.packages("tidyverse"))
ifelse("igraph" %in% rownames(installed.packages()), "installed", install.packages("igraph"))
ifelse("ggnetwork" %in% rownames(installed.packages()), "installed", install.packages("ggnetwork"))
ifelse("gridExtra" %in% rownames(installed.packages()), "installed", install.packages("gridExtra"))
ifelse("grid" %in% rownames(installed.packages()), "installed", install.packages("grid"))
ifelse("sna" %in% rownames(installed.packages()), "installed", install.packages("sna"))
ifelse("statnet.common" %in% rownames(installed.packages()), "installed", install.packages("statnet.common"))
ifelse("statnet.common" %in% rownames(installed.packages()), "installed", install.packages("statnet.common"))
# Due to egonet being dumped from CRAN every once an a while, we are sourcing
#  the archived 1.2 version of egonet from the cran website.
packageurl <- "https://cran.r-project.org/src/contrib/Archive/egonet/egonet_1.2.tar.gz"
ifelse("egonet" %in% rownames(installed.packages()), "installed",
       install.packages(packageurl, repos = NULL, type = "source") )
rm(packageurl)

library(tidyverse) # For data management
library(igraph) # To transform and analyze network data
library(ggnetwork) # To make good-looking network graphs
library(gridExtra) # For montage of networks
library(grid) # For montage of networks
#Although not supposed to load here, the functions below auto-loads the 
#following. If not already, make sure to install these packages as well.
#  egonet
#  sna
#  statnet.common
#  statnet.common

#Set working directory to current file location
#This code should already work, but if not do this:
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("~/Desktop/PersonalNetworks-master")

load("ego_data.rda")
load("alter_data.rda")

#Function which makes a basic network matrix used by multiple functions
make_base_mat <- function(x, use_names = TRUE){
  ##########
  # Function: Creates and outputs an adjacency matrix from input raw tie data
  # Inputs: x = input data, must be single time point of data, can be either a
  #   selected row of a dataframe or a vector/apply input. Data must contain
  #   variables with ego-alter tie info (tie#), alter-alter tie info (a_tie#).
  #   Alter names (name#) will be included if present in data, otherwise will
  #   use default naming (You, 1:15). Use default variable names from raw data.
  #   Function should be able to adapt to any number (larger than 1) of maximum
  #   alters (default of PersNet is 15)
  #         use_names = Accepts logical TRUE or FALSE, determines if the output
  #   matrix's col/rownames are pulled from the network or are just numbers
  # Ouputs: Function returns single matrix (mat), named
  ##########
  
  #Accessing tie data for a vector, using grepl to identify names this function
  #  should work even when number of network members extends/reduces the normal
  #  15+1 network members. Ensure input uses default raw variable names
  x[grepl("^tie|^a_tie", names(x))] %>%
    as.integer() ->
    ties
  
  #Calculates the number of ties we should have. Taken from number of possible
  #  unique pairs in a set: n(n-1)/2 = y where n = number of values in set and
  #  y = number of ties. As we know y already, then we can simplify the equation:
  #    x^2 - x - (length(ties) * 2) = 0
  #We can then solve for x using the quadratic equation and get the
  #  (positive) value from this simplified formula:
  tie_num = (1 + sqrt(1 - (4 * 1 * (- length(ties) * 2) ))) / 2 * 1
  
  #Creates a blank matrix
  mat   <- matrix(NA, tie_num, tie_num)
  #Fills the lower triangle of the matrix with the vector "ties"
  mat[lower.tri(mat)] <- ties
  #Transposes the lower triangle into the upper triangle of the matrix
  mat   <- t(mat)
  #Refills the lower triangle of the matrix with ties
  mat[lower.tri(mat)] <- ties
  #Names the columns and rows with EGO as the first row/col, row/col 2:16 are numbered
  #  1:15 respectively.
  colnames(mat) <- rownames(mat) <- c("EGO", 1:(nrow(mat) - 1))
  
  #Removes columns and rows which have no tie entries, this removes people who are
  #  duplicates or over 10 and thus were not given any tie values.
  present_ties <- !colSums(mat,1) == 0
  mat <- mat[present_ties, present_ties]
  #Fills diagonal with 0s
  diag(mat) <- 0
  
  if(any(grepl("^name[0-9]+$", names(x))) & use_names){
    x[grepl("^name[0-9]+$", names(x))] %>%
      as.character() %>%
      c("You", .) %>%
      "["(present_ties) -> colnames(mat) -> rownames(mat)
  }
  
  return(mat)
}

#Function which makes grid of General Social networks
make_net_array <- function(l){
  ##########
  # Function: Creates and outputs a list of network graphs from input dataset
  # Inputs: l = input dataset
  # Ouputs: function returns contents of variable "plots", list of network graphs
  ##########
  
  # Function which creates simple social network images
  fast_graph <- function(z){
    ##########
    #Function: turns imported network matrix into network image
    #Inputs: network matrix
    #Ouputs: network graph of imported matrix
    ##########
    ego_g <- graph.adjacency(z, mode = "undirected",
                             weighted = TRUE)
    #Creates color palette for ties
    colors <- c("1" = "blue", "2" = "red")
    
    #Creates unique color palette for "ego" node
    V(ego_g)$ego_col <- ifelse(V(ego_g)$name == "EGO", "black", "white")
    
    #Determines structures of networks
    ggplot(ego_g, aes(x = x, y = y, xend = xend, yend = yend)) + 
      #Determines curvature and thickness of lines (unweighted)
      geom_edges(aes(color = as.factor(weight)), curvature = 0.1) +
      #Determines size and shape of nodes (shape21 = circle)
      geom_nodes(aes(fill = ego_col), size = 1, shape = 21) +
      #Eliminates labels on each matrix
      theme_blank(legend.position = "none") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = c("black", "white"))
  }
  
  #Applying make_base_mat function to create an adjacency matrix for each ID.
  #  Also naming each constructed matrix by its ID, name is used in ID labels
  #  for each network image in the montage. If you want label to look different,
  #  simply change the text contained w/in the paste0() function.
  apply(l, 1, make_base_mat) %>%
    "names<-"(paste0("ID ", sample_data$record_id)) ->
    matrix_list
  
  #Eliminates the network matrices which have < 2 nodes (3 rows including EGO)
  #  as ggplot2 is unable to create network graphs that small
  matrix_list <- matrix_list[!sapply(matrix_list, nrow) < 3]
  
  #Creates network image for each network matrix using fast_graph
  #  function. lapply always outputs as list
  plots <- lapply(matrix_list, fast_graph)
  
  #Applying a title to each plot so that they are labeled in the montage.
  lapply(1:length(plots), function(x){plots[[x]] + ggtitle(names(matrix_list)[x])}) %>%
    "names<-"(names(matrix_list)) ->
    plots
  
  return(plots)
}

#Function which makes Social Network Image 
make_image <- function(x) {
  ##########
  # Function: Creates and outputs a network graph with nodes named and ties colored
  # Inputs: x = input dataset with 1 row or input vector/apply object.
  #    Follows same rules as make_base_mat()
  # Ouputs: plot1, a single network graph
  ##########
  
  #Creates a network matrix from input dataset file
  mat <- make_base_mat(x)
  
  #Saves important values for determining graph formatting
  ego4.g  <- graph.adjacency(mat, mode = "undirected", weighted = TRUE)
  V(ego4.g)$ego_col <- ifelse(V(ego4.g)$name == "You", "grey17", "white")

  #Saves logic to determine the strength of ties between nodes
  E(ego4.g)$weight_cat <- sapply(E(ego4.g)$weight, function(yk){
    if(is.na(yk)){
      return("Unknown")
    }else if(yk == 1){
      return("Weak Tie")
    }else{
      return("Strong Tie")
    }
  })
  
  if("Unknown" %in% E(ego4.g)$weight_cat ){
    #Error check to see if network has sufficient ties, will output a blank graph with
    #  error message.
    print("Error: Some networks ties are unknown ")
    plot1 <- ggplot(ego4.g, aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) +
      geom_blank() + ggtitle("Data doesn't work: some network ties are unknown")
      
  }else{
    #Creates actual network graph
    plot1 <- ggplot(ggnetwork(ego4.g), aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) + 
      #Determines coloration and style of network edges
      geom_edges(aes(linetype = weight_cat, color = weight_cat), curvature = 0.1) +
      #Fills nodes with ego_color pallate
      geom_nodes(aes(fill = ego_col), size = 14, shape = 21) +
      #Names each node with alter names
      geom_nodelabel(aes(label = name)) +
      theme_blank() +
      #Formats the legend which describes edge weight to the reader
      theme(legend.position = "bottom", #format the legend 
            legend.title = element_text(face = "bold", size = 15),
            legend.text = element_text(size = 10)) + 
      theme(legend.title.align = 0.5) + 
      theme(plot.title = element_text(size = 18, face = "bold")) +
      scale_colour_manual(name = "Tie Strength", values = c("red", "blue")) +
      scale_fill_manual(values = c("grey17", "white")) +
      scale_shape_manual(name = "Tie Strength", values = c(22, 21)) +
      scale_linetype_manual(name = "Tie Strength", values = c("solid", "dashed")) +
      #Determins the margins around plot
      theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
      #Formatting for legend's keys
      guides(fill = FALSE) +
      theme(legend.direction = 'vertical',
            legend.key.height = unit(1, "line"),
            legend.key = element_rect(colour = NA, fill = NA))
  }
}

#Function which constructs proportions for make_table()
prop_calculator <- function(input_alters, input_check, na.rm = TRUE){
  ##########
  #Function: Calculates a proportion value from an input data and categories
  #Input: input_alters - input data, accepts either vector or matrix/dataframe.
  #         Expects input to be either character or factor.
  #       input_check - vector of values the function will check the proportion of
  #       checks if NA's are included in proportion, otherwise they are removed
  #Output:Numeric with length of 1, proportion of given input_check present in input_alters.
  ##########
  
  #Logic for vectors. Potential area for error as is.atomic sometimes picks up
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

#Function which constructs diversity calcuations (IQV) for the table function
iqv_calculator <- function(input_alters, possible_categories = NA){
  ##########
  #Function: Calculates a diversity statistic from an input data
  #Input: input_alters - input data, accepts either vector or matrix/dataframe.
  #         Expects input to be either character or factor.
  #       possible_categories - vector of possible categories a variable may
  #         contain. If data is not in factor form, the function will assume that
  #         all possible categories are included in the input data. To give the
  #         function the full set of possible categories or limit the categories to
  #         a certain set, this input can be used to manually override.
  #         checks if NA's are included in proportion, otherwise they are removed
  #Output:Numeric with length of 1, diversity calculation using IQV
  ##########
  
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
  
  #Calculation, w/ logic to return NA rather than Inf if denominator is 0
  if(alter_total == 0){
    return(NA)
  }else if(all(is.na(input_alters))){
    return(NA)
  }else{
    return((1 - sum(iqv_chunk(cat_count, alter_total))) /
             (1 - (1/possible_categories_count) ))
  }
}

#Function to Make Social Network Table
make_table <- function(x) {
  ##########
  # Function: Creates and outputs a graphical data table which contains importatnt stats
  # Inputs: x = input dataset with 1 row
  # Ouputs: table, a single graphical table which contains health stats from dataset
  ##########

  #Isolating our alter information for our specific ID
  alter_chunk <- alter_frame %>% subset(record_id == x$record_id)
  
  percentage <- function(input){paste0(input * 100, "%")}
  
  #### Constructing table statistics #####
  
  #Network size from ego_data calculations
  size <- x$network_size
  
  #Density from ego_data calculations
  Density_1 <- x$density
  
  # % Kin 
  kin_prop <- prop_calculator(alter_chunk[grepl("^relat", colnames(alter_chunk))],
                              c("Spouse", "Family"))
  
  # % of ties who heavily drink alcohol 
  alcohol_prop <- prop_calculator(alter_chunk$alcohol, c("Yes", "No"))
  
  # % of ties who don't exericse 
  exer_prop <- prop_calculator(alter_chunk$exer, c("No"))
  
  # % of ties who don't eat a healthy diet
  diet_prop <-  prop_calculator(alter_chunk$diet, c("No"))
  
  # %of ties with health conditions
  healthprob_prop <- prop_calculator(alter_chunk[grepl("^health", colnames(alter_chunk))],
                                     c("General Health", "Pain",
                                       "Cognitive/Mental Health", "Cardiac"))
  
  ###### Table Construction and Assignment ######
  
  #Format all percents into a table, these
  table_output <- data.frame(size, Density_1, kin_prop, diet_prop, exer_prop,
                      alcohol_prop, healthprob_prop)
  #Transposes dataframe to verticle orientation
  table_output <- t(table_output)
  #Names each row of the data table
  rownames(table_output) <- c("Size of your network", "Density of ties in your network",
                       "Percent who are family", "Percent who don't eat a healthy diet", 
                       "Percent who don't exercise regularly",
                       "Percent who heavily drink alcohol",
                       "Percent who have health problems")
  #Prints the table_output
  table_output <- data.frame('Social Network Characteristics' = row.names(table_output), table_output)
  #Rounding table values
  table_output$table_output <- round(table_output$table_output, digits = 4)
  #Percentage-ing our proportions
  table_output$table_output[c(-1,-2)] <- percentage(table_output$table_output[c(-1,-2)])
  #Table labeling
  colnames(table_output) <- c("Social Network Characteristics", "Your Network")
  
  
  return(table_output)
  
}

#Creates a series of pdf documents containing the network graph
for(i in c(1:nrow(sample_data))){
  #Isolates a single row from the sample_data into variable input. Used in many fxns
  input <- sample_data[i, ]
  #Checks to see if row in sample_data has at least 3 ties, if not it skips the row
  if (rowSums(select(input, tie1:tie15), na.rm = TRUE) < 3) { 
    next
  }
  
  #Creates a pdf with the name of: "ID[record_id] Social Network Image.pdf", as the
  #  study id's are unique to each row, it will create a seperate pdf file for each
  #  input rather than overlap each other.
  pdf(file = (paste("ID", input$record_id, " Social Network Image", ".pdf",
  									sep = "")), width = 11, height = 7)
  #Creates a network graph and draws it onto the pdf
  grid.draw(make_image(input))
  #Makes a new page in pdf
  grid.newpage(recording = TRUE)
  #Creates the data table from input
  grid.table(make_table(input), rows = NULL)
  #Closes pdf file
  dev.off()
}

#Determins the margins seperating each network graph for network grid
margin = theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))

#Creates and opens PDF
pdf(file = "Social Network Grid.pdf", width = 11, height = 7)

#Determines organisation of network graphs on grid and prints them
grid.arrange(grobs = lapply(make_net_array(sample_data), "+", margin),
             ncol = ceiling(sqrt(dim(sample_data)[1])))
#Closes pdf file
dev.off()
