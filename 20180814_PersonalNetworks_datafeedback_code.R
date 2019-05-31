# ##############################################################################
# PROJECT: PersonalNetworks 
# PURPOSE: Create a pdf for each entry in network survey, as well as a network 
#					 grid of all surveys. Each survey is individually named and identfied 
#					 by study_id
# DIR:     "~/Desktop/PersonalNetworks"
# INPUTS:  Fake data created in REDCap ("20180806_PersonalNetwork_data.csv") 
#          Can be replaced with real data of participants.
# OUTPUTS: 1) A series of pdfs, each with the name "ID[study_id] Social Network 
#					 Image.pdf", each pdf contains a network graph w/ labels.
#          2) A pdf with the name "Social Network Grid.pdf" which contains a 
#					 montage of all created network graphs (w/out node labels) with 
#					 study_id's.
# AUTHOR:  Liam McCafferty, Meghan Hutch, Nuzulul Kurniansyah, Amar Dhand
# CREATED: 06/08/18
# LATEST:  09/20/18
# NOTES:   Code works on raw .csv outputs from REDCap, no processing required.
# ##############################################################################

# Empties Global Environment cache
rm(list=ls())

#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd("~/Desktop/London Project")

#Importing packages. If not yet installed, packages can be installed by going to:
#Tools -> Install Packages, then enter their exact names from within each 
#library()
library(tidyverse) # For data management
library(igraph) # To transform and analyze network data
library(ggnetwork) # To make good-looking network graphs
library(scales) # To add percentages
library(gridExtra) # For montage of networks
library(grid) # For montage of networks
#Although not supposed to load here, the functions below auto-loads the 
#following. If not already, make sure to install these packages as well.
#  egonet
#  sna
#  statnet.common
#  network

#Imports data and assigns it to variable "dataset"
dataset <- read.csv("ToolForVisuallyMappi_DATA_2019-05-31_2140.csv")

#Check if REDCap has changed study_id to record_id, replace if so
colnames(dataset)[colnames(dataset) == "record_id"] <- "study_id"

#Function which makes a basic network matrix used by multiple functions
make_base_mat <- function(x){
  ##########
  # Function: Creates an NA-stripped matrix from a single row dataset
  # Inputs: x = Variable that stores the dataset
  # Ouputs: matrix "mat", the matrix will be stripped of people which have zero 
	#					ties, the matrix will also turn diagonal NA's (and mistaken NA's) 
	#					into 0's
  ##########
  
  #Saves the ties (edge data) of all egos and nodes as a 2D vector
  shape <- select(x, tie1:a_tie105)
  #Saves tie values as a 1D vector of integers.
  ties  <- as.integer(shape)
  #Creates a blank matrix
  mat   <- matrix(NA, 16, 16)
  
  #Name matrix columns and rows
  colnames(mat) <- rownames(mat) <- c("EGO", paste0("Alter",c(1:15)))
  
  #Fills the lower triangle of the matrix with the vector "ties"
  mat[lower.tri(mat)] <- ties
  #Transposes the lower triangle into the upper triangle of the matrix
  mat   <- t(mat)
  #Refills the lower triangle of the matrix with ties
  mat[lower.tri(mat)] <- ties
  #Names the columns and rows with EGO as the first row/col, row/col 2:16 are numbered
  #  1:15 respectively.
  
  
  #Removes columns and rows which have no tie entries, this removes people who are
  #  duplicates or over 10 and thus were not given any tie values.
  mat <- mat[(!colSums(mat,1) == 0), (!colSums(mat,1) == 0)]
  #Fills diagonal with 0s
  diag(mat) <- 0
  
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
    ego_g <- graph.adjacency(matrix_list[[z]], mode = "undirected",
                             weighted = TRUE)
    #Creates color palette for ties
    colors <- c("1" = "blue", "2" = "red")
    
    #Creates unique color palette for "ego" node
    ego_col <- ifelse(V(ego_g)$name == "EGO", "black", "white")
    
    #Determines structures of networks
    ggplot(ego_g, aes(x = x, y = y, xend = xend, yend = yend)) + 
      #Determines curvature and thickness of lines (unweighted)
      geom_edges(aes(color = as.factor(weight)), curvature = 0.1) +
      #Determines size and shape of nodes (shape21 = circle)
      geom_nodes(fill = ego_col, size = 1, shape = 21) +
      #Eliminates labels on each matrix
      theme_blank(legend.position = "none") +
      scale_color_manual(values = colors) +
      ggtitle(names(matrix_list[z]))
  }
  
  #Function makes network matrices out of raw tie data
  make_mat <- function(iterate){
    ##########
    # Function: Creates an NA-stripped matrix from a specified row of imported .csv
    # Inputs: x = Variable that stores the .csv file
    #        iterate = The iteration # of the for-loop the function is called from,
    #                  used to access a specific row in the imported .csv file
    # Ouputs: matrix "mat", the matrix will be stripped of people which have zero ties,
    #        the matrix will also turn diagonal NA's (and mistaken NA's) into 0's
    ##########
    
    #Saves the ties (edge data) of all egos and nodes as a 2D vector
    shape <- select(dataset[iterate,], tie1:a_tie105)
    #Saves tie values as a 1D vector of integers.
    ties  <- as.integer(shape)
    #Creates a blank matrix
    mat   <- matrix(NA, 16, 16)
    
    #Name matrix columns and rows
    colnames(mat) <- rownames(mat) <- c("EGO", paste0("Alter",c(1:15)))
    
    #Fills the lower triangle of the matrix with the vector "ties"
    mat[lower.tri(mat)] <- ties
    #Transposes the lower triangle into the upper triangle of the matrix
    mat   <- t(mat)
    #Refills the lower triangle of the matrix with ties
    mat[lower.tri(mat)] <- ties
    #Names the columns and rows with EGO as the first row/col, row/col 2:16 are numbered
    #  1:15 respectively.
    
    
    #Removes columns and rows which have no tie entries, this removes people who are
    #  duplicates or over 10 and thus were not given any tie values.
    mat <- mat[(!colSums(mat,1) == 0), (!colSums(mat,1) == 0)]
    #Fills diagonal with 0s
    diag(mat) <- 0
    
    return(mat)
  }
  
  
  #Acquires the # of rows of the dataset to determine length of for-loop
  mat_height <- dim(l)[1]
  #Creates a sequence from 1 to the height of the dataset to be used in for-loop
  mat_seq <- c(1:mat_height)
  #Creates a blank list to store the created network matrices, the list is made to have
  #  the same number of blank entries as rows in the dataset.
  matrix_list <- vector("list", mat_height)
  
  #for-loop which iterates a number of times equal to the number of rows in the dataset.
  #  Each iteration will call the function "make_mat", inputing the dataset and also
  #  inputing the for-loops's iteration number (to call that row from the dataset).
  #  "make_mat"'s  network matrix output is then assigned to matrix_list at the same index
  #  as the row the network matrix was created from.
  for(i in mat_seq){
    matrix_list[[i]] <- make_mat(i)
  }
  
  #Names each matrix in the list of matrices by their study_id number, used to name in grid
  names(matrix_list) <- sprintf("ID %s", seq(l$study_id))
  
  #Saves the row size of each matrix in the list of network matrices as a vector
  rowsize <- sapply(matrix_list, nrow)
  #Eliminates the network matrices which have < 2 nodes (3 rows including EGO)
  #  as ggplot2 is unable to create network graphs that small
  matrix_list <- matrix_list[!rowsize < 3]
  #Creates a blank array with the length of the matrix_list variable,
  #  assigns this array to variable z. This variable will be used
  #  to power the for-loop which creates network graphs
  blank_array <- array(1:length(matrix_list))
  
  #Assigns blank list to variable "plots" to concatinate network images
  plots <- list()
  
  #Creates network image for each network matrix using fast_graph
  #  function. Then concatinates network images into blank plots list
  for(i in blank_array){
    pl = fast_graph(i)
    plots[[i]] <- pl
  }
  #Makes the function return the value of plots
  return(plots)
  
}

#Function which makes Social Network Image 
make_image <- function(x) {
  ##########
  # Function: Creates and outputs a network graph with nodes named and ties colored
  # Inputs: x = input dataset with 1 row
  # Ouputs: plot1, a single network graph
  ##########
  
  #transform data to dataframe-table
  x <- tbl_df(x)
  
  #Creates a network matrix from input dataset file
  mat <- make_base_mat(x)
  
  #Saves important values for determining graph formatting
  ego4.g  <- graph.adjacency(mat, mode = "undirected", weighted = TRUE)
  colors  <- c("blue", "red") #create color palette for ties
  ego_col <- ifelse(V(ego4.g)$name == "You", "grey17", "white") 
  
  #Saves logic to determine the strength of ties between nodes
  weight.ego <- sapply(E(ego4.g)$weight, function(yk){
    if(is.na(yk)){
      return("Unknown")
    }else if(yk == 1){
      return("Weak Tie")
    }else{
      return("Strong Tie")
    }
  })
  
  if ("Unknown" %in% weight.ego ){
    #Error check to see if network has sufficient ties, will output a blank graph with
    #  error message.
    print("Error: Some networks ties are unknown ")
    plot1 <- ggplot(ego4.g, aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) +
      geom_blank() + ggtitle("Data doesn't work: some network ties are unknown")
      
  }else{
    E(ego4.g)$weight <- weight.ego
    #Creates actual network graph
    plot1 <- ggplot(ego4.g, aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) + 
      #Determines coloration and style of network edges
      geom_edges(aes(linetype = as.factor(weight), color = (weight)), curvature = 0.1) +
      #Fills nodes with ego_color pallate
      geom_nodes(fill = ego_col, size = 14, shape = 21) +
      #Names each node with alter names
      geom_nodelabel(label = rownames(mat))+
      theme_blank() +
      #Formats the legend which describes edge weight to the reader
      theme(legend.position = "bottom", #format the legend 
            legend.title = element_text(face = "bold", size = 15),
            legend.text = element_text(size = 10)) + 
      theme(legend.title.align = 0.5) + 
      theme(plot.title = element_text(size = 18, face = "bold")) +
      scale_colour_manual(name = "Tie Strength", values = c("red", "blue"))+
      scale_shape_manual(name = "Tie Strength", values = c(22, 21)) +
      scale_linetype_manual(name = "Tie Strength", values = c("solid", "dashed")) +
      #Determins the margins around plot
      theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
      #Formatting for legend's keys
      theme(legend.direction = 'vertical',
            legend.key.height = unit(1, "line"),
            legend.key = element_rect(colour = NA, fill = NA))
  }
}

#Function to Make Social Network Table
make_table <- function(x) {
  ##########
  # Function: Creates and outputs a graphical data table which contains importatnt stats
  # Inputs: x = input dataset with 1 row
  # Ouputs: table, a single graphical table which contains health stats from dataset
  ##########

  #transform data to dataframe-table
  x <- tbl_df(x)
  
  #Creates a network matrix from input dataset file
  mat <- make_base_mat(x)
  
  #Function used to find proportions of checkboxes
  checkbox_prop_maker <- function(prop_check){
    ##########
    # Function: Takes a selected set of variables from a REDCap checkbox and finds the
    #   proportion of them (in relation to the total tie count) which are 1.
    # Inputs: prop_check = set of isolated variables with na's and unused data stripped
    # Ouputs: proportion (in percentages) of 1's to used network size.
    # Notes: Checks all variables! You will need to get rid of unused variables.
    #   As well the tie identifies are determined by the first 8 characters of the
    #   variable names. Variable names require 2 things: tie identifiers must be
    #   before and separate from variable identifiers, tie identfiers must be either
    #   farther than 8 characters from the beginning or change the "width" value to
    #   cut variable identifiers out)
    ##########
    #Uses read.fwf to isolate column names of input, then takes the unique values
    prop_name_int <- unique(read.fwf(textConnection(colnames(prop_check)), widths = 8))
    #Changes list of names to characters rather than int
    prop_names <- apply(prop_name_int, 1, as.character)
    #Creates a list of 1's with length of the prop_names to be summed
    prop_count <- rep.int(1, length(prop_names))
    #Takes the entries per tie and checks if they have a 1, adds the TRUE/FALSE into
    #  the prop_count to eliminate zero ties so it can be summed
    for(i in 1:length(prop_names)){
      prop_count[i] <- any(prop_check[, grepl(prop_names[i], names(prop_check))])
    }
    #Calculates the proportion from the sum the number alters who have at least a 1
    proportion <- percent(sum(prop_count)/(nrow(mat) - 1))
    
    return(proportion)
  }
  
  
  #Calculate Network size
  size <- x %>% select(name_1:name_15) %>% sum()
  size <- paste(size, "People")
  
  #Calculate Density
  Density_1 <- graph.density(graph.adjacency(mat[-1, -1], mode = "undirected",
                                             weighted = TRUE))
  density <- percent(Density_1)
  
  # % Kin 
  kin  <- x %>% select(name1relat___1:name15relat___77) 
  # relat1 and relat2 represent kin
  kin1 <- kin[, grepl( "___1", names(kin))]
  kin2 <- kin[, grepl( "___2", names(kin))]
  # Bind kin together
  kin  <- cbind(kin1, kin2)
  
  # Calculate a proportion of kin
  kin_prop <- checkbox_prop_maker(kin)
  
  # % of ties who heavily drink alcohol 
  alcohol <- x %>% select(name1alcohol:name15alcohol) 
  alcohol$sum  <- length(which(alcohol == 0 | alcohol == 1))
  alcohol_prop <- percent(alcohol$sum / (nrow(mat) - 1))
  
  # % of ties who don't exericse 
  exer <- x %>% select(name1exer:name15exer) 
  exer$sum  <- length(which(exer == 0))
  exer_prop <- percent(exer$sum / (nrow(mat) - 1))
  
  # % of ties who don't eat a healthy diet 
  diet <- x %>% select(name1diet:name15diet) 
  diet$sum  <- length(which(diet == 0))
  diet_prop <- percent(diet$sum / (nrow(mat) - 1))
  
  #%of ties with health conditions 
  health  <- x %>% select(name1health___1:name15health___99)
  #Removes answers of no health problems and unknown
  health <- health[, !grepl("___99", names(health))]
  health <- health[, !grepl("___0", names(health))]
  
  healthprob_prop <- checkbox_prop_maker(health)
  
  #Format all percents into a table
  table <- data.frame(size, density, kin_prop, diet_prop, exer_prop, alcohol_prop,
                      healthprob_prop)
  #Transposes dataframe to verticle orientation
  table <- t(table)
  #Names each row of the data table
  rownames(table) <- c("Size of your network", "Density of ties in your network",
                       "Percent who are family", "Percent who don't eat a healthy diet", 
                       "Percent who don't exercise regularly",
                       "Percent who heavily drink alcohol",
                       "Percent who have health problems")
  #Eliminates names of the table columns
  colnames(table) <- ""
  #Prints the table
  table <- print(table, quote = FALSE)
  #Table formattting
  table <- data.frame('Social Network Characteristics' = row.names(table), table)
  table <- as.data.frame(table)
  table <- table %>% select(Social.Network.Characteristics, V1)
  #Table labeling
  colnames(table) <- c("Social Network Characteristics", "Your Network")
  
  return(table)
  
} 


#Creates a series of pdf documents containing the network graph
for(i in c(1:nrow(dataset))){
  #Isolates a single row from the dataset into variable input. Used in many fxns
  input <- dataset[i, ]
  #Checks to see if row in dataset has at least 3 ties, if not it skips the row
  if (rowSums(select(input, tie1:tie15), na.rm = TRUE) < 3) { 
    next
  }
  
  #Creates a pdf with the name of: "ID[study_id] Social Network Image.pdf", as the
  #  study id's are unique to each row, it will create a seperate pdf file for each
  #  input rather than overlap each other.
  pdf(file = (paste("ID", input$study_id, " Social Network Image", ".pdf",
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
grid.arrange(grobs = lapply(make_net_array(dataset), "+", margin),
             ncol = ceiling(sqrt(dim(dataset)[1])))
#Closes pdf file
dev.off()
