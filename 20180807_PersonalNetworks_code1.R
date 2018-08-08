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
# LATEST:  08/07/18
# NOTES:   Code works on raw .csv outputs from REDCap, no processing required.
# ##############################################################################

# Empties Global Environment cache
rm(list=ls())

#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd("~/Desktop/PersonalNetworks")

#Importing packages. If not yet installed, packages can be installed by going to:
#Tools -> Install Packages, then enter their exact names from within each 
#library()
library(tidyverse) # For data management
library(igraph) # To transform and analyze network data
library(ggnetwork) # To make good-looking network graphs
library(intergraph) # May need this package
library('scales') # To add percentages
library(gridExtra) # For montage of networks
library(grid) # For montage of networks
#Although not supposed to load here, the functions below auto-loads the 
#following. If not already, make sure to install these packages as well.
#  sna
#  statnet.common
#  network

#Imports data and assigns it to variable "dataset"
dataset <- read.csv("20180806_PersonalNetwork_data.csv")

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
  #Fills the lower triangle of the matrix with the vector "ties"
  mat[lower.tri(mat)] <- ties
  #Transposes the lower triangle into the upper triangle of the matrix
  mat   <- t(mat)
  #Refills the lower triangle of the matrix with ties
  mat[lower.tri(mat)] <- ties
  #Names the columns and rows with EGO as the first row/col, row/col 2:16 are numbered
  #  1:15 respectively.
  colnames(mat) <- rownames(mat) <- c("EGO", "1", "2", "3", "4", "5", "6", "7", 
      "8", "9", "10", "11", "12", "13", "14", "15")
  
  #Removes columns and rows which have no tie entries, this removes people who are
  #  duplicates or over 10 and thus were not given any tie values.
  mat <- mat[(!colSums(mat,1) == 0), (!colSums(mat,1) == 0)]
  #Fills diagonal with 0s
  diag(mat) <- 0
  
  #Saves the named social ties from the survey
  name_ties <- x %>% select(name1, name2, name3, name4, name5, name6, name7, name8,
     name9, name10, name11, name12, name13, name14, name15)
  #Converts vector of names into a dataframe
  name_ties <- as.data.frame(t(name_ties))
  #Add a column to name_ties which matches the names of the matrix coloumns (1-15)
  name_ties$Replacement <- c("1", "2", "3", "4", "5", "6", "7", 
      "8", "9", "10", "11", "12", "13", "14", "15")
  #Saves names to the columns of name_ties for sorting
  colnames(name_ties) <- c("Name", "Current")
  #Create a new row to replace the name "EGO" from the matrix with the word "You"
  ego_df <- c("You", "EGO")
  ego_df <- as.data.frame(t(ego_df))
  colnames(ego_df) <- c("Name", "Current") 
  #Bind the name_ties with the new ego_df
  name_ties <- rbind(ego_df, name_ties)
  
  #Replace the matrix names with those from name_ties
  names <- match(colnames(mat), name_ties$Current) 
  colnames(mat) <- rownames(mat) <- name_ties$Name[names]
  
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
    shape <- select(l, tie1:a_tie105)
    
    #Isolates the ties of a single row, specified by the for-loop iteration, as a 1D
    #  vector of integers. Each row in the dataset is a single survey, and thus graph
    ties <- as.integer(shape[iterate, ])
    
    #Creates a blank matrix
    mat <- matrix(NA, 16, 16)
    #Fills the lower triangle of the matrix with the vector "ties"
    mat[lower.tri(mat)] <- ties
    #Transposes the lower triangle into the upper triangle of the matrix
    mat <- t(mat)
    #Refills the lower triangle of the matrix with ties
    mat[lower.tri(mat)] <- ties #refill lower triangle
    #Names the columns and rows with EGO as the first row/col, row/col 2:16 are numbered
    #  1:15 respectively.
    colnames(mat) <- rownames(mat) <- c("EGO", "1", "2", "3", "4", "5", "6", "7", 
                                        "8", "9", "10", "11", "12", "13", "14", "15")
    
    #Removes columns and rows which have no tie entries, this removes people who are
    #  duplicates and thus were not given any tie values.
    mat <- mat[(!colSums(mat, 1) == 0), (!colSums(mat, 1) == 0)]
    #Converts all ties labeled NA into 0's. Designed to turn the diagnal divider of NA's 
    #  into zero's, but will also correct any accidental NA's
    mat[is.na(mat)] = 0
    #Calls the variable mat so it is output
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
