################################################################################
# This script cleans the Ecuador data 
# it keeps firms that are in the largest weakly connected component (lwcc)

# YEARS: from 2007 to 2015
# VARIABLES:   
#  - id_prov: node source, i.e., supplier
#  - id_inf: node target, i.e., customer
#  - total_tax: weight, i.e., transaction value in US dollars 

# INPUTS:
# Data:
# from data/raw/ecuador/network
#     - totaltax_ND(*)_firms.csv
# from code/utils/
#     - fun_get_LWCC.R
# * = year in [2007, 2015]

# OUTPUT:
# to data/cleaned/ecuador
#     - ecuador_edgelist_lwcc.csv

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = setdiff(ls(), c("nSim", "nSim_pl", "nPairs", "weight")))

library(igraph)
library(data.table)
library(dplyr)

# Find root folder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Get directory of the input data (outside of the project)
dirdata <- file.path(rootfolder, 'data', 'raw', 'ecuador', 'network')
# Set directory for Output
dirOutput <- file.path(rootfolder, 'data', 'cleaned', 'ecuador')
file_name <- 'ecuador_edgelist_lwcc.csv'

# ------------------------------------------------------------------------------
#  Clean data and get LWCC
# ------------------------------------------------------------------------------

get_csv_files <- function(directory) {
  all_files <- list.files(path = directory)
  csv_files <- all_files[grepl("\\.csv$", all_files)]
  return(csv_files)
}

csvfiles <- get_csv_files(dirdata)

# create empty df to be used for appending the cleaned data
df_giant <- data.frame(supplier = character(),
                       customer = character(),
                       weight = numeric(),
                       year = integer())

# loop through yearly csv files, clean and create one big csv file
for (f in csvfiles) {
  
  df <- read.csv(paste0(dirdata, "/", f), stringsAsFactors = FALSE)
  df$id_inf <- as.character(df$id_inf)
  df$id_prov <- as.character(df$id_prov)

  # Eliminate zero weight transactions
  df <- df[df$total_tax != 0, ]
  
  # Build network
  df <- df[, c("id_prov", "id_inf", "total_tax")]
  colnames(df) <- c("supplier", "customer", "weight")
  g <- graph_from_data_frame(df, directed = TRUE)
  
  # print number of nodes and edges before simplifying
  cat(sprintf("Before simplify: Nodes = %d, Edges = %d\n", vcount(g), ecount(g)))
  # Delete self-loops
  g <- simplify(g)
  # print number of nodes and edges after simplifying
  cat(sprintf("After simplify: Nodes = %d, Edges = %d\n", vcount(g), ecount(g)))

  # Get lwcc
  cl <- components(g, mode = "weak")
  g <- induced_subgraph(g, which(cl$membership == which.max(cl$csize)))
  
  # convert to data frame
  df <- get.data.frame(g, what = "edges")
  colnames(df) <- c("supplier", "customer", "weight")
  # get year
  yr <- substr(f, nchar(f) - 14, nchar(f) - 11)
  df$year <- as.integer(yr)
  # append to df storing all yearly nets
  df_giant <- rbind(df_giant, df)
}


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

write.csv(df_giant, file = file.path(dirOutput, file_name), row.names = FALSE)