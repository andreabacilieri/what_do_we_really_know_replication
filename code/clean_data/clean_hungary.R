################################################################################
# This scripts cleans the hungarian network
# it keeps firms that are in the largest weakly connected component (lwcc)

# INPUT:
# from data/raw/hungary
#       - hungary_raw_data.csv
# from code/utils/
#       - fun_get_LWCC.R

# OUTPUT:
# from data/cleaned/hungary
#       - hungary_edgelist_lwcc.csv

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(igraph)
library(data.table)

# Get root folder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
# Get directory of the input data
dirdata <- file.path(rootfolder, 'data', 'raw', 'hungary')
# Where to save the output
dirOutput <- file.path(rootfolder, 'data', 'cleaned', 'hungary')
# Source function to compute LWCC
source(file.path(rootfolder, 'code', 'utils', 'fun_get_LWCC.R'))


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

filename <- paste(dirdata, 'hungary_raw_data.csv', sep = .Platform$file.sep)
df <- fread(filename)
# keep only columns we are interested in
df <- df[, c('subject', 'partner', 'aev', 'purchase_total')]
# rename columns
colnames(df) <- c("customer", "supplier", "year", "weight")
# order columns
setcolorder(df, c("supplier", "customer", "weight", "year"))


# ------------------------------------------------------------------------------
#  Compute LWCC and stats
# ------------------------------------------------------------------------------

lwcc_res <- get_LWCC(df)

# ------------------------------------------------------------------------------
#  Export LWCC and stats
# ------------------------------------------------------------------------------

csv_name <- paste(dirOutput, 'hungary_edgelist_lwcc.csv', sep = .Platform$file.sep)
fwrite(lwcc_res$edgelist, csv_name, row.names = F)

print('Created edgelists of the LWCC')
print('Csv with number of nodes and edges in the full network and lwcc exported')