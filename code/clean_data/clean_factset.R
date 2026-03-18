################################################################################
# This script cleans the FactSet data
# it keeps firms that are in the largest weakly connected component (lwcc)

# INPUTS:
# Data:
# from data/raw/factset
#     - shipping/sc_ship_trans_curr_*.txt
#     - supply_chain/ent_scr_supply_chain.txt
# from code/utils/
#     - fun_get_LWCC.R
# * in [1, 6]

# OUTPUT:
# to data/cleaned/factset
#     - factset_edgelist_lwcc.csv

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(igraph)
library(data.table)

# Find root folder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Get directory of the input data (outside of the project)
dirdata <- file.path(rootfolder, 'data', 'raw', 'factset')
# Set directory for Output
dirOutput <- file.path(rootfolder, 'data', 'cleaned', 'factset')


# ------------------------------------------------------------------------------
# Load and clean shipping data
# ------------------------------------------------------------------------------

x <- list()
colstokeep <- c("SHIPPER_FACTSET_ENTITY_ID", "CONSIGNEE_FACTSET_ENTITY_ID", "ACTUAL_ARRIVAL_DATE")
for(i in 1:6){
  fp <- file.path(dirdata, "shipping", paste0("sc_ship_trans_curr_", i, ".txt") )
  x[[i]] <- fread(fp, data.table = F, colClasses = "character", select = colstokeep)
  x[[i]] <- x[[i]][which(duplicated(x[[i]]) == F), ]
  print(i)
}
fship <- do.call(rbind, x)
rm(x)

# there are three dates: RECORD_DATE, ESTIMATED_ARRIVAL_DATE, ACTUAL_ARRIVAL_DATE
# we choose ACTUAL_ARRIVAL_DATE and keep the year only
fship[["ACTUAL_ARRIVAL_DATE"]] <- as.numeric(substr(fship[["ACTUAL_ARRIVAL_DATE"]], 1, 4))

# Clean generic entity identifiers in shipping data
# NOTE: FactSet manual "Standard DataFeed User Guide: Supply Chain Shipping Transactions V1.1.2" page 11
#       "Transactions with unknown shippers and consignees are represented by generic entity identifiers.
#       The fields will display identifiers as follows:
#       SHIPPER_FACTSET_ENTITY_ID: 0JWCDG-E
#       CONSIGNEE_FACTSET_ENTITY_ID: 0JWCDH-E:
torm <- which(fship[["SHIPPER_FACTSET_ENTITY_ID"]] == "0JWCDG-E" | fship[["CONSIGNEE_FACTSET_ENTITY_ID"]] == "0JWCDH-E")
fship <- fship[-torm,]


# ------------------------------------------------------------------------------
# Load and clean supply chain data
# ------------------------------------------------------------------------------

fsc <- fread(file.path(dirdata, "supply_chain", "ent_scr_supply_chain.txt"), data.table = F, colClasses = "character")
# all entries have a proper start year
fsc[["START_DATE"]] <- as.numeric(substr(fsc[["START_DATE"]], 1, 4))
# sometimes there is no end date
sort(unique(substr(fsc$END_DATE, 1, 4)))
# when there is no end date, it means the link is still active: we replace "" by "2024"
fsc$END_DATE[fsc$END_DATE == ""] <- "2024"
fsc[["END_DATE"]] <- as.numeric(substr(fsc[["END_DATE"]], 1, 4))
all(sort(unique(fsc$END_DATE)) == 2013:2024)

fsc <- fsc[, c("SUPPLIER_FACTSET_ENTITY_ID", "CUSTOMER_FACTSET_ENTITY_ID", "START_DATE", "END_DATE")]
# remove duplicates
tokeep <- which(duplicated(fsc) == F)
fsc <- fsc[tokeep, ]


# ------------------------------------------------------------------------------
# Create yearly network that merges the two sources
# ------------------------------------------------------------------------------

# NOTE: - we do not use years prior to 2014 because in 2013 FactSet changed the data collection methodology, enhancing the quality of the dataset
#       - we stop in 2023, because we downloaded the data in April 2024

yearslist <- 2014:2023
EL_export <- list()

for(tt in 1:length(yearslist)){

  year <- yearslist[tt]

  # Get data from "supply chain" file
  ww <- which(fsc[["START_DATE"]] <= year & fsc[["END_DATE"]] >= year )
  ELsc <- fsc[ww, c("SUPPLIER_FACTSET_ENTITY_ID", "CUSTOMER_FACTSET_ENTITY_ID")]

  # Get data from "shipping" files
  ww <- which(fship[["ACTUAL_ARRIVAL_DATE"]] == year )
  ELship <- fship[ww, c("SHIPPER_FACTSET_ENTITY_ID", "CONSIGNEE_FACTSET_ENTITY_ID")]

  # Merge
  colnames(ELship) <- c("SUPPLIER_FACTSET_ENTITY_ID", "CUSTOMER_FACTSET_ENTITY_ID")
  EL <- rbind(ELsc, ELship)
  rm(ELsc, ELship)

  # Remove duplicates
  dups <- duplicated(EL)
  if(sum(dups)>0){
    EL <- EL[which(dups == F), ]
  }
  g <- simplify(graph_from_edgelist(as.matrix(EL), directed = T))
  rm(EL)

  # Print number of nodes and edges before simplifying
  cat(sprintf("Before simplify: Nodes = %d, Edges = %d\n", vcount(g), ecount(g)))

  # Keep LWCC
  g <- simplify(g)
  cc <- components(g, mode = "weak")
  g <- delete_vertices(g, which(cc$membership != which.max(cc$csize)))

  # Print number of nodes and edges after simplifying
  cat(sprintf("After simplify: Nodes = %d, Edges = %d\n", vcount(g), ecount(g)))

  EL_export[[tt]] <- cbind(as_edgelist(g), rep(year, ecount(g)))
  colnames(EL_export[[tt]]) <- c("supplier", "customer", "year")
}


# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------

EL <- do.call(rbind, EL_export)
filename <- file.path(dirOutput, 'factset_edgelist_lwcc.csv')
write.csv(EL, file = filename, row.names = F)