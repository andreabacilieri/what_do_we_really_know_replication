############################################################################################################
# This script: 
# - cleanses the raw data
# - runs the initial network analysis
# - compares national accounts to network data

# NOTE: This script needs to be run on Hungary's central bank computer
#       because it manipulates the raw data that can be accessed only there
############################################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------
rm(list = ls())
mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
setwd(mainrootfolder)

#-------------------------------------------------------------------------------
# Clean raw data
#-------------------------------------------------------------------------------
source(file.path('code', 'clean_data', 'clean_hungary.R'))

#-------------------------------------------------------------------------------
# Compute I-O table and network-based I-O statistics
#-------------------------------------------------------------------------------
source(file.path('code', 'clean_data', 'I_O_table_statistics.R'))

#-------------------------------------------------------------------------------
# Run the initial network analysis
#-------------------------------------------------------------------------------
source(file.path('code', 'network_analysis', 'network_analysis.R'))
conduct_network_analysis(nPairs = 10^4, 'hungary', weight = T, remove = F, labour_share = 0.5)