############################################################################################################
# This script: 
# - cleans the raw data
# - runs the initial network analysis
# - runs the truncation analysis

# NOTE: This script requires access to the Ecuadorian data to be run
############################################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------
rm(list = ls())
mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
setwd(mainrootfolder)

#--------------------------------------------------------------------------------------------------
# Clean raw data
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'clean_data', 'clean_ecuador.R'))

#--------------------------------------------------------------------------------------------------
# Run the initial network analysis
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'network_analysis', 'network_analysis.R'))
conduct_network_analysis(nPairs = 10^4, 'ecuador', weight = T, remove = F, labour_share = 0.5) 

#--------------------------------------------------------------------------------------------------
# Run the truncation network analysis
#--------------------------------------------------------------------------------------------------
source(file.path('code', 'network_analysis', 'network_analysis.R'))
conduct_network_analysis(nPairs = 10^4, 'ecuador', weight = T, trunc = T, remove = F)
