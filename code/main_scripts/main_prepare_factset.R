############################################################################################################
# This script: 
# - cleanses the raw data
# - runs the initial network analysis

# NOTE: This script needs to be run with a FactSet license
#       because it manipulates the raw data that can be accessed only with a license
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
source(file.path('code', 'clean_data', 'clean_factset.R'))

#-------------------------------------------------------------------------------
# Run the initial network analysis
#-------------------------------------------------------------------------------
source(file.path('code', 'network_analysis', 'network_analysis.R'))
conduct_network_analysis(nPairs = 10^4, 'factset', weight = F, remove = F)