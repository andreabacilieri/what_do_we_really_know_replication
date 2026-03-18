#################################################################################################################
# Main script : calls all the functions to reproduce all (non-hardcoded) Tables and Figures
#################################################################################################################

# Set environment/parameters
mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
setwd(mainrootfolder)

#-----------------------------------------------------------------------------------------------------
# 0 - Install packages
#-----------------------------------------------------------------------------------------------------
renv::restore()

#-----------------------------------------------------------------------------------------------------
# 1 - Clean and analyse confidential data 
#-----------------------------------------------------------------------------------------------------

# Reload environment
mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
setwd(mainrootfolder)

print('Prepoare Ecuador')
weight <- T
source(file.path('code', 'main_scripts', 'main_prepare_ecuador.R'))
print('Prepoare Hungary')
source(file.path('code', 'main_scripts', 'main_prepare_hungary.R'))

weight <- F
print('Prepoare FactSet')
source(file.path('code', 'main_scripts', 'main_prepare_factset.R'))


#-----------------------------------------------------------------------------------------------------
# 2 - Simulate random graphs from confidential data and compute power law exponents
#-----------------------------------------------------------------------------------------------------

# Reload environment
mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
setwd(mainrootfolder)
source(file.path('code', 'main_scripts', 'main_sim.R'))


#-----------------------------------------------------------------------------------------------------
# 3 - Produce Figures and Tables of the paper
#-----------------------------------------------------------------------------------------------------

mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
setwd(mainrootfolder)
source(file.path('code', 'main_scripts', 'main_visualisation.R'))
