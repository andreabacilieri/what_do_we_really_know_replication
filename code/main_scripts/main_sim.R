################################################################################

# Simulates random graphs from confidential data and computes power-law exponents for the
# - degrees
# - strengths
# - weights
# - influence vector
# - clustering coefficients
# - path lengths

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------
rm(list = ls())
mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
setwd(mainrootfolder)


# ------------------------------------------------------------------------------
# Simulate random graphs, and compute clustering coefficients and path lengths
# ------------------------------------------------------------------------------
source(file.path('code', 'sim_random_graphs', 'sim_random_graphs_get_data.R'))
simulate_random_graphs(nSim = 100, nSim_pl = 10, nPairs = 10^4)

# ------------------------------------------------------------------------------
# Compute power-law exponents
# ------------------------------------------------------------------------------

# !! Set the python executable path here
# change to your environment's python
python_path <- ""
if (is.na(python_path) || python_path == "") {
  stop(sprintf("Error Python executable not specified in python_path (see main_scripts/main_sim.R)."), call. = FALSE)
}

## Degrees and strengths ##
mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
setwd(mainrootfolder)
source(file.path('code', 'code_tail_estimates', 'compute_tail_exponents_degs_strengths.R'))
compute_tail_exponents_degs_strengths(python_path)


## Weights and influence vector ##
mainrootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
setwd(mainrootfolder)
source(file.path('code', 'code_tail_estimates', 'compute_tail_exponents_weights_pageRank.R'))
compute_tail_exponents_weights_pageRank(python_path)
