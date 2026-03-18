##########################################################################################################################
# Calculate tail exponents for the local properties of the networks
# (inD, outD, inS, outS) for the three datasets
# (Ecuador, Hungary, FactSet)

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_local_properties.csv
#     - hungary/hungary_local_properties.csv
#     - factset/factset_local_properties.csv

# OUTPUT:
# to data/analysis/
#   - ecuador/ecuador_tails_*.csv
#   - hungary/hungary_tails_*.csv
#   - factset/factset_tails_*.csv
# * in {inD, outD, inS, outS}

#######################################################################################################################


compute_tail_exponents_degs_strengths <- function(python_path){
  
  #' @param python_path Sets directory of the python environment to use
  
  # ------------------------------------------------------------------------------
  #  Set environment/parameters
  # ------------------------------------------------------------------------------
  rm(list = ls())
  library(igraph)
  library(data.table)
  
  # Find root folder
  rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
  # Set directory for Output
  dirOutput <- file.path(rootfolder, 'data', 'analysis')
  # Get directory of the input data
  dirdata <- file.path(rootfolder, 'data', 'analysis')
  dir_fun_utils <- file.path(rootfolder, 'code', 'code_tail_estimates', 'utils')
  
  # Function to compute tail exponents
  source(file.path(dir_fun_utils,'fun_compute_tail_exponents.R'))
  # Get working directory for python call (needs to use path.expand because this gets passed to the terminal to run the python code)
  mywd <- path.expand(dir_fun_utils)
  
  
  # ------------------------------------------------------------------------------
  #  Load data
  # ------------------------------------------------------------------------------
  df_f <- fread(file.path(dirdata, 'factset', 'factset_local_properties.csv'))
  df_h <- fread(file.path(dirdata, 'hungary', 'hungary_local_properties.csv'))
  df_e <- fread(file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv'))
  DFlist <- list(df_e, df_h, df_f)
  rm(df_e, df_h, df_f)
  
  
  # ------------------------------------------------------------------------------
  #  Empirical exponents
  # ------------------------------------------------------------------------------
  myvarlist <- c("inD", "outD", "inS", "outS")
  countryname <- c("ecuador", "hungary", "factset")
  t0 <- Sys.time()
  
  for(dd in 1:length(DFlist)){
    
    for(vv in 1:length(myvarlist)){
      
      # do not try to compute tail exponents for strengths for in FactSet
      if(countryname[dd] == "factset" & myvarlist[vv] %in% c("inS", "outS")) { next }
      
      # get right variable from right dataset
      DF <- DFlist[[dd]]
      myvar <- myvarlist[vv]
      
      # Compute tail exponents for each year
      alpha_list_empi <- list()
      Years <- sort(unique(DF$year))
      nn <- rep(NA, length(Years))
      
      for(tt in 1:length(Years)){
        alpha_list_empi[[tt]] <- get_tail_exponents(datavector = DF[[myvar]][which(DF$year == Years[tt])], 
                                                    mywd = mywd,
                                                    addnoise = T,
                                                    COMMAND = python_path)
      }
      
      # store everything in a dataframe
      mydf <- data.frame(
        "year" = Years,
        "CNS_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["CNS"]]})),
        "CNS_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["CNS_kstar"]]})),
        "AdjHill_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["AdjHill"]]})),
        "AdjHill_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["AdjHill_kstar"]]})),
        "Moms_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["Moms"]]})),
        "Moms_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["Mom_kstar"]]})),
        "Kern_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["Kern"]]})),
        "Kern_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["Ker_kstar"]]})))
      
      #---------------------------------------------------------------------------
      # Export
      #---------------------------------------------------------------------------
      filename <- file.path(dirOutput, countryname[dd], paste0(countryname[dd], "_tails_", myvar, ".csv"))
      write.csv(mydf, file = filename, row.names = F)
      
    }
    
    print(paste0("Tail exponents for ", countryname[dd], " exported"))
    
  }
}
