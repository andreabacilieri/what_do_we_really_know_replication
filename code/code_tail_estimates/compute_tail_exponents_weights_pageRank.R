##########################################################################################################################

# Computes tail exponents for the weights and influence vector of the input-output network

# INPUTS:
# Data:
# from data/analysis/
#     - hungary/hungary_weights_input_output_shares.csv
#     - hungary/hungary_influence_vector.csv
#     - ecuador/ecuador_weights_input_output_shares.csv
#     - ecuador/ecuador_influence_vector.csv
# from code/code_tail_estimates/utils/
#     - compute_tail_exponents_fun.R

# OUTPUT:
# to data/analysis/
#   - ecuador/ecuador_tails_*.csv
#   - hungary/hungary_tails_*.csv
# * in {weights, influence}

#######################################################################################################################

compute_tail_exponents_weights_pageRank <- function(python_path){
  
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
  # Get directory of the input data (outside of the project)
  dirdata <- file.path(rootfolder, 'data', 'analysis')
  dir_fun_utils <- file.path(rootfolder, 'code', 'code_tail_estimates', 'utils')
  
  # Function to compute tail exponents
  source(file.path(dir_fun_utils,'fun_compute_tail_exponents.R'))
  # Get working directory for python call (needs to use path.expand because this gets passed to the terminal to run the python code)
  mywd <- path.expand(dir_fun_utils)
  
  countryname <- c("ecuador", "hungary")
  
  
  # ------------------------------------------------------------------------------
  #  Weights
  # ------------------------------------------------------------------------------
  
  ## LOAD DATA ## 
  df_h <- fread(file.path(dirdata, 'hungary', 'hungary_weights_input_output_shares.csv'))
  df_e <- fread(file.path(dirdata, 'ecuador', 'ecuador_weights_input_output_shares.csv'))
  DFlist <- list(df_e, df_h)
  rm(df_e, df_h)
  
  ##  EMPIRICAL EXPONENTS ##
  t0 <- Sys.time()
  
  for(dd in 1:length(DFlist)){
    
    # Get right variable from right dataset
    DF <- DFlist[[dd]]
    # For weights, keep only the observations greater than 10,000 to speed up calculations
    DF <- DF[which(DF$weight > 10^4),]
    
    # Tail exponents
    alpha_list_empi <- list()
    Years <- sort(unique(DF$year))
    nn <- rep(NA, length(Years))
    
    for(tt in 1:length(Years)){
      
      alpha_list_empi[[tt]] <- get_tail_exponents(datavector = DF[["weight"]][which(DF$year == Years[tt])], 
                                                  mywd = mywd,
                                                  addnoise = F,
                                                  COMMAND = python_path)
    }
    
    # Store everything in a dataframe
    mydf <- data.frame(
      "year" = Years,
      "CNS_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["CNS"]]})),
      "CNS_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["CNS_kstar"]]})),
      "AdjHill_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["AdjHill"]]})),
      "AdjHill_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["AdjHill_kstar"]]})),
      "Moms_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["Moms"]]})),
      "Moms_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["Mom_kstar"]]})),
      "Kern_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["Kern"]]})),
      "Kern_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["Ker_kstar"]]}))
    )
    
    ## EXPORT ##
    filename <- file.path(dirOutput, countryname[dd], paste0(countryname[dd], "_tails_weights.csv"))
    write.csv(mydf, file = filename, row.names = F)
    
    print(paste0("Tail exponents for Weights in ", countryname[dd]," exported"))
  }
  
  
  # ------------------------------------------------------------------------------
  #  Influence vector
  # ------------------------------------------------------------------------------
  
  ## LOAD DATA ## 
  df_h <- fread(file.path(dirdata, 'hungary', 'hungary_influence_vector.csv'))
  df_e <- fread(file.path(dirdata, 'ecuador', 'ecuador_influence_vector.csv'))
  DFlist <- list(df_e, df_h)
  rm(df_e, df_h)
  
  ##  EMPIRICAL EXPONENTS ##
  for(dd in 1:length(DFlist)){
    
    # Get right variable from right dataset
    DF <- DFlist[[dd]]
    
    # Tail exponents for each year
    alpha_list_empi <- list()
    Years <- sort(unique(DF$year))
    nn <- rep(NA, length(Years))
    
    for(tt in 1:length(Years)){
      # NOTE: - we use pagerank to compute the influence vector
      #       - we rescale PageRank values to have a mean of 1
      #         to avoid numerical issues causing a crash of tailestimates.py
      datavec <- DF[["pageRank"]][which(DF$year == Years[tt])]
      datavec <- 100 * datavec/mean(datavec)
      alpha_list_empi[[tt]] <- get_tail_exponents(datavector = datavec,
                                                  mywd = mywd,
                                                  addnoise = F,
                                                  COMMAND = python_path)
    }
    
    # Store everything in a dataframe
    mydf <- data.frame(
      "year" = Years,
      "CNS_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["CNS"]]})),
      "CNS_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["CNS_kstar"]]})),
      "AdjHill_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["AdjHill"]]})),
      "AdjHill_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["AdjHill_kstar"]]})),
      "Moms_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["Moms"]]})),
      "Moms_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["Mom_kstar"]]})),
      "Kern_alpha" = unlist(lapply(alpha_list_empi, function(x){x[["Kern"]]})),
      "Kern_kstar" = unlist(lapply(alpha_list_empi, function(x){x[["Ker_kstar"]]}))
    )
    
    ## EXPORT ##
    filename <- file.path(dirOutput, countryname[dd], paste0(countryname[dd], "_tails_influence.csv"))
    write.csv(mydf, file = filename, row.names = F)
    
    print(paste0("Tail exponents for Influence Vector in ", countryname[dd], " exported"))
    
  }
}
  