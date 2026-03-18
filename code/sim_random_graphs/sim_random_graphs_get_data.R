################################################################################
# This script simulates random graphs, and computes clustering coefficients and path lengths.

# INPUTS:
# from data/analysis/
#     - */*_local_properties.csv
#     - */*_global_properties.csv
#     - literature_network_properties.csv
# from code/sim_random_graphs/utils/
#     - fun_clustering_paths_random_networks.R

# Path length is computed only for these:
# Ecuador: 2015
# Hungary: 2015, 2019, 2021
# FactSet: 2021

# OUTPUT:
# to data/analysis/
#     - */*_global_local_clustering_config_model.csv
#     - */*_path_length_random_graphs.csv
#     - literature/literature_path_length_random_graphs.csv

# * {ecuador, hungary, factset}

################################################################################


#' Simulates random graphs

#' @param nSim A scalar with the number of random graphs to simulate for calculating the clustering coeff
#' @param nSim_pl A scalar with the number of random graphs to simulate for calculating the path lengths
#' @param nPairs A scalar indicating the number of pairs to sample to calculate the shortest paths

#' @return NONE, but saves csv files with the results during the computations


simulate_random_graphs <- function(nSim, nSim_pl, nPairs) {

  # ----------------------------------------------------------------------------
  #  Set environment/parameters
  # ----------------------------------------------------------------------------

  rm(list = setdiff(ls(), c("nSim", "nSim_pl", "nPairs")))
  library(igraph)
  library(data.table)
  library(foreach)
  library(parallel)
  library(doSNOW)
  library(dplyr)
  library(stringr)

  # Rootfolder
  rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
  # Folder to store output
  dirOutput <- file.path(rootfolder, 'data', 'analysis')
  # Folder for the input data
  dirdata <- file.path(rootfolder, 'data')

  # Load functions
  fun_rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
  source(file.path(fun_rootfolder, 'code', 'sim_random_graphs', 'utils', 'fun_clustering_paths_random_networks.R'))
  
  objects_to_keep <- ls()
  

  # ----------------------------------------------------------------------------
  #  Clustering coefficients
  # ----------------------------------------------------------------------------
  print('Simulating random graphs and calculating clustering coefficients...')

  # set up parallel computing
  ncores <- detectCores()
  cl <- makeCluster(ncores)
  registerDoSNOW(cl)

  # export necessary functions to the parallel workers
  clusterExport(cl, c("clustering_simulation"))
  
  ## FACTSET ##
  print('Running simulations for FactSet')
  start_time <- Sys.time()

  filename <- file.path(dirdata, 'analysis', 'factset', 'factset_local_properties.csv')
  df <- data.table(fread(filename))
  df_save <- clustering_sim_yearly_parallel(df, nSim, clustering_simulation)
  csv_name <- file.path(dirOutput, 'factset', 'factset_global_local_clustering_config_model.csv')
  write.csv(df_save, csv_name, row.names = F)

  end_time <- Sys.time()
  difference_ <- difftime(end_time, start_time, units = 'hour')
  print(paste('Done. Elapsed time (hour): ', difference_))

  ## ECUADOR ##
  print('Running simulations for Ecuador')
  start_time <- Sys.time()

  filename <- file.path(dirdata, 'analysis', 'ecuador', 'ecuador_local_properties.csv')
  df <- data.table(fread(filename))
  df_save <- clustering_sim_yearly_parallel(df, nSim, clustering_simulation)
  csv_name <- file.path(dirOutput, 'ecuador', 'ecuador_global_local_clustering_config_model.csv')
  write.csv(df_save, csv_name, row.names = F)

  end_time <- Sys.time()
  difference_ <- difftime(end_time, start_time, units = 'hour')
  print(paste('Done. Elapsed time (hour): ', difference_))

  ## HUNGARY ##
  print('Running simulations for Hungary')
  start_time <- Sys.time()

  filename <- file.path(dirdata, 'analysis', 'hungary', 'hungary_local_properties.csv')
  df <- data.table(fread(filename))
  df_save <- clustering_sim_yearly_parallel(df, nSim, clustering_simulation)
  csv_name <- file.path(dirOutput, 'hungary', 'hungary_global_local_clustering_config_model.csv')
  write.csv(df_save, csv_name, row.names = F)

  end_time <- Sys.time()
  difference_ <- difftime(end_time, start_time, units = 'hour')
  print(paste('Done. Elapsed time (hour): ', difference_))

  stopCluster(cl)
  rm(list = setdiff(ls(), objects_to_keep))

  
  # ----------------------------------------------------------------------------
  # Path lengths for Ecuador, FactSet, Hungary and literature
  # ----------------------------------------------------------------------------
  
  print('Simulating random graphs and calculating path lengths...')
  start_time <- Sys.time()

  ## Prepare df with degrees ##
  
  # FactSet
  filename <- file.path(dirdata,'analysis','factset', 'factset_local_properties.csv')
  df_deg <- fread(filename)
  df_deg <- df_deg[, c('deg', 'outD', 'inD', 'year')]
  df_deg$country <- 'factset'
  df_deg <- df_deg[year == 2021]

  # Ecuador
  filename <- file.path(dirdata, 'analysis', 'ecuador', 'ecuador_local_properties.csv')
  df_ <- fread(filename)
  df_ <- df_[, c('deg', 'outD', 'inD', 'year')]
  df_$country <- 'ecuador'
  df_ <- df_[year == 2015]
  df_deg <- rbind(df_deg, df_)
  df_deg <- data.table(df_deg)
  rm('df_')

  # Hungary
  filename <- file.path(dirdata, 'analysis', 'hungary', 'hungary_local_properties.csv')
  df_ <- fread(filename)
  df_ <- df_[, c('deg', 'outD', 'inD', 'year')]
  df_ <- df_[year %in% c(2015, 2019, 2021)]
  df_ <- df_ %>%
    mutate(country = case_when(
      year == 2015 ~ "hungary_15",
      year == 2019 ~ "hungary_19",
      year == 2021 ~ "hungary_21",
    ))
  df_deg <- rbind(df_deg, df_)
  df_deg <- data.table(df_deg)
  rm('df_')

  ## Prepare df with number of nodes and density ##
  
  # FactSet
  filename <- file.path(dirdata, 'analysis', 'factset', 'factset_global_properties.csv')
  df_nodes_edges <- data.table(read.csv(filename))
  df_nodes_edges <- df_nodes_edges[, c('Nnodes', 'Nedges', 'year')]
  df_nodes_edges <- df_nodes_edges[year == 2021]
  df_nodes_edges$country <- 'factset'

  # Ecuador
  filename <- file.path(dirdata, 'analysis', 'ecuador', 'ecuador_global_properties.csv')
  df_ <- data.table(read.csv(filename))
  df_ <- df_[, c('Nnodes', 'Nedges', 'year')]
  df_ <- df_[year == 2015]
  df_$country <- 'ecuador'
  df_nodes_edges <- rbind(df_nodes_edges, df_)
  rm('df_')

  # Literature
  filename <- file.path(dirdata, 'literature', 'literature_network_properties.csv')
  df_ <- fread(filename)
  df_ <- df_[ER_sim_yesorno == 1,]
  df_ <- df_[, list(Dataset, Years, N, E)]
  colnames(df_) <- c('country', 'year', 'Nnodes', 'Nedges')
  df_[year == 2005]$country <- "Japan_05"
  df_[year == 2006]$country <- "Japan_06"
  df_[, Nedges := as.numeric(gsub("\\.", "", Nedges))]
  df_[, Nnodes := as.numeric(gsub("\\.", "", Nnodes))]
  setcolorder(df_, colnames(df_nodes_edges))
  df_nodes_edges <- rbind(df_nodes_edges, df_)
  rm('df_')

  # Hungary
  filename <- file.path(dirdata, 'analysis', 'hungary', 'hungary_global_properties.csv')
  df_ <- fread(filename)
  df_ <- df_[year %in% c(2015, 2019, 2021)]
  df_ <- df_[, c('Nnodes', 'Nedges', 'year')]
  df_ <- df_ %>%
    mutate(country = case_when(
      year == 2015 ~ "hungary_15",
      year == 2019 ~ "hungary_19",
      year == 2021 ~ "hungary_21",
    ))
  df_nodes_edges <- rbind(df_nodes_edges, df_)
  rm('df_')

  ## Calculate density ##
  df_nodes_edges$density <- 2*(df_nodes_edges$Nedges / ((df_nodes_edges$Nnodes - 1) * df_nodes_edges$Nnodes))
  df_nodes_edges <- data.table(df_nodes_edges)

  # Set up parallel computing
  ncores <- detectCores()
  cl <- makeCluster(ncores)
  registerDoSNOW(cl)

  # Export necessary functions to the parallel workers
  clusterExport(cl, c("path_length_sim"))

  df_save <- path_length_sim(df_deg, df_nodes_edges, nSim_pl, nPairs, path_length_random_graphs, path_length_sample_pairs)
  df_save <- df_save %>%
    mutate(year = case_when(
      country == "factset" ~ "2021", 
      country == "ecuador" ~ "2015",
      country == "hungary_15" ~ "2015",
      country == 'hungary_19' ~ "2019",
      country == 'hungary_21' ~ "2021",
      country == 'Japan_05' ~ "2005",
      country == 'Japan_06' ~ "2006"))
  
  df_save <- df_save %>%
    mutate(country = ifelse(str_detect(country, "hungary"), "hungary", country))

  dataset ='factset'
  df_ <- df_save[country == dataset, ]
  csv_name <- file.path(dirOutput, dataset, paste0(dataset, '_path_length_random_graphs.csv'))
  write.csv(df_, csv_name, row.names = F)

  dataset ='ecuador'
  df_ <- df_save[country == dataset, ]
  csv_name <- file.path(dirOutput, dataset, paste0(dataset, '_path_length_random_graphs.csv'))
  write.csv(df_, csv_name, row.names = F)

  dataset ='hungary'
  df_ <- df_save[country == dataset, ]
  csv_name <- file.path(dirOutput, dataset, paste0(dataset, '_path_length_random_graphs.csv'))
  write.csv(df_, csv_name, row.names = F)

  dataset = c('Japan_05', 'Japan_06')
  df_ <- df_save[df_save$country %in% dataset, ]
  csv_name <- file.path(dirdata, 'literature', 'literature_path_length_random_graphs.csv')
  write.csv(df_, csv_name, row.names = F)

  end_time <- Sys.time()
  difference_ <- difftime(end_time, start_time, units = 'hour')
  print(paste('Done. Elapsed time (hour): ', difference_))
  # stop the cluster
  stopCluster(cl) 
}
