################################################################################
# Functions that simulate random graphs, and compute clustering coefficients and path lengths

# NETWORK PROPERTIES:
# 1. global clustering coefficient using the configuration model
# 2. average local clustering coefficients using the configuration model
# 3. path lengths using the Erdos-Renyi and configuration model

################################################################################

# ------------------------------------------------------------------------------
# Global and local clustering
# ------------------------------------------------------------------------------

#' Simulates a random graph using the configuration model.
#' Calculates the global and local clustering coefficients for the random graph.

#' @param df_degrees A data.table with the degrees 
#' @param yr A scalar with the year for which we are running the analysis
#' @param sim_number A scalar indicating the number of the simulation

#' @return df A data.table with the results


clustering_simulation <- function(df_degrees, yr, sim_number){
  
  # Step 1: Get the in-degree and out-degree sequences
  in_degree_sequence <- df_degrees$inD
  out_degree_sequence <- df_degrees$outD

  # Step 2: Generate configuration model based on the in-degree and out-degree sequences
  g_random <- sample_degseq(out.deg = out_degree_sequence, in.deg = in_degree_sequence, method = "simple")  
  
  # Step 3: Collapse the network into an undirected network
  g_undirected <- as.undirected(g_random, mode = "collapse")
  
  # get clustering coeffs
  clustering_global_rand <- transitivity(g_undirected, type = "globalundirected")
  clustering_local_rand <- transitivity(g_undirected, type = "localaverageundirected")
  
  # build df with outputs
  df <- data.table(year = yr,
                   sim = sim_number,
                   global_clust_sim = clustering_global_rand,
                   average_local_clust_sim = clustering_local_rand)
  return(df)
}


#' Calculates the global and local clustering coefficients for nSim random graphs (we use the configuration model).

#' @param df_degrees A data.table with the degrees 
#' @param nSim A scalar with the number of random graphs to simulate
#' @param clustering_simulation A function simulating the random graphs and calculating the clustering coefficients

#' @return df_save A data.table with the results, e.i. the mean and standard deviation of the clustering coefficients over the nSim simulations 
 

clustering_sim_yearly_parallel <- function(df_edgelist, nSim, clustering_simulation){
  
  years <- sort(unique(df_edgelist$year))
  # df where to store the results of the simulations
  df_simulations <- data.table(year = NA,
                               sim = NA,
                               global_clust_sim = NA,
                               average_local_clust_sim = NA)
  
  for(yr in years){
    
    df_results_par <- foreach(i=1:nSim, .combine=rbind, .packages=c('data.table', 'igraph')) %dopar%{
      set.seed(8675309+i)
      args <- list(df_edgelist[year == yr], yr, i)
      df_ <- do.call(clustering_simulation, args)
    }
    
    df_simulations <- rbind(df_simulations, df_results_par)
  }
  
  # remove rows with all NaNs
  df_simulations <- df_simulations[rowSums(is.na(df_simulations)) != ncol(df_simulations), ]
  # get mean and standard deviation of the simulations
  df_save <- data.table(year = years)
  # gloabl clustering
  df_merge <- df_simulations[, mean(global_clust_sim), by = year]
  colnames(df_merge) <- c('year', 'mean_clustering_global_config_model')
  df_save <- merge(df_save, df_merge, by = 'year')
  df_merge <- df_simulations[, sd(global_clust_sim), by = year]
  colnames(df_merge) <- c('year', 'std_clustering_global_config_model')
  df_save <- merge(df_save, df_merge, by = 'year')
  # local clustering
  df_merge <- df_simulations[, mean(average_local_clust_sim), by = year]
  colnames(df_merge) <- c('year', 'mean_clustering_local_config_model')
  df_save <- merge(df_save, df_merge, by = 'year')
  df_merge <- df_simulations[, sd(average_local_clust_sim), by = year]
  colnames(df_merge) <- c('year', 'std_clustering_local_config_model')
  df_save <- merge(df_save, df_merge, by = 'year')
  
  return(df_save)
  
}


# ------------------------------------------------------------------------------
# PATH LENGTHS
# ------------------------------------------------------------------------------

## FUNCTION FOR ALL ##

#' Simulates random graphs: we use the configuration model and Erdos-Renyi.

#' @param degree_seq A data.table with the in- and out-degrees 
#' @param number_nodes A scalar with the number of nodes in the network
#' @param density_net A scalar indicating the density of the network
#' @param both_models True or False, default T, so it simulates an Erdos-Renyi and a configuration model. 
#'                    If F, only Erdos-Renyi.

#' @return A list with the Erdos-Renyi random graph and the configuration model if both_models = T, if both_models = F only Erdos-Renyi


path_length_random_graphs <- function(degree_seq, number_nodes, density_net, both_models = T){
  
  if(both_models == T){
    
    ## CONFIGURATIO MODEL ##
    in_degree_seq <- degree_seq$inD
    out_degree_seq <- degree_seq$outD

    g_c <- sample_degseq(method = "simple",
                         out.deg = out_degree_seq,
                         in.deg = in_degree_seq)
    g_c <- as.undirected(g_c, mode = "collapse")
    # get giant component
    comp <- components(g_c)
    g_c <- induced_subgraph(g_c, which(comp$membership == which.max(comp$csize)))
  }
  
  ## ER GRAPH ##
  g_er <- sample_gnp(number_nodes, density_net, directed = F, loops = F)
  # get giant component
  comp <- components(g_er)
  g_er <- induced_subgraph(g_er, which(comp$membership == which.max(comp$csize)))
 
  if(both_models == T){
    return(list('er_model' = g_er, 'config_model' = g_c))
  }else{
    return(list('er_model' = g_er))
  }
  
}


## AUXILIARY FUNCTION ##

#' Calculate the distance between 2 nodes 

#' @param random_graph A graph, igraph object 
#' @param nPairs A scalar indicating the number of node pairs to sample

#' @return A scalar giving the average shortest path


path_length_sample_pairs <- function(random_graph, nPairs){
  
  # collect path lengths of sampled node pairs
  dist_sim_i <- c(rep(NA, nPairs))
  
  for(k in 1:nPairs){
    # sample pair
    sampled_nodes <- sample(as_ids(V(random_graph)), 2, replace = F)
    # compute distance
    dist_sim_i[k] <- distances(random_graph, v = sampled_nodes[1], to = sampled_nodes[2],
                               weights = NULL, algorithm = "unweighted")[1, 1]
  }
  # get average shortest path for sim i
  return(mean(dist_sim_i, na.rm = T))
}


## MAIN FUNCTION ##

#' Calculates the path length for nSim random graphs.
#' For each random graph, we sample nPairs at random and calculate their shortest path.
#' We then take the average over the nPairs, for each simulated random graph.
#' random graphs used:
#' - Erdos-Renyi and configuration model for Hungary, Ecuaodr and FactSet
#' - Erdos-Renyi for Japan 2005 and 2006

#' @param df_degrees A data.table with the degrees 
#' @param df_nodes_edges A data.table with the number of nodes and edges for different networks
#' @param nSim A scalar with the number of random graphs to simulate
#' @param nPairs A scalar indicating the number of node pairs to sample
#' @param path_length_random_graphs A function simulating the random graphs
#' @param path_length_sample_pairs A function calculating the distance between 2 nodes
#' 
#' @return df_save A data.table with the results for each random graph and each simulation (mean over pairs)


path_length_sim <- function(df_degrees, df_nodes_edges, nSim, nPairs, path_length_random_graphs, path_length_sample_pairs){
  
  countries <- unique(df_nodes_edges$country)
  
  df_save <- foreach(c = 1:length(countries), .combine = rbind, .packages = c('data.table', 'igraph')) %dopar% {
    
    country_ <- countries[c]
    set.seed(8675309+c)
    density_net <- df_nodes_edges[country == country_, ]$density
    number_nodes <- df_nodes_edges[country == country_, ]$Nnodes
    df_output <- data.table(country = NA,
                            sim = NA,
                            av_short_path = NA,
                            model = NA)
    
    if(country_ != "Japan_05" & country_ != "Japan_06"){
      
      for(i in 1:nSim){
        # this is only here because no config. model for japan
        degree_seq <- df_degrees[country == country_, ]

        #----------------------------
        # 1) Create random graphs
        args <- list(degree_seq, number_nodes, density_net)
        list_graphs <- do.call(path_length_random_graphs, args)
        g_er <- list_graphs$er_model
        g_c <- list_graphs$config_model

        #-----------------------------
        # 2) ER model, average path length
        args <- list(g_er, nPairs)
        average_path_length <- do.call(path_length_sample_pairs, args)

        # store in data frame
        df_output_sim_i_er <- data.table(country = country_,
                                         sim = i,
                                         av_short_path = average_path_length,
                                         model = 'ER')
        #-------------------------------------------
        # 3) CONFIGURATION model, average shortest path
        args <- list(g_c, nPairs)
        average_path_length <- do.call(path_length_sample_pairs, args)

        # store in data frame
        df_output_sim_i_c <- data.table(country = country_,
                                        sim = i,
                                        av_short_path = average_path_length,
                                        model = 'Config')
        # Add results to main df
        df_output <- data.table(rbind(df_output, df_output_sim_i_er, df_output_sim_i_c))
      }

    }else{
      for(i in 1:nSim){

        #----------------------------
        # 1) Create random graphs
        args <- list(0, number_nodes, density_net, both_models = F)
        list_graphs <- do.call(path_length_random_graphs, args)
        g_er <- list_graphs$er_model

        #-----------------------------
        # 2) ER model, average path length
        args <- list(g_er, nPairs)
        average_path_length <- do.call(path_length_sample_pairs, args)

        # store in data frame
        df_output_sim_i_er <- data.table(country = country_,
                                         sim = i,
                                         av_short_path = average_path_length,
                                         model = 'ER')
        # Add results to main df
        df_output <- data.table(rbind(df_output, df_output_sim_i_er))
      }
    }
    return(df_output)
  }
  # delete row with all NaNs
  df_save <- df_save[rowSums(is.na(df_save)) != dim(df_save)[2], ]
  return(df_save)
}
