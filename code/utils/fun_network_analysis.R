##################################################################################################################
# Functions performing the network analysis for the empirical datasets

# NETWORK PROPERTIES:
# 1. global properties: number of nodes, number of edges, average path length and various assortativity measures
# 2. degrees, strengths
# 3. local and global clustering
# 4. reciprocity
# 5. paths
# 6. weights
# 7. input and output shares
# 8. influence vector
##################################################################################################################


# --------------------------------------------------------------------------------
#  Global properties: size (x2), assortativity (x5), reciprocity, transitivity
# --------------------------------------------------------------------------------

#' Calculates global properties
#'  - n. nodes and edges
#'  - undirected and directed assortativity
#'  - reciprocity
#'  - global clustering (transitivity)

#' @param df_edgelist A data.table with the edge list (supplier, customer, weight (if any), year)
#' @param yr A scalar with the year for which we are running the analysis
#' @param weight Either TRUE or FALSE; default is T.
#'               In this function the "weight" argument would not be necessary but it is kept
#'               so that function "build_yearly_csv_parallel", which calls it, runs smoothly

#' @return df_results_par A data.table with the results


global_properties <- function(df_edgelist, yr, weight = T){
  # construct graph
  G <- graph_from_data_frame(df_edgelist[, c("supplier", "customer")], directed = T)

  # build df of global properties
  df_ <- data.table(
          Nnodes = vcount(G),
          Nedges = ecount(G),
          undirected_assort = assortativity_degree(G, directed = F),
          assort_out_in  = assortativity(G, types1 = degree(G, mode = "out"), types2 = degree(G, mode = "in"),  directed = T),
          assort_in_out  = assortativity(G, types1 = degree(G, mode = "in"),  types2 = degree(G, mode = "out"), directed = T),
          assort_out_out = assortativity(G, types1 = degree(G, mode = "out"), types2 = degree(G, mode = "out"), directed = T),
          assort_in_in   = assortativity(G, types1 = degree(G, mode = "in"),  types2 = degree(G, mode = "in"),  directed = T),
          reciprocity_emp = reciprocity(G),
          clustering_global_emp = transitivity(G, type = 'global'),
          year = yr)

  return(df_)
}


# ------------------------------------------------
#  Local properties
# ------------------------------------------------

#' Calculates local properties
#'  - in- and out-degrees, and degrees in the undirected network
#'  - in- and out-strengths
#'  - clustering
#'  Adds industry information for each firm, if requested.

#' @param df_edgelist A data.table with the edge list (supplier, customer, weight (if any), year)
#' @param yr A scalar with the year for which we are running the analysis
#' @param weight Either TRUE or FALSE; default is T. If T, calculates in- and out-strengths, otherwise not
#' @param df_industry A data.table with the industry information, columns need to be c('id', 'indu_code', indu_descr')

#' @return df_results_par A data.table with the results
 
 
local_properties <- function(df_edgelist, yr, weight = T, df_industry = NULL){

  if(weight == T){
    G <- graph_from_data_frame(df_edgelist[, c("supplier", "customer", "weight")], directed = T)
    # Get degrees, strengths, clustering
    df_ <- data.table(
      id = V(G)$name,
      deg  = degree(as.undirected(G, mode = 'collapse')),
      outD = degree(G, mode = 'out'),
      inD  = degree(G, mode = 'in'),
      outS = strength(G, mode = 'out'),
      inS  = strength(G, mode = 'in'),
      clustering = transitivity(as.undirected(G, 'collapse'), type = 'local'),
      year = yr)
  }else{
    G <- graph_from_data_frame(df_edgelist[, c("supplier", "customer")], directed = T)
    # Get degrees, clustering
    df_ <- data.table(
      id = as.character(V(G)$name),
      deg  = degree(as.undirected(G, mode = 'collapse')),
      outD = degree(G, mode = 'out'),
      inD  = degree(G, mode = 'in'),
      clustering = transitivity(as.undirected(G, 'collapse'), type = 'local'),
      year = yr)
  }
  
  # Add industry information when required
  if(!is.null(df_industry)){
    df_industry <- df_industry[id %in% df_$id,]
    df_industry[, id := as.character(id)]
    df_ <- merge(df_industry, df_, by = 'id', all = T)
    rm(df_industry)
    # Remove firms' ids
    df_$id <- NULL
  }
  return(df_)
}


# ------------------------------------------------
#  Paths: full, and sampling only some pairs
# ------------------------------------------------

## FULL NETWORK ##

#' Calculates the shortest paths for the entire network

#' @param df_edgelist A data.table with the edge list (supplier, customer, weight (if any), year)
#' @param yr A scalar with the year for which we are running the analysis
#' @param weight Either TRUE or FALSE; default is T. 
#'               In this function the "weight" argument is not necessary but it is kept
#'               so that function "build_yearly_csv_parallel", which calls it, runs smoothly

#' @return df_results_par A data.table with the results as a frequency table
 
 
shortest_paths <- function(df_edgelist, yr, weight = T){
  # construct graph
  if(weight == T){
    G <- graph_from_data_frame(df_edgelist[, c("supplier", "customer", "weight")], directed = T)
  }else{
    G <- graph_from_data_frame(df_edgelist[, c("supplier", "customer")], directed = T)
  }
  
  # paths
  shrt_paths = distance_table(as.undirected(G, mode = 'collapse'))
  shrt_p = seq_along(shrt_paths$res)
  freq_ = shrt_paths$res/sum(shrt_paths$res)
  df_ <- data.table(shrt_path = shrt_p,
                    frequency = freq_,
                    year = yr)
  return(df_)
}



## SAMPLE THE NETWORK ##

#' Calculates the shortest paths only for nPairs of nodes

#' @param df_edgelist A data.table with the edgelist (supplier, customer, weight (if any), year)
#' @param yr A scalar with the year for which we are running the analysis
#' @param nPairs A scalar indicating the number of pairs to sample

#' @return df_results_par A data.table with the results as a frequency table


shortest_paths_sample <- function(df_edgelist, years, nPairs){

  start_time_all <- Sys.time()

  # initialise df
  df_paths <- data.table(shrt_path = NA,
                         sim_number = NA,
                         year = NA)

  for (yr in years) {

    start_time <- Sys.time()
    print(paste('Computing path length for year:', yr))

    # get unweighted, undirected graph
    G <- graph_from_data_frame(df_edgelist[year == yr, c("supplier", "customer")], directed = T)
    G <- as.undirected(G, mode = 'collapse')
    
    # for each year, the node pair sampling is parallel
    packages_ = c('data.table', 'igraph')
    
    df_paths_onesim <- foreach(i = 1:nPairs, .combine = rbind, .packages = packages_) %dopar%{

      set.seed(8675309 + i)
      # sample pair of nodes
      sampled_nodes <- sample(as_ids(V(G)), 2, replace = F)
      # compute distance
      dist <- distances(G, v = sampled_nodes[1], to = sampled_nodes[2],
                        weights = NULL, algorithm = "unweighted")[1, 1]
      # store results
      df_ <- data.table(shrt_path = dist,
                        sim_number = i)
      return(df_)
    }

    df_paths_onesim$year <- yr
    df_paths <- rbind(df_paths, df_paths_onesim)

    end_time <- Sys.time()
    difference_ <- difftime(end_time, start_time, units = 'hour')
    print(paste('Elapsed time (hour): ', difference_))
  }

  end_time <- Sys.time()
  difference_ <- difftime(end_time, start_time_all, units = 'hour')
  print(paste('Elapsed time for all years (hour): ', difference_))

  # get frequency
  df_paths <- na.omit(df_paths)
  df_paths <- df_paths[, c('year', 'shrt_path')]
  # get cumulative number of obs path per path length
  df_merge <- df_paths[, .(.N), by = .(year, shrt_path)]
  colnames(df_merge) <- c('year', 'shrt_path', 'frequency')
  df_merge$frequency <- as.numeric(df_merge$frequency)
  for (yr in years) {
    normalisation <- sum(df_merge[year == yr]$frequency)
    df_merge[year == yr, frequency := df_merge[year == yr]$frequency/normalisation]

  }

  return(df_merge)

}

#' Calculates the shortest paths only for nPairs of nodes for the truncated network

#' @param df_edgelist A data.table with the edgelist (supplier, customer, weight (if any), year)
#' @param yr A scalar with the year for which we are running the analysis
#' @param nPairs A scalar indicating the number of pairs to sample

#' @return df_results_par A data.table with the results as a frequency table


shortest_paths_sample_trunc <- function(df_edgelist, yr, nPairs){

  # initialise df
  df_paths <- data.table(shrt_path = NA,
                         sim_number = NA)

  # get unweighted, undirected graph
  G <- graph_from_data_frame(df_edgelist[c("supplier", "customer")], directed = T)
  G <- as.undirected(G, mode = 'collapse')
  
  # for each year, the node pair sampling is parallel
  packages_ = c('data.table', 'igraph')
  
  df_paths <- foreach(i = 1:nPairs, .combine = rbind, .packages = packages_) %dopar%{

    set.seed(8675309 + i)
    # sample pair of nodes
    sampled_nodes <- sample(as_ids(V(G)), 2, replace = F)
    # compute distance
    dist <- distances(G, v = sampled_nodes[1], to = sampled_nodes[2],
                      weights = NULL, algorithm = "unweighted")[1, 1]
    # store results
    df_ <- data.table(shrt_path = dist,
                      sim_number = i)
    return(df_)
  }

  # get frequency
  df_paths <- na.omit(df_paths)
  df_paths <- df_paths[, c('shrt_path')]
  # get cumulative number of obs path per path length
  df_merge <- df_paths[, .(.N), by = .(shrt_path)]
  colnames(df_merge) <- c('shrt_path', 'frequency')
  df_merge$frequency <- as.numeric(df_merge$frequency)
  normalisation <- sum(df_merge$frequency)
  df_merge[, frequency := frequency / normalisation]

  return(df_merge)

}


# ------------------------------------------------
#  Weights, input and output shares
# ------------------------------------------------

#' Calculates the weights, and input and output shares

#' @param df_edgelist A data.table with the edgelist (supplier, customer, weight (if any), year)
#' @param yr A scalar with the year for which we are running the analysis
#' @param df_industry A data.table with the industry information, columns need to be c('id', 'indu_code', indu_descr')

#' @return df_results_par A data.tablee with the results


weights_input_output_shares <- function(df_edgelist, yr, df_industry = NULL){

  # Construct graph
  df_edgelist <- df_edgelist[, c("supplier", "customer", "weight")]
  df_edgelist$supplier <- as.character(df_edgelist$supplier)
  df_edgelist$customer <- as.character(df_edgelist$customer)
  G <- graph_from_data_frame(df_edgelist, directed = T)

  # Construct df
  df_ <- data.table(
    id = as.character(V(G)$name),
    weight = E(G)$weight,
    # weight as a share of the network expenses (in-strength) of the customer ("to")
    input_share = E(G)$weight / strength(G, mode = "in")[df_edgelist$customer],
    # weight as a share of the network sales (out-strength) of the supplier ("from")
    output_share = E(G)$weight / strength(G, mode = "out")[df_edgelist$supplier],
    year = yr)
  
  # Add industry information when required
  if(!is.null(df_industry)){
    df_industry <- df_industry[id %in% df_$id,]
    df_industry[, id := as.character(id)]
    df_ <- merge(df_industry, df_, by = 'id', all = T)
    rm(df_industry)
    # Remove firms' ids
    df_$id <- NULL
  }

  return(df_)
}


# ------------------------------------------------
#  Influence vector
# ------------------------------------------------

#' Calculates the influence vector

#' @param df_edgelist A data.table with the edgelist (supplier, customer, weight (if any), year)
#' @param yr A scalar indicating the year for which we are running the analysis
#' @param labour_share A scalar defining the labour share, default is 0.5
#' @param df_industry A data.table with the industry information, columns need to be c('id', 'indu_code', indu_descr')

#' @return df_results_par A data.table with the results

influence_vector <- function(df_edgelist, yr, labour_share = 0.5, df_industry = NULL){
  # Build graph
  df_edgelist <- df_edgelist[, c("supplier", "customer", "weight")]
  colnames(df_edgelist) <- c('from', 'to', 'weight')
  
  # NOTE: iGraph's PageRank function gives the influence vector
  #       if it is applied to the *transposed* network
  #       i.e., customer, supplier, weight
  df_edgelist <- setcolorder(df_edgelist, c("to", "from", "weight"))
  G <- graph_from_data_frame(df_edgelist, directed = T)
  # Set "labour share"
  alpha_ <- labour_share
  # Set dampening in PageRank
  d <- 1 - alpha_
  # Compute PageRank
  v_pgr <- page_rank(G, directed = T, damping = d)
  
  df_ <- data.table(id = as.character(V(G)$name),
                    pageRank = v_pgr$vector,
                    year = as.numeric(yr))
  
  # Add industry information when required
  if(!is.null(df_industry)){
    df_industry <- df_industry[id %in% df_$id,]
    df_industry[, id := as.character(id)]
    df_ <- merge(df_industry, df_, by = 'id', all = T)
    rm(df_industry)
    # Remove firms' ids
    df_$id <- NULL
  }
    
  return(df_)
}


