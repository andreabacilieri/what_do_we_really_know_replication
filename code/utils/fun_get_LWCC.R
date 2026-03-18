#' Cleans a network so that nodes in the largest weakly connected component (lwcc) are kept
#' It also removes 
#' - weights that are equal to zero
#' - self-loops
#' - multiple edges 

#' @param df A data.frame with the edge list (supplier, customer, weight (if any), years)
#' 
#' @return A list with two elements 
#'        (1) A data.table with the cleaned edge list, for nodes in the lwcc
#'        (2) A data.frame with statistics about the number of nodes and edges in the lwcc
        

get_LWCC <- function(df){
  
  df <- data.table(df)
  
  if(!is.null(df$weight)){
    # remove weights = 0
    df <- df[weight != 0]
  }
  
  years <- sort(unique(df$year))
  nyears <- length(years)
  # create table for shares in LWCC
  shares_lwcc_table <- data.frame("Nnodes_full" = rep(NA, nyears),
                                  "Nnodes_lwcc" = rep(NA, nyears),
                                  "Nedges_full" = rep(NA, nyears),
                                  "Nedges_lwcc" = rep(NA, nyears),
                                  "year" = years)
  df_save <- c()
  
  for(yr in years){
    # get full network
    if(!is.null(df$weight)){
      G <- graph_from_data_frame(df[year == yr, c("supplier","customer","weight","year")], directed = T)
    }else{
      G <- graph_from_data_frame(df[year == yr, c("supplier","customer","year")], directed = T)
    }
    
    # eliminate self-loops and multiple edges
    G <- simplify(G, edge.attr.comb = 'sum')
    co <- components(G, mode ='weak')
    
    # Print number of nodes and edges before simplifying
    cat(sprintf("Before simplify: Nodes = %d, Edges = %d\n", vcount(G), ecount(G)))

    # get the LWCC
    co = components(G, mode = 'weak')
    Glwcc <- induced_subgraph(G, which(co$membership == which.max(co$csize)))
    dflwcc <- data.table(igraph::as_data_frame(Glwcc, what = "edges"))

    # Print number of nodes and edges after simplifying
    cat(sprintf("After simplify: Nodes = %d, Edges = %d\n", vcount(G), ecount(G)))

    df_save <- rbind(df_save, dflwcc)
    shares_lwcc_table[which(shares_lwcc_table$year == yr), "Nnodes_full" ] <- vcount(G)
    shares_lwcc_table[which(shares_lwcc_table$year == yr), "Nnodes_lwcc" ] <- vcount(Glwcc)
    shares_lwcc_table[which(shares_lwcc_table$year == yr), "Nedges_full" ] <- ecount(G)
    shares_lwcc_table[which(shares_lwcc_table$year == yr), "Nedges_lwcc" ] <- ecount(Glwcc)
  }
  
  colnames(df_save)[1:2] <- c("supplier", "customer")
  return(list("edgelist" = df_save, "stats_lwcc" = shares_lwcc_table))
}

