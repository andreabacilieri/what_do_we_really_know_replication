#' Creates a synthetic graph that is then used to generate synthetic networks for the synthetic data generation process

#' @param n A scalar indicating the number of nodes in the network
#' @param weighted Either TRUE or FALSE; default is T. If T, it generates a weighted network, otherwise a binary network

#' @return g An igraph graph


synth_graph <- function(n, weighted = T){
  
  # generate random graph with given out-degree sequence using BA model
  outseq <- round(rlnorm(n, meanlog = 1, sdlog = 1.5), 0)
  g <- simplify(sample_pa(n, zero.appeal = 1, power = 0.9, out.seq = outseq, directed = T))
  # add small disconnected component
  n_disconnected <- 10
  g <- g + make_full_graph(n_disconnected, directed = T, loops = F)
  
  if(weighted == T){
    # generate random weights proportional to out-degrees + small noise so weights are not integers
    kout <- degree(g, mode = 'out')

    W <- as.matrix(kout) %*% kout
    W <- W * (1 + matrix(nr = n_disconnected + n, nc = n_disconnected + n, runif((n_disconnected + n)^2, min = -0.01, max = 0.01)))
    A <- as_adjacency_matrix(g)
    A <- A * W
    rm(W)
    
    g <- graph_from_adjacency_matrix(A, mode = "directed", weighted = T)
  }
  
  return(g)
}