################################################################################
# This script performs a network analysis for Ecuador, Hungary and FactSet. Results are saved in csv files.
# The script does not perform the analysis for all datasets at the same time, but rather for one dataset at a time, 
# as specified by the `dataset` parameter.

# INPUTS:
# from data/cleaned/
#       - *_edgelist_lwcc.csv 
# from data/national_statistics/
#       - P_Data_Extract_From_World_Development_Indicators/48a79be1-202d-43d9-b82e-385fc17c97b5_Data.csv
# from code/utils/
#       - fun_network_analysis.R
#       - fun_build_yearly_csv_parallel.R

# OUTPUT:
# to data/analysis/
#       - */*_global_properties.csv
#       - */*_local_properties.csv
#       - */*_shortest_paths.csv
#       - **/**_weights_input_output_shares.csv
#       - **/**_influence_vector.csv

# * {ecuador, hungary, factSet}
# ** {ecuador, hungary}

################################################################################

#' Performs the network analysis for a given dataset.

#' @param nPairs A scalar indicating the number of pairs to sample in the function calculating the shortest paths
#' @param dataset A character with the name of the dataset ('ecuador', 'hungary' or 'factset')
#' @param weight A string either "weights", "influence vec", TRUE or FALSE; default is T.
#'               - "weights" calculates the weights, and input and output shares
#'               - "influence vec" calculates the influence vector
#'               - T calculates all the other network properties for weighted networks
#'               - F calculates all the other network properties for binary networks
#' @param remove Either True of False, default is F. If T, it deletes the csv files with the edge list.
#'               From data/cleaned/, it deletes [dataset]_edgelist_lwcc.csv (for dataset = {Ecuador, Hungary, FactSet})
#' @param labour_share A scalar defining the labour share to be used to compute the influence vector, default is 0.5
#' @param trunc Either True of False, default is F. If T, it runs the truncation analysis.

#' @return saves several csv files with the network properties of interest

conduct_network_analysis <- function(nPairs, dataset, weight, remove = F, labour_share = 0.5, trunc = F) {

    # ------------------------------------------------------------------------------
    #  Set environment/parameters
    # ------------------------------------------------------------------------------

    rm(list = ls()[!ls() %in% c("nPairs", "dataset", "weight", "remove", "labour_share", "trunc", "trunc_list")])
    library(igraph)
    library(data.table)
    library(foreach)
    library(parallel)
    library(doSNOW)

    # Get root folder
    rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
    # Get directory of the input data
    dirdata <- file.path(rootfolder, 'data', 'cleaned')
    # Get directory of the output data
    dirOutput <- file.path(rootfolder, 'data', 'analysis')

    # Source functions
    source(file.path(rootfolder,'code', 'utils', 'fun_network_analysis.R'))
    source(file.path(rootfolder,'code', 'utils', 'fun_build_yearly_csv_parallel.R'))

    # Setup parallel computing
    cl <- makeCluster(detectCores())
    registerDoSNOW(cl)
    packages_ <- c('data.table', 'igraph')

    # ------------------------------------------------------------------------------
    #  Load and prepare data
    # ------------------------------------------------------------------------------

    # Load edgelist
    source_filename <- paste0(dataset, '_edgelist_lwcc.csv')
    df <- fread(file.path(dirdata, dataset, source_filename))
    years <- sort(unique(df$year))
    # Load industry info
    if(dataset != 'factset'){
      source_filename <- paste0(dataset, '_industry_info.csv')
      df_industry <- fread(file.path(dirdata, dataset, source_filename))
    }

    # truncated graph if specified
    if (trunc == T) {

        # Load files for truncation analysis
        ec_gdp <- read.csv(file.path(rootfolder, 'data', 'national_statistics', 'P_Data_Extract_From_World_Development_Indicators', '48a79be1-202d-43d9-b82e-385fc17c97b5_Data.csv'))
        select_col <- which(colnames(ec_gdp)=="X2015..YR2015.")
        select_row <- which(ec_gdp$Country.Code=="ECU" & ec_gdp$Series.Name=="GDP per capita (current LCU)")
        ec_gdppp <- as.numeric(ec_gdp[select_row, select_col])
        trunc_list <- c(0.02, seq(from = 0.05, to = 0.95, by = 0.05))
        yr <- 2015
        df <- subset(df, year == yr)
        
        # Create graph from edgelist
        EL <- as.matrix(apply(df[, c("supplier", "customer")], 2, as.character))
        g <- graph_from_edgelist(EL)
        E(g)$weight <- as.numeric(df[["weight"]])
        w <- E(g)$weight

        trunc_vals <- rep(NA, length(trunc_list))
        for (i in 1:length(trunc_list)) {

            trunc_val = trunc_list[i]

            # Determine minimum truncation value corresponding to 0.02% of GDPpp
            if (trunc_val == 0.02) {
                quantile_value <- (0.02 / 100) * ec_gdppp
                min_trunc_val <- ecdf(E(g)$weight)(quantile_value)
                trunc_val <- min_trunc_val
                trunc_list[i] <- trunc_val
            }

            trunc_gdppp_val = 100*as.numeric(quantile(E(g)$weight, trunc_val) / ec_gdppp)
            trunc_vals[i] <- trunc_gdppp_val

            start_time <- Sys.time()
  
            ## Truncate graph without keeping lwcc ##
            gtrunc <- delete.edges(g,which(w < quantile(w,trunc_val) ))
            # turn the truncated graph back into a data frame
            EL_trunc <- as.data.frame(get.edgelist(gtrunc))
            colnames(EL_trunc) <- colnames(df)[1:2]
            weights_trunc <- E(gtrunc)$weight
            df_all <- cbind(EL_trunc, weight = weights_trunc)

            ## Truncate graph keeping lwcc ##
            components <- clusters(gtrunc, mode = "weak")
            largest_component <- which.max(components$csize)
            gtrunc_lwcc <- induced_subgraph(gtrunc, which(components$membership == largest_component))
            
            # turn the truncated graph back into a data frame
            EL_trunc_lwcc <- as.data.frame(get.edgelist(gtrunc_lwcc))
            colnames(EL_trunc_lwcc) <- colnames(df)[1:2]
            weights_trunc_lwcc <- E(gtrunc_lwcc)$weight
            df_lwcc <- cbind(EL_trunc_lwcc, weight = weights_trunc_lwcc)
            dfs <- list(df_all = df_all, df_lwcc = df_lwcc)

            for (name in names(dfs)) {
                df <- dfs[[name]]

                # Determine the file name based on the name of the data frame
                file_suffix <- if (name == "df_all") "_all.csv" else "_lwcc.csv"
                
                # Global properties: assortativity, transitivity, reciprocity, number of nodes and edges
                filename <- paste0(dataset, '_', trunc_val, '_global_properties', file_suffix)
                csv_name <- file.path(dirOutput, dataset, 'truncation', filename)
                write.csv(global_properties(df, yr, weight = weight), csv_name, row.names = F)

                # Local properties: node-level degrees, strengths and clustering
                filename <- paste0(dataset, '_', trunc_val, '_local_properties', file_suffix)
                csv_name <- file.path(dirOutput, dataset, 'truncation', filename)
                write.csv(local_properties(df, yr, weight = weight), csv_name, row.names = F)

                # Distribution of shortest paths
                filename <- paste0(dataset, '_', trunc_val, '_shortest_paths', file_suffix)
                csv_name <- file.path(dirOutput, dataset, 'truncation', filename)
                df_save <- shortest_paths_sample_trunc(df, yr, nPairs)
                write.csv(df_save, csv_name, row.names = F)

                # Only continue with ecuador and hungary
                if (weight == T) {
                    # Weights, input shares and output shares
                    filename <- paste0(dataset, '_', trunc_val, '_weights_input_output_shares', file_suffix)
                    csv_name <- file.path(dirOutput, dataset, 'truncation', filename)
                    write.csv(weights_input_output_shares(df, yr), csv_name, row.names = F)

                    # Influence vectors
                    filename <- paste0(dataset, '_', trunc_val, '_influence_vector', file_suffix)
                    csv_name <- file.path(dirOutput, dataset, 'truncation', filename)
                    write.csv(influence_vector(df, yr, labour_share = labour_share), csv_name, row.names = F)
                }
            }
            end_time <- Sys.time()
            difference_ <- difftime(end_time, start_time, units = 'hour')
            print(paste('Elapsed time (hour): ', difference_, ' for truncation value: ', trunc_val))
        }
        df_trunc_vals <- data.frame(trunc_prop_vals = trunc_list,
                                    trunc_gdppp_vals = trunc_vals)
        filename <- paste0(dataset, '_truncation_prop_gdppp_conversion.csv')
        csv_name <- file.path(dirOutput, dataset, 'truncation', filename)
        write.csv(df_trunc_vals, csv_name, row.names = F)
        
    } else {

        # Global properties: assortativity, transitivity, reciprocity, number of nodes and edges
        filename <- paste0(dataset, '_global_properties.csv')
        csv_name <- file.path(dirOutput, dataset, filename)
        build_yearly_csv_parallel(df, years, global_properties, packages_, csv_name, weight = weight)

        # Local properties: node-level degrees, strengths and clustering
        filename <- paste0(dataset, '_local_properties.csv')
        csv_name <- file.path(dirOutput, dataset, filename)
        if(dataset != 'factset'){
          build_yearly_csv_parallel(df, years, local_properties, packages_, csv_name, 
                                    weight = weight, df_industry = df_industry)
        }else{
          build_yearly_csv_parallel(df, years, local_properties, packages_, csv_name, 
                                    weight = weight)
        }
        

        # Distribution of shortest paths
        # (Note that parallelisation is done over sampling of pairs, rather than years)
        filename <- paste0(dataset, '_shortest_paths.csv')
        csv_name <- file.path(dirOutput, dataset, filename)
        df_save <- shortest_paths_sample(df, years, nPairs)
        write.csv(df_save, csv_name, row.names = F)

        # Only continue with ecuador and hungary
        if (weight == T) {
            # Weights, input shares and output shares
            filename <- paste0(dataset, '_weights_input_output_shares.csv')
            csv_name <- file.path(dirOutput, dataset, filename)
            build_yearly_csv_parallel(df, years, weights_input_output_shares, packages_, csv_name, 
                                      weight = 'weights', df_industry = df_industry)

            # Influence vector
            filename <- paste0(dataset, '_influence_vector.csv')
            csv_name <- file.path(dirOutput, dataset, filename)
            build_yearly_csv_parallel(df, years, influence_vector, packages_, csv_name, 
                                      weight = 'influence vec', labour_share = labour_share,
                                      df_industry = df_industry)
        }
    }
    stopCluster(cl)
  
    # Delete the edgelist file
    if (remove == T) {
        remove_file <- file.remove(file.path(dirdata, dataset, source_filename))
        # Check the file was successfully deleted
        if (remove_file) {
        cat("File deleted successfully.\n")
        } else {
        cat("Failed to delete the file.\n")
        }
    }
}