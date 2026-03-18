#' Loops through the years in the data frame and computes the network quantities that we show in the paper.
#' The network quantities are calculated by calling another function (myfun)
#' These functions are in file "code/utils/fun_network_analysis.R"

#' @param df_edgelist A data.table with the edge list (supplier, customer, weight (if any), year)
#' @param years A numeric vector with the years the network is observed
#' @param myfun A function to be run for each year in parallel
#' @param packages A character vector with the packages' names to pass to the parallel computation, c('data.table', 'igraph')
#' @param csv_name A string specifying the path and file name for saving the results, e.g. "folder/subfolder/filename.csv"
#' @param weight A string either "weights", "influence vec", TRUE or FALSE; default is T.
#'               - "weights" calculates the weights, and input and output shares
#'               - "influence vec" calculates the influence vector
#'               - T calculates all the other network quantities for weighted networks
#'               - F calculates all the other network quantities for binary networks
#' @param labour_share A scalar defining the labour share to be used to compute the influence vector, default is 0.5
#' @param df_industry A data.table with the industry information, columns need to be c('id', 'indu_code', indu_descr')

#' @return df_results_par A data.table or data.frame, with the results from myfun, saved as a csv file


build_yearly_csv_parallel <- function(df_edgelist, years, myfun, packages_, csv_name, 
                                      weight = T, labour_share = 0.5, df_industry = NULL){
  

  df_results_par <- foreach(i = 1:length(years), .combine = rbind, .packages = packages_) %dopar%{

    yr <- years[i]
    df <- df_edgelist[year == yr]

    if(weight == 'weights'){
      # calculate weights, and input and output shares
      if(!is.null(df_industry)){
        args <- list(df, yr, df_industry = df_industry)
      }else{
        args <- list(df, yr)
      }
      df_output <- do.call(myfun, args)

    }else if(weight == 'influence vec'){
      # calculates influence vector
      if(!is.null(df_industry)){
        args <- list(df, yr, labour_share = labour_share, df_industry = df_industry)
      }else{
        args <- list(df, yr, labour_share = labour_share)
      }
      df_output <- do.call(myfun, args)

    }else{
      # calculates all the other properties
      if(!is.null(df_industry)){
        args <- list(df, yr, weight = weight, df_industry = df_industry)
      }else{
        args <- list(df, yr, weight = weight)
      }
      df_output <- do.call(myfun, args)
    }

  }
  # save results to csv file
  write.csv(df_results_par, csv_name, row.names = F)
}
