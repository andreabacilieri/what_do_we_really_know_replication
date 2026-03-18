#---------------------------
# Empirical CCDF
#---------------------------

#' Calculates the empirical complementary cumulative distribution function (ccdf) given a sequence of data, e.g. the out-degree sequence

#' @param degree_seq A numeric vector with the degrees

#' @return df A data.table with the frequency of each degree


empirical_ccdf <- function(degree_seq){
  df = data.table(degree = sort(degree_seq, decreasing = T))
  N <- dim(df)[1]
  df = df[, .N, by = degree]
  df$ccdf <- cumsum(df$N)/N
  return(df)
}


#-------------------------------------
# Total least square regression
#------------------------------------

#' Calculates the total least square regression

#' @param x A numeric vector with the regressor
#' @param y A numeric vector with the dependent variable
#' 
#' @return A numeric vector with intercept (aa) and the slope (bb) of the regression line
 

tlsfn <- function(y, x){
  ybar <- mean(y)
  xbar <- mean(x)
  mydfc <- data.frame("yc" = y - ybar, "xc" = x - xbar)
  bb <- tls( yc ~ xc + 0, data = mydfc)$coefficient
  aa <- ybar - bb * xbar
  return(c(aa, bb))
}


#---------------------------------------------
# Total least square regression by industry
#---------------------------------------------
#' Calculates the total least square regression for each industry
#' uses the 1-digt level codes

#' @param df A data.tables with the in- and out-degrees
#' @param yr A scalar with the year the regression should be performed
#' @param ci_method A string indicating the method for computing the confidence intervals; 
#'                  c("normal", "bootstrap"), see https://cran.r-project.org/web/packages/tls/tls.pdf
#' 
#' @return A list with 2 data.tables with the tls estimates. 
#'          - df_plot_all has the tls estimate for all firms
#'          - df_plot has the tls estimates by industry

tlsfn_industry <- function(df, yr, ci_method = 'bootstrap'){
  
  ## TLS for all firms ##
  # keep only with positive in- and out-degree
  df <- df[inD > 0 & outD > 0, ]
  # subtract mean
  ybar <- mean(log10(df$inD))
  xbar <- mean(log10(df$outD))
  mydfc <- data.table("yc" = log10(df$inD) - ybar, 
                      "xc" = log10(df$outD) - xbar)
  # TLS estimate
  tls_params <- tls(yc ~ xc + 0, data = mydfc, method = ci_method)
  df_plot_all <- data.table(year = yr, 
                              tls_param_all = tls_params$coefficient, 
                              ci_low = tls_params$confidence.interval[1], 
                              ci_up = tls_params$confidence.interval[2])
  
  
  # TLS for firms in each industry
  df_industries <- unique(df[, .(indu_code_1, indu_descr_1)])
  # Do not estimate for irrelevant industries or indu with few obs
  df_industries <- df_industries[!df_industries$indu_code_1 %in% c('T', 'U', 'V', '', 9)]
  k <- 0
  for(i in df_industries$indu_code_1){
    df_tls <- df[indu_code_1 == i, ]
    # only estimate TLS if enough data and variance
    if(nrow(df_tls) > 2 && sd(log10(df_tls$inD)) > 0 && sd(log10(df_tls$outD)) > 0){
      # prepare data
      ybar <- mean(log10(df_tls$inD))
      xbar <- mean(log10(df_tls$outD))
      mydfc <- data.table("yc" = log10(df_tls$inD) - ybar, 
                          "xc" = log10(df_tls$outD) - xbar)
      # catch error if matrix is singular
      tlsparam <- tryCatch(
        tls_params <- tls(yc ~ xc + 0, data = mydfc, method = ci_method),
        error = function(e) NA  # save NA if TLS fails
      )
      tls_params <- tls_params
    } else {
      tls_params <- NA  # if can't estimate because matrix is singular
    }
    if(k == 0){
      if(all(!is.na(tls_params))){
        df_plot <- data.table(indu_code = i, 
                              indu_descr = df_industries[indu_code_1 == i]$indu_descr_1,
                              tls_param = tls_params$coefficient,
                              ci_low = tls_params$confidence.interval[1],
                              ci_up = tls_params$confidence.interval[2],
                              year = yr)
        k <- 1
      }
    }else{
      if(all(!is.na(tls_params))){
        df_append <- data.table(indu_code = i, 
                                indu_descr = df_industries[indu_code_1 == i]$indu_descr_1,
                                tls_param = tls_params$coefficient,
                                ci_low = tls_params$confidence.interval[1],
                                ci_up = tls_params$confidence.interval[2],
                                year = yr)
        df_plot <- rbind(df_plot, df_append)
      }
    }
  }
  # Order of industries same as industrial codes
  setorder(df_plot, -indu_code) 
  
  return(list(df_plot = df_plot,
              df_plot_all = df_plot_all))
}





