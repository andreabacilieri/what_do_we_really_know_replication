################################################################################
# This script produces Table E.1, E.2: Variance decomposition of key network metrics 

# In line 225: one needs to input the name of the country for which the table should be produced.

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_local_properties.csv
#     - hungary/hungary_local_properties.csv
# from code/utils_plots/
#     - set_size.R

# OUTPUT:
# to results/figures/offline_appendix/
#     - variance_decomposition_*.pdf

# * in {ecuador, hungary}

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)
library(ggplot2)
library(latex2exp)
library(patchwork)
library(metafor)

# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'tables')
# Folder to the Input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

# Functions
source(file.path(rootfolder, 'code', 'utils', 'fun_variance_decomposition.R'))

# For plots
width_LaTeX <- 418.25368  # in pt
source(file.path(rootfolder, 'code', 'utils_plots', 'set_size.R'))


# ------------------------------------------------------------------------------
#  Load  and prepare data
# ------------------------------------------------------------------------------

tokc <- c("inD", "outD", "inS", "outS")
country_list <- c("Ecuador", 'Hungary')

for (country in country_list) {
  
  filename <- file.path(dirdata, tolower(country), paste0(tolower(country), '_local_properties.csv'))
  df <- data.table(read.csv(filename))

  
  # ------------------------------------------------------------------------------
  # Variance decomposition 
  # For in- and out-degree, in- and out-strength, local clustering.
  # ------------------------------------------------------------------------------
  
  metrics_to_use <- c("inD_log", "outD_log", "inS_log", "outS_log", "clustering")
  
  # Select df and grouping variable based on country
  if (country == "Ecuador") {
    df <- df[year == 2015]
    df[, (tokc) := lapply(.SD, function(x) ifelse(x == 0, NA_real_, x)), .SDcols = tokc]
    df <- df[!is.na(df$indu_code_1) & !(df$indu_code_1 %in% c('9','0', '')), ]
    df[, inD_log  := log(inD)]
    df[, outD_log := log(outD)]
    df[, inS_log  := log(inS)]
    df[, outS_log := log(outS)]
    
    group_col <- c("indu_code_1", "indu_code_2", "indu_code_3", "indu_code_4")
    year <- 2015
    label <- "tab:var_decomp_ecuador"
    
    } else if (country == "Hungary") {
      df <- df[year == 2021]
      df[, (tokc) := lapply(.SD, function(x) ifelse(x == 0, NA_real_, x)), .SDcols = tokc]
      df <- df[!is.na(df$indu_code_1) & !(df$indu_code_1 %in% c('.', '')), ]
      df[, inD_log  := log(inD)]
      df[, outD_log := log(outD)]
      df[, inS_log  := log(inS)]
      df[, outS_log := log(outS)]
        
      group_col <- c("indu_code_1", "indu_code_2", "indu_code_3", "indu_code_4")
      year <- 2021
      label <- "tab:var_decomp_hungary"
      
      } else {
        stop("Unknown country: ", country)
      }
  
  # If group_col is a vector, compute one res_df per grouping level
  res_df_list <- vector("list", length(group_col))
  names(res_df_list) <- group_col
  
  for (i in seq_along(group_col)) {
    
    gcol <- group_col[i]
    res_list_i <- lapply(metrics_to_use, function(metric) {
      variance_decomp_by_group(df, metric = metric, group = gcol)})
    
    res_df_list[[gcol]] <- rbindlist(lapply(res_list_i, as.data.table), use.names = TRUE, fill = TRUE)
  }
  
  
  # ------------------------------------------------------------------------------
  #  Table with results
  # ------------------------------------------------------------------------------
  
  cols <- c("metric", "N", "var_total", "var_between",
            "share_between", "overall_mean")
  
  # Rename metric values
  label_map <- c(
    "inD_log"   = "In-degree (log)",
    "outD_log"  = "Out-degree (log)",
    "inS_log"   = "In-strength (log)",
    "outS_log"  = "Out-strength (log)",
    "clustering"= "Local clustering coefficient")
  
  titles_hu <- c(
    "NACE 1-digit",
    "NACE 2-digit",
    "NACE 3-digit",
    "NACE 4-digit")
  
  titles_ec <- c(
    "ISIC 1-digit",
    "ISIC 2-digit",
    "ISIC 3-digit",
    "ISIC 4-digit")
  
  if (country == "Hungary") {
    titles <- titles_hu
  } else if (country == "Ecuador") {
    titles <- titles_ec
  } else {
    stop("Unknown country: ", country)
  }
  
  for (nm in names(res_df_list)) {
    res_df_list[[nm]][, metric := as.character(metric)]
    res_df_list[[nm]][, metric := ifelse(metric %in% names(label_map), label_map[metric], metric)]
    res_df_list[[nm]] <- res_df_list[[nm]][, ..cols]
  }
  
  caption <- sprintf("Variance decomposition of key network metrics for %s in %d.", country, year)
  
  digits <- 2
  
  df_list <- list(
    res_df_list[[group_col[1]]],
    res_df_list[[group_col[2]]],
    res_df_list[[group_col[3]]],
    res_df_list[[group_col[4]]])
  
  nblocks <- length(df_list)
  
  t <- block_table(
    dflist = df_list,
    titles = titles,
    cols = cols,
    label_map = label_map,
    digits = digits,
    caption = caption,
    nblocks = nblocks,
    label = label)
  
  if(country == 'Ecuador'){
    filename = paste(dirOutput, paste0('E1_variance_decomposition_ecuador.tex'), sep = .Platform$file.sep)
  }else{
    filename = paste(dirOutput, paste0('E2_variance_decomposition_hungary.tex'), sep = .Platform$file.sep)
  }
  
  write(t, file = filename)
  
}

print('Table E.1, E.2: Variance decomposition of key network metrics exported.')
