################################################################################
# This script produces input for
# Figure A.1: Sectoral composition in the national I-O table at the sector level and in our firm-level dataset in 2015 and 2020.

# We had to preprocess the input xlsx files so that R could read them
# The following steps are required:
# 1) Move row 7 by one cell to the right (This error appears when one exports the data to csv from the website of the Hungarian Central Statistical Office.)
# 2) Delete rows 1-6 and 8-9 (meta information about the data)
# 3) Add "Var_names" to cell A1
# -> saved the file as "symmetric_input-output_table_(industry_by_industry),_at_basic_prices_NACE_Rev._2_(ESA2010)_[year]_method[year_meth].csv" (for year = {2015, 2020} and year_meth = {2019, 2023}).
# We downloaded the raw data from: https://statinfo.ksh.hu/Statinfo/themeSelector.jsp?&lang=en (date of downloading: 30 january 2025)

# INPUTS:
# from data/national_statistics/hungary
#   - symmetric_input-output_table_(industry_by_industry),_at_basic_prices_NACE_Rev._2_(ESA2010)_2015_method2019.csv
#   - symmetric_input-output_table_(industry_by_industry),_at_basic_prices_NACE_Rev._2_(ESA2010)_2020_method2023.csv
#   - nace_rev2_aggregation_table.csv

# OUTPUT:
# to data/national_statistics/hungary
# (1) I_O_table_aggregate_statistics_2015.csv
# (2) I_O_table_aggregate_statistics_2020.csv

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(ggplot2)
library(ggpubr)
library(data.table)
library(dplyr)

# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
# Folder to store output
dirOutput <- file.path(rootfolder, 'data', 'national_statistics', 'hungary')
# Folder for the input data
dirdata <- file.path(rootfolder, 'data', 'national_statistics', 'hungary')


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

filename <- file.path(dirdata,'Symmetric_input-output_table_(industry_by_industry),_at_basic_prices_NACE_Rev._2_(ESA2010)_2015_method2019.csv')
IO_2015_table <- fread(filename)

filename <- file.path(dirdata,'Symmetric_input-output_table_(industry_by_industry),_at_basic_prices_NACE_Rev._2_(ESA2010)_2020_method2023.csv')
IO_2020_table <- fread(filename)

nace_aggregation <- fread(file.path(dirdata, 'nace_rev2_aggregation_table.csv'))


# ------------------------------------------------------------------------------
#  Extract statistics
# ------------------------------------------------------------------------------

networks <- list(IO_2015_table, IO_2020_table)
years <- c(2015, 2020)

# generate codes for merging the csv with nace codes with the I-O table
nace_code <- gsub("[a-zA-Z ] ", "", nace_aggregation$nace_2digit)
nace_aggregation$code <- nace_code

for (t in seq_along(networks)){
  
  IO_table <- networks[[t]]
  year <- years[t]
  
  df <- data.table(sector = IO_table[1:65, "Var_name"][[1]], 
                   interm_sales = rowSums(IO_table[1:65, 2:66]),
                   gfcf = IO_table[1:65, "Gross fixed capital formation"][[1]],
                   gross_output = IO_table[1:65, "Total use"][[1]] - IO_table[1:65, "Exports"][[1]])
  
  # generating codes for merging with nace aggregation csv
  x <- strsplit(df$sector, ": ")
  y <- c(rep(NA, length(x)))
  for(i in 1:length(x)){
    y[i] <- x[[i]][[1]]
  }
  df$code <- y
  df <- merge(df, nace_aggregation, by = 'code')
  df_aggregate <- df[, c('nace_1digit', 'interm_sales', 'gfcf', 'gross_output')]
  df_aggregate <- df_aggregate[, lapply(.SD,sum), by = .(nace_1digit)]
  
  
  # ------------------------------------------------------------------------------
  #  Export
  # ------------------------------------------------------------------------------
  
  filename <- file.path(dirOutput , paste('I_O_table_aggregate_statistics_', as.character(year), '.csv', sep = ''))
  write.csv(df_aggregate, file = filename, row.names = F)
  
}
