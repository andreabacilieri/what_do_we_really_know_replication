################################################################################
# This script produces 
# Table E.3, E.4: Strength-degree elasticities under alternative industry and year fixed-effects specifications.

# Line 46: one needs to input the name of the country for which the table should be produced.

# The regression table has 5 columns:
    # regression for a single year
    # regression adding industry fixed-effects
    # pooled regression: OLS for all years and firms
    # regression for all years and firms with industry fixed-effects
    # regression with all years and firms with industry and year fixed-effects

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_local_properties_industry.csv
#     - hungary/hungary_local_properties.csv

# OUTPUT:
# to results/tables/
#     - E_strength_degree_elasticities_agg_ind_year_*.tex

# * in {ecuador, hungary}

################################################################################


# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------
rm(list = ls())
library(xtable)
library(data.table)
library(stargazer)  # for latex reg table
library(plm) # panel data

# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'tables')
# Folder to the Input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

# Functions
source(file.path(rootfolder, 'code', 'utils', 'fun_strength_degree_elasticity_table_industry.R'))


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

# List of regressions
yvarlist <- c("outS", "outD", "inS", "inD")
xvarlist <- c("outD", "outS", "inD", "inS")
yvarlistlabel <- c("Out-Strength", "Out-Degree", "In-Strength", "In-Degree")
xvarlistlabel <- c("Out-Degree", "Out-Strength", "In-Degree", "In-Strength")
# List of countries
country_list <- c('Ecuador', 'Hungary')

for (country in country_list) {

  filename <- file.path(dirdata, tolower(country), paste0(tolower(country), '_local_properties.csv'))
  df <- as.data.frame(fread(filename))
  df <- df[!is.na(df$indu_code_1) & !(df$indu_code_1 %in% c('9','0', '.', '')), ]

  
  # ------------------------------------------------------------------------------
  #  Table for aggregate data with industry/year-fixed effects
  # ------------------------------------------------------------------------------
  
  # Select df and grouping variable based on country
  if (country == "Ecuador") {
    
    group_col <- c("indu_code_1", "indu_code_2", "indu_code_3", "indu_code_4")
    year <- 2015
    pooled_years <- c(2007:2015)
    pooled_str <- "2015 only"
    
  } else if (country == "Hungary"){
    group_col <- c("indu_code_1", "indu_code_2", "indu_code_3", "indu_code_4")
    year <- 2021
    pooled_years <- c(2020:2021)
    pooled_str <- "2021 only"
    
  } else {
    stop("Unknown country: ", country)
  }
  
  res <- LM_data(df = df, year = year,
                 yvarlist = yvarlist, xvarlist = xvarlist,
                 group_col = group_col)
  
  # Produce output tables
  label_map_hu <- c(
    "indu_code_1"  = "NACE 1-digit",
    "indu_code_2"  = "NACE 2-digit",
    "indu_code_3"  = "NACE 3-digit",
    "indu_code_4"  = "NACE 4-digit")
  
  label_map_ec <- c(
    "indu_code_1"  = "ISIC 1-digit",
    "indu_code_2"  = "ISIC 2-digit",
    "indu_code_3"  = "ISIC 3-digit",
    "indu_code_4"  = "ISIC 4-digit")
  
  mymat_inD_inS <- make_slope_matrix(res, group_col, pair_name = "inD_inS", label_map = label_map, country = country)
  mymat_inS_inD <- make_slope_matrix(res, group_col, pair_name = "inS_inD", label_map = label_map, country = country)
  mymat_outD_outS <- make_slope_matrix(res, group_col, pair_name = "outD_outS", label_map = label_map, country = country)
  mymat_outS_outD <- make_slope_matrix(res, group_col, pair_name = "outS_outD", label_map = label_map, country = country)
  
  mymat <- rbind(mymat_outS_outD,
                 mymat_outD_outS,
                 mymat_inS_inD,
                 mymat_inD_inS)
  
  mymat <- apply(mymat, 2, function(col) {
    suppressWarnings( as.numeric(col) ) -> numcol
    
    ifelse(
      is.na(numcol), 
      col,                # keep non-numeric entries
      sprintf("%.2f", numcol)  # round & format to 3 decimals
    )
  })
  
  n <- ncol(mymat)
  align_str <- paste0("ll", paste(rep("c", n-1), collapse = ""))
  
  XTAB <- xtable(
    mymat,
    caption = sprintf("Strength–degree elasticities under alternative industry and year fixed-effects specifications for %s.", country),
    label = paste0("tab:strength_degree_elasticities_by_industry_", country),
    align = align_str)
  
  add.to.row <- list()
  
  add.to.row$pos <- list(
    # Header block
    -1,  # \toprule
    -1,  # Network sales header
    -1,  # \midrule
    -1,  # ln s | ln k
    -1,  # spacing
    
    # First block separator
    4,  # cmidrule
    4,  # ln k | ln s
    4,  # spacing
    
    # Second main block header
    8,  # \toprule
    8,  # Network expenses header
    8,  # \midrule
    8,  # ln s | ln k
    
    # Second block separator
    12,  # spacing
    12,  # cmidrule
    12,  # ln k | ln s
    
    # Footer block
    16,  # addlinespace[6pt]
    16,  # hiline (1)
    16,  # hline (2)
    16,  # addlinespace[6pt]
    16,  # only
    16,  # Industry FE
    16,  # Year FE
    16   # bottomrule
  )
  
  
  add.to.row$command <- c(
    "\\toprule\n",
    "\\multicolumn{6}{l}{\\textit{Network sales \\& number of customers}} \\\\\n",
    "\\midrule\n",
    "\\textbf{$\\ln s \\mid \\ln k$} & & & & & \\\\\n",
    "\\addlinespace[3pt]\n",
    "\\cmidrule{1-6}\n",
    "\\textbf{$\\ln k \\mid \\ln s$} & & & & & \\\\\n",
    "\\addlinespace[3pt]\n",
    "\\toprule\n",
    "\\multicolumn{6}{l}{\\textit{Network expenses \\& number of suppliers}} \\\\\n",
    "\\midrule\n",
    "\\textbf{$\\ln s \\mid \\ln k$} & & & & & \\\\\n",
    "\\addlinespace[3pt]\n",
    "\\cmidrule{1-6}\n",
    "\\textbf{$\\ln k \\mid \\ln s$} & & & & & \\\\\n",
    "\\addlinespace[3pt]\n",
    "\\hline\n",
    "\\hline\n",
    "\\addlinespace[6pt]\n",
    sprintf("%s & Yes & Yes & No & No & No \\\\\n", pooled_str),
    "Industry FE & No & Yes & No & Yes & Yes \\\\\n",
    "Year FE & No & No & No & No & Yes \\\\\n",
    "\\bottomrule"
  )
  
  if(country == 'Ecuador'){
    filename <- file.path(dirOutput, paste0('E3_strength_degree_elasticities_agg_ind_year_ecuador.tex'))
  }else{
    filename <- file.path(dirOutput, paste0('E4_strength_degree_elasticities_agg_ind_year_hungary.tex'))
  }
  
  print.xtable(XTAB, file = filename,
               hline.after = NULL,
               sanitize.text.function = function(x){x},
               sanitize.rownames.function = function(x){x},
               include.rownames = F, include.colnames = F,
               add.to.row	= add.to.row,
               caption.placement = "top")

}

print('Table E.3, E.4: Strength-degree elasticities under alternative industry and year fixed-effects specifications exported.')