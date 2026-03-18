##########################################################################################################################
# This script produces Table 6: Reciprocity, path lengths and clustering.

# INPUTS:
# Data:
# from data/analysis/
#     - *_local_properties.csv
#     - *_global_properties.csv
#     - *_shortest_paths.csv
#     - *_global_local_clustering_config_model.csv
#     - *_path_length_random_graphs.csv
# from data/literature/
#     - literature_path_length_random_graphs.csv
# * in {factset, ecuador, hungary}

# OUTPUT:
# to results/tables/
# (1) 6_tab_recip_pl_clust.tex

#######################################################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)
library(xtable)

# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
# folder to store output
dirOutput <- file.path(rootfolder, 'results', 'tables')
# folder to the input data
dirdata <- file.path(rootfolder, 'data', 'analysis')


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

## FACTSET ##

# Local clustering coeff
filename <- file.path(dirdata, 'factset', 'factset_local_properties.csv')
df <- data.table(read.csv(filename))
yr_f <- 2021
# average local clustering coeff
average_local_clust_f <- mean(df[year == yr_f]$clustering, na.rm = T)

# Global clustering coeff and reciprocity
filename <- file.path(dirdata, 'factset', 'factset_global_properties.csv')
df <- data.table(read.csv(filename))
# global clustering coeff
global_clust_f <- df[year == yr_f]$clustering_global_emp
# reciprocity
recip_f <- df[year == yr_f]$reciprocity_emp

# Path lengths
filename <- file.path(dirdata, 'factset', 'factset_shortest_paths.csv')
df <- data.table(read.csv(filename))
# average path length
average_path_len_f <- weighted.mean(x = df[year == yr_f]$shrt_path, w = df[year == yr_f]$frequency)

# Simulated random graphs data
# clustering
filename <- file.path(dirdata, 'factset', 'factset_global_local_clustering_config_model.csv')
df <- data.table(read.csv(filename))
# global clustering config model
global_clust_CM_f <- df[year == yr_f]$mean_clustering_global_config_model
# average local clustering config model
local_clust_CM_f <- df[year == yr_f]$mean_clustering_local_config_model
# path length
filename <- file.path(dirdata, 'factset', 'factset_path_length_random_graphs.csv')
df <- data.table(read.csv(filename))
average_path_len_CM_f <- mean(df[model == 'Config']$av_short_path)
average_path_len_ER_f <- mean(df[model == 'ER']$av_short_path)

## ECUADOR ##

# Local clustering coeff
filename <- file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv')
df <- data.table(read.csv(filename))
yr_e <- 2015
# average local clustering coeff
average_local_clust_e <- mean(df[year == yr_e]$clustering, na.rm = T)

# Global clustering coeff and reciprocity
filename <- file.path(dirdata, 'ecuador', 'ecuador_global_properties.csv')
df <- data.table(read.csv(filename))
# global clustering coeff
global_clust_e <- df[year == yr_e]$clustering_global_emp
# reciprocity
recip_e <- df[year == yr_e]$reciprocity_emp

# Path lengths
filename <- file.path(dirdata, 'ecuador', 'ecuador_shortest_paths.csv')
df <- data.table(read.csv(filename))
# average path length
average_path_len_e <- weighted.mean(x = df[year == yr_e]$shrt_path, w = df[year == yr_e]$frequency)

# Simulated random graphs data
# clustering
filename <- file.path(dirdata, 'ecuador', 'ecuador_global_local_clustering_config_model.csv')
df <- data.table(read.csv(filename))
# global clustering config model
global_clust_CM_e <- df[year == yr_e]$mean_clustering_global_config_model
# average local clustering config model
local_clust_CM_e <- df[year == yr_e]$mean_clustering_local_config_model
# path length
filename <- file.path(dirdata, 'ecuador', 'ecuador_path_length_random_graphs.csv')
df <- data.table(read.csv(filename))
average_path_len_CM_e <- mean(df[model == 'Config']$av_short_path)
average_path_len_ER_e <- mean(df[model == 'ER']$av_short_path)

## LITERATURE ##

# Path length ER model
filename <- file.path(rootfolder, 'data', 'literature', 'literature_path_length_random_graphs.csv')
df <- data.table(read.csv(filename))
average_path_len_ER_jap05 <- mean(df[country == 'Japan_05' & model == 'ER']$av_short_path)
average_path_len_ER_jap06 <- mean(df[country == 'Japan_06' & model == 'ER']$av_short_path)

## HUNGARY ##

# Local clustering coeff
filename <- file.path(dirdata, 'hungary', 'hungary_local_properties.csv')
df <- data.table(read.csv(filename))
yr_h <- c(2015, 2019, 2021)
# average local clustering coeff
average_local_clust_h <- c('2015' = NA, '2019' = NA, '2021' = NA)
for(yr in yr_h){
  average_local_clust_h[as.character(yr)] <- mean(df[year == yr]$clustering, na.rm = T)
}

# Global clustering coeff and reciprocity
filename <- file.path(dirdata, 'hungary', 'hungary_global_properties.csv')
df <- data.table(read.csv(filename))
# global clustering coeff
global_clust_h <- c('2015' = NA, '2019' = NA, '2021' = NA)
for(yr in yr_h){
  global_clust_h[as.character(yr)] <- df[year == yr]$clustering_global_emp
}
# reciprocity
recip_h <- c('2015' = NA, '2019' = NA, '2021' = NA)
for(yr in yr_h){
  recip_h[as.character(yr)] <- df[year == yr]$reciprocity_emp
}

# Path lengths
filename <- file.path(dirdata, 'hungary', 'hungary_shortest_paths.csv')
df <- data.table(read.csv(filename))
# average path length
average_path_len_h <- c('2015' = NA, '2019' = NA, '2021' = NA)
for(yr in yr_h){
  average_path_len_h[as.character(yr)] <- weighted.mean(x = df[year == yr]$shrt_path, w = df[year == yr]$frequency)
}

# Simulated random graphs data
# clustering
filename <- file.path(dirdata, 'hungary', 'hungary_global_local_clustering_config_model.csv')
df <- data.table(read.csv(filename))
# global clustering config model
global_clust_CM_h <- c('2015' = NA, '2019' = NA, '2021' = NA)
for(yr in yr_h){
  global_clust_CM_h[as.character(yr)] <- df[year == yr]$mean_clustering_global_config_model
}
# average local clustering config model
local_clust_CM_h <- c('2015' = NA, '2019' = NA, '2021' = NA)
for(yr in yr_h){
  local_clust_CM_h[as.character(yr)] <- df[year == yr]$mean_clustering_local_config_model
}
# path length
filename <- file.path(dirdata, 'hungary', 'hungary_path_length_random_graphs.csv')
df <- data.table(read.csv(filename))
average_path_len_CM_h <- c('2015' = NA, '2019' = NA, '2021' = NA)
for(yr in yr_h){
  average_path_len_CM_h[as.character(yr)] <- mean(df[year == yr & model == 'Config']$av_short_path)
}

average_path_len_ER_h <- c('2015' = NA, '2019' = NA, '2021' = NA)
for(yr in yr_h){
  average_path_len_ER_h[as.character(yr)] <- mean(df[year == yr & model == 'ER']$av_short_path)
}


# ------------------------------------------------
#  Table
# ------------------------------------------------

## INITIALISE TABLE ##
#  5 country-year + 2 from literature, 4 stats times 3 (empi + 2 null models)
mytab <- as.data.frame(array(dim = c(8, 11)))
rownames(mytab) <- c("Ecuador 2015", "Hungary 2021", "Hungary 2019", "Hungary 2015", "FactSet 2021", "FactSet US",
                     "Japan 2005","Japan 2006")
colnames(mytab) <- c("Dataset", "Year",
                     "R_empi",
                     "Cg", "Cg_CM",
                     "Ci", "Ci_CM",
                     "PL", "PL_ER", "PL_CM","Source")

## OUR 3 NETWORKS ##
#  Reciprocity
mytab["Ecuador 2015", "R_empi"] <- recip_e
mytab["Hungary 2021", "R_empi"] <- recip_h['2021']
mytab["Hungary 2019", "R_empi"] <- recip_h['2019']
mytab["Hungary 2015", "R_empi"] <- recip_h['2015']
mytab["FactSet 2021", "R_empi"] <- recip_f

#  Global Clustering
mytab["Ecuador 2015", "Cg"] <- global_clust_e
mytab["Hungary 2021", "Cg"] <- global_clust_h['2021']
mytab["Hungary 2019", "Cg"] <- global_clust_h['2019']
mytab["Hungary 2015", "Cg"] <- global_clust_h['2015']
mytab["FactSet 2021", "Cg"] <- global_clust_f

mytab["Ecuador 2015", "Cg_CM"] <- global_clust_CM_e
mytab["Hungary 2021", "Cg_CM"] <- global_clust_CM_h['2021']
mytab["Hungary 2019", "Cg_CM"] <- global_clust_CM_h['2019']
mytab["Hungary 2015", "Cg_CM"] <- global_clust_CM_h['2015']
mytab["FactSet 2021", "Cg_CM"] <- global_clust_CM_f

#  Local Clustering
mytab["Ecuador 2015", "Ci"] <- average_local_clust_e
mytab["Hungary 2021", "Ci"] <- average_local_clust_h['2021']
mytab["Hungary 2019", "Ci"] <- average_local_clust_h['2019']
mytab["Hungary 2015", "Ci"] <- average_local_clust_h['2015']
mytab["FactSet 2021", "Ci"] <- average_local_clust_f

mytab["Ecuador 2015", "Ci_CM"] <- local_clust_CM_e*100 # seems to have been done in the original working paper (and below)
mytab["Hungary 2021", "Ci_CM"] <- local_clust_CM_h['2021']*100
mytab["Hungary 2019", "Ci_CM"] <- local_clust_CM_h['2019']*100
mytab["Hungary 2015", "Ci_CM"] <- local_clust_CM_h['2015']*100
mytab["FactSet 2021", "Ci_CM"] <- local_clust_CM_f*100

#  Paths
mytab["Ecuador 2015", "PL"] <- average_path_len_e
mytab["Hungary 2021", "PL"] <- average_path_len_h['2021']
mytab["Hungary 2019", "PL"] <- average_path_len_h['2019']
mytab["Hungary 2015", "PL"] <- average_path_len_h['2015']
mytab["FactSet 2021", "PL"] <- average_path_len_f

mytab["Ecuador 2015", "PL_ER"] <- average_path_len_ER_e
mytab["Hungary 2021", "PL_ER"] <- average_path_len_ER_h['2021']
mytab["Hungary 2019", "PL_ER"] <- average_path_len_ER_h['2019']
mytab["Hungary 2015", "PL_ER"] <- average_path_len_ER_h['2015']
mytab["FactSet 2021", "PL_ER"] <- average_path_len_ER_f

mytab["Ecuador 2015", "PL_CM"] <- average_path_len_CM_e
mytab["Hungary 2021", "PL_CM"] <- average_path_len_CM_h['2021']
mytab["Hungary 2019", "PL_CM"] <- average_path_len_CM_h['2019']
mytab["Hungary 2015", "PL_CM"] <- average_path_len_CM_h['2015']
mytab["FactSet 2021", "PL_CM"] <- average_path_len_CM_f

## LITERATURE ##
# Ohnishi et al. (2010)
mytab["Japan 2005", "PL"]    <- 4.59
mytab["Japan 2005", "PL_ER"] <- average_path_len_ER_jap05
# Fujiwara & Aoyama (2010)
mytab["Japan 2006", "PL"]    <- 5.62
mytab["Japan 2006", "PL_ER"] <- average_path_len_ER_jap06
mytab["Japan 2006", "Ci"]    <- 4.58/100
mytab["Japan 2006", "Cg"]    <- 0.187/100
mytab["Japan 2006", "Cg_CM"] <- 1.81/100
# Tascherau-Dumouchel (2022)
mytab["FactSet US", "Cg"] <- 2.4/100
mytab["FactSet US", "PL"]    <- 4.8

## ADD SOURCE COLUMN ##
mytab[1:5, "Source"] <- "This paper"
mytab["FactSet US", "Source"] <- "\\citet{taschereau2022cascades}"
mytab["Japan 2005", "Source"] <- "\\citet{ohnishi2010network}"
mytab["Japan 2006", "Source"] <- "\\citet{fujiwara2010large}"


# Transform all columns except path length to percents
mytab[,"Ci"] <- mytab[,"Ci"]*100
mytab[,"Cg"] <- mytab[,"Cg"]*100
mytab[,"Cg_CM"] <- mytab[,"Cg_CM"]*100
mytab[,"R_empi"] <- mytab[,"R_empi"]*100

## ADD DATASET AND YEAR COLUMN ##
mytab[1:8,"Dataset"] <- c("Ecuador", "Hungary", "Hungary", "Hungary", "FactSet", "FactSet US", "Japan", "Japan")
mytab[1:8,"Year"] <- c(2015L, 2021L, 2019L, 2015L, 2021L, 2016L, 2005L, 2006L)


# ------------------------------------------------
#  Export
# ------------------------------------------------

XTAB <- xtable(mytab, align = "llrrrrrrrrrl", digits = 1)

addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0, 0, 0, 2, 8)
addtorow$command <- c('\\begin{table}[ht]
\\centering
\\begingroup\\footnotesize
\\caption{Reciprocity, clustering and average path length}
\\begin{tabular}{llrrrrrrrrll}',
'\\toprule ',
'Dataset & Year & Recip. & \\multicolumn{2}{c}{$C_g$} & \\multicolumn{2}{c}{$\\bar{C}$} & \\multicolumn{3}{c}{Average path length} & Source \\\\',
'\\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-10}',
'& & Emp. & Emp. & CM & Emp. & CM & Emp. & ER & CM & \\\\',
'\\hline ',
'\\hdashline ',
'\\bottomrule \\multicolumn{12}{c}{ \\begin{minipage}{16cm}~\\\\%
\\justifying \\noindent
\\textit{Notes}:  All values are in percent except for the average shortest path length (``Average path length\'\'). ``Recip.\'\' stands for reciprocity, $C_g$ and $\\bar{C}$ are the global and average local clustering coefficients. ``Emp.\'\' stands for empirical, ER and CM for Erdos-Renyi and configuration model.  For both the ER and CM,  we run 100 simulations for the clustering coefficients,  and 10 for path lengths;  each simulation samples $10^4$ pairs at random.
\\end{minipage} } \\end{tabular} \\label{tab:recip_pl_clust} \\endgroup \\\\ \\end{table}')

filename <- paste(dirOutput, '6_tab_recip_pl_clust.tex', sep = .Platform$file.sep)

print.xtable(XTAB,hline.after = NULL, size = "small", only.contents = T,
            file = filename, include.colnames = F, include.rownames = F,
            table.placement = "ht", sanitize.text.function = function(x){x},
            tabular.environment = F, floating = F,
            add.to.row	= addtorow)

print("Table 6: 'Reciprocity, paths lengths and clustering' exported")