################################################################################
# This script produces Table C.3 to C.8 showing the tail exponents for Ecuador, FactSet and Hungary over time

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador_tails_*.csv
#     - hungary_tails_*.csv
#     - factset_tails_inD.csv
#     - factset_tails_outD.csv

# OUTPUT:
# to results/tables/
#     - appendix/C_tabl_tailExp_*.tex

# * in {inD, outD, inS, outS, weights, influence}

################################################################################


# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------
rm(list = ls())
library(xtable)
library(data.table)

# Find root folder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Set directory for Output
dirOutput <- file.path(rootfolder, 'results', 'tables')
# Get directory of the input data (outside of the project)
dirdata <- file.path(rootfolder, 'data', 'analysis')


# ------------------------------------------------------------------------------
# Tables
# ------------------------------------------------------------------------------

export_tail_estimates_table <- function(myvar, mycaption){
  
  Xec <- read.csv(file.path(dirdata, "ecuador", paste0("ecuador_tails_", myvar, ".csv")))
  Xhu <- read.csv(file.path(dirdata, "hungary", paste0("hungary_tails_", myvar, ".csv")))

  if(myvar %in% c("inD","outD")){
    Xfs <- read.csv(file.path(dirdata, "factset", paste0("factset_tails_", myvar, ".csv")))
  }else{
    Xfs <- c()
  }

  X <- as.data.frame(rbindlist(list(Xec, Xhu, Xfs)))
  X[,c(2, 4, 6, 8)] <- round(X[, c(2, 4, 6, 8)], 2)
  X[, "year"] <- as.character(X[, "year"])
  XTAB <- xtable(X)

  XTAB <- xtable(X,
                 align = "ccR{12mm}R{12mm}R{12mm}R{12mm}R{12mm}R{12mm}R{12mm}R{12mm}",
                 caption = paste0("Tail exponents for ", mycaption , " distributions"),
                 label = paste0("tab:tailexp_", myvar))

  lhu <- nrow(Xhu)
  lec <- nrow(Xec)
  if(myvar %in% c("inD", "outD")){
    lfs <- nrow(Xfs)
  }else{
    lfs <- 0
  }

  addtorow <- list()

  if(myvar %in% c("inD", "outD")){
    addtorow$pos <- as.list(c(-1, -1, -1, 0, 0, rep(lec, 2), rep(lhu + lec, 2), lhu + lec + lfs))
    addtorow$command <- c('\\toprule ',
                          '& \\multicolumn{2}{c}{plfit} & \\multicolumn{2}{c}{Hill} & \\multicolumn{2}{c}{Moment} & \\multicolumn{2}{c}{Kernel}  \\\\ ',
                          '& \\multicolumn{1}{c}{$\\gamma$} & \\multicolumn{1}{c}{$\\kappa$} & \\multicolumn{1}{c}{$\\gamma$} & \\multicolumn{1}{c}{$\\kappa$} & \\multicolumn{1}{c}{$\\gamma$} & \\multicolumn{1}{c}{$\\kappa$} & \\multicolumn{1}{c}{$\\gamma$} & \\multicolumn{1}{c}{$\\kappa$} \\\\ ',
                          '\\midrule ',
                          '\\multicolumn{9}{l}{\\textit{Ecuador}} \\\\ ',
                          '\\midrule ',
                          '\\multicolumn{9}{l}{\\textit{Hungary}} \\\\ ',
                          '\\midrule ',
                          '\\multicolumn{9}{l}{\\textit{FactSet}} \\\\ ',
                          '\\bottomrule \\multicolumn{9}{c}{ \\begin{minipage}{14cm}~\\\\%
  \\justifying \\noindent
  \\textit{Notes}: Parameters estimated using \\texttt{plfit} \\citep{clauset2009power} and the three tail-index estimators for generalized extreme value distributions of \\cite{voitalov2019scale}. $\\kappa$ is the number of data points.
  \\end{minipage} }\\\\')
  }else{
    addtorow$pos <- as.list(c(-1, -1, -1, 0, 0, rep(lec, 2), lhu + lec))
    addtorow$command <- c('\\toprule ',
                          '& \\multicolumn{2}{c}{plfit} & \\multicolumn{2}{c}{Hill} & \\multicolumn{2}{c}{Moment} & \\multicolumn{2}{c}{Kernel}  \\\\ ',
                          '& \\multicolumn{1}{c}{$\\gamma$} & \\multicolumn{1}{c}{$\\kappa$} & \\multicolumn{1}{c}{$\\gamma$} & \\multicolumn{1}{c}{$\\kappa$} & \\multicolumn{1}{c}{$\\gamma$} & \\multicolumn{1}{c}{$\\kappa$} & \\multicolumn{1}{c}{$\\gamma$} & \\multicolumn{1}{c}{$\\kappa$} \\\\ ',
                          '\\midrule ',
                          '\\multicolumn{9}{l}{\\textit{Ecuador}} \\\\ ',
                          '\\midrule ',
                          '\\multicolumn{9}{l}{\\textit{Hungary}} \\\\ ',
                          '\\bottomrule \\multicolumn{9}{c}{ \\begin{minipage}{14cm}~\\\\%
  \\justifying \\noindent
  \\textit{Notes}: Parameters estimated using \\texttt{plfit} \\citep{clauset2009power} and the three tail-index estimators for generalized extreme value distributions of \\cite{voitalov2019scale}. $\\kappa$ is the number of data points.
  \\end{minipage} }\\\\')
  }

  
  # ------------------------------------------------------------------------------
  #  Export
  # ------------------------------------------------------------------------------

  print.xtable(XTAB,
               file = paste0(dirOutput, paste0("/C_tab_tailExp_", myvar, ".tex")),
               hline.after = NULL,
               size = "small",
               format.args = list(big.mark = ",", decimal.mark = "."),
               include.colnames = F, include.rownames = F,
               table.placement = "htbp",
               caption.placement = "top",
               sanitize.text.function = function(x){x},
               sanitize.rownames.function = function(x){substr(x, 2, 6)},
               add.to.row	= addtorow)

}

export_tail_estimates_table(myvar = "inD",       mycaption = "in-degree")
export_tail_estimates_table(myvar = "outD",      mycaption = "out-degree")
export_tail_estimates_table(myvar = "inS",       mycaption = "in-strength")
export_tail_estimates_table(myvar = "outS",      mycaption = "out-strength")
export_tail_estimates_table(myvar = "weights",   mycaption = "weights")
export_tail_estimates_table(myvar = "influence", mycaption = "influence")

print('Table C.3 to C.8 showing the tail exponents for Ecuador, FactSet and Hungary over time exported.')