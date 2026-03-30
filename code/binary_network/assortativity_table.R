################################################################################
# This script produces Table 5: Assortativity coefficients
#
# INPUTS:
# Data:
# from data/analysis/
#   - ecuador_global_properties.csv
#   - factset_global_properties.csv
#   - hungary_global_properties.csv
#
# OUTPUT:
# to results/tables/
# (1) 6_tab_assort.tex

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)
library(xtable)

# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Folder to store output
dirOutput <- file.path(rootfolder, 'results', 'tables')
# Folder for the input data
dirdata <- file.path(rootfolder, 'data', 'analysis')


# ------------------------------------------------------------------------------
#  Load data and build Table
# ------------------------------------------------------------------------------

X <- array(dim = c(8, 8), data = NA)
colnames(X) <- c("Dataset", "Year", "kk", "kinkout", "koutkin", "kinkin", "koutkout", "Source")

## ECUADOR ##
filename <- file.path(dirdata, 'ecuador', 'ecuador_global_properties.csv')
df <- data.table(read.csv(filename))
df <- df[year == 2015]

X[1,"Dataset"] <- "Ecuador"
X[1,"Year"] <- 2015
X[1,"kk"]       <- formatC(round(100*df$undirected_assort, 1), format = "f", digits = 1)
X[1,"kinkout"]  <- formatC(round(100*df$assort_in_out, 1), format = "f", digits = 1)
X[1,"koutkin"]  <- formatC(round(100*df$assort_out_in, 1), format = "f", digits = 1)
X[1,"kinkin"]   <- formatC(round(100*df$assort_in_in, 1), format = "f", digits = 1)
X[1,"koutkout"] <- formatC(round(100*df$assort_out_out, 1), format = "f", digits = 1)
X[1,"Source"] <- "This paper"

## HUNGARY ##
filename <- file.path(dirdata, 'hungary', 'hungary_global_properties.csv')
df <- data.table(read.csv(filename))
huyears <- c(2021, 2019, 2015)
df <- df[year %in% huyears]
df
for(i in 1:length(huyears)){

  yr <- huyears[i]
  X[i+1, "Dataset"] <- "Hungary"
  X[i+1, "Year"] <- yr
  X[i+1, "kk"] <- formatC(round(100*df[year == yr]$undirected_assort, 1), format = "f", digits = 1)
  X[i+1, "kinkout"] <- formatC(round(100*df[year == yr]$assort_in_out, 1), format = "f", digits = 1)
  X[i+1, "koutkin"] <- formatC(round(100*df[year == yr]$assort_out_in, 1), format = "f", digits = 1)
  X[i+1, "kinkin"] <- formatC(round(100*df[year == yr]$assort_in_in, 1), format = "f", digits = 1)
  X[i+1, "koutkout"] <- formatC(round(100*df[year == yr]$assort_out_out, 1), format = "f", digits = 1)
  X[i+1, "Source"] <- "This paper"

}

## FACTSET ##
filename <- file.path(dirdata, 'factset', 'factset_global_properties.csv')
df <- data.table(read.csv(filename))
df <- df[year == 2021]

X[5, "Dataset"] <- "FactSet"
X[5, "Year"] <- 2021
X[5, "kk"]       <- formatC(round(100*df$undirected_assort, 1), format = "f", digits = 1)
X[5, "kinkout"]  <- formatC(round(100*df$assort_in_out, 1), format = "f", digits = 1)
X[5, "koutkin"]  <- formatC(round(100*df$assort_out_in, 1), format = "f", digits = 1)
X[5, "kinkin"]   <- formatC(round(100*df$assort_in_in, 1), format = "f", digits = 1)
X[5, "koutkout"] <- formatC(round(100*df$assort_out_out, 1), format = "f", digits = 1)
X[5, "Source"] <- "This paper"

## LITERATURE ##

# taking the last quarter, largest component
X[6, "Dataset"] <- "Japan"
X[6, "Year"] <- "2006"
X[6, "kk"] <- (-7.5)
X[6, "kinkout"] <- "negative"
X[6, "koutkin"] <- "negative"
X[6, "Source"] <- "\\cite{fujiwara2010large}"


# taking the last quarter, largest component
X[7, "Dataset"] <- "Japan listed"
X[7, "Year"] <- "2016"
X[7, "kk"] <- (-21)
X[7, "Source"] <- "\\citet{krichene2019emergence}"


# taking the last quarter, largest component
X[8, "Dataset"] <- "West Bengal"
X[8, "Year"] <- "2016Q4"
X[8, "kk"] <- (-6.2)
X[8, "Source"] <- "\\citet{kumar2021distress}"


# ------------------------------------------------
#  Export
# ------------------------------------------------

XTAB <- xtable(X, align = "lllrrrrrl",
               label ="tab:assort")

addtorow <- list()
addtorow$pos <- list(0, 0, 0, 2, 8)
addtorow$command <- c('\\begin{table}[ht]
\\centering
\\begingroup\\small
\\caption{Assortativity coefficients}
\\begin{tabular}{llrrrrrl}
\\toprule ',
'Dataset & Year & $r_{k,k}$ & $r_{k^\\text{in},k^\\text{out}}$ & $r_{k^\\text{out},k^\\text{in}}$ & $r_{k^\\text{in},k^\\text{in}}$ & $r_{k^\\text{out},k^\\text{out}}$ & Source \\\\',
'\\midrule ',
'\\hdashline ',
'\\bottomrule \\multicolumn{8}{l}{ \\begin{minipage}{16cm}~\\\\%
\\justifying \\noindent
\\textit{Notes}: Assortativity coefficients as defined in \\citet{newman2003mixing}. $r_{k^\\text{in},k^\\text{out}}$ denotes the correlation between the suppliers\' in-degrees and the customers\' out-degrees, where each edge is a data point; other columns are interpreted in a similar way. All values are multiplied by 100.
\\end{minipage} } 
\\end{tabular} 
\\label{tab:assort}
\\endgroup 
\\end{table}')

filename = paste(dirOutput, '5_tab_assort.tex', sep = .Platform$file.sep)

print.xtable(XTAB,hline.after = NULL, size = "small", only.contents = T,
             file = filename, include.colnames = F, include.rownames = F,
             table.placement = "ht", sanitize.text.function = function(x){x},
             tabular.environment = F, floating = F,
             add.to.row	= addtorow)

print('Table 5: Assortativity coefficients exported.')