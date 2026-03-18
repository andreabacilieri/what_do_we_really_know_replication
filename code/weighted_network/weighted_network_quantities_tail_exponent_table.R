################################################################################
# This script produces Table 7: Tail exponents for weighted network quantities

# INPUTS:
# Data:
# from data/analysis/
#       - ecuador/ecuador_tails_*.csv
#       - hungary/hungary_tails_*.csv
# * in {inS, outS, weights, influence}

# OUTPUT:
# to results/tables/
#       - 7_tab_tailExp_weighted_quantities.tex

################################################################################


# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)
library(xtable)
library(rstudioapi) # for rootfolder identification

# define data paths
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
dirData <- file.path(rootfolder, 'data/analysis') # input path
dirOutput <- file.path(rootfolder,'results/tables') # output path


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

## ECUADOR ##
in_filename <- file.path(dirData, 'ecuador/ecuador_tails_inS.csv')
out_filename <- file.path(dirData, 'ecuador/ecuador_tails_outS.csv')
weights_filename <- file.path(dirData, 'ecuador/ecuador_tails_weights.csv')
influence_filename <- file.path(dirData, 'ecuador/ecuador_tails_influence.csv')
df_in_ec <- data.table(read.csv(in_filename))
df_out_ec <- data.table(read.csv(out_filename))
df_weights_ec <- data.table(read.csv(weights_filename))
df_influence_ec <- data.table(read.csv(influence_filename))
c_ec <- "Ecuador"
y_ec <- "2015"

## HUNGARY ##
in_filename <- file.path(dirData, 'hungary/hungary_tails_inS.csv')
out_filename <- file.path(dirData, 'hungary/hungary_tails_outS.csv')
weights_filename <- file.path(dirData, 'hungary/hungary_tails_weights.csv')
influence_filename <- file.path(dirData, 'hungary/hungary_tails_influence.csv')
df_in_h <- data.table(read.csv(in_filename))
df_out_h <- data.table(read.csv(out_filename))
df_weights_h <- data.table(read.csv(weights_filename))
df_influence_h <- data.table(read.csv(influence_filename))
c_h <- "Hungary"
y_h1 <- "2021"
y_h2 <- "2019"
y_h3 <- "2015"


# ------------------------------------------------------------------------------
#  Table
# ------------------------------------------------------------------------------

mytab <- array(dim = c(4 + 3, 7))
colnames(mytab) <- c("Dataset", "Year", "Weights", "In-Strength", "Out-Strength", "Influence", "Source")

## ECUADOR ##
mytab[1,] <- c(c_ec, 
               y_ec, 
               formatC(df_weights_ec[df_weights_ec$year == y_ec]$CNS_alpha, format = "f", digits = 2),
               formatC(df_in_ec[df_in_ec$year == y_ec]$CNS_alpha, format = "f", digits = 2), 
               formatC(df_out_ec[df_out_ec$year == y_ec]$CNS_alpha, format = "f", digits = 2), 
               formatC(df_influence_ec[df_influence_ec$year == y_ec]$CNS_alpha, format = "f", digits = 2), 
               "This paper")

## HUNGARY ##
mytab[2,] <- c(c_h, 
               y_h1, 
               formatC(df_weights_h[df_weights_h$year == y_h1]$CNS_alpha, format = "f", digits = 2),
               formatC(df_in_h[df_in_h$year == y_h1]$CNS_alpha, format = "f", digits = 2), 
               formatC(df_out_h[df_out_h$year == y_h1]$CNS_alpha, format = "f", digits = 2), 
               formatC(df_influence_h[df_influence_h$year == y_h1]$CNS_alpha, format = "f", digits = 2), 
               "This paper")
mytab[3,] <- c(c_h, 
               y_h2, 
               formatC(df_weights_h[df_weights_h$year == y_h2]$CNS_alpha, format = "f", digits = 2),
               formatC(df_in_h[df_in_h$year == y_h2]$CNS_alpha, format = "f", digits = 2), 
               formatC(df_out_h[df_out_h$year == y_h2]$CNS_alpha, format = "f", digits = 2), 
               formatC(df_influence_h[df_influence_h$year == y_h2]$CNS_alpha, format = "f", digits = 2), 
               "This paper")
mytab[4,] <- c(c_h, 
               y_h3, 
               formatC(df_weights_h[df_weights_h$year == y_h3]$CNS_alpha, format = "f", digits = 2),
               formatC(df_in_h[df_in_h$year == y_h3]$CNS_alpha, format = "f", digits = 2), 
               formatC(df_out_h[df_out_h$year == y_h3]$CNS_alpha, format = "f", digits = 2), 
               formatC(df_influence_h[df_influence_h$year == y_h3]$CNS_alpha, format = "f", digits = 2), 
               "This paper")

## LITERATURE ##
mytab[5,] <- c("Belgium", "2012", NA,  NA, NA, 1.12, "\\citet{magerman2016heterogeneous}")
mytab[6,] <- c("Dutch bank 1", "2019", NA,  1.03, 1.05, NA, "\\citet{ialongo2021reconstructing}")
mytab[7,] <- c("Dutch bank 2", "2019", NA, 0.69, 0.72, NA, "\\citet{ialongo2021reconstructing}")


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

XTAB <- xtable(mytab)

addtorow <- list()
addtorow$pos <- list(0, 2, 7)
addtorow$command <- c('
\\begin{table}[htbp]
\\small
\\centering
\\caption{Tail exponents for weighted network quantities}
\\begin{tabular}{lrrrrrl}
\\toprule Dataset & Year & Weight & In-strength & Out-strength & Influence & Source \\\\
\\midrule ',
'\\hdashline ',
paste('\\bottomrule
\\multicolumn{7}{l}{\\textit{Notes}: Parameters estimated using \\texttt{plfit}.}
\\end{tabular}
\\label{tab:weighted_tailExp}
\\end{table}', 
sep=""))

output_filename <- paste(dirOutput, '7_tab_tailExp_weighted_quantities.tex', sep = .Platform$file.sep)

print.xtable(XTAB, file = output_filename, only.contents = T,
             hline.after = NULL,
             sanitize.text.function = function(x){x},
             sanitize.rownames.function = function(x){x},
             include.rownames = F, include.colnames = F,
             floating = F,
             tabular.environment = F,
             add.to.row	= addtorow,
             comment = F)

print('Table 7: Tail exponents for weighted network quantities exported.')