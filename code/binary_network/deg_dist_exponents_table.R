##########################################################################################################################
# This script produces Table 4: Power-law fit of the degree distributions

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_tails_inD.csv
#     - ecuador/ecuador_tails_outD.csv
#     - hungary/hungary_tails_inD.csv
#     - hungary/hungary_tails_outD.csv
#     - factset/factset_tails_inD.csv
#     - factset/factset_tails_outD.csv

# OUTPUT:
# to results/tables/
# (1) 4_tab_power_law_fit_degree_dist.tex

#######################################################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)
library(xtable)
library(rstudioapi) # for rootfolder identification

# define data paths
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
dirData <- file.path(rootfolder, 'data/analysis') # input path
dirOutput <- file.path(rootfolder,'results/tables') # output path


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

## ECUADOR ##
in_filename <- file.path(dirData, 'ecuador/ecuador_tails_inD.csv')
out_filename <- file.path(dirData, 'ecuador/ecuador_tails_outD.csv')
df_in_ec <- data.table(read.csv(in_filename))
df_out_ec <- data.table(read.csv(out_filename))
c_ec <- "Ecuador"
y_ec <- "2015"

## HUNGARY ##
in_filename <- file.path(dirData, 'hungary/hungary_tails_inD.csv')
out_filename <- file.path(dirData, 'hungary/hungary_tails_outD.csv')
df_in_h <- data.table(read.csv(in_filename))
df_out_h <- data.table(read.csv(out_filename))
c_h <- "Hungary"
y_h1 <- "2021"
y_h2 <- "2019"
y_h3 <- "2015"

# FACTSET ##
in_filename <- file.path(dirData, 'factset/factset_tails_inD.csv')
out_filename <- file.path(dirData, 'factset/factset_tails_outD.csv')
df_in_fs <- data.table(read.csv(in_filename))
df_out_fs <- data.table(read.csv(out_filename))
c_fs <- "FactSet"
y_fs <- "2021"


# ------------------------------------------------------------------------------
#  Table
# ------------------------------------------------------------------------------

mytab <- array(dim=c(5 + 12, 7))
colnames(mytab) <- c("Dataset", "Year", "In-degree", "Out-degree",  "Estimation\\nmethod", "Data type", "Source")

## ECUADRO, HUNGARY AND FACTSET ##
mytab[1,] <- c(c_ec,
               y_ec,
               formatC(df_in_ec[df_in_ec$year == y_ec]$CNS_alpha, format = "f", digits = 2),
               formatC(df_out_ec[df_out_ec$year == y_ec]$CNS_alpha, format = "f", digits = 2),
               "\\texttt{plfit}",
               "VAT",
               "This paper")
mytab[2,] <- c(c_h,
               y_h1,
               formatC(df_in_h[df_in_h$year == y_h1]$CNS_alpha, format = "f", digits = 2),
               formatC(df_out_h[df_out_h$year == y_h1]$CNS_alpha, format = "f", digits = 2),
               "\\texttt{plfit}",
               "VAT",
               "This paper")
mytab[3,] <- c(c_h,
               y_h2,
               formatC(df_in_h[df_in_h$year == y_h2]$CNS_alpha, format = "f", digits = 2),
               formatC(df_out_h[df_out_h$year == y_h2]$CNS_alpha, format = "f", digits = 2),
               "\\texttt{plfit}",
               "VAT",
               "This paper")
mytab[4,] <- c(c_h,
               y_h3,
               formatC(df_in_h[df_in_h$year == y_h3]$CNS_alpha, format = "f", digits = 2),
               formatC(df_out_h[df_out_h$year == y_h3]$CNS_alpha, format = "f", digits = 2),
               "\\texttt{plfit}",
               "VAT",
               "This paper")
mytab[5,] <- c(c_fs,
               y_fs,
               formatC(df_in_fs[df_in_fs$year == y_fs]$CNS_alpha, format = "f", digits = 2),
               formatC(df_out_fs[df_out_fs$year == y_fs]$CNS_alpha, format = "f", digits = 2),
               "\\texttt{plfit}",
               "Financial reporting",
               "This paper")

## LITERATURE  ##
mytab[6,] <- c("Japan", "2005", sprintf("%.2f", 1.37), sprintf("%.2f", 1.46), "OLS$_{\\text{CCDF}}$", "Business services", "\\citet{bernard2019production}")
mytab[7,] <- c("Japan", "2005", sprintf("%.2f", 1.37), sprintf("%.2f", 1.25), "\\texttt{plfit}", "Business services", "\\citet{ohnishi2010network}")
mytab[8,] <- c("Japan", "2006", sprintf("%.2f", 1.35), sprintf("%.2f", 1.26), "\\texttt{plfit}", "Business services", "\\citet{fujiwara2010large}")
mytab[9,] <- c("Dutch bank 1", "2019", sprintf("%.2f", 1.44), sprintf("%.2f", 1.28), "\\texttt{plfit}", "Payment system", "\\citet{ialongo2021reconstructing}")
mytab[10,] <- c("Dutch bank 2", "2019", sprintf("%.2f", 1.77), sprintf("%.2f", 1.31), "\\texttt{plfit}", "Payment system", "\\citet{ialongo2021reconstructing}")
mytab[11,] <- c("Chile", "2019", sprintf("%.2f", 0.28), sprintf("%.2f", 0.40), "" , "VAT", "\\citet{grigoli2023idiosyncratic}")
mytab[12,] <- c("Dominican Rep.", "2019", sprintf("%.2f", 0.30), sprintf("%.2f", 0.45), "\\texttt{paretofit}" , "VAT", "\\citet{cardoza_worker_2025}")
mytab[13,] <- c("Costa Rica", "2008", sprintf("%.2f", 0.58), sprintf("%.2f", 0.73), "" , "VAT", "\\citet{urena2018costa}")
mytab[14,] <- c("US listed", "\\makecell[l]{04/2012- \\\\ 06/2013}", sprintf("%.2f", 2.76), sprintf("%.2f", 1.88), "\\texttt{plfit}", "Financial reporting", "\\citet{wu2014supply}")
mytab[15,] <- c("US listed", "1979-2007", sprintf("%.2f", 1.00), NA, "" , "Financial reporting", "\\citet{atalay2011network}")
mytab[16,] <- c("US listed", "1978-2013", sprintf("%.2f", 1.25), sprintf("%.2f", 1.44), "" , "Financial reporting", "\\citet{barrot2016input}")
mytab[17,] <- c("FactSet US", "2016", sprintf("%.2f", 0.97), sprintf("%.2f", 0.83), "\\makecell[l]{Rank 1/2 \\\\ estimator}", "Financial reporting", "\\citet{taschereau2022cascades}")


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

XTAB <- xtable(mytab)

addtorow <- list()
addtorow$pos <- list(0, 2, 17)
addtorow$command <- c('
\\begin{table}[htbp]
\\def\\arraystretch{1}
\\centering
\\caption{Power-law fit of the degree distributions}
\\resizebox{\\textwidth}{!}{%
\\begin{tabular}{l l r r l l l}
\\toprule
& & &  &  Estimation & &\\\\
Dataset & Year  & In-degree & Out-degree & method  & Data type & Source \\\\
\\midrule ',
'\\hdashline ',
paste('\\bottomrule
\\multicolumn{7}{l}{%
\\begin{minipage}{20.4cm}
\\vspace{0.1cm}
\\noindent
\\large
\\textit{Notes}: The in-degree is the number of suppliers and the out-degree the number of customers.  Most studies use \\texttt{plfit} \\citep{clauset2009power}.  \\citet{bernard2019production} regress the log CCDF on the log degree (OLS$_{\\text{CCDF}}$).  \\citet{cardoza_worker_2025} fit a pure Pareto using Stata\'s \\texttt{paretofit}.  A few other studies also appear to fit a pure Pareto \\citep{barrot2016input,urena2018costa,grigoli2023idiosyncratic}.  \\citet{taschereau2022cascades} uses the rank 1/2 estimator of \\citet{gabaix2011rank}. For the data type taxonomy,  see Table~\\ref{tab:data_set_types}.
\\end{minipage}
}\\
\\end{tabular}
}
\\label{tab:binary_power_law_main}
\\end{table}',
sep = ""))

output_filename <- paste(dirOutput, '4_tab_power_law_fit_degree_dist.tex', sep = .Platform$file.sep)
print.xtable(XTAB, file = output_filename, only.contents = T,
             hline.after = NULL,
             sanitize.text.function = function(x){x},
             sanitize.rownames.function = function(x){x},
             include.rownames = F, include.colnames = F,
             floating = F,
             tabular.environment = F,
             add.to.row	= addtorow,
             comment = F)


print('Table 4: Power-law fit of the degree distributions exported.')