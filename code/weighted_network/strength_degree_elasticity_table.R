################################################################################
# This script produces Table 8: Strength-Degree elasticities

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_local_properties.csv
#     - hungary/hungary_local_properties.csv

# OUTPUT:
# to results/tables/
#     - 8_tab_strength_degree_elasticities.tex

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(xtable)
library(data.table)

# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'tables')
# Folder to the Input data
dirdata <- file.path(rootfolder, 'data', 'analysis')


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

# Hungary
filename <- file.path(dirdata, 'hungary', 'hungary_local_properties.csv')
dfh <- as.data.frame(fread(filename))

# Ecuador
filename <- file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv')
dfe <- as.data.frame(fread(filename))


# ------------------------------------------------------------------------------
#  Table
# ------------------------------------------------------------------------------

# List of regressions
yvarlist <- c("outS", "outD", "inS", "inD")
xvarlist <- c("outD", "outS", "inD", "inS")
yvarlistlabel <- c("Out-Strength", "Out-Degree", "In-Strength", "In-Degree" )
xvarlistlabel <- c("Out-Degree", "Out-Strength", "In-Degree", "In-Strength")

# compute everything before making table
LMh21 <- LMh19 <- LMh15 <- LMe <- list()

for(u in 1:length(yvarlist)){
  yvar <- yvarlist[[u]]
  xvar <- xvarlist[[u]]

  # Hungary 2021
  tok <- which(dfh$year == 2021 & dfh[[yvar]] > 0 & dfh[[xvar]] > 0)
  LMh21[[u]] <- summary(lm(log(dfh[[yvar]][tok]) ~ log(dfh[[xvar]][tok])))
  # Hungary 2019
  tok <- which(dfh$year == 2019 & dfh[[yvar]] > 0 & dfh[[xvar]] > 0)
  LMh19[[u]] <- summary(lm(log(dfh[[yvar]][tok]) ~ log(dfh[[xvar]][tok])))
  # Hungary 2015
  tok <- which(dfh$year == 2015 & dfh[[yvar]] > 0 & dfh[[xvar]] > 0)
  LMh15[[u]] <- summary(lm(log(dfh[[yvar]][tok]) ~ log(dfh[[xvar]][tok])))
  ## Ecuador 2015
  tok <- which(dfe$year == 2015 & dfe[[yvar]] > 0 & dfe[[xvar]] > 0)
  LMe[[u]] <- summary(lm(log(dfe[[yvar]][tok]) ~ log(dfe[[xvar]][tok])))

}

mytab <- array(dim = c(10+10+2, 6), data = NA)

# Sales and customers
mytab[1,] <- c("Ecuador", "2015", LMe[[1]]$coefficients[2,1], LMe[[2]]$coefficients[2,1], LMe[[1]]$r.squared, "This paper")
mytab[2,] <- c("Hungary", "2021", LMh21[[1]]$coefficients[2,1], LMh21[[2]]$coefficients[2,1], LMh21[[1]]$r.squared, "This paper")
mytab[3,] <- c("Hungary", "2019", LMh19[[1]]$coefficients[2,1], LMh19[[2]]$coefficients[2,1], LMh19[[1]]$r.squared, "This paper")
mytab[4,] <- c("Hungary", "2015", LMh15[[1]]$coefficients[2,1], LMh15[[2]]$coefficients[2,1], LMh15[[1]]$r.squared, "This paper")
mytab[5,] <- c("Belgium", "2014", 0.77, "", 0.35, "\\citet{bernard2021origins}")
mytab[6,] <- c("Chile", "2018--2019", "", 0.42, 0.46, "\\citet{arkolakis2023spatial}")
mytab[7,] <- c("Costa Rica", "2008--2015", 1.2, "", "", "\\citet{urena2018costa}")
mytab[8,] <- c("Estonia", "2021", 1.10, 0.46, 0.51, "\\citet{criscuolo_estonias_2024}")
mytab[9,] <- c("Estonia", "2015", 1.14, 0.47, 0.53, "\\citet{criscuolo_estonias_2024}")
mytab[10,] <- c("Turkey", "2015", "", 0.44, 0.33, "\\citet{demir2023ring}")

# Expenses and suppliers
mytab[11,] <- c("Ecuador", "2015", LMe[[3]]$coefficients[2,1], LMe[[4]]$coefficients[2,1], LMe[[3]]$r.squared, "This paper")
mytab[12,] <- c("Hungary", "2021", LMh21[[3]]$coefficients[2,1], LMh21[[4]]$coefficients[2,1], LMh21[[3]]$r.squared, "This paper")
mytab[13,] <- c("Hungary", "2019", LMh19[[3]]$coefficients[2,1], LMh19[[4]]$coefficients[2,1], LMh19[[3]]$r.squared, "This paper")
mytab[14,] <- c("Hungary", "2015", LMh15[[3]]$coefficients[2,1], LMh15[[4]]$coefficients[2,1], LMh15[[3]]$r.squared, "This paper")
mytab[15,] <- c("Chile", "2018--2019", "", 0.45, 0.20, "\\citet{arkolakis2023spatial}")
mytab[16,] <- c("Costa Rica", "2008--2015", 0.89, "", "", "\\citet{urena2018costa}")
mytab[17,] <- c("Estonia", "2021", 1.47, 0.50, 0.74, "\\citet{criscuolo_estonias_2024}")
mytab[18,] <- c("Estonia", "2015", 1.46, 0.51, 0.74, "\\citet{criscuolo_estonias_2024}")
mytab[19,] <- c("Turkey", "2015", "", 0.58, 0.61, "\\citet{demir2023ring}")
mytab[20,] <- c("Japan", "2005--2010", "", 0.33, "", "\\citet{bernard2019production}")

# Sales and number of partners
mytab[21,] <- c("Chile", "2014--2020", "", 0.33, 0.25, "\\citet{miranda2023business}")
mytab[22,] <- c("Japan", "2005", 1.3, "", "", "\\citet{watanabe2013relations}")

# clean up
mytab[, 3] <- formatC(as.numeric(mytab[,3]), format = "f", digits = 2)
mytab[, 4] <- formatC(as.numeric(mytab[,4]), format = "f", digits = 2)
mytab[, 5] <- formatC(as.numeric(mytab[,5]), format = "f", digits = 2)
mytab[mytab == " NA"] <- ""


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

XTAB <- xtable(mytab, align =  "llrcccl", digits = 2,
               caption = "Strength-degree elasticities",
               label = "table:reg_strength_degrees")

addtorow <- list()
addtorow$pos <- list(
  -1, -1, -1, -1, -1,  # header
  2,                  # AFTER Hungary 2021 (block 1)  ← NEW
  10, 10, 10,
  12,                 # AFTER Hungary 2021 (block 2)  ← NEW
  20, 20, 20,
  22
)
addtorow$command <- c('\\toprule',
                      'Dataset & \\multicolumn{1}{c}{Year} & $\\ln s | \\ln k$ & $\\ln k | \\ln s$ & $R^2$  & Source\\\\',
                      '\\midrule',
                      '\\multicolumn{6}{l}{\\textit{Network sales \\& number of customers}} \\\\',
                      '\\midrule ',
                      '\\hdashline ',
                      '\\midrule ',
                      '\\multicolumn{6}{l}{\\textit{Network expenses \\& number of suppliers}} \\\\',
                      '\\midrule ',
                      '\\hdashline ',
                      '\\midrule ',
                      '\\multicolumn{6}{l}{\\textit{Sales \\& number of partners}} \\\\',
                      '\\midrule ',
                      '\\bottomrule \\multicolumn{6}{c}{ \\begin{minipage}{13cm}~\\\\%
  \\justifying \\noindent
  \\textit{Notes}: OLS regressions of either the log of strength on the log of degree (column $\\ln s | \\ln k$) or the log of degree on the log of strength (column $\\ln k | \\ln s$).  All the observations equal to zero are dropped.  Japan is the only non-VAT dataset.  \\citet{bernard2021origins} use network sales and add 4-digit industry fixed effects.  \\citet{urena2018costa} use total sales, demeaned by industry, and keep only firms in the 3 highest volume industries.  \\citet{arkolakis2023spatial} use total sales and include year and state fixed effects.  \\citet{demir2023ring} consider manufacturing firms and use sales for both the in-degree and out-degree regressions.  \\citet{miranda2023business} use total sales and firms with more than 5 employees.  In \\citet{miranda2023business} and \\citet{watanabe2013relations}, the degree is the number of suppliers and customers.
  \\end{minipage} }\\\\')

filename <- paste(dirOutput, '8_tab_strength_degree_elasticities.tex', sep = .Platform$file.sep)

print.xtable(XTAB, hline.after = NULL, size = "small",
             file = filename,
             include.colnames = F, include.rownames = F,
             table.placement = "ht", sanitize.text.function = function(x){x},
             add.to.row	= addtorow)

print('Table 8: Strength-Degree elasticities exported.')