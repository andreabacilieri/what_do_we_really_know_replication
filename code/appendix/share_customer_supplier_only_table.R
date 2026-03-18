##########################################################################################################################
# This script produces Table C.2: Share of customer-only or supplier-only firms

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador_local_properties.csv
#     - factset_local_properties.csv
#     - hungary_local_properties.csv

# OUTPUT:
# to results/tables/
# (1) C2_tab_share_custsupp_only.tex

#######################################################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------
rm(list = ls())
library(data.table)
library(tls)
library(xtable)

# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder,'results','tables')
# Folder to the Input data
dirdata <- file.path(rootfolder,'data','analysis')


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

## FACTSET ##
filename <- file.path(dirdata, 'factset', 'factset_local_properties.csv')
dff <- data.table(read.csv(filename))

## ECUADOR ##
filename <- file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv')
dfe <- data.table(read.csv(filename))

## HUNGARY ##
filename <- file.path(dirdata, 'hungary', 'hungary_local_properties.csv')
dfh <- data.table(read.csv(filename))


# ------------------------------------------------------------------------------
#  Check Nodes with positive degree have positive strength
# ------------------------------------------------------------------------------

# Ecuador
if(all(which(dfe$outS == 0) == which(dfe$outD == 0)) == F){
  print('Mismatch between out-strength and out-degree: some of the firms in ecuador have positive out-strength but no outgoing link, or the reverse.')
}
if(all(which(dfe$inS == 0)  == which(dfe$inD == 0)) == F){
  print('Mismatch between in-strength and in-degree: some of the firms in ecuador have positive in-strength but no incoming link, or the reverse.')
}

# Hungary
if(all(which(dfh$outS == 0) == which(dfh$outD == 0)) == F){
  print('Mismatch between out-strength and out-degree: some of the firms in hungary have positive out-strength but no outgoing link, or the reverse.')
}
if(all(which(dfh$inS == 0)  == which(dfh$inD == 0)) == F){
  print('Mismatch between in-strength and in-degree: some of the firms in hungary have positive in-strength but no incoming link, or the reverse.')
}


# ------------------------------------------------------------------------------
#  Table 
# ------------------------------------------------------------------------------

# Initialize table
mymat <- array(dim = c(10, 5), data = NA)
colnames(mymat) <- c("Dataset", "Year", "Supplier-only", "Customer-only", "Source")

# Ecuador
toke <- which(dfe$year == 2015)
Ne <- dim(dfe[toke, ])[1]
mymat[1,] <- c("Ecuador", "2015",
               100*sum(dfe$inD[toke] == 0)/Ne,
               100*sum(dfe$outD[toke] == 0)/Ne,
               "This paper")
rm(toke,Ne,dfe)

# Hungary 2021
tokh <- which(dfh$year == 2021)
Nh <- dim(dfh[tokh, ])[1]
mymat[2,] <- c("Hungary", "2021",
               100*sum(dfh$inD [tokh] == 0)/Nh,
               100*sum(dfh$outD[tokh] == 0)/Nh,
               "This paper")
rm(tokh,Nh)

# Hungary 2019
tokh <- which(dfh$year == 2019)
Nh <- dim(dfh[tokh, ])[1]
mymat[3,] <- c("Hungary", "2019",
               100*sum(dfh$inD [tokh] == 0)/Nh,
               100*sum(dfh$outD[tokh] == 0)/Nh,
               "This paper")
rm(tokh,Nh)

# Hungary 2015
tokh <- which(dfh$year == 2015)
Nh <- dim(dfh[tokh, ])[1]
mymat[4,] <- c("Hungary", "2015",
               100*sum(dfh$inD [tokh] == 0)/Nh,
               100*sum(dfh$outD[tokh] == 0)/Nh,
               "This paper")
rm(tokh,Nh,dfh)

# FactSet 2023
tokf <- which(dff$year == 2021)
Nf <- dim(dff[tokf, ])[1]
mymat[5,] <- c("FactSet", "2021",
               100*sum(dff$inD [tokf] == 0)/Nf,
               100*sum(dff$outD[tokf] == 0)/Nf,
               "This paper")
rm(tokf,Nf,dff)

# Literature
# Belgium: Magerman et al p.16
# US listed: Wu and Bridge 2014 p. 17
# Estonia: Criscoulo et al. 2024 p. 16

mymat[6,] <- c("Belgium", "2012", 100*(1 - 79689/79788), 100*(1 - 67466/79788), "\\citet{magerman2016heterogeneous}")
mymat[7,] <- c("Costa Rica", "2008--2015", 9.7, 30.4, "\\citet{urena2018costa}")
#mymat[8,] <- c("Dominican Rep.", "2012--2017", 3.0, 18.0, "\\citet{cardoza2020worker} ")
mymat[8,] <- c("Estonia", "2015--2021", 11, 35--41, "\\citet{criscuolo_estonias_2024} ")
mymat[9,] <- c("Spain", "2009", 8.0, 24.0, "\\citet{jimenez2020production}")
mymat[10,]<- c("US listed", "04/2012--06/2013", (670/2152)*100, (587/2152)*100, "\\citet{wu2014supply} ")


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

mymat[, 3] <- round(as.numeric(mymat[, 3]), 1)
mymat[, 4] <- round(as.numeric(mymat[, 4]), 1)
# estonia gets added when rounding
mymat[8, 4] <- "35--41"
XTAB <- xtable(mymat,
               caption = "Share of customer-only ($k^\\text{out} = 0$) or supplier-only ($k^\\text{in} = 0$) firms",
               label = "tab:share_custsupp_only", align = "llrrrl")
addtorow <- list()
addtorow$pos <- list(-1, -1, -1, 2, 10)
addtorow$command <- c('\\toprule ',
                      'Dataset & Year & Supplier-only & Customer-only \\\\',
                      '\\midrule ',
                      '\\hdashline ',
                      '\\bottomrule \\multicolumn{5}{c}{ \\begin{minipage}{16cm}~\\\\%
  \\justifying \\noindent
  \\end{minipage} }\\\\')

filename <- file.path(dirOutput, 'C2_tab_share_custsupp_only.tex')

print.xtable(XTAB, file = filename, hline.after = NULL,
             sanitize.text.function = function(x){x},
             sanitize.rownames.function = function(x){x},
             include.rownames = F, include.colnames = F,
             add.to.row	= addtorow,
             caption.placement = "top")

print('Table C.2: Share of customer-only or supplier-only firms exported.')