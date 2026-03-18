################################################################################
# This script
# (1) produces Table B.1: Covariance matrix keeping only nodes with pairwise positive values
# (2) produces Table B.2: Covariance matrix keeping only nodes which have positive values for all four metrics
# (3) calculates Example 1 and 2 in Section B.2.2

# INPUTS:
# from data/analysis/
#   - hungary_local_properties.csv
#   - ecuador_local_properties.csv
# from code/utils/
#   - fun_ccdf_tls_network.R (for the tls function)

# OUTPUT:
# to results/
# (1) tables/
#   1a. B1_tab_cov.tex
#   1b. B2_tab_cov_pairwise.tex
# (2) B3_example_1_2.tex

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
# load packages
library(data.table)
library(tls)
library(xtable)

# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Folder to store output
dirOutput <- file.path(rootfolder, 'results', 'tables')
# Folder for the input data
dirdata <- file.path(rootfolder, 'data', 'analysis')

source(paste0(rootfolder, "code/utils/fun_ccdf_tls_network.R"))

# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

# Hungary
filename <- file.path(dirdata, 'hungary', 'hungary_local_properties.csv')
dfh <- fread(filename, data.table=F)

# Ecuador
filename <- file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv')
dfe <- fread(filename, data.table = F)

# ------------------------------------------------------------------------------
#  Covariance matrices : Remove nodes with ANY zero entry
# ------------------------------------------------------------------------------

tokc <- c("outD", "inD", "outS", "inS")

## ECUADOR ##
toke <- which(dfe$year == 2015 & dfe$inD > 0 & dfe$outD > 0 & dfe$inS > 0 & dfe$outS > 0)
ec_mat <- as.matrix(dfe[toke, tokc])
ec_mat <- apply(ec_mat, 2, log)
ec_cov <- cov(ec_mat, use = "everything")
ec_mean <- apply(ec_mat, 2, mean, na.rm = T)

## HUNGARY ##
tokh <- which(dfh$year == 2021 & dfh$inD > 0 & dfh$outD > 0 & dfh$inS > 0 & dfh$outS > 0)
hu_mat <- as.matrix(dfh[tokh, tokc])
hu_mat <- apply(hu_mat, 2, log)
hu_cov <- cov(hu_mat, use = "everything")
hu_mean <- apply(hu_mat, 2, mean, na.rm = T)

## EXPORT ##
mymat <- cbind(ec_cov, hu_cov)
mymat <- rbind(mymat,c(ec_mean, hu_mean))
rn <- c("$k^{\\text{out}}$", "$k^{\\text{in}}$", "$s^{\\text{out}}$", "$s^{\\text{in}}$")
colnames(mymat) <- c(rn, rn)
rownames(mymat) <- c(rn, "Mean")
XTAB <- xtable(mymat,
               caption = "Covariance matrix keeping only nodes which have positive values for all four metrics",
               label = "tab:cov_mat", align = "lcccccccc", digits = 2)
addtorow <- list()
addtorow$pos <- list(-1, -1, -1, 4, 5)
addtorow$command <- c('\\toprule ',
                      '& \\multicolumn{4}{c}{Ecuador (2015)} & \\multicolumn{4}{c}{Hungary (2021)} \\\\',
                      '\\midrule ',
                      '\\midrule ',
                      '\\bottomrule \\multicolumn{9}{c}{ \\begin{minipage}{10cm}~\\\\%
    \\justifying \\noindent
    \\textit{Notes}: All variables are log-transformed.
    \\end{minipage} }\\\\')

filename <- file.path(dirOutput, 'B2_tab_cov.tex')
print.xtable(XTAB,
             file = filename,
             hline.after = NULL,
             sanitize.text.function = function(x){x},
             sanitize.rownames.function = function(x){x},
             include.rownames = T, add.to.row	= addtorow,
             caption.placement = "top")

rm(addtorow, ec_mat, hu_mat, mymat, XTAB, ec_mean, hu_mean, toke, tokh)

print('Table B.2: Covariance matrix keeping only nodes which have positive values on all 4 metrics exported.')


# ------------------------------------------------------------------------------
#  Covariance matrices : Remove nodes with PAIRWISE zero entry
# ------------------------------------------------------------------------------

## ECUADOR ##
toke <- which(dfe$year == 2015)
ec_mat <- as.matrix(dfe[toke, tokc])
ec_mat[ec_mat == 0] <- NA
ec_mat <- apply(ec_mat, 2, log)
ec_cov_pco <- cov(ec_mat, use = "pairwise.complete.obs")
ec_mean_pco <- apply(ec_mat, 2, mean, na.rm = T)

## HUNGARY ##
tokh <- which(dfh$year == 2021)
hu_mat <- as.matrix(dfh[tokh, tokc])
hu_mat[hu_mat == 0] <- NA
hu_mat <- apply(hu_mat, 2, log)
hu_cov_pco <- cov(hu_mat, use = "pairwise.complete.obs")
hu_mean_pco <- apply(hu_mat, 2, mean, na.rm = T)

## EXPORT ##
mymat <- cbind(ec_cov_pco, hu_cov_pco)
mymat <- rbind(mymat, c(ec_mean_pco, hu_mean_pco))
rn <- c("$k^{\\text{out}}$", "$k^{\\text{in}}$", "$s^{\\text{out}}$", "$s^{\\text{in}}$")
colnames(mymat) <- c(rn, rn)
rownames(mymat) <- c(rn, "Mean")
XTAB <- xtable(mymat,
               caption = "Covariance matrix keeping only nodes with pairwise positive values",
               label = "tab:cov_mat_pairwise", align = "lcccccccc", digits = 2)
addtorow <- list()
addtorow$pos <- list(-1, -1, -1, 4, 5)
addtorow$command <- c('\\toprule ',
                      '& \\multicolumn{4}{c}{Ecuador (2015)} & \\multicolumn{4}{c}{Hungary (2021)} \\\\',
                      '\\midrule ',
                      '\\midrule ',
                      '\\bottomrule \\multicolumn{9}{c}{ \\begin{minipage}{10cm}~\\\\%
    \\justifying \\noindent
    \\textit{Notes}: All variables are log-transformed. The row \`Mean\' shows the average of the log-transform of the positive values.
    \\end{minipage} }\\\\')

filename <- file.path(dirOutput, 'B1_tab_cov_pairwise.tex')
print.xtable(XTAB,
             file = filename,
             hline.after = NULL,
             sanitize.text.function = function(x){x},
             sanitize.rownames.function = function(x){x},
             include.rownames = T,
             add.to.row	= addtorow,
             caption.placement = "top")

print('Table B.1: Covariance matrix keeping only nodes with pairwise positive values exported.')


# ------------------------------------------------------------------------------
#  Examples from Appendix B.2.2
# ------------------------------------------------------------------------------

# In and Out-strength total least squares
# the first eigenvector of the covariance matrix
# NOTE: take the cov matrix that removes nodes with any non-zero
X <- ec_cov[c("outS", "inS"), c("outS", "inS")]
ev <- eigen(X)$vectors[, 1]

## is equal to the TLS estimate
tok <- which(dfe$year == 2015 & dfe$inS > 0 & dfe$outS > 0)
y <- log(dfe$outS[tok])
x <- log(dfe$inS[tok])

# gather results in df
df <- data.table(country = 'Ecuador',
                 year = 2015,
                 reported_hardcoded = "0.93",
                 reported = tlsfn(y, x)[2],
                 check = ev[1]/ev[2],
                 example = 1,
                 notes = "positive in- and out- strength")


# ------------------------------------------------------------------------------
#  Regressions
# ------------------------------------------------------------------------------

tok <- which(dfh$year == 2021 & dfh$outD > 0 & dfh$outS > 0)
# prepare for regression
y <- log(dfh$outS[tok])
x <- log(dfh$outD[tok])

# NOTE: take the cov matrix that removes only nodes with pairwise non-zero
betahat <- hu_cov_pco["outS", "outD"] / hu_cov_pco["outD", "outD"]

df_append <- data.table(country = 'Hungary',
                        year = 2021,
                        reported_hardcoded = "1.044",
                        reported = coef(lm(y~x))[2],
                        check = betahat,
                        example = 2,
                        notes = "positive out-degree and out-strength")
df <- rbind(df, df_append)

# print output of example
print('Results of example 1 and 2 Section B.2.2')
print(df)