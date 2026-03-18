##################################################################################
# This script has a function that produces Figure B.1: Binned scatter plots for the conditional relations.

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador_local_properties.csv
#     - hungary_local_properties.csv
# from code/utils/
#     - fun_ccdf_tls_network.R


# OUTPUT:
# to results/figures/appendix/
# (1) B1_fig_quantile_regs.pdf

##################################################################################

#' Makes Figure B.1: binned scatter plot for the conditional relations

#' @param NBINS A scalar specifying the number of bins, default is 100; 
#'             the default is as in the paper, but this can be reduced when using smaller synthetic datasets

#' @return NONE, but saves a pdf file with the plot

make_plot_binscatter_conditional_rel <- function(NBINS = 100){
  
  # ------------------------------------------------------------------------------
  #  Set environment/parameters
  # ------------------------------------------------------------------------------

  rm(list = ls()[-which(ls() == "NBINS")])
  library(data.table)
  library(tls)
  library(xtable)
  library(binsreg)

  # Rootfolder
  rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
  # Folder to store output
  dirOutput <- file.path(rootfolder, 'results', 'figures')
  # Folder for the input data
  dirdata <- file.path(rootfolder, 'data', 'analysis')

  source(file.path(rootfolder, 'code', 'utils', 'fun_ccdf_tls_network.R'))

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
  #  Compute for binned scatter plot
  # ------------------------------------------------------------------------------

  # List of regressions
  yvarlist <- c("inS", "outS", "inD", "outD", "inS", "outS", "inD", "outD")
  xvarlist <- c("inD", "outD", "inS", "outS", "outS", "inS", "outD", "inD")
  yvarlistlabel <- c("In-strength", "Out-strength", "In-degree", "Out-degree", "In-strength", "Out-strength", "In-degree", "Out-degree")
  xvarlistlabel <- c("In-degree", "Out-degree", "In-strength", "Out-strength", "Out-strength", "In-strength", "Out-degree", "In-degree")

  # need to compute everything before plotting
  # because "noplot = T" option makes the $data.plot$.. empty
  LMh <- BLMh <- BLMh_90 <- BLMh_10 <- list()
  LMe <- BLMe <- BLMe_90 <- BLMe_10 <- list()
  yh <- xh <- ye <- xe <- list()

  for(u in 1:length(yvarlist)){
    yvar <- yvarlist[[u]]
    xvar <- xvarlist[[u]]

    ## HUNGARY ##
    tok <- which(dfh$year == 2021 & dfh[[yvar]] > 0 & dfh[[xvar]] > 0)
    yh[[u]] <- log10(dfh[[yvar]][tok])
    xh[[u]] <- log10(dfh[[xvar]][tok])

    LMh[[u]] <- lm(yh[[u]] ~ xh[[u]])
    BLMh[[u]] <- binsglm(yh[[u]], xh[[u]], nbins = NBINS, noplot = F)
    BLMh_90[[u]] <- binsqreg(yh[[u]], xh[[u]], nbins = NBINS, noplot = F, quantile = 0.90)
    BLMh_10[[u]] <- binsqreg(yh[[u]], xh[[u]], nbins = NBINS, noplot = F, quantile = 0.10)

    ## ECUADOR ##
    tok <- which(dfe$year == 2015 & dfe[[yvar]] > 0 & dfe[[xvar]] > 0)
    ye[[u]] <- log10(dfe[[yvar]][tok])
    xe[[u]] <- log10(dfe[[xvar]][tok])

    LMe[[u]] <- lm(ye[[u]] ~ xe[[u]])
    BLMe[[u]] <- binsglm(ye[[u]], xe[[u]], nbins = NBINS, noplot = F)
    BLMe_90[[u]] <- binsqreg(ye[[u]], xe[[u]], nbins = NBINS, noplot = F, quantile = 0.90)
    BLMe_10[[u]] <- binsqreg(ye[[u]], xe[[u]], nbins = NBINS, noplot = F, quantile = 0.10)

  }


  # ------------------------------------------------------------------------------
  #  Binned scatter plot
  # ------------------------------------------------------------------------------

  pdf(file = paste0(dirOutput, "/B1_fig_quantile_regs.pdf"), width = 6, height = 6)

  par(mar=c(2.5, 2.5, 0.8, 0.8), las = 1, mfrow = c(4, 4), mgp = c(1.5, 0.65, 0))

  for(u in 1:length(yvarlist)){
    
    ## HUNGARY ##
    plot(fit ~ x, type = "o",
     data = BLMh[[u]]$data.plot$`Group Full Sample`$data.dots,
     pch = 16, cex = 0.7, col = 1,
     ylim = range(yh[[u]]), xlim = range(xh[[u]]),
     ylab = paste0("", yvarlistlabel[u]),
     xlab = paste0("", xvarlistlabel[u]),
     main = "", cex.main = 1,
     axes = FALSE)

    # Generate pretty axis ticks with 4 labels including 0
    x_ticks <- pretty(xh[[u]], n = 4)
    y_ticks <- pretty(yh[[u]], n = 4)

    # Add custom x-axis with 0 decimal places
    axis(1, at = x_ticks, labels = format(x_ticks, nsmall = 0, trim = TRUE))

    # Add custom y-axis with 0 decimal places
    axis(2, at = y_ticks, labels = format(y_ticks, nsmall = 0, trim = TRUE))

    box()


    points(fit ~ x, type = "o",
           data = BLMh_90[[u]]$data.plot$`Group Full Sample`$data.dots,
           pch = 17, cex = 0.7, col = 4)

    points(fit ~ x, type = "o",
           data = BLMh_10[[u]]$data.plot$`Group Full Sample`$data.dots,
           pch = 17, cex = 0.7, col = 4)

    abline(a = LMh[[u]]$coeff[1], b = LMh[[u]]$coeff[2], lwd = 1, col = 1)

    legend("topleft", bty = "n", legend = "Hungary")

    ## ECUADOR ##
    plot(fit ~ x, type = "o",
       data = BLMe[[u]]$data.plot$`Group Full Sample`$data.dots,
       pch = 16, cex = 0.7, col = 1,
       ylim = range(ye[[u]]), xlim = range(xe[[u]]),
       ylab = paste0("", yvarlistlabel[u]),
       xlab = paste0("", xvarlistlabel[u]),
       main = "", cex.main = 1,
       axes = FALSE)

    # Generate pretty axis ticks with 4 labels including 0
    x_ticks <- pretty(xe[[u]], n = 4)
    y_ticks <- pretty(ye[[u]], n = 4)

    # Add custom x-axis with 0 decimal places
    axis(1, at = x_ticks, labels = format(x_ticks, nsmall = 0, trim = TRUE))

    # Add custom y-axis with 0 decimal places
    axis(2, at = y_ticks, labels = format(y_ticks, nsmall = 0, trim = TRUE))

    box()

    points(fit ~ x, type = "o",
           data = BLMe_90[[u]]$data.plot$`Group Full Sample`$data.dots,
           pch = 17, cex = 0.7, col = 4)

    points(fit ~ x, type = "o",
           data = BLMe_10[[u]]$data.plot$`Group Full Sample`$data.dots,
           pch = 17, cex=0.7, col = 4)

    abline(a = LMe[[u]]$coeff[1], b = LMe[[u]]$coeff[2], lwd = 1, col = 1)

    legend("topleft", bty = "n", legend = "Ecuador")

  }

  dev.off()
  print("Figure 'B1: Binned scatter plots for the conditional relations' exported")
}
