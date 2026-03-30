################################################################################
# This script produces Figure 3: Empirical CCDF of number of
# suppliers (in-degree) and number of customers (out-degree) over time for the 3 networks we study

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador_local_properties.csv
#     - factset_local_properties.csv
#     - hungary_local_properties.csv
# from code/utils/
#     - fun_ccdf_tls_network.R
# from code/utils_plots/
#     - set_size.R

# OUTPUT:
# to results/figures/
# (1) 3_fig_degs_over_time.pdf

################################################################################


# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(ggpubr)
library(ggplot2)
library(igraph)
library(data.table)
library(latex2exp)

# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]

# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Folder to the Input data
dirdata <- file.path(rootfolder, 'data', 'analysis')


# Get function for calculating empirical CCDF
function_name <- paste(rootfolder, 'code', 'utils', sep = .Platform$file.sep)
function_name <- paste(function_name, 'fun_ccdf_tls_network.R', sep = .Platform$file.sep)
source(function_name)

# For plots
width_LaTeX = 418.25368  # in pt
function_name <- paste(rootfolder, 'code', 'utils_plots', sep = .Platform$file.sep)
function_name <- paste(function_name, 'set_size.R', sep = .Platform$file.sep)
source(function_name)


# ------------------------------------------------------------------------------
# Load and prepare data
# ------------------------------------------------------------------------------

## HUNGARY ##
filename <- file.path(dirdata,'hungary', 'hungary_local_properties.csv')
df_degH <- data.table(read.csv(filename))
df_degH <- df_degH[order(year)]
df_degH <- df_degH[, list(inD, outD, year)]
df_degH <- na.omit(df_degH)
years <- sort(unique(df_degH$year))

# CCDF in-degree
for (yr in years){

  data <- empirical_ccdf(df_degH[year == yr, ]$inD)
  degree_ <- data$degree
  density_ <- data$ccdf

  if (yr == 2015){

    df_dens_in <- data.frame(year = yr,
                              country = 'Hungary',
                              inD = degree_,
                              density_in = density_)
  }else{
    df_ <- data.frame(year = yr,
                      country = 'Hungary',
                      inD = degree_,
                      density_in = density_)
    
    df_dens_in <- rbind(df_dens_in, df_)
  }

}

rm(df_)

# CCDF out-degree
for (yr in years){

  data <- empirical_ccdf(df_degH[year == yr, ]$outD)
  degree_ <- data$degree
  density_ <- data$ccdf

  if (yr == 2015){

    df_dens_out <- data.frame(year = yr,
                              country = 'Hungary',
                              outD = degree_,
                              density_out = density_)
  }else{
    
    df_ <- data.frame(year = yr,
                      country = 'Hungary',
                      outD = degree_,
                      density_out = density_)
    df_dens_out <- rbind(df_dens_out, df_)
    }

}

rm(list=c('df_'))

## ECUADOR ##
filename <- file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv')
df_degE <- data.table(read.csv(filename))
df_degE <- df_degE[,list(inD, outD, year)]
df_degE <- na.omit(df_degE)

# CCDF in-degree
years = sort(unique(df_degE$year))
for (yr in years){

  data <- empirical_ccdf(df_degE[year == yr, ]$inD)
  degree_ <- data$degree
  density_ <- data$ccdf

  if (yr == 2007){

    df <- data.frame(year = yr,
                      country = 'Ecuador',
                      inD = degree_,
                      density_in = density_)
  }else{
    df_ <- data.frame(year = yr,
                      country = 'Ecuador',
                      inD = degree_,
                      density_in = density_)
    df <- rbind(df, df_)
  }

}

df_dens_in <- rbind(df_dens_in, df)
rm(list=c('df_', 'df'))

# CCDF out-degree
for (yr in years){

  data <- empirical_ccdf(df_degE[year == yr, ]$outD)
  degree_ <- data$degree
  density_ <- data$ccdf

  if (yr == 2007){

    df <- data.frame(year = yr,
                      country = 'Ecuador',
                      outD=degree_,
                      density_out=density_)
  }else{
    df_ <- data.frame(year = yr,
                      country = 'Ecuador',
                      outD = degree_,
                      density_out = density_)
    df <- rbind(df, df_)
    }

}

df_dens_out <- rbind(df_dens_out, df)

df_degE$country <- 'Ecuador'
df_degH$country <- 'Hungary'
df_degs_all <- rbind(df_degH, df_degE)

rm(list=c('df_', 'df_degE', 'df', 'df_degH'))

## FACTSET ##
filename <- file.path(dirdata, 'factset', 'factset_local_properties.csv')
df_degs <- data.table(read.csv(filename))
df_degs <- df_degs[,list(inD, outD, year)]
df_degs$country <- 'FactSet'
# df_degs <- na.omit(df_degs)
years <- sort(unique(df_degs$year))

# CCDF in-degree
for (yr in years){

  data <- empirical_ccdf(df_degs[year == yr, ]$inD)
  degree_ <- data$degree
  density_ <- data$ccdf

  if (yr == 2014){

    df <- data.frame(year = yr,
                      country = 'FactSet',
                      inD = degree_,
                      density_in = density_)
  }else{
    df_ <- data.frame(year = yr,
                      country = 'FactSet',
                      inD = degree_,
                      density_in = density_)
    df <- rbind(df, df_)
    }

}

df_dens_in <- data.table(rbind(df_dens_in, df))
rm(list=c('df_', 'df'))

# CCDF out-degree
for (yr in years){

  data <- empirical_ccdf(df_degs[year == yr, ]$outD)
  degree_ <- data$degree
  density_ <- data$ccdf

  if (yr == 2014){

    df <- data.frame(year = yr,
                      country = 'FactSet',
                      outD = degree_,
                      density_out = density_)
  }else{
    df_ <- data.frame(year = yr,
                      country = 'FactSet',
                      outD = degree_,
                      density_out = density_)
    df <- rbind(df, df_)
    }

}

df_dens_out <- data.table(rbind(df_dens_out, df))
df_degs_all <- data.table(rbind(df_degs_all, df_degs))

rm(list=c('df_', 'df', 'df_degs'))


# ------------------------------------------------------------------------------
#  Plot 
# ------------------------------------------------------------------------------

# colour palette
ncol <- length(unique(df_dens_in$year))
pal <- rainbow(n = ncol, start = 0, end = 5/6)
pal[9] <- "#009966"
par(mar = rep(0, 4))
pie(rep(1, length(pal)), col = pal)

xmin <- min(df_dens_in[inD > 0]$inD)
xmin <- min(df_dens_out[outD > 0]$outD, xmin)
xmax <- max(df_dens_in[inD > 0]$inD)
xmax <- max(df_dens_out[outD > 0]$outD, xmax)


p1 <- ggplot(data = df_dens_in[inD > 0],  aes(x = inD, y = density_in, color = factor(year), shape = factor(country))) +

  geom_point(size = 0.8, alpha = 0.2) +

  scale_shape_manual(values = c(3, 1, 6)) +

  labs(x = TeX(r'(In-degree (number of suppliers))'), y = TeX(r'($P(X \, \geq \, x)$)')) +

  scale_x_log10(breaks =  scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(xmin, xmax)) +
  scale_y_log10(breaks =  scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(outside = F, colour = 'gray') +

  scale_colour_manual(values = pal) +

  annotate(geom = "text", x = 700, y = 0.11, label = "Ecuador", color = "black", hjust = "left", size = 3) +
  annotate(geom = "segment", x = 680, y = 0.1, xend = 200, yend = 0.06,  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +

  annotate(geom = "text", x = 90, y = 6*10^(-5), label = "FactSet", color = "black", hjust = "right", size = 3) +
  annotate(geom = "segment", x = 94, y = 6*10^(-5), xend = 340, yend = 6*10^(-5),  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +

  annotate(geom = "text", x = 2, y = 10^(-3), label = "Hungary", color = "black", hjust = "left", size = 3) +
  annotate(geom = "segment", x = 42, y = 10^(-3), xend = 7*10^1, yend = 10^(-2),  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +
  annotate(geom = "segment", x = 42, y = 10^(-3), xend = 2.2*10^2, yend = 8*10^(-3),  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +
  annotate(geom = "segment", x = 42, y = 10^(-3), xend = 4*10^2, yend = 2*10^(-4),  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +

  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1), vjust = 0),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1)),
        axis.text.y = element_text(size = rel(1), hjust = 0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.text.x = element_text(size = rel(1), vjust = 0),
        axis.ticks.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key.size = unit(0.2, "in"),
        legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(0.25, "cm"),
        legend.background = element_rect(fill = F),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(t = 0.4, r = 0, b = 0.4, l = 0.4), "cm")) +

  guides(shape = guide_legend(override.aes = list(size = 2, alpha = 1)),
          colour = guide_legend(override.aes = list(alpha = 1)))


p2 <- ggplot(data = df_dens_out[outD > 0],  aes(x = outD, y = density_out, color = factor(year), shape = factor(country))) +

  geom_point(size = 0.8, alpha = 0.2) +

  scale_shape_manual(values = c(3, 1, 6)) +

  labs(x = TeX(r'(Out-degree (number of customers))'), y = TeX(r'($P(X \, \geq \, x)$)')) +

  scale_x_log10(breaks =  scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(xmin, xmax)) +
  scale_y_log10(breaks =  scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(outside = F, colour = 'gray') +

  scale_colour_manual(values = pal) +

  annotate(geom = "text", x = 700, y = 0.11, label = "Ecuador", color = "black", hjust = "left", size = 3) +
  annotate(geom = "segment", x = 680, y = 0.1, xend = 1.3*10^(2), yend = 0.06,  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +

  annotate(geom = "text", x = 90, y = 6*10^(-5), label = "FactSet", color = "black", hjust = "right", size = 3) +
  annotate(geom = "segment", x = 94, y = 6*10^(-5), xend = 2*10^(2), yend = 6*10^(-5),  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +

  annotate(geom = "text", x = 2, y = 10^(-3), label = "Hungary", color = "black", hjust = "left", size = 3) +
  annotate(geom = "segment", x = 42, y = 10^(-3), xend = 7*10^1, yend = 10^(-2),  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +
  annotate(geom = "segment", x = 42, y = 10^(-3), xend = 3.3*10^2, yend = 8*10^(-3),  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +
  annotate(geom = "segment", x = 42, y = 10^(-3), xend = 3.1*10^2, yend = 2*10^(-4),  size = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +


  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1), vjust = 0),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.text.x = element_text(size = rel(1), vjust = 0),
        axis.ticks.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key.size = unit(0.2, "in"),
        legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(0.25, "cm"),
        legend.background = element_rect(fill = F),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin = unit(c(t = 0.4, r = 0.5, b = 0.4, l = 0), "cm")) +

  guides(shape = guide_legend(override.aes = list(size = 2, alpha = 1)),
          colour = guide_legend(override.aes = list(alpha = 1)))

g <- ggarrange(p1, p2, 
               nrow = 1, ncol = 2, 
               common.legend = T, legend = 'bottom', 
               widths = c(1.2, 1), font.label = element_text(size = rel(1)))

# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]]

filename <- paste(dirOutput, '3_fig_degs_over_time.png', sep = .Platform$file.sep)
ggsave(filename, plot = g,  width = width_l, height = height_l, dpi = 600)

print('Figure 3: Empirical CCDF of number of suppliers and number of customers over time exported.')


