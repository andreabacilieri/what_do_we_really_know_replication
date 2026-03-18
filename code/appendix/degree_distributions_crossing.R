##########################################################################################################################
# This script produces Figure B.2: Empirical CCDF of the in- and out-degrees
# It shows the 2 cross

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_local_properties.csv
#     - hungary/hungary_local_properties.csv
# from code/utils/
#     - fun_ccdf_tls_network.R
# from code/utils_plots/
#     - set_size.R
#     - 2D_hist.R

# OUTPUT:
# to results/figures/
#     - B2_fig_in_out_degrees_crossing.png

#######################################################################################################################

# ------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------
rm(list = ls())
library(data.table)
library(ggplot2)
library(ggpubr)
library(binsreg)
library(igraph)
library(zoo)
library(latex2exp)
# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
# Get directory of the input data
dirdata <- file.path(rootfolder, 'data', 'analysis')
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Get function for calculating empirical CCDF
function_name <- paste(rootfolder, 'code', 'utils', sep = .Platform$file.sep)
function_name <- paste(function_name, 'fun_ccdf_tls_network.R', sep = .Platform$file.sep)
source(function_name)
# For plots
width_LaTeX = 418.25368  # in pt
source(file.path(rootfolder, 'code', 'utils_plots', 'set_size.R'))


# ------------------------------------------------
# Load data & keep only 2015
# ------------------------------------------------

# Ecuador
df_e <- fread(file.path(dirdata, 'ecuador', 'ecuador_local_properties.csv'))
df_e <- df_e[year == 2015, c('outD', 'inD', 'year')]
# Hungary
df_h <- fread(file.path(dirdata, 'hungary', 'hungary_local_properties.csv'))
df_h <- df_h[year == 2021, c('outD', 'inD', 'year')]


# --------------------------------------------
# Get CCDF
# --------------------------------------------
# Ecuador
data_in <- empirical_ccdf(df_e$inD)
data_out <- empirical_ccdf(df_e$outD)
df_ccdf_e <- data.table(degree = data_in$degree,
                        ccdf = data_in$ccdf,
                        deg_type = 'In-degree')
df_ <- data.table(degree = data_out$degree,
                        ccdf = data_out$ccdf,
                        deg_type = 'Out-degree')
df_ccdf_e <- rbind(df_ccdf_e, df_)

# Hungary
data_in <- empirical_ccdf(df_h$inD)
data_out <- empirical_ccdf(df_h$outD)
df_ccdf_h <- data.table(degree = data_in$degree,
                        ccdf = data_in$ccdf,
                        deg_type = 'In-degree')
df_ <- data.table(degree = data_out$degree,
                  ccdf = data_out$ccdf,
                  deg_type = 'Out-degree')
df_ccdf_h <- rbind(df_ccdf_h, df_)


# ------------------------------------------------
# Find where CCDFs cross
# ------------------------------------------------

# Ecuador
df_in  <- df_ccdf_e[deg_type == "In-degree",  .(degree, ccdf_in = ccdf)]
df_out <- df_ccdf_e[deg_type == "Out-degree", .(degree, ccdf_out = ccdf)]
df_merge <- merge(df_in, df_out, by = "degree", all = TRUE)
df_merge[, diff := ccdf_in - ccdf_out]
df_merge <- data.table(df_merge)
df_merge[diff < 0, ] # cross at degree 199, ccdf 0.032

# Hungary
df_in  <- df_ccdf_h[deg_type == "In-degree",  .(degree, ccdf_in = ccdf)]
df_out <- df_ccdf_h[deg_type == "Out-degree", .(degree, ccdf_out = ccdf)]
df_merge <- merge(df_in, df_out, by = "degree", all = TRUE)
df_merge[, diff := ccdf_in - ccdf_out]
df_merge <- data.table(df_merge)
df_merge[diff < 0, ] # cross at degree 162, ccdf 0.038


# ------------------------------------------------------------------------------
# Plot: In- and out-degrees, show crossing
# ------------------------------------------------------------------------------
pal <- c('black', 'slateblue1')

df_ccdf <- df_ccdf_e
df_ccdf$country <- 'Ecuador'
df_ccdf_h$country <- 'Hungary'
df_ccdf <- rbind(df_ccdf, df_ccdf_h)

p1 <- ggplot(df_ccdf[degree > 0], aes(x = degree, y = ccdf, colour = deg_type, shape = country)) +
  geom_point(size = 0.8, alpha = 0.2) +
  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(x = TeX('Degree') , y = TeX(r'($P(X \, > \, x)$)')) +
  scale_shape_manual(values = c(3, 6)) +
  scale_colour_manual(values = pal) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1), vjust = 0),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = -0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.81, 0.76),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  
  guides(colour = guide_legend(ncol = 1, override.aes = list(alpha = 1, size = 1)),
         shape  = guide_legend(ncol = 1, override.aes = list(alpha = 1, size = 1)))


# Export 
fig_size <- set_size(width_LaTeX, fraction = 0.8)
width_l <- fig_size[[1]] - 1.2
height_l <- fig_size[[2]]

filename <- paste(dirOutput, '/B2_fig_in_out_degrees_crossing.png', sep = .Platform$file.sep)
ggsave(plot = p1, filename = filename, width = width_l, height = height_l, dpi = 600)



# ------------------------------------------------------------------------------
# OLD!
# Plot: In- and out-degrees, show crossing (subplots)
# ------------------------------------------------------------------------------
pal <- c('black', 'slateblue1')

p1 <- ggplot(df_ccdf_e[degree > 0], aes(x = degree, y = ccdf, colour = deg_type, shape = deg_type)) +
  geom_point(size = 0.8, alpha = 0.2) +
  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(x = TeX('Degree') , y = TeX(r'($P(X \, > \, x)$)')) +
  scale_shape_manual(values = c(3, 2, 1, 0)) +
  scale_colour_manual(values = pal) +
  
  
  annotate("text", x = 5*10^2, y = 0.7, label = "Ecuador, 2015", 
           color = "black", hjust = "left", size = 3) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1), vjust = 0),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(vjust = 2, size = rel(0.8)),
        axis.title.x = element_text(vjust = -0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  
  guides(colour = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)),
         shape  = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)))


p2 <- ggplot(df_ccdf_h[degree > 0], aes(x = degree, y = ccdf, colour = deg_type, shape = deg_type)) +
  geom_point(size = 0.8, alpha = 0.2) +
  
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(x = TeX('Degree') , y = TeX(r'($P(X \, > \, x)$)')) +
  scale_shape_manual(values = c(3, 2, 1, 0)) +
  scale_colour_manual(values = pal) +
  
  annotate("text", x = 2*10^3, y = 0.7, label = "Hungary, 2021", 
           color = "black", hjust = "left", size = 3) +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1), vjust = 0),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(vjust = 2, size = rel(0.8)),
        axis.title.x = element_text(vjust = -0.5),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines")) +
  
  guides(colour = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)),
         shape  = guide_legend(ncol = 2, override.aes = list(alpha = 1, size = 1)))

g <- ggarrange(p1, p2 + rremove('ylab'), 
               nrow = 1, ncol = 2, 
               common.legend = T, legend = "bottom",
               widths = c(1, 1), font.label = element_text(size = rel(1)))


# Export 
fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]] - 0.5

filename <- paste(dirOutput, '/XXX_fig_in_out_degrees_crossing_subplots.png', sep = .Platform$file.sep)
ggsave(plot = g, filename = filename, width = width_l, height = height_l, dpi = 600)