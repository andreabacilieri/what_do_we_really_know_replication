################################################################################
# This script  produces Figure 5: Distribution of the length of the shortest paths in Ecuador, Hungary and FactSet over time

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador_shortest_paths.csv
#     - factset_shortest_paths.csv
#     - hungary_shortest_paths.csv
# from code/utils_plots/
#     - set_size.R

# OUTPUT:
# to results/figure/
# (1) 5_fig_path_lengths.pdf

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)
library(ggplot2)
library(lemon)

# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder,'results','figures')
# Folder to the Input data
dirdata <- file.path(rootfolder,'data','analysis')

# For plots
width_LaTeX = 418.25368  # in pt
function_name <- paste(rootfolder, 'code', 'utils_plots', sep = .Platform$file.sep)
function_name <- paste(function_name, 'set_size.R', sep = .Platform$file.sep)
source(function_name)


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

## FACTSET ##
filename <- file.path(dirdata, 'factset', 'factset_shortest_paths.csv')
df_f <- data.table(read.csv(filename))
df_f$year <- as.character((df_f$year))
df_f$country <- 'FactSet'

## ECUADOR ##
filename <- file.path(dirdata, 'ecuador', 'ecuador_shortest_paths.csv')
df_e <- data.table(read.csv(filename))
df_e$year <- as.character((df_e$year))
df_e$country <- 'Ecuador'

## HUNGARY ##
filename <- file.path(dirdata, 'hungary', 'hungary_shortest_paths.csv')
df_h <- data.table(read.csv(filename))
df_h$year <- as.character((df_h$year))
df_h$country <- 'Hungary'

df <- data.table(rbind(df_e, df_h, df_f))
rm('df_e', 'df_h', 'df_f')


# ------------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------------

# color palette
ncol = length(unique(df$year))
pal <- rainbow(n = ncol, start = 0, end = 5/6)
pal[9] <- "#009966"
par(mar = rep(0, 4))
pie(rep(1, length(pal)), col = pal)


p1 <- ggplot(df, aes(x = shrt_path, y = frequency, color = year, shape = country, linetype = country)) +

  geom_pointline(alpha = 0.9, size = 1.2, linesize = 0.05) +

  scale_x_continuous(breaks = round(seq(min(df$shrt_path), max(df$shrt_path), by = 2),1)) +

  xlab('Path length') + ylab('Frequency') +

  scale_shape_manual(values = c(3, 16, 17)) +
  scale_linetype_manual(values = c("dotted", "dashed", 'solid')) +
  scale_colour_manual(values = pal) +

  annotate(geom = "text", x = 7, y = 0.46, label = "Hungary", color = "black", hjust = "left", size = 3) +
  annotate(geom = "segment", x = 6.9, y = 0.45, xend = 5.4, yend = 0.35,  linewidth = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +
  annotate(geom = "segment", x = 6.9, y = 0.45, xend = 4.6, yend = 0.45,  linewidth = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +
  annotate(geom = "segment", x = 6.9, y = 0.45, xend = 2.8, yend = 0.47,  linewidth = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +

  annotate(geom ="text", x = 5, y = 0.58, label = "Ecuador", color = "black", hjust = "left", size = 3) +
  annotate(geom = "segment", x = 4.9, y = 0.575, xend = 3.2, yend = 0.575,  linewidth = 0.2, arrow = arrow(length = unit(1.6, "mm"))) +

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
        legend.position = c(0.75, 0.6),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.75)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.15, "in"),
        legend.key.height = unit(0.75, "lines"),
        legend.key.width = unit(0.5, "lines"),
        legend.background = element_rect(fill = F)) +

  guides(col = guide_legend(ncol = 3),
         shape = guide_legend(override.aes = list(size = 1)))


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 0.8)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]]

filename <- paste(dirOutput, '/5_fig_path_lengths.png', sep = '')
ggsave(filename, p1, width = width_l, height = height_l, dpi = 600)

print('5: Distribution of the length of the shortest paths in Ecuador, Hungary and FactSet over time exported.')