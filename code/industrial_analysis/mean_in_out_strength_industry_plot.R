###############################################################################
# This script produces Figure E.1: Mean in- and out-strength by industry 

# INPUTS:
# Data:
# from data/analysis/
# - ecuador/ecuador_local_properties.csv
# - hungary/hungary_local_properties.csv

# OUTPUT: 
# to results/figures
#     - E1_fig_average_in_out_strength_industry.png

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(igraph)
library(data.table)
library(ggplot2)
library(ggpubr)

# Find root folder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Get directory of the input data (outside of the project)
dirData <- file.path(rootfolder, 'data', 'analysis')
# Set directory for Output
dirOutput <- file.path(rootfolder,'results','figures')
# For plots
width_LaTeX = 418.25368  # in pt
function_name <- paste(rootfolder, 'code', 'utils_plots', sep = .Platform$file.sep)
function_name <- paste(function_name, 'set_size.R', sep = .Platform$file.sep)
source(function_name)


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

# Ecuador
df_e <- fread(file.path(dirData, 'ecuador', 'ecuador_local_properties.csv'))
df_e <- setDT(df_e)
# Hungary
df_h <- fread(file.path(dirData, 'hungary', 'hungary_local_properties.csv'))
df_h <- setDT(df_h)

# ------------------------------------------------------------------------------
#  Prepare data
# ------------------------------------------------------------------------------

## HUNGARY ##
# NaNs are coded in a non-standard way
weird_nas <- c("", ".", ".NA")
# replace with actual NaN
df_h <- df_h[, lapply(.SD, function(x) ifelse(x %in% weird_nas, NA, x))]
# percentage of missing values (20-35% of missing values per year)
df_h[, lapply(.SD, function(col) sum(is.na(col)) / .N * 100), by = year]

# get average degree for the whole economy
df_allecon_h <- df_h[, .(mean_outS = mean(outS, na.rm = T),
                         mean_inS = mean(inS, na.rm = T)),
                     by = year]
# delete missing values (indu_codes)
df_h <- na.omit(df_h)
cols <- grep("^indu_code_", names(df_h), value = TRUE)
df_h <- melt(df_h,
             id.vars = c("year", "outS", "inS"),
             measure.vars = cols,
             value.name = "indu_code",
             variable.name = "code_level")
df_h <- setDT(df_h)

# Get average deg
df_h <- df_h[, .(mean_outS = mean(outS, na.rm = TRUE),
                 mean_inS = mean(inS, na.rm = TRUE)), 
             by = .(year, code_level, indu_code)]


## ECUADOR ##
# missing values none (only for clustering but we don't care)
colSums(is.na(df_e))
# get average degree for the whole economy
df_allecon_e <- df_e[, .(mean_outS = mean(outS, na.rm = T),
                         mean_inS = mean(inS, na.rm = T)),
                     by = year]

# delete industry codes 
delete_indu <- c("", "9", "T", "U", "V")
df_e <- df_e[!indu_code_1 %in% delete_indu]

cols <- grep("^indu_code_", names(df_e), value = TRUE)
df_e <- melt(df_e,
             id.vars = c("year", "outS", "inS"),
             measure.vars = cols,
             value.name = "indu_code",
             variable.name = "code_level")
df_e <- setDT(df_e)
# Get average deg
df_e <- df_e[, .(mean_outS = mean(outS, na.rm = TRUE),
                 mean_inS = mean(inS, na.rm = TRUE)), 
             by = .(year, code_level, indu_code)]


# ------------------------------------------------------------------------------
#  Plot: Mean in-strength vs mean out-strength by industry
# ------------------------------------------------------------------------------
df_plot_e <- df_e[code_level == 'indu_code_1' & year == 2015]
df_plot_h <- df_h[code_level == 'indu_code_1' & year == 2021]

ymin <- min(df_plot_e$mean_outS)
ymax <- max(df_plot_e$mean_outS)

p1 <- ggplot(df_plot_e, 
             aes(x = mean_inS, y = mean_outS, label = indu_code)) +
  geom_text(alpha = 0.8, colour = 'blue', size = 3, key_glyph = "point") + 
  # Unit line
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") + 
  # Economy-wide values
  geom_point(data = df_allecon_e[year == 2015], 
             aes(x = mean_inS, y = mean_outS, colour = 'all_firms'),
             shape = 15, size = 2.5, alpha = 0.8, inherit.aes = F) +
  
  scale_x_log10(breaks = 10^(4:7),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = 10^(4:7),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(outside = F, colour = 'gray') + 
  
  labs(x = 'Mean in-strength', y = 'Mean out-strength') +
  scale_colour_manual(values = c("all_firms" = "red"),
                      labels = c("all_firms" = "All firms")) +
  
  annotate("text", x = 1.4*10^5, y = 8*10^6, label = "Ecuador, 2015", 
           color = "black", hjust = "left", size = 3) +
  
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
        legend.position = c(0.22, 0.82),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines"),
        plot.margin = unit(c(t = 0.4, r = 0, b = 0.4, l = 0.4), "cm"))

p2 <- ggplot(df_plot_h, 
             aes(x = mean_inS, y = mean_outS, label = indu_code)) +
  geom_text(alpha = 0.8, colour = 'blue', size = 3, key_glyph = "point") + 
  # Unit line
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") + 
  # Economy-wide values
  geom_point(data = df_allecon_h[year == 2021], 
             aes(x = mean_inS, y = mean_outS, colour = 'all_firms'),
             shape = 15, size = 2.5, alpha = 0.8, inherit.aes = F) +
  
  scale_x_log10(
    breaks = 10^(4:7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(
    breaks = 10^(4:7),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(outside = F, colour = 'gray') + 
  
  labs(x = 'Mean in-strength', y = 'Mean out-strength') +
  scale_colour_manual(values = c("all_firms" = "red"),
                      labels = c("all_firms" = "All firms")) +
  
  annotate("text", x = 3.2*10^4, y = 2.2*10^6, label = "Hungary, 2021", 
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
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.8, "lines"),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(0.6, "lines"),
        legend.background = element_rect(fill = F),
        strip.text = element_text(colour = "black", size = rel(0.7)),  
        strip.background = element_blank(),
        panel.spacing.y = unit(0, "lines"),
        plot.margin = unit(c(t = 0.4, r = 0.5, b = 0.4, l = 0), "cm"))

# Combine plots
g <- ggarrange(p1, p2 + rremove('ylab'), 
               nrow = 1, ncol = 2, 
               common.legend = F,
               widths = c(1, 1), font.label = element_text(size = rel(1)))

# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]] - 0.5

filename <- paste(dirOutput, '/E1_fig_average_in_out_strength_industry.png', sep = '')
ggsave(filename, g, width = width_l, height = height_l, dpi = 600)

print('Figure E.1: Mean in- and out-strength by industry exported.')