################################################################################
# This script produces Figure 2: Number of nodes and average degree over time

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_global_properties.csv
#     - factset/factset_global_properties.csv
#     - hungary/hungary_global_properties.csv
# from data/literature/
#     - literature_network_properties.csv
# from code/utils_plots/
#     - set_size.R

# OUTPUT:
# to results/figures/
# (1) 2_fig_number_nodes_vs_average_deg.pdf

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(ggplot2)  # for plots
library(grid)  # for annatation plots
library(data.table)
library(ggsci)  # for cool cols
library(RColorBrewer) # for cool cols
library(cowplot)   # get_legend() & plot_grid() functions
library(patchwork) # blank plot: plot_spacer()
library(rstudioapi) # for rootfolder identification

# defining rootfolder and input/output data location
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
dirData <- file.path(rootfolder, 'data') # input data
dirOutput <- file.path(rootfolder, 'results/figures') # output data

# For plots
width_LaTeX = 418.25368  # in pt
function_name <- paste(rootfolder, 'code', 'utils_plots', sep=.Platform$file.sep)
function_name <- paste(function_name, 'set_size.R', sep=.Platform$file.sep)
source(function_name)


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

## ECUADOR ##
filename <- file.path(dirData, 'analysis/ecuador/ecuador_global_properties.csv')
df_ec <- fread(filename)
df_ec <- df_ec[, list(year, Nnodes, Nedges)]
# setting country variable
df_ec$country <- 'Ecuador' 
# setting data collection variable
df_ec$data_collection_method <- 'VAT' 


## FACTSET ##
filename <- file.path(dirData, 'analysis/factset/factset_global_properties.csv')
df_f <- fread(filename)
df_f <- df_f[, list(year, Nnodes, Nedges)] 
df_f$country <- 'FactSet' 
df_f$data_collection_method <- 'Financial reporting + other' 

## HUNGARY ##
filename <- file.path(dirData, 'analysis/hungary/hungary_global_properties.csv')
df_h <- fread(filename)
df_h <- df_h[order(year)]
df_h<- df_h[, list(year, Nnodes, Nedges)]
df_h$country <- 'Hungary' 
df_h$data_collection_method <- 'VAT'

## MERGE ##
# Ecuador, Hungary and Factset
df <- rbind(df_ec, df_h, df_f)
# define acceleration variable
df$yes_no_line <- 1 
# create average degree variable
df$k_av <- df$Nedges/df$Nnodes 
rm(df_ec, df_f, df_h) 

## LITERATURE ##
filename <- file.path(dirData, 'literature/literature_network_properties.csv')
df_lit <- fread(filename)
# applying exclusion criteria of no pooled years
df_lit <- df_lit[pooled_yrs == 0, list(Years, N, E, Dataset, fig2_yesorno, data_collection_method)] 
colnames(df_lit) <- c('year', 'Nnodes', 'Nedges', 'country', 'yes_no_line', 'data_collection_method')
# remove dots from cells
#df_lit[, c("Nnodes", "Nedges")] <- data.frame(lapply(df_lit[, c("Nnodes", "Nedges")], function(x) gsub("\\.", "", x))) 
# converting column cells to integers
df_lit[, c("Nnodes", "Nedges")] <- lapply(df_lit[, c("Nnodes", "Nedges")], as.integer)
# create average degree variable
df_lit$k_av <- df_lit$Nedges/df_lit$Nnodes 
df_lit <- setcolorder(data.table(df_lit), 
                      c('year', 'Nnodes', 'Nedges', 'k_av', 'country', 'data_collection_method', 'yes_no_line'))
# merge with our datasets
df <- rbind(df, df_lit)
df <- na.omit(df)


# ------------------------------------------------------------------------------
#  Main plot
# ------------------------------------------------------------------------------

pal_pink <- "#FF00FF"
pal_red <- "#FF0033"
pal_blue <- "#0000CC"
pal_green <- '#33CC33'
pal_lighb <- '#0099CC'
pal <- c(pal_blue, pal_lighb, pal_green, pal_red, pal_pink)

# define the desired order of countries
country_order <- c("Ecuador", "Hungary", "FactSet",
                  "Belgium", "Costa Rica", "Dominican Republic",
                  "Estonia", "Global automotive", "Japan", 
                  "Japan automotive", "Japan electronics", "Kenya",
                  "Netherlands", "Northern Italy", "Spain", "U.S. listed",
                  "West Bengal")

# set the factor levels of `country` in df
df$country <- factor(df$country, levels = country_order)

## MAIN PLOT ##
p1 <- ggplot(data = df, aes(x = Nnodes, y = k_av, shape = country, color = data_collection_method)) +

  geom_point(size = 3, alpha = 1, stroke = 0.8) +
  scale_shape_manual(values = c(17, 15, 19, 0, 8, 1, 6, 4, 13, 9, 10, 7, 12, 14, 3, 2)) +
  scale_colour_manual(values = pal) +

  geom_line(data = df[yes_no_line == 1,], aes(x = as.numeric(Nnodes), y = k_av),
            linewidth = 0.7, alpha = 0.8, colour = 'black') +

  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = c(3, 5, 10, 30, 50)) +
  annotation_logticks(outside = F, colour = 'gray') +
  expand_limits(x = 10^2, y = 2) +
  coord_fixed(ratio = 2/1) +  

  labs(x = 'Number of nodes', y = 'Average degree') +
  
  theme_bw() +

  theme(axis.line = element_line(colour = "gray"),
        panel.border = element_rect(linewidth = 1, colour = "gray"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(1)),
        axis.title.y = element_text(vjust = 1, size = rel(1)),
        axis.text.y = element_text(hjust = 1),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(vjust = -1, size = rel(1)),
        axis.text.x = element_text(vjust = -1),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.key.size = element_blank(),
        legend.background = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  
        theme(text = element_text(family = "serif"))


# ------------------------------------------------------------------------------
#  Legends
# ------------------------------------------------------------------------------

## LITERATURE ##
p2 <- ggplot(data = df[!country %in% c('Ecuador', 'Hungary', 'FactSet'), ], 
             aes(x = Nnodes, y = k_av, shape = country)) +
  
  geom_point(size = 3, alpha = 1, stroke = 0.8) +
  scale_shape_manual(values = c(0, 8, 1, 6, 4, 13, 9, 10, 7, 12, 14, 3, 2)) +
  
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10() +
  expand_limits(x = 10^2, y = 2) +
  coord_fixed(ratio = 2/1) + 
  
  labs(x = 'Number of nodes', y = 'Average degree') +
  
  theme_bw() +
  
  theme(panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.text = element_text(size = rel(0.9)),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F),
        legend.margin = margin(t = 2, r = 2, b = 2, l = 2)) + 
  
        theme(text = element_text(family = "serif"))


## ECUADOR, HUNGARY, FACTSET ## 
p3 <- ggplot(data = df[country %in% c('Ecuador', 'Hungary', 'FactSet'), ], aes(x = Nnodes, y = k_av, shape = country)) +
  
  geom_point(size = 3, alpha = 1, stroke = 0.8) +
  scale_shape_manual(values = c(17, 15, 19)) +
  
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10() +
  annotation_logticks(linewidth = 0.3, long = unit(0.2, "cm"), outside = T) +  coord_cartesian(clip = "off") +
  expand_limits(x = 10^2, y = 2) +
  
  labs(x = 'Number of nodes', y = 'Average degree') +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = 2),
        axis.text.y = element_text(hjust = -1),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.text.x = element_text(vjust = -1),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
        legend.direction = "vertical",
        legend.text = element_text(size = rel(0.9), color = 'darkorange'),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F)) + 
  
        theme(text = element_text(family = "serif"))


## DATA COLLECTION METHOD ##
p4 <- ggplot(data = df, aes(x = Nnodes, y = k_av, colour = data_collection_method)) +
  
  geom_point(size = 3, alpha = 1, stroke = 0.8) +
  scale_colour_manual(values = pal) +
  
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10() +
  annotation_logticks(linewidth = 0.3, long = unit(0.2, "cm"), outside = T) +  coord_cartesian(clip = "off") +
  expand_limits(x = 10^2, y = 2) +
  
  labs(x = 'Number of nodes', y = 'Average degree') +
  
  theme_bw() +
  
  theme(axis.line = element_line(colour = "gray"),
        panel.border = element_rect(colour = "gray"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1), vjust = 0, hjust = 0.5),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), vjust = 2),
        axis.text.y = element_text(hjust = -1),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1), vjust = 0.5),
        axis.text.x = element_text(vjust = -1),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
        legend.direction = "horizontal",
        legend.text = element_text(size = rel(0.9)),
        legend.key.size = unit(0.2, "in"),
        legend.background = element_rect(fill = F)) + 

        guides(colour = guide_legend(nrow = 2, byrow = TRUE)) + 
  
        theme(text = element_text(family = "serif"))


# ------------------------------------------------------------------------------
#  Combine main plot and legends into final output
# ------------------------------------------------------------------------------

# get legends
leg1 <- suppressWarnings(get_legend(p2))
leg2 <- suppressWarnings(get_legend(p3))
leg3 <- suppressWarnings(get_legend(p4))
# create a blank plot for alignment
blank_p <- plot_spacer() + theme_void()

# stack right-hand legends tightly (leg2 BELOW leg1)
right_legends <- plot_grid(
  blank_p, # to align with plot p1
  leg1,
  blank_p, # needs some space between legends
  plot_grid(leg2, blank_p, # to left align with legend leg1
            ncol = 2, rel_widths = c(0.6, 0.4)),
  ncol = 1,
  rel_heights = c(0.3, 0.8, 0.2, 0.6)
)

# combine main plot + right legends
top_row <- plot_grid(
  p1,
  right_legends,
  ncol = 2,
  rel_widths = c(1, 0.42),
  align = "v",
  axis = "tb"
)

# add bottom legend with minimal height
final_p <- plot_grid(
  top_row,
  leg3,
  ncol = 1,
  rel_heights = c(1, 0.18)
)

# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]] + .6
height_l <- fig_size[[2]] + 1

filename <- paste(dirOutput, '2_fig_number_nodes_vs_average_deg.png', sep = .Platform$file.sep)
ggsave(filename, plot = final_p, width = width_l, height = height_l, dpi = 600)

print('Figure 2: Number of nodes and average degree over time exported.')
