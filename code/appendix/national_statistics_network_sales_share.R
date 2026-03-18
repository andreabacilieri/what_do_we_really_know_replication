######################################################################################################################################################
# This script produces Figure A.1: Sectoral composition in the national I-O table at the sector level and in our firm-level dataset in 2015 and 2020.

# INPUTS:
# from data/
#   - national_statistics/hungary/I_O_table_aggregate_statistics_2015.csv
#   - national_statistics/hungary/I_O_table_aggregate_statistics_2020.csv
#   - national_statistics/hungary/nace_1digit_rev2.csv
# from code/utils_plots/
#   - set_size.R

# OUTPUT:
# to results/figures
# (1) A1_fig_intermediate_sales_share.pdf

######################################################################################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(ggplot2)
library(ggpubr)
library(data.table)
library(dplyr)
library(stringr)

# Rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
# Folder to store output
dirOutput <- file.path(rootfolder, 'results', 'figures')
# Folder for the input data
dirdata <- file.path(rootfolder, 'data')

# For plots
width_LaTeX = 472.3157  # in pt
source(file.path(rootfolder, 'code', 'utils_plots', 'set_size.R'))


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

# Industrial codes with description
nace_1digit <- fread(file.path(dirdata, 'national_statistics', 'hungary', 'nace_1digit_rev2.csv'))
colnames(nace_1digit) <- c("nace_1digit", "nace_descr")

# National accounts data (in million HUF)
df_IO_2015 <- fread(file.path(dirdata, 'national_statistics', 'hungary', 'I_O_table_aggregate_statistics_2015.csv'))
df_IO_2020 <- fread(file.path(dirdata, 'national_statistics', 'hungary', 'I_O_table_aggregate_statistics_2020.csv'))

# Network data (in thousand HUF)
filename_network <- file.path(dirdata, 'analysis', 'hungary', 'hungary_local_properties.csv')
df_net <- fread(filename_network)
keep_vars <- c('year', 'outS', 'indu_descr_1')
df_net <- df_net[, ..keep_vars]

network_2015 <- df_net[year == 2015, .(network_sales = sum(outS)), by = indu_descr_1]
colnames(network_2015) <- c('nace_1digit', 'network_sales')
network_2015[, network_sales := network_sales / 1000]

network_2020 <- df_net[year == 2020, .(network_sales = sum(outS)), by = indu_descr_1]
colnames(network_2020) <- c('nace_1digit', 'network_sales')
network_2020[, network_sales := network_sales / 1000]


# ------------------------------------------------------------------------------
#  Prepare data
# ------------------------------------------------------------------------------

## Compute intermediate sales share in national I-O table ##

# Redefine intermediate sales: gross fixed capital formation + intermediate sales
df_IO_2015$my_interm_sales <- df_IO_2015$interm_sales + df_IO_2015$gfcf
df_IO_2020$my_interm_sales <- df_IO_2020$interm_sales + df_IO_2020$gfcf
df_IO_2015$sales_share <- df_IO_2015$my_interm_sales / sum(df_IO_2015$my_interm_sales)
df_IO_2020$sales_share <- df_IO_2020$my_interm_sales / sum(df_IO_2020$my_interm_sales)
# add nace description
df_IO_2015 <- merge(df_IO_2015, nace_1digit, by = "nace_1digit")
df_IO_2020 <- merge(df_IO_2020, nace_1digit, by = "nace_1digit")


## Compute intermediate sales share for firm network ##

# Stats for firms with NaN industrial code
print(paste0('Percent of network sales to NaN sector (2015): ', 
             network_2015[nace_1digit == '']$network_sales/sum(network_2015$network_sales)))
print(paste0('Percent of network sales to NaN sector (2020): ', 
             network_2020[nace_1digit == '']$network_sales/sum(network_2020$network_sales)))
# Remove them before getting the sectoral shares
network_2015 <- network_2015[nace_1digit != '']
network_2020 <- network_2020[nace_1digit != '']

# Compute sectoral shares
network_2015$sales_share <- network_2015$network_sales/sum(network_2015$network_sales)
network_2020$sales_share <- network_2020$network_sales/sum(network_2020$network_sales)

# Rename certain sectors in firm network
network_2015 <- network_2015 %>% mutate(nace_1digit = ifelse(nace_1digit == "Water supply; sewerage; waste managment and remediation activities\"\"\"\"", 
                                                             "Water supply; sewerage, waste management and remediation activities", nace_1digit))
network_2015 <- network_2015 %>% mutate(nace_1digit = ifelse(nace_1digit == "Wholesale and retail trade; repair of motor vehicles and motorcycles\"\"\"\"", 
                                                             "Wholesale and retail trade; repair of motor vehicles and motorcycles", nace_1digit))
network_2015 <- network_2015 %>% mutate(nace_1digit = ifelse(nace_1digit == "Public administration and defence; compulsory social security\"\"\"\"", 
                                                             "Public administration and defence; compulsory social security", nace_1digit))
network_2015 <- network_2015 %>% mutate(nace_1digit = ifelse(nace_1digit == "Other services activities", 
                                                             "Other service activities", nace_1digit))
network_2015 <- network_2015 %>% mutate(nace_1digit = ifelse(nace_1digit == "Transporting and storage", 
                                                             "Transportation and storage", nace_1digit))


network_2020 <- network_2015 %>% mutate(nace_1digit = ifelse(nace_1digit == "Water supply; sewerage; waste managment and remediation activities\"\"\"\"", 
                                                             "Water supply; sewerage, waste management and remediation activities", nace_1digit))
network_2020 <- network_2015 %>% mutate(nace_1digit = ifelse(nace_1digit == "Wholesale and retail trade; repair of motor vehicles and motorcycles\"\"\"\"", 
                                                             "Wholesale and retail trade; repair of motor vehicles and motorcycles", nace_1digit))
network_2020 <- network_2015 %>% mutate(nace_1digit = ifelse(nace_1digit == "Public administration and defence; compulsory social security\"\"\"\"", 
                                                             "Public administration and defence; compulsory social security", nace_1digit))
network_2020 <- network_2020 %>% mutate(nace_1digit = ifelse(nace_1digit == "Other services activities", 
                                                             "Other service activities", nace_1digit))
network_2020 <- network_2020 %>% mutate(nace_1digit = ifelse(nace_1digit == "Transporting and storage", 
                                                             "Transportation and storage", nace_1digit))


## Make data for plots (merge tables with I-O and network data) ##
colnames(network_2015) <- c("nace_descr", "network_sales", "network_sales_share")
network_2015 <- merge(network_2015, nace_1digit, by = "nace_descr", all = T)
network_2015 <- network_2015[order(nace_1digit)]
colnames(network_2020) <- c("nace_descr", "network_sales", "network_sales_share")
network_2020 <- merge(network_2020, nace_1digit, by = "nace_descr")
network_2020 <- network_2020[order(nace_1digit)]

df_plot <- merge(network_2015[, c("network_sales", "network_sales_share", "nace_1digit")], 
                 df_IO_2015[, c("nace_1digit", "nace_descr", "sales_share", "my_interm_sales")],
                 by = "nace_1digit")
df_plot <- df_plot[order(nace_1digit)]
df_plot <- df_plot[, c("nace_descr", "network_sales_share", "sales_share")]
df_plot <- melt(df_plot, id.vars = c("nace_descr"), variable.name = "dataset", value.name = "sales_share")
setDT(df_plot)
df_plot[dataset == "network_sales_share", dataset := "Network"]
df_plot[dataset == "sales_share", dataset := "National statistics"]

df <- merge(network_2020[, c("network_sales", "network_sales_share", "nace_1digit")], 
                 df_IO_2020[, c("nace_1digit", "nace_descr", "sales_share", "my_interm_sales")],
                 by = "nace_1digit")
df <- df[order(nace_1digit)]
df <- df[, c("nace_descr", "network_sales_share", "sales_share")]
df <- melt(df, id.vars = c("nace_descr"), variable.name = "dataset", value.name = "sales_share")
setDT(df)
df[dataset == "network_sales_share", dataset := "Network"]
df[dataset == "sales_share", dataset := "National statistics"]

df_plot$year <- 2015
df$year <- 2020
df_plot <- rbind(df_plot, df)
rm(df)
drop_indu <- c('Activities of extraterritorial organisations and bodies', 
               'Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use')
df_plot <- df_plot[!nace_descr %in% drop_indu]


# ------------------------------------------------------------------------------
#  Plot
# ------------------------------------------------------------------------------

# Create a horizontal bar plot for the gross_output_share_IO and gross_output_share_network columns of the merged_2015 data table
df <- df_plot[year == 2015 & sales_share > 0.01 & dataset == 'National statistics', ]
big_sectors <- df[order(sales_share, decreasing = T)]$nace_descr
df <- df_plot[year == 2015 & sales_share <= 0.01 & dataset == 'National statistics', ]
small_sectors <- df[order(sales_share, decreasing = T)]$nace_descr

df <- df_plot[year == 2015 & nace_descr %in% big_sectors, ]
df <- df[order(match(nace_descr, big_sectors))]
nace_factor <- ordered(df$nace_descr, levels = rev(big_sectors))
df$nace_factor <- nace_factor

big_plot_2015 <- ggplot(df, aes(x = sales_share, y = nace_factor, fill = dataset)) + 
  
                  geom_bar(position = position_dodge(width = 0.6), stat = "identity", width = 0.7, colour = 'white', lwd = 0.1) +
                  scale_fill_manual(values = c("National statistics" = "#1f78b4", "Network" = "#b2df8a")) + 
          
                  labs(x = "Sales shares, 2015", y = NULL) +
                  scale_x_continuous(limits = c(0, max(df$sales_share)+0.02), expand = c(0, 0)) +
                  scale_y_discrete(labels = function(x) str_wrap(x, width = 36)) +
                  
                  theme(text = element_text(family = "serif"),
                  axis.title = element_blank(),
                  axis.text = element_text(size = rel(0.8)),
                  axis.text.y = element_text(hjust = 0, colour = "black"),
                  axis.ticks = element_blank(),
                  legend.position = "none",
                  legend.title = element_blank(),
                  legend.text = element_text(size = rel(0.7)),
                  legend.key = element_rect(fill = 'transparent', colour = 'transparent'),
                  legend.key.size = unit(0.3, "cm"),
                  legend.key.height = unit(0.3, "cm"),
                  legend.key.width = unit(0.6, "cm"),
                  legend.background = element_rect(fill = 'transparent', colour = 'transparent'),
                  panel.background = element_rect(fill = "transparent", colour = "lightgrey"),
                  panel.grid.major = element_line(colour = "lightgrey", linewidth = 0.1),
                  plot.margin = margin(0.07, 0, 0.02, 0.4, "cm")) 

df <- df_plot[year == 2015 & nace_descr %in% small_sectors, ]
df <- df[order(match(nace_descr, small_sectors))]
nace_factor <- ordered(df$nace_descr, levels = rev(small_sectors))
df$nace_factor <- nace_factor

small_plot_2015 <- ggplot(df, aes(x = sales_share, y = nace_factor, fill = dataset)) + 
  
  geom_bar(position = position_dodge(width = 0.6), stat = "identity", width = 0.7, colour = 'white', lwd = 0.1) +
  scale_fill_manual(values = c("National statistics" = "#1f78b4", "Network" = "#b2df8a")) + 
  
  labs(x = "Sales share, 2015", y = NULL) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(df$sales_share)+0.0001)) + 
  scale_y_discrete(labels = function(x) str_wrap(x, width = 36)) +
  
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.8)),
        axis.text.y = element_text(hjust = 0, colour = "black"),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.7)),
        legend.key = element_rect(fill = 'transparent', colour = 'transparent'),
        legend.key.size = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"),
        legend.background = element_rect(fill = 'transparent', colour = 'transparent'),
        panel.background = element_rect(fill = "transparent", colour = "lightgrey"),
        panel.grid.major = element_line(colour = "lightgrey", linewidth = 0.1),
        plot.margin = margin(0.02, 0, 0.1, 0.4, "cm")) 


df <- df_plot[year == 2020 & nace_descr %in% big_sectors, ]
df <- df[order(match(nace_descr, big_sectors))]
nace_factor <- ordered(df$nace_descr, levels = rev(big_sectors))
df$nace_factor <- nace_factor

big_plot_2020 <- ggplot(df, aes(x = sales_share, y = nace_factor, fill = dataset)) + 
  
  geom_bar(position = position_dodge(width = 0.6), stat = "identity", width = 0.7, colour = 'white', lwd = 0.1) +
  scale_fill_manual(values = c("National statistics" = "#1f78b4", "Network" = "#b2df8a")) + 
  
  labs(x = "Sales share, 2020", y = NULL) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(df$sales_share)+0.02)) +
  
  theme(text = element_text(family = "serif"),
        axis.title = element_blank(),
        axis.text = element_text(size = rel(0.8)),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.6, 0.125),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.7)),
        legend.key = element_rect(fill = 'transparent', colour = 'transparent'),
        legend.key.size = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"),
        legend.background = element_rect(fill = 'transparent', colour = 'transparent'),
        panel.background = element_rect(fill = "transparent", colour = "lightgrey"),
        panel.grid.major = element_line(colour = "lightgrey", linewidth = 0.1),
        plot.margin = margin(0.07, 0.2, 0.02, 0.2, "cm")) 

df <- df_plot[year == 2020 & nace_descr %in% small_sectors, ]
df <- df[order(match(nace_descr, small_sectors))]
nace_factor <- ordered(df$nace_descr, levels = rev(small_sectors))
df$nace_factor <- nace_factor

small_plot_2020 <- ggplot(df, aes(x = sales_share, y = nace_factor, fill = dataset)) + 
  
  geom_bar(position = position_dodge(width = 0.6), stat = "identity", width = 0.7, colour = 'white', lwd = 0.1) +
  scale_fill_manual(values = c("National statistics" = "#1f78b4", "Network" = "#b2df8a")) + 
  
  labs(x = "Sales share, 2020", y = NULL) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(df$sales_share)+0.0001)) + 
  
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = rel(0.8)),
        axis.text.y = element_blank(),
        axis.title = element_text(size = rel(0.8)),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.7)),
        legend.key = element_rect(fill = 'transparent', colour = 'transparent'),
        panel.background = element_rect(fill = "transparent", colour = "lightgrey"),
        panel.grid.major = element_line(colour = "lightgrey", linewidth = 0.1),
        legend.key.size = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"),
        plot.margin = margin(0.02, 0.2, 0.1, 0.2, "cm")) 

# Combine
g <- ggarrange(big_plot_2015, big_plot_2020,
               small_plot_2015, small_plot_2020,
               nrow = 2, ncol = 2,
               widths = c(1, 0.6),
               heights = c(1, 0.9))


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

fig_size <- set_size(width_LaTeX, fraction = 1)
width_l <- fig_size[[1]]
height_l <- fig_size[[2]] + 2.2

ggsave(file.path(dirOutput, "A1_fig_intermediate_sales_share.png"), g, width = width_l, height = height_l, dpi = 320)

print('Figure A1: Bar chart on intermediate sales share per sector for 2015 and 2020 across I-O table and firm-level dataset exported.')


# ------------------------------------------------------------------------------
#  Where are the biggest difference?
# ------------------------------------------------------------------------------

# Get stats
drop_indu <- c('T', 'U')
df_IO_2015 <- df_IO_2015[!nace_1digit %in% drop_indu]
df_IO_2020 <- df_IO_2020[!nace_1digit %in% drop_indu]

df_2015 <- merge(network_2015[, c("network_sales_share", "nace_1digit")],
                df_IO_2015[, c("nace_1digit", "nace_descr", "sales_share")],
                by = "nace_1digit")
df_2015$pct_diff <- ((df_2015$network_sales_share - df_2015$sales_share)/df_2015$sales_share) * 100
df_2015$ratio <- df_2015$network_sales_share/df_2015$sales_share
df_2015 <- df_2015[order(abs(pct_diff))]
df_2015$nace_factor <- ordered(df_2015$nace_descr, levels = df_2015$nace_descr)
df_2015$year <- 2015

df_2020 <- merge(network_2020[, c("network_sales_share", "nace_1digit")],
                df_IO_2020[, c("nace_1digit", "nace_descr", "sales_share")],
                by = "nace_1digit")
df_2020$pct_diff <- ((df_2020$network_sales_share - df_2020$sales_share)/df_2020$sales_share) * 100
df_2020$ratio <- df_2020$network_sales_share/df_2015$sales_share
df_2020 <- df_2020[order(abs(pct_diff))]
df_2020$nace_factor <- ordered(df_2020$nace_descr, levels = df_2020$nace_descr)
df_2020$year <- 2020

df <- rbind(df_2015, df_2020)
df <- df[, c('pct_diff', 'ratio', 'nace_factor', 'year')]
df[abs(pct_diff) > 80, .N, by = year] / df[, .N, by = year]
df[abs(pct_diff) > 50]


# Plot the differences
ggplot(df, aes(y = nace_factor, x = pct_diff, color = as.factor(year), shape=as.factor(year))) +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_manual(values = c("#1f78b4", "#b2df8a")) +
  labs(x = "Percentage difference, 2015", y = NULL) +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        axis.title = element_blank(),
        axis.text = element_text(size = rel(0.8)),
        axis.text.y = element_text(hjust = 0, colour = "black"),
        axis.ticks = element_blank(),
        legend.position = c(0.6, 0.2),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.7)))
