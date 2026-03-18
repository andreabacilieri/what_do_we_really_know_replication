################################################################################
# This script produces Table 3: Regression results for the mean degree and network size

# INPUTS:
# Data:
# from data/literature
#   - literature_network_properties.csv
# from data/analysis
#   - ecuador_global_properties.csv
#   - factset_global_properties.csv
#   - hungary_global_properties.csv

# OUTPUT:
# to results/tables/
# (1) 3_tab_mean_degree.tex

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

library(data.table)
library(stargazer)  # for latex reg table
library(plm) # panel data

# rootfolder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'/code')[[1]][1]
# Folder to Store output
dirOutput <- file.path(rootfolder, 'results')
# Folder to the Input data
dirdata <- file.path(rootfolder, 'data', 'analysis')
dirdata_lit <- file.path(rootfolder, 'data', 'literature')


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

## NUMBER OF NODES AND EDGES ##

# Ecuador
filename <- file.path(dirdata, 'ecuador', 'ecuador_global_properties.csv')
df_ec <- fread(filename)

# Factset
filename <- file.path(dirdata, 'factset', 'factset_global_properties.csv')
df_f <- fread(filename)

# Hungary
filename <- file.path(dirdata, 'hungary', 'hungary_global_properties.csv')
df_h <- fread(filename)

## GET AVERAGE DEGREE ##

# Hungary
df_h <- df_h[order(year)]
df_h<- df_h[, list(year, Nnodes, Nedges)]
df_h$k_av <- df_h$Nedges/df_h$Nnodes
df_h$country <- 'Hungary'
df_h$data_collection_method <- 'VAT'

# Ecuador
df1 <- df_ec[, list(year, Nnodes, Nedges)]
df1$k_av <- df1$Nedges/df1$Nnodes
df1$country <- 'Ecuador'
df1$data_collection_method <- 'VAT'

# Factset
df2 <- df_f[, list(year, Nnodes, Nedges)]
df2$k_av <- df2$Nedges/df2$Nnodes
df2$country <- 'FactSet'
df2 <- df2[year != 2022 & year != 2023]
df2$data_collection_method <- 'Annual filings + others'
df <- rbind(df1, df_h, df2)
df$acceleration <- 1
rm(df1, df2, df_h, df_ec, df_f)

## DATA FROM LITERATURE ##
filename <- file.path(dirdata_lit, 'literature_network_properties.csv')
df_lit <- fread(filename)

df_lit$N <- as.integer(gsub("\\.", "", df_lit$N))
df_lit$E <- as.integer(gsub("\\.", "", df_lit$E))

df3 <- df_lit[pooled_yrs == 0, list(Years, N, E, Dataset, fig2_yesorno, data_collection_method)]
colnames(df3) <- c('year', 'Nnodes', 'Nedges', 'country', 'acceleration', 'data_collection_method')
df3$k_av <- df3$Nedges/df3$Nnodes
df3 <- setcolorder(data.table(df3), c('year', 'Nnodes', 'Nedges', 'k_av', 'country', 'data_collection_method', 'acceleration'))
df <- rbind(df, df3)
df <- na.omit(df)
rm(df3)


# ------------------------------------------------------------------------------
#  Regressions
# ------------------------------------------------------------------------------

## SINGLE-COUNTRY OLS REGRESSIONS ##
countries_reg <- c( 'Belgium', 'Costa Rica', 'Ecuador', 'Estonia', 'FactSet', 'Hungary', 'Kenya', 'West Bengal')
df_reg <- df[country %in% countries_reg]
# Hungary: use only years that have same threshold
df_reg[country == 'Hungary' & !year %in% 2015:2017, acceleration := 0]
df_reg <- df_reg[acceleration == 1]

lm_costarica  <- lm(log(Nedges/Nnodes) ~ log(Nnodes), data = df_reg[country == 'Costa Rica', ])
lm_ecuador    <- lm(log(Nedges/Nnodes) ~ log(Nnodes), data = df_reg[country == 'Ecuador', ])
lm_factset    <- lm(log(Nedges/Nnodes) ~ log(Nnodes), data = df_reg[country == 'FactSet', ])
lm_hungary    <- lm(log(Nedges/Nnodes) ~ log(Nnodes), data = df_reg[country == 'Hungary', ])
lm_belgium    <- lm(log(Nedges/Nnodes) ~ log(Nnodes), data = df_reg[country == 'Belgium', ])
lm_estonia    <- lm(log(Nedges/Nnodes) ~ log(Nnodes), data = df_reg[country == 'Estonia', ])
lm_kenya      <- lm(log(Nedges/Nnodes) ~ log(Nnodes), data = df_reg[country == 'Kenya', ])
lm_westbengal <- lm(log(Nedges/Nnodes) ~ log(Nnodes), data = df_reg[country == 'West Bengal', ])
# spain is implausibly high, we only have 2 obs
#lm_spain     <- lm(log(Nedges/Nnodes) ~ log(Nnodes), data = df[country == 'Spain', ])

## PANEL REGRESSIONS ##

# Without Spain, West Bengal, Factset, Belgium
countries_reg_plm <- countries_reg[-which(countries_reg %in% c("FactSet", "West Bengal", "Belgium"))]
df_panel <- df[country %in% countries_reg_plm]
# Hungary: keep years with no changes in reporting threshold
df_panel[country == 'Hungary' & !year %in% 2015:2017, acceleration := 0]
df_panel <- df_panel[ acceleration == 1]
df_panel <- pdata.frame(df_panel, index = c("country","year"))

PLM <- plm(log(k_av) ~ log(Nnodes), data = df_panel,
           model = "within", effect = "individual")
summary(PLM)

# Without West Bengal, Factset, Belgium
countries_reg_plm <- countries_reg[-which(countries_reg %in% c("FactSet", "West Bengal", "Belgium"))]
countries_reg_plm <- c(countries_reg_plm, "Spain")
df_panel <- df[country %in% countries_reg_plm]
# Hungary: keep years with no changes in reporting threshold
df_panel[country == 'Hungary' & !year %in% 2015:2017, acceleration := 0]
df_panel <- df_panel[ acceleration == 1]
df_panel <- pdata.frame(df_panel, index = c("country","year"))

PLM_final <- plm(log(k_av) ~ log(Nnodes), data = df_panel,
           model = "within", effect = "individual")
summary(PLM_final)

# Without West Bengal, Factset, Spain
countries_reg_plm <- countries_reg[-which(countries_reg %in% c("FactSet", "West Bengal"))]
df_panel <- df[country %in% countries_reg_plm]
# Hungary: keep years with no changes in reporting threshold
df_panel[country == 'Hungary' & !year %in% 2015:2017, acceleration := 0]
df_panel <- df_panel[ acceleration == 1]
df_panel <- pdata.frame(df_panel, index = c("country","year"))

PLM <- plm(log(k_av) ~ log(Nnodes), data = df_panel,
                 model = "within", effect = "individual")
summary(PLM)

# Without West Bengal, Factset
countries_reg_plm <- countries_reg[-which(countries_reg %in% c("FactSet", "West Bengal"))]
countries_reg_plm <- c(countries_reg_plm, "Spain")
df_panel <- df[country %in% countries_reg_plm]
# pick only some years for Hungary
df_panel[country == 'Hungary' & !year %in% 2015:2017, acceleration := 0]
df_panel <- df_panel[ acceleration == 1]
df_panel <- pdata.frame(df_panel, index = c("country","year"))

PLM <- plm(log(k_av) ~ log(Nnodes), data = df_panel,
           model = "within", effect = "individual")
summary(PLM)

# Without West Bengal
countries_reg_plm <- countries_reg[-which(countries_reg %in% c("West Bengal"))]
countries_reg_plm <- c(countries_reg_plm, "Spain")
df_panel <- df[country %in% countries_reg_plm]
# Hungary: only years with same threshold 
df_panel[country == 'Hungary' & !year %in% 2015:2017, acceleration := 0]
df_panel <- df_panel[ acceleration == 1]
df_panel <- pdata.frame(df_panel, index = c("country","year"))

PLM <- plm(log(k_av) ~ log(Nnodes), data = df_panel,
           model = "within", effect = "individual")
summary(PLM)

# spain is always excluded form the signle-copuntry regs as only 2 observations
single_reg_coeff_all <- c(summary(lm_costarica)$coefficient[2, 1], summary(lm_belgium)$coefficient[2, 1],
                            summary(lm_ecuador)$coefficient[2, 1], summary(lm_estonia)$coefficient[2, 1], summary(lm_factset)$coefficient[2, 1],
                            summary(lm_hungary)$coefficient[2, 1], summary(lm_kenya)$coefficient[2, 1],
                            summary(lm_westbengal)$coefficient[2, 1])

single_reg_coeff_no_wb <- c(summary(lm_costarica)$coefficient[2, 1], summary(lm_belgium)$coefficient[2, 1],
                          summary(lm_ecuador)$coefficient[2, 1], summary(lm_estonia)$coefficient[2, 1], summary(lm_factset)$coefficient[2, 1],
                          summary(lm_hungary)$coefficient[2, 1], summary(lm_kenya)$coefficient[2, 1])

single_reg_coeff_no_wb_f_be <- c(summary(lm_costarica)$coefficient[2, 1], summary(lm_ecuador)$coefficient[2, 1], 
                              summary(lm_estonia)$coefficient[2, 1], summary(lm_hungary)$coefficient[2, 1], 
                              summary(lm_kenya)$coefficient[2, 1])

# mean coeff of the single regressions with west bengal
mean(single_reg_coeff_all)

# mean coeff of the single regressions without west bengal, factset and belgium
mean(single_reg_coeff_no_wb_f_be)

df_export_csv <- data.table(mean_eta_all = round(mean(single_reg_coeff_all), digits = 2),
                            mean_eta_no_wb_f_be = round(mean(single_reg_coeff_no_wb_f_be), digits = 2),
                            mean_eta_no_wb = round(mean(single_reg_coeff_no_wb), digits = 2))

filename <- file.path(dirOutput, 'tables', 'average_degree_number_nodes_regression_examples.csv')
write.csv(df_export_csv, file = filename, row.names = F)


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

myl <- list(lm_belgium, lm_costarica, lm_ecuador, lm_estonia, lm_factset, lm_hungary, lm_kenya, lm_westbengal, PLM_final)
coln <- c(countries_reg, 'Fixed eff.')

filename <- paste(dirOutput, 'tables', '3_tab_mean_degree.tex', sep = .Platform$file.sep)

stargazer(myl,
          column.labels = coln,
          dep.var.labels.include = F,
          digits = 2,
          covariate.labels = "$\\log{N}$",
          omit.stat = c("adj.rsq", "f","ser","n"),
          add.lines = list(c("Obs.", unlist(lapply(myl, nobs)))),
          model.numbers = FALSE,
          model.names = FALSE,
          column.sep.width = '-4pt',
          font.size = "small",
          title = "Mean degree and network size",
          notes.append = TRUE,
          notes.align = "l",
          notes.label = "",
          notes = "\\parbox[t]{16cm}{\\textit{Notes:} See Figure~\\ref{fig:accelar_avDeg_N} and Appendix~\\ref{app:density_growth} for the data sources. For Hungary, we run the regression only for 2015--2017, when the reporting threshold did not change.  For West Bengal, the regression is run quarterly for the year 2016.  The fixed effects model excludes FactSet, West Bengal and Belgium but includes Spain, for which we have 2 data points.  We remove Belgium because observations are not over consecutive years, FactSet because it has little variation and West Bengal because it is not a nation. If we remove Spain and/or introduce Belgium, the estimated coefficient does not change.  Standard error in parenthesis.}",
          label ="tab:mean_degree",
          dep.var.caption = 'Dependent variable: $\\log \\bar{k}$',
          out = filename)

print('Table 3: regression average degree and network size exported.')