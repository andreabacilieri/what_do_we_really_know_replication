###############################################################################
# This script counts the number of firm that have no industrial code
# for Ecuador and Hungary over time

# Output numbers are discussed in Appendix A.2.3, Data: Hungary, Sectoral composition.

################################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(data.table)

# Find root folder
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, 'code')[[1]][1]
# Get directory of the input data (outside of the project)
dirData <- file.path(rootfolder, 'data', 'analysis')


# ------------------------------------------------------------------------------
#  Load data
# ------------------------------------------------------------------------------

# Ecuador
df_e <- fread(file.path(dirData, 'ecuador', 'ecuador_local_properties.csv'))
df_e <- df_e[, list(year, inD, indu_code_1, indu_code_2, indu_code_3, indu_code_4,
                    indu_descr_1, indu_descr_2, indu_descr_3, indu_descr_4)]
# Hungary
df_h <- fread(file.path(dirData, 'hungary', 'hungary_local_properties.csv'))
df_h <- df_h[, list(year, inD, indu_code_1, indu_code_2, indu_code_3, indu_code_4,
                    indu_descr_1, indu_descr_2, indu_descr_3, indu_descr_4)]


# ------------------------------------------------------------------------------
#  Number of firms without industrial code
# ------------------------------------------------------------------------------

# Ecuador
unknown_codes <- c('', '9', 'V')
escluded_codes <- c('T', 'U', 'W', 'X') # none has W and X
df_tot <- df_e[, .(tot_firms = .N), by = year]
df_unknown <- df_e[indu_code_1 %in% unknown_codes, .(n_firms_unk = .N), by = year]
df_escluded <- df_e[indu_code_1 %in% escluded_codes, .(n_firms_esc = .N), by = year]
df_counts_e <- merge(df_tot, df_escluded, by = 'year')
df_counts_e <- merge(df_counts_e, df_unknown, by = 'year')
df_counts_e$pct_firms_esc <- (df_counts_e$n_firms_esc/df_counts_e$tot_firms)*100
df_counts_e$pct_firms_unk <- (df_counts_e$n_firms_unk/df_counts_e$tot_firms)*100
df_counts_e$pct_firms_any <- ((df_counts_e$n_firms_unk + df_counts_e$n_firms_esc)/df_counts_e$tot_firms)*100
print('Ecuador, firms with missing industrial code')
df_counts_e
print(paste0('Ecuador, average percentage of firms with missing industrial code: ', mean(df_counts_e$pct_firms_unk)))

# Hungary
sort(unique(df_h$indu_code_1))
unknown_codes <- c('', '.')
# Missing entirely code T:  
# Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use
df_tot <- df_h[, .(tot_firms = .N), by = year]
df_unknown <- df_h[indu_code_1 %in% unknown_codes, .(n_firms_unk = .N), by = year]
df_counts_h <- merge(df_tot, df_unknown, by = 'year')
df_counts_h$pct_firms_unk <- (df_counts_h$n_firms_unk/df_counts_h$tot_firms)*100
print('Hungary, percentage of firms with missing industrial code:')
(df_counts_h)
print(paste0('Hungary: Average percentage of firms with missing industrial code: ',mean(df_counts_h$pct_firms_unk)))

