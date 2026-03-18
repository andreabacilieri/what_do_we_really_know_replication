################################################################################
# This script produces Table C1: Binary network statistics.

# INPUTS:
# Data:
# from data/analysis/
#     - ecuador/ecuador_global_properties.csv
#     - factset/factset_global_properties.csv
#     - hungary/hungary_global_properties.csv
# from data/literature/
#     - literature_network_properties.csv

# OUTPUT:
# to results/
# (1) literature_network_properties.tex

################################################################################

# ------------------------------------------------------------------------------
# Set environment/parameters
# ------------------------------------------------------------------------------

# loading packages
library(data.table)
library(xtable) # for creating the latex table
library(rstudioapi) # for rootfolder identification

# defining rootfolder and input/output data location
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path,'code')[[1]][1]
dirData <- file.path(rootfolder, 'data') # input data
dirOutput <- file.path(rootfolder, 'results/tables') # output data


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

## ECUADOR ##
filename <- file.path(dirData, 'analysis', 'ecuador/ecuador_global_properties.csv')
df_ec <- data.table(read.csv(filename))
df_ec$Dataset <- "Ecuador"

## FACTSET ##
filename <- file.path(dirData, 'analysis', 'factset/factset_global_properties.csv')
df_f <- data.table(read.csv(filename))
df_f$Dataset <- "FactSet"

## HUNGARY ##
filename <- file.path(dirData, 'analysis', 'hungary/hungary_global_properties.csv')
df_h <- data.table(read.csv(filename))
df_h$Dataset <- "Hungary"

## MERGE ##
df <- rbind(df_ec, df_h, df_f)
df <- df[, list(Dataset, year, Nnodes, Nedges)]
names(df) <- c('Dataset', 'Years', 'N', 'E')
df$Average_degree <- df$E/df$N
df$Average_degree <- as.character(round(df$Average_degree, digits = 1))
df$Calculated_k_bar <- 0
df$Calculated_E <- 0
df[, c('N', 'E')] <- lapply(df[, c('N', 'E')], function(x) format(x, big.mark = ",", scientific = FALSE))

## LITERATURE ##
input_filename <- file.path(dirData, 'literature', 'literature_network_properties.csv')
df_lit <- fread(input_filename, na.strings = c("NA", ""))
df_lit <- df_lit[, c('Dataset', 'Years', 'N', 'E', 'Average_degree', 'bibtex_key', 'Calculated_k_bar', 'Calculated_E')]
# add commas to mark thousands
df_lit[, N := as.numeric(N)]
df_lit[, E := as.numeric(E)]
df_lit[, c("N", "E") := lapply(.SD, \(x)
                               ifelse(is.na(x), NA_character_,
                                      format(x, big.mark = ",", scientific = FALSE))
), .SDcols = c("N", "E")]

# merge with our datasets
data <- rbind(df, df_lit, fill = TRUE)
data[is.na(Calculated_k_bar), Calculated_k_bar := 0]
data[is.na(Calculated_E), Calculated_E := 0]

for (i in 1:nrow(data)) {
  if (data[i,Calculated_k_bar] == 1) {
    data[i, Average_degree := paste0(data[i, Average_degree], "\\textsuperscript{*}")]
  }
}

for (i in 1:nrow(data)) {
  if (data[i,Calculated_E] == 1) {
    data[i, E := paste0(data[i, E], "\\textsuperscript{*}")]
  }
}


# ------------------------------------------------------------------------------
#  Table
# ------------------------------------------------------------------------------

## ARRAY WITH DATA ##

# listing unique country names
datasets <- unique(data$Dataset)

# defining table array
len_arr <- nrow(data)
wid_arr <- 6
arr <- array(dim = c(len_arr, wid_arr), data = NA)

# creating list for position of additional commands in print.xtable function
add2row_pos <- list()

# populating table array
arr_counter <- 1
# looping over countries in data
for (dataset in datasets) {
  temp_data <- data[data$Dataset == dataset]
  sub_counter <- 1

  # looping over row per country subdata
  for (i in 1:nrow(temp_data)) {
    
    if (is.na(temp_data[i, bibtex_key])) {
      bibtex_key_str <- "This paper"
    } else {
      bibtex_key_str <- paste("\\cite{", temp_data[i,bibtex_key], "}", sep = "")
    }
    
    if (sub_counter == 1) {
      arr[arr_counter, ] <- c(dataset, temp_data[i, Years], temp_data[i, N], 
                              temp_data[i, E], temp_data[i, Average_degree], bibtex_key_str)
      add2row_pos <- c(add2row_pos, arr_counter - 1)
    } else {
      arr[arr_counter, ] <- c("", temp_data[i, Years], temp_data[i, N], 
                              temp_data[i, E], temp_data[i, Average_degree], bibtex_key_str)
    }
    
  arr_counter <- arr_counter + 1
  sub_counter <- sub_counter + 1
  }
}

XTAB <- xtable(arr)

# defining latex code pasted before xtable output
starter_code <- '
\\label{app:density_growth}
\\begin{longtable}[c]{
    >{\\raggedright}p{0.15\\textwidth}
    >{\\centering\\arraybackslash}p{0.18\\textwidth}
    >{\\raggedleft\\arraybackslash}p{0.08\\textwidth}
    r
    >{\\raggedleft\\arraybackslash}p{0.08\\textwidth}
    >{\\centering\\arraybackslash}p{0.28\\textwidth}}
\\caption{Binary network statistics.}\\\\
\\toprule
Country & Year & \\multicolumn{1}{c}{$N$} & \\multicolumn{1}{c}{$E$} & \\multicolumn{1}{c}{$\\bar{k}$} & \\multicolumn{1}{c}{Source(s)} \\\\
\\midrule
\\endfirsthead
\\toprule
Country & Year & \\multicolumn{1}{c}{$N$} & \\multicolumn{1}{c}{$E$} & \\multicolumn{1}{c}{$\\bar{k}$} & \\multicolumn{1}{c}{Source(s)} \\\\
\\midrule
\\endhead
'

# defining latex code pasted after xtable output
end_code <- paste("
\\bottomrule
\\multicolumn{6}{l}{\\textit{Notes}: Values denoted by an asterisk (*) are inferred; see the text above this table.}
\\label{tab:binary_ntwk_stats}
\\end{longtable}", 
sep = "")

# defining positions and commands of extra of additional commands in print.xtable function
# drop first midrule as it would clash with the toprule
add2row_pos <- add2row_pos[-1] 
# drop midrule for global listed and global listed cleaned
add2row_pos <- add2row_pos[-11] 
add2row_pos <- c(0, add2row_pos)
add2row_pos <- c(add2row_pos, len_arr)
add2row_com <- rep("\\midrule ", times = length(add2row_pos) - 2)
add2row_com <- c(starter_code, add2row_com)
add2row_com <- c(add2row_com, end_code)
addtorow <- list()
addtorow$pos <- add2row_pos
addtorow$command <- add2row_com


# ------------------------------------------------------------------------------
#  Export
# ------------------------------------------------------------------------------

output_filename <- file.path(dirOutput, 'C1_tab_literature.tex')

# printing output file to output folder
print.xtable(XTAB, file = output_filename, only.contents = T,
             hline.after = NULL,
             sanitize.text.function = function(x){x},
             sanitize.rownames.function = function(x){x},
             include.rownames = F, include.colnames = F,
             floating = F,
             tabular.environment = F,
             add.to.row	= addtorow,
             comment = F)

print('Table C1: Binary network statistics exported.')
