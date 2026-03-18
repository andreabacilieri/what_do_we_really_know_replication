###########################################################################
# Compute Table 2 showing thresholds as a function of GDP per population

# INPUTS:
# Data:
# from data/
#   - literature
#       - thresholds.xlsx
#   - national_statistics/P_Data_Extract_From_World_Development_Indicators/
#       - 48a79be1-202d-43d9-b82e-385fc17c97b5_Data.csv

# OUTPUT:
# to results/table/
#       - 2_tab_thresholds.tex

###########################################################################

# ------------------------------------------------------------------------------
#  Set environment/parameters
# ------------------------------------------------------------------------------

rm(list = ls())
library(xtable)
library(readxl)

# Rootfolder 
rootfolder <- strsplit(rstudioapi::getSourceEditorContext()$path, '/code')[[1]][1]
# Folder to store output
dirOutput <- file.path(rootfolder, 'results', 'tables')
# Folder for the input data
dirdata <- file.path(rootfolder, 'data')

# Functions
source(file.path(rootfolder, 'code', 'utils', 'fun_thresholds_table.R'))


# ------------------------------------------------------------------------------
#  Load and prepare data
# ------------------------------------------------------------------------------

## THRESHOLDS: our networks & literature ##

# File with thresholds of different network data (our networks and the literature)
filename <- file.path(dirdata,'literature', 'thresholds.csv')
xl <- read.csv(filename, colClasses = "character", na.strings = "")

# Prepare data
xl <- xl[, 1:9]
rowstorm <- which(apply(xl[, 3:9], 1, function(x){sum(x == "NA")}) == 6)
xl <- xl[-rowstorm, ]

xl$weight_thres <- paste(xl[["weight_thres_NCU"]], xl[["NCU"]])
isna <- which(xl$weight_thres == "NA" )
xl$weight_thres[isna] <- paste(xl$weight_thres_USD[isna], xl$USD[isna])

xl$size_thres <- paste(xl[["size_thres_NCU"]], xl[["NCU"]])
isna <- which(xl$size_thres == "NA" )
xl$size_thres[isna] <- paste(xl$size_thres_USD[isna], xl$USD[isna])

## WORLD BANK: GDP & GDP per pop ##

filename <- file.path(dirdata,'national_statistics', 'P_Data_Extract_From_World_Development_Indicators', '48a79be1-202d-43d9-b82e-385fc17c97b5_Data.csv')
wb <- read.csv(filename)  

# Prepare data
# NOTE: - the file includes some notes at the end of first column
#       - first line that is no longer the data is which(wb[,1]=="")[1] so the last line is
lastline <- which(wb[, 1] == "")[1] - 1
# print note to screen (data source and last update)
wb[(lastline + 1):dim(wb)[1], 1]
# keep only the data
wb <- wb[1:lastline, ]
cbind(unique(wb$Series.Name), unique(wb$Series.Code))
# create new columns to fill in normalized thresholds
newcols <- data.frame("weight_thres_pp" = rep(NA, dim(xl)[1]),
                      "size_thres_pp" = rep(NA, dim(xl)[1]))
xl <- cbind(xl, newcols)


# ------------------------------------------------
#  Data per Country in table
# ------------------------------------------------

## BELGIUM ##
# NOTE: it is in NCU
xl[1, 1:8]
ww <- which(wb$Country.Name == "Belgium"
            & wb$Series.Name == "GDP per capita (current LCU)")
normfactor <- as.numeric(wb[ww,grep(2014, colnames(wb))])

thres <- as.numeric(xl[xl$Country == "Belgium", "weight_thres_NCU"])
xl[xl$Country == "Belgium", "weight_thres_pp"] <-  100*thres/normfactor

xl[xl$Country == "Belgium", "size_thres"] <-  ""
xl[xl$Country == "Belgium", "size_thres_pp"] <-  ""

## COSTA RICA ##
# NOTE: - it is in CRC
#       - https://data.worldbank.org/country/costa-rica?view=chart
#       - GDP = USD 64b
#       - GDP = LCU 40tr
ww <- which(wb$Country.Name == "Costa Rica"
            & wb$Series.Name == "GDP (current LCU)")
as.numeric(wb[ww, grep(2021, colnames(wb))]) / 10^12

xl[2, 1:8]
ww <- which(wb$Country.Name == "Costa Rica"
            & wb$Series.Name == "GDP per capita (current LCU)")
normfactor <- as.numeric(wb[ww, grep(2015, colnames(wb))])
thres <- as.numeric(xl[xl$Country == "Costa Rica", "weight_thres_NCU"])
xl[xl$Country == "Costa Rica", "weight_thres_pp"] <-  100*thres/normfactor
xl[xl$Country == "Costa Rica", "size_thres"]      <-  ""
xl[xl$Country == "Costa Rica", "size_thres_pp"]   <-  ""
rm(thres, normfactor, ww)

## DOMINICAN REPUBLIC ##
# NOTE: it has no threshold
xl[3, 1:8]
xl[xl$Country == "Dominican Republic", "weight_thres_pp"] <-  0
xl[xl$Country == "Dominican Republic", "size_thres_pp"]   <-  0

## CHILE ##
# NOTE: it has no weight threshold but has a SIZE threshold
xl[4, 1:8]
ww <- which(wb$Country.Name == "Chile" & wb$Series.Name == "GDP per capita (current LCU)")
normfactor <- as.numeric(wb[ww, grep(2015, colnames(wb))])

thres <- as.numeric(xl[xl$Country == "Chile","size_thres_NCU"] )
xl[xl$Country == "Chile", "size_thres_pp"]   <-  thres/normfactor*100
xl[xl$Country == "Chile", "weight_thres_pp"] <-  0
xl[xl$Country == "Chile", "weight_thres"]    <-  paste0(0, " CLP")
rm(thres, normfactor, ww)

## ECUADOR ##
# NOTE: it has no threshold
xl[5, 1:8]
xl[xl$Country == "Ecuador", "weight_thres_pp"] <-  0
xl[xl$Country == "Ecuador", "size_thres_pp"]   <-  0

## ESTONIA ##
# NOTE: It has a range for threshold
xl[6, 1:8]
ww <- which(wb$Country.Name == "Estonia" & wb$Series.Name == "GDP per capita (current LCU)")
normfactor <- as.numeric(wb[ww, grep(2021, colnames(wb))])
threshold_string <- xl[xl$Country == "Estonia", "weight_thres_NCU"]
threshold_values <- as.numeric(unlist(strsplit(threshold_string, "--")))
threshold_tuple <- paste(round(100*(threshold_values / normfactor), 2), collapse = "--")
thres <- as.numeric(xl[xl$Country == "Estonia","size_thres_NCU"] )
xl[xl$Country == "Estonia", "size_thres_pp"]   <-  100*thres/normfactor
xl[xl$Country == "Estonia", "weight_thres_pp"] <-  threshold_tuple
xl[xl$Country == "Estonia", "weight_thres"]    <-  paste0(threshold_string, " EUR")

## HUNGARY ##
# NOTE: it has several thresholds

# Hungary 2018
xl[7, 1:8]
ww <- which(wb$Country.Name == "Hungary" & wb$Series.Name == "GDP per capita (current LCU)")
normfactor <- as.numeric(wb[ww, grep(2018, colnames(wb))])
wwxl <- which(xl$Country == "Hungary" & xl$Year == "2015--2018")
thres <- as.numeric(xl[wwxl, "weight_thres_NCU"])
xl[wwxl, "weight_thres_pp"] <-  100*thres/normfactor
xl[wwxl, "size_thres"]      <-  0
xl[wwxl, "size_thres_pp"]   <-  0
rm(thres, wwxl, normfactor, ww)

# Hungary 2020
xl[8, 1:8]
ww <- which(wb$Country.Name == "Hungary" & wb$Series.Name == "GDP per capita (current LCU)")
normfactor <- as.numeric(wb[ww, grep(2020, colnames(wb))])
wwxl <- which(xl$Country == "Hungary" & xl$Year == "2018--2020")
thres <- as.numeric(xl[wwxl, "weight_thres_NCU"])
xl[wwxl, "weight_thres_pp"] <-  100*thres/normfactor
xl[wwxl, "size_thres"]      <-  0
xl[wwxl, "size_thres_pp"]   <-  0
rm(thres, wwxl, normfactor, ww)

# Hungary 2021
# NOTE: threshold is 0
xl[9, 1:8]
ww <- which(wb$Country.Name == "Hungary" & wb$Series.Name == "GDP per capita (current LCU)")
normfactor <- as.numeric(wb[ww, grep(2021, colnames(wb))])
wwxl <- which(xl$Country == "Hungary" & xl$Year == "2021")
thres <- as.numeric(xl[wwxl, "weight_thres_NCU"])
xl[wwxl, "weight_thres_pp"] <-  100*thres/normfactor
xl[wwxl, "size_thres"]      <-  0
xl[wwxl, "size_thres_pp"]   <-  0
rm(thres, wwxl, normfactor, ww)

## KENYA ##
# NOTE: it has a SIZE threshold
xl[10, 1:8]
ww <- which(wb$Country.Name == "Kenya" & wb$Series.Name == "GDP per capita (current LCU)")
normfactor <- as.numeric(wb[ww, grep(2019, colnames(wb))])
thres <- as.numeric(xl[xl$Country == "Kenya", "size_thres_NCU"])
xl[xl$Country == "Kenya", "size_thres_pp"]   <-  100*thres/normfactor
xl[xl$Country == "Kenya", "weight_thres"]    <-  paste0(0, " KES")
xl[xl$Country == "Kenya", "weight_thres_pp"] <-  0
rm(thres, normfactor, ww)

## SPAIN ##
# NOTE: it is in NCU
xl[11, 1:8]
ww <- which(wb$Country.Name == "Spain" & wb$Series.Name == "GDP per capita (current LCU)")
# NOTE: we use 2010 rather than 2009
normfactor <- as.numeric(wb[ww, grep(2010, colnames(wb))])
thres <- as.numeric(xl[xl$Country == "Spain", "weight_thres_NCU"])
xl[xl$Country == "Spain", "weight_thres_pp"] <-  100*thres/normfactor
xl[xl$Country == "Spain", "size_thres"]      <- ""
xl[xl$Country == "Spain", "size_thres_pp"]   <- ""
rm(thres, normfactor, ww)

## TURKEY ##
# NOTE: it is in NCU
xl[12, 1:8]
ww <- which(wb$Country.Name == "Turkiye" & wb$Series.Name == "GDP per capita (current LCU)")
normfactor <- as.numeric(wb[ww, grep(2014, colnames(wb))])
thres <- as.numeric(xl[xl$Country == "Turkey", "weight_thres_NCU"])
xl[xl$Country == "Turkey", "weight_thres_pp"] <-  100*thres/normfactor
xl[xl$Country == "Turkey", "size_thres"]      <-  ""
xl[xl$Country == "Turkey", "size_thres_pp"]   <-  ""
rm(thres, normfactor, ww)


# ------------------------------------------------
#  Clean and Export Table
# ------------------------------------------------

# remove redundant columns
tokeep <- match(c("Country", "Year",
                  "weight_thres", "weight_thres_pp",
                  "size_thres", "size_thres_pp", "Paper"), colnames(xl))
xl <- xl[, tokeep]
# clean up some names
xl[xl$Country == "Dominican Republic",1] <- "Domin.  Rep. "
# round up values
xl[xl$Country != "Estonia", "weight_thres_pp"] <- round(as.numeric(xl[xl$Country != "Estonia" , "weight_thres_pp"]), 2)
xl[, "size_thres_pp"] <- as.character(round(as.numeric(xl[, "size_thres_pp"])))
# clean up currency units
xl[xl$Country == "Chile","size_thres"] <- "250m CLP"
xl[xl$Country == "Kenya","size_thres"] <- "5m KES"
# commas indicating thusands
xl$weight_thres <- fmt_thousands(xl$weight_thres)
xl$size_thres <- fmt_thousands(xl$size_thres)
xl$size_thres_pp <- fmt_thousands(xl$size_thres_pp)

XTAB <- xl
XTAB <- xtable(XTAB)

addtorow <- list()
addtorow$pos <- list(0, 12)
addtorow$command <- c(
'\\begin{table}[htbp]
\\centering
\\caption{Reporting thresholds by country} 
\\resizebox{\\textwidth}{!}{%
\\begin{tabular}{llrrrrl}
\\toprule 
Dataset    &  Year    & \\multicolumn{2}{c}{Transaction size threshold} & \\multicolumn{2}{c}{Firm size threshold} & Source \\\\
 &  & \\multicolumn{1}{c}{Raw}  & \\multicolumn{1}{c}{/GDPpp} & \\multicolumn{1}{c}{Raw} & \\multicolumn{1}{c}{/GDPpp} &  \\\\
\\midrule ',
paste('\\bottomrule \\multicolumn{7}{c}{ \\begin{minipage}{20.4cm}~\\\\%
\\justifying \\noindent
\\textit{Notes}: The table shows the official reporting thresholds as gathered from the literature,  omitting details of each country\'s idiosyncratic rules.
The thresholds on the value of transactions and on firm sizes are shown in absolute and relative terms, expressed as percentage of GDP per person (``/GDPpp\'\'),  using World Bank data for the most recent year in the ``Year\'\' column. 
The table does not consider thresholds imposed by researchers (e.g.  removing small firms). 
In Estonia, the threshold is \\euro 1,000 per month, although there are a lot of voluntary registrations below the threshold. We determined an effective annual firm-level transaction threshold as follows. The maximum a pair can trade while staying below the threshold is \\euro 999 in each of the 12 months, which we round to \\euro 12,000. The minimum a pair needs to trade be subject to mandatory reporting is \\euro 1,000, occurring in only one of the 12 months. Thus, the effective threshold is somewhere between \\euro 1,000 and \\euro 12,000. A similar but more complex issue arises for Hungary (see Appendix~\\ref{app:SupplyChain_hungary}), where we report the threshold as if it were always on an annual basis. Figure~\\ref{fig:weights_distr} shows that this works well, in the sense that the data exhibits a strong shift at that value.
\\end{minipage}
}\\
\\end{tabular}
}
\\label{tab:thresholds}
\\end{table}',
sep = ""))

table_name <- file.path(dirOutput, '2_tab_thresholds.tex')

print.xtable(XTAB, 
             file = table_name, only.contents = T,
             hline.after = NULL,
             sanitize.text.function = function(x){x},
             sanitize.rownames.function = function(x){x},
             include.rownames = F,
             include.colnames = F,
             caption.placement = "top",
             tabular.environment = F,
             add.to.row = addtorow,
             comment = F)


# save the table
write.csv(xl, file = file.path(dirdata, 'literature', 'thresholds_cleaned.csv'), row.names = F)

print('Table 2 of reporting threshold by country exported.')