if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])
  wd <- if (length(script_path)) dirname(normalizePath(script_path)) else getwd()
}
setwd(wd)

allpaths <- c(
"data/analysis/ecuador/",
"data/analysis/ecuador/truncation/",
"data/analysis/factset/",
"data/analysis/hungary/",
"data/cleaned/ecuador/",
"data/cleaned/factset/",
"data/cleaned/hungary/",
"data/national_statistics/hungary/",
"data/national_statistics/P_Data_Extract_From_World_Development_Indicators/",
"data/raw/ecuador/",
"data/raw/factset/shipping",
"data/raw/factset/supply_chain",
"data/raw/hungary/",
"data/literature",
"results/figures/",
"results/figures/truncation/",
"results/tables/")

for(x in 1:length(allpaths)){
  path <- paste(wd,allpaths[x],sep="/")
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

intermediate_paths <- c(
"data/",
"data/analysis/",
"data/cleaned/",
"data/raw/")

paths <- c(allpaths, intermediate_paths)
for (path_type in paths) {
  for (path in path_type) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(path, ".gitkeep"))
  }
}
