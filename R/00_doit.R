# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_pre_analysis.R")
source(file = "R/04_plots.R")
source(file = "R/05_plots_heatmap.R")
source(file = "R/06_kmeans.R")
source(file = "R/07_pca.R")
rmarkdown::render(input = "doc/report.Rmd", output_dir = "doc/")
# ADD KNITR
# CREATE A FLOW CHART