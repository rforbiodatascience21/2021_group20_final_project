# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_pre_analysis.R")
source(file = "R/04_plots.R")
source(file = "R/05_plots_heatmap.R")
source(file = "R/06_kmeans.R")
source(file = "R/07_pca.R")
source(file = "R/08_regression.R")


# Knit the presentation ---------------------------------------------------
rmarkdown::render(input = "doc/presentation.Rmd", 
                  output_dir = "doc/")