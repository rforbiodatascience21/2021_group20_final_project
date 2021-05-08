R Notebook
================

# 2021\_group20\_final\_project

Final project for the course “22100 - R for Bio Data Science”. The
contributors to the project are Mikkel Spallou Eriksen, Rut Mas de Les
Valls, Anna Oliver Almirall, Paul Jeremy Simon and Elisa Catafal Tardos.

# Dataset

Original paper : Yeoh EJ, Ross ME, Shurtleff SA, et al. Classification,
subtype discovery, and prediction of outcome in pediatric acute
lymphoblastic leukemia by gene expression profiling. Cancer Cell.
2002;1(2):133-143. <doi:10.1016/s1535-6108(02)00032-6>

The dataset has been downloaded from

``` r
library(stjudem)
```

It has been tidied to reach a clean dataset similar to the one given
[here](https://github.com/ramhiser/datamicroarray/wiki/Yeoh-%282002%29).

# Outline

All R scripts are available in the folder R/  
1\. 01\_load loads the dataset and writes it into tsv.gz files  
2\. 02\_clean filters cancer types and convert probe names into gene
names  
3\. 03\_pre\_analysis selects the top40 differetially expressed gene for
each cancer subtype  
4\. 04\_plot performs several kind of statistical descriptive plots  
5\. 05\_plots\_heatmap aims at reproducing heatmaps from the paper  
6\. 06\_kmeans performs a k means clustering on raw dataset and top40
genes dataset  
7\. 07\_pca performs a PCA on raw dataset and top40 genes dataset

A shiny app with plots has been created and is available
[here](https://paul-simon.shinyapps.io/yeoh_plots/)
