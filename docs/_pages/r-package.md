---
title: "R package"
permalink: /r-package
layout: pages
---
<br>

## Tutorials
*  <a href="{{ '/wikipathways-visualization' | relative_url }}">WikiPathways pathway visualization</a>
*  <a href="{{ '/kegg-visualization' | relative_url }}">KEGG pathway visualization</a>

<br>

## Installation
Use the following R code to install the development version of the PinPath package:

```r
# install "remotes" package
install.packages("remotes")
      
# Install PinPath from GitHub
remotes::install_github("SyNUM-lab/PinPath") 
```
<br>

## Quick start
First, load necessary packages and data:
```r
# Load packages
library(PinPath)
library(rWikiPathways)
library(org.Hs.eg.db)

# Load example data
lung_expr <- read.csv(
system.file("extdata", "data-lung-cancer.csv", package="PinPath"), stringsAsFactors = FALSE)
```

Now, you can plot the data onto the *Non-small cell lung cancer (WP4255)* pathway:
```r
# Select pathway
infile <- rWikiPathways::getPathway("WP4255")

# Draw pathway
pathVis <- PinPath::drawGPML(
  infile = infile,
  annGenes = "org.Hs.eg.db",
  inputDB = "ENSEMBL",
  featureIDs = lung_expr$GeneID,
  colorVar = lung_expr[,"log2FC"],
  colorNames = "logFC",
  nodeTable = TRUE,
  legend = TRUE) 
```

You can find the file locations of the pathway and legend images in `pathVis[["Pathway"]]` and `pathVis[["Legend"]]`, respectively.

<img
  src="{{ "/assets/img/pathways/Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg" | relative_url }}"
  alt="Pathway"
  style="width: 70%; height: auto;">
<img
  src="{{ "/assets/img/pathways/legend_Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg" | relative_url }}"
  alt="Legend"
  style="width: 40%; height: auto;">

You can also plot the pathway as a network:
```r
pathVis <- PinPath::GPML2Network(
  infile = infile,
  annGenes = "org.Hs.eg.db",
  inputDB = "ENSEMBL",
  featureIDs = lung_expr$GeneID,
  colorVar = lung_expr[,"log2FC"],
  colorNames = "logFC",
  nodeTable = TRUE,
  legend = TRUE) 
```

<img
  src="{{ "/assets/img/pathways/network_Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg" | relative_url }}"
  alt="Network"
  style="width: 70%; height: auto;">
<img
  src="{{ "/assets/img/pathways/legend_Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg" | relative_url }}"
  alt="Legend"
  style="width: 40%; height: auto;">
<br>
