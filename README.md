![PinPath](/inst/img/logo_grey.png)

# About PinPath
PinPath is an R package for visualizing (omics) data onto pathway diagrams, and **pinpoint** where in the pathway the relevant changes occur.
Results from (epi)genomics, transcriptomics, (phospho)proteomics, metabolomics and many more experiments can be visualized onto pathway diagrams from KEGG and WikiPathways. 
You can also use your own GPML and KGML files to visualize data onto custom pathways. 
As long as your data can be linked to genes, proteins, or metabolites, you can visualize it using PinPath. 

Please visit our website for more information: [SyNUM-lab.github.io/PinPath](https://synum-lab.github.io/PinPath/)

## Installation
Use the following R code to install the development version of PinPath:

```r
# install "remotes" package
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")
      
# Install PinPath from GitHub
remotes::install_github("SyNUM-lab/PinPath") 
```

## Quick start
First, load necessary packages and data:
```r
# Load packages
library(PinPath)
library(rWikiPathways)
library(org.Hs.eg.db)

# Load example data
lung_expr <- read.csv(
    system.file("extdata", "data-lung-cancer.csv", package="PinPath"), 
    stringsAsFactors = FALSE)
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

![Pathway](/inst/img/Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg)
![Legend](/inst/img/legend_Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg)

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

![Pathway](/inst/img/network_Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg)
![Legend](/inst/img/legend_Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg)
