# PinPath
![PinPath](/docs/assets/img/logo.png)

PinPath is an R package for visualizing (omics) data onto pathway diagrams, and **pinpoint** where in the pathway the relevant changes occur.
Results from (epi)genomics, transcriptomics, (phospho)proteomics, metabolomics and many more experiments can be visualized onto pathway diagrams from KEGG and WikiPathways. 
You can also use your own GPML and KGML files to visualize data onto custom pathways. 
As long as your data can be linked to genes, proteins, or metabolites, you can visualize it using PinPath. 
Do you want to get more information or run the PinPath web app? Please visit our website: [tba]

## Publications
No publications yet....

## Installation
Use the following R code to install the development version of the PinPath package:

```r
# install "remotes" package
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
system.file("extdata", "data-lung-cancer.csv",package="PinPath"),
                      stringsAsFactors = FALSE)
```

Now, you can plot the data onto the *Non-small cell lung cancer (WP4255)*:
```r
# Select pathway
infile <- rWikiPathways::getPathway("WP4255")

# Draw pathway
pathVis <- PinPath::drawGPML(
           infile = infile,
           outdir = tempdir(),
           annGenes = "org.Hs.eg.db",
           inputDB = "ENSEMBL",
           geneIDs = lung_expr$GeneID,
           colorVar = lung_expr[,"log2FC"],
           nodeTable = TRUE,
           legend = TRUE) 
```

![Pathway](/docs/assets/img/pathways/Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg)
![Legend](/docs/assets/img/pathways/legend_Non.small_cell_lung_cancer_WP4255_r140411_Homo_sapiens.svg)

You can also plot it as a network:
```r
 pathVis <- GPML2Network(
   infile = infile,
   annGenes = "org.Hs.eg.db",
   inputDB = "ENSEMBL",
   geneIDs = lung_expr$GeneID,
   colorVar = lung_expr[,"log2FC"],
   nodeTable = TRUE,
   legend = TRUE)
```

![Pathway](/docs/assets/img/pathways/network_Pleural_mesothelioma_WP5087_r140461_Homo_sapiens.svg)
