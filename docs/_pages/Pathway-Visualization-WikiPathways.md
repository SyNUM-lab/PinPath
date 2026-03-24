---
title: "WikiPathways pathway visualization"
permalink: /wikipathways-visualization
layout: pages
---
<br>

# WikiPathways pathway visualization

This tutorial will cover visualizing differential expression analysis
results onto *WikiPathways* pathway diagrams.

<br>

## Installation

First, make sure you have *PinPath* installed.

```r
library(PinPath)
library(rWikiPathways)
library(org.Hs.eg.db)
```

<br>

## Dataset

The example dataset we will use compares the expression of transcripts
in lung cancer biopsies versus normal tissue. Differential expression
analysis has already been performed, generating log2FCs and p-values for
each gene.

```r
lung_expr <- read.csv(
	system.file("extdata","data-lung-cancer.csv", package ="PinPath"), 
	stringsAsFactors = FALSE)
```

In the pathway, we want to show which gene is significantly
differentially expressed. For this, we will use an adjusted p-value
cutoff of 0.05.

```r
lung_expr$Significant <- ifelse(lung_expr$adj.P.Value < 0.05, "Yes", "No")
```

<br>

## Set colors

After we have loaded and prepared the differential expression analysis
statistics, we need to define how to color the statistics in the pathway
diagram. In this vignette, we will plot the log2FC and significance of
each gene on the pathway diagram. We can start by loading the default
color palette.

```r
colorList <- PinPath::defaultColorList(lung_expr[,c("log2FC", "Significant")])
```

In the next step, we can adjust the default color palette to the desired
color values. For instance, we want to display a green color when a gene
is differentially expressed and a white color when it is not.

```r
colorList[["Significant"]]$Color <- c(
	"Yes" = "green",
	"No" = "white")
```

Furthermore, we can set the minimum and maximum value of the log2FC
color gradient to -1.5 and 1.5, respectively.

```r
colorList[["log2FC"]]$ColorVal <- c(
	"MinVal" = -1.5,
	"MidVal" = 0,
	"MaxVal" = 1.5)
```

<br>

## Plot pathway

We can now plot the differential expression statistics on the *WP5087:
Pleural mesothelioma* pathway. If not specified otherwise, the pathway
and legend image will be saved in your working directory. The pathway
image will be opened by default.

```r
# WP5087: Pleural mesothelioma
pathway_id <- "WP5087"
infile <- rWikiPathways::getPathway(pathway_id)

# Draw pathway
pathVis <- PinPath::drawGPML(
	infile = infile,
	annGenes = "org.Hs.eg.db",
	inputDB = "ENSEMBL",
	featureIDs = lung_expr$GeneID,
	colorVar = lung_expr[,c("log2FC", "Significant")],
	colorList = colorList,
	nodeTable = TRUE,
	legend = TRUE)
```

You can find the file locations of the pathway and legend images in `pathVis[["Pathway"]]` and `pathVis[["Legend"]]`, respectively.

<p>Pathway image:</p>
<img src="{{ "/assets/img/pathways/Pleural_mesothelioma_WP5087_r140461_Homo_sapiens.svg" | relative_url }}"
width="1000">
<hr>
<p>Legend image:</p>
<img src="{{ "/assets/img/pathways/legend_Pleural_mesothelioma_WP5087_r140461_Homo_sapiens.svg" | relative_url }}"
width="500">

<br>

## Plot network

You can also plot the pathway as a network. In contrast to the pathway
diagram, every element (e.g., gene/protein) is represented exactly once.

```r
pathVis <- PinPath::GPML2Network(
	infile = infile,
	annGenes = "org.Hs.eg.db",
	inputDB = "ENSEMBL",
	featureIDs = lung_expr$GeneID,
	colorVar = lung_expr[,c("log2FC", "Significant")],
	colorList = colorList,
	nodeSize = 0.5,
	nodeTable = TRUE,
	legend = TRUE) 
```

<p>Network image:</p>
<img src="{{ "/assets/img/pathways/network_Pleural_mesothelioma_WP5087_r140461_Homo_sapiens.svg" | relative_url }}"
width="1000">
<hr>
<p>Legend image:</p>
<img src="{{ "/assets/img/pathways/legend_Pleural_mesothelioma_WP5087_r140461_Homo_sapiens.svg" | relative_url }}"
width="500">

<br>

## Session info
```r
sessionInfo()

    ## R version 4.5.1 (2025-06-13 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 26200)
    ## 
    ## Matrix products: default
    ##   LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_Europe.utf8  LC_CTYPE=English_Europe.utf8   
    ## [3] LC_MONETARY=English_Europe.utf8 LC_NUMERIC=C                   
    ## [5] LC_TIME=English_Europe.utf8    
    ## 
    ## time zone: Europe/Amsterdam
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats4    stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ## [1] org.Hs.eg.db_3.22.0  AnnotationDbi_1.72.0 IRanges_2.44.0      
    ## [4] S4Vectors_0.48.0     Biobase_2.70.0       BiocGenerics_0.56.0 
    ## [7] generics_0.1.4       rWikiPathways_1.30.0 PinPath_0.99.0      
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyr_1.3.2         xml2_1.5.2          bitops_1.0-9       
    ##  [4] shape_1.4.6.1       stringi_1.8.7       RSQLite_2.4.5      
    ##  [7] digest_0.6.39       magrittr_2.0.4      grid_4.5.1         
    ## [10] evaluate_1.0.5      fastmap_1.2.0       blob_1.3.0         
    ## [13] DBI_1.2.3           httr_1.4.7          purrr_1.2.1        
    ## [16] XML_3.99-0.20       Biostrings_2.78.0   textshaping_1.0.4  
    ## [19] cli_3.6.5           rlang_1.1.7         crayon_1.5.3       
    ## [22] XVector_0.50.0      bit64_4.6.0-1       withr_3.0.2        
    ## [25] cachem_1.1.0        yaml_2.3.12         otel_0.2.0         
    ## [28] tools_4.5.1         memoise_2.0.1       dplyr_1.1.4        
    ## [31] curl_7.0.0          vctrs_0.7.1         R6_2.6.1           
    ## [34] png_0.1-8           magick_2.9.0        lifecycle_1.0.5    
    ## [37] stringr_1.6.0       Seqinfo_1.0.0       KEGGREST_1.50.0    
    ## [40] bit_4.6.0           pkgconfig_2.0.3     pillar_1.11.1      
    ## [43] Rcpp_1.1.1          data.table_1.18.2.1 glue_1.8.0         
    ## [46] systemfonts_1.3.1   xfun_0.56           tibble_3.3.1       
    ## [49] tidyselect_1.2.1    rstudioapi_0.18.0   knitr_1.51         
    ## [52] rjson_0.2.23        htmltools_0.5.9     svglite_2.2.2      
    ## [55] rmarkdown_2.30      compiler_4.5.1      gridBase_0.4-7     
    ## [58] RCurl_1.98-1.17
```
