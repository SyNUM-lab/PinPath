
#******************************************************************************#
# load Functions
#******************************************************************************#
sapply(list.files("Functions", full.names = TRUE), source)

#******************************************************************************#
# CRAN packages
#******************************************************************************#

library(XML)
library(xml2)
library(shiny)
library(svglite)
library(BiocManager)
library(stringr)
library(dplyr)
library(tidyr)
library(magick)
library(magrittr)
library(purrr)
library(grid)
library(gridBase)
library(shape)
library(data.table)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(shinybusy)
library(prompter)
library(colourpicker)
library(colorspace)
library(shinythemes)
#library(metaboliteIDmapping)

whichID <- function(x){
  IDtype <- "SYMBOL"
  if (sum(substr(x,1,3) == "ENS") > 0.5*length(x)){
    IDtype <- "ENSEMBL"
  }
  if (sum(!is.na(as.numeric(x))) > 0.5*length(x)){
    IDtype <- "ENTREZID"
  }
  return(IDtype)
}
# Get required CRAN packages
# CRANpackages <- read.table("PackageList/CRANpackages.txt", header = TRUE)
# 
# # Install remotes package
# if (!requireNamespace("remotes", quietly = TRUE)){
#   install.packages("remotes", ask = FALSE)
# }
# require("remotes", character.only = TRUE)
# 
# #Install (if not yet installed) and load the required packages:
# for (pkg in 1:nrow(CRANpackages)) {
# 
#   # Install latest version
#   if (CRANpackages$direction[pkg] == "smaller"){
# 
#     # Install package if not installed
#     if (!requireNamespace(CRANpackages$name[pkg], quietly = TRUE)){
#       install.packages(CRANpackages$name[pkg],
#                        ask = FALSE,
#                        repos = "https://cloud.r-project.org")
#     } else {
# 
#       #Install package if package version is too low:
#       if (packageVersion(CRANpackages$name[pkg]) < CRANpackages$version[pkg]){
#         if (CRANpackages$name[pkg] %in% .packages()){
#           detach(paste0("package:", CRANpackages$name[pkg]), unload = TRUE,
#                  character.only = TRUE)
#         }
#         install.packages(CRANpackages$name[pkg],
#                          ask = FALSE,
#                          repos = "https://cloud.r-project.org",
#                          upgrade = "never")
#       }
#     }
#   }
# 
#   # Install specific version (not needed yet)
#   if (CRANpackages$direction[pkg] == "unequal"){
# 
#     # Install package if not installed
#     if (!requireNamespace(CRANpackages$name[pkg], quietly = TRUE)){
#       remotes::install_version(CRANpackages$name[pkg],
#                                CRANpackages$version[pkg],
#                                repos = "https://cloud.r-project.org")
#     } else {
# 
#       #Install package if package version is not correct
#       if (packageVersion(CRANpackages$name[pkg]) != CRANpackages$version[pkg]){
#         if (CRANpackages$name[pkg] %in% .packages()){
#           detach(paste0("package:", CRANpackages$name[pkg]), unload = TRUE,
#                  character.only = TRUE)
#         }
#         remotes::install_version(CRANpackages$name[pkg],
#                                  CRANpackages$version[pkg],
#                                  repos = "https://cloud.r-project.org",
#                                  upgrade = "never")
#       }
#     }
#   }
# 
#   require(as.character(CRANpackages$name[pkg]), character.only = TRUE)
# }

#******************************************************************************#
#Bioconductor packages
#******************************************************************************#

library(org.Hs.eg.db)
# library(org.Mm.eg.db)
# library(org.Bt.eg.db)
# library(org.Rn.eg.db)
library(rWikiPathways)
library(enrichplot)
library(clusterProfiler)
library(BiocFileCache)

# # Get required Bioconductor packages:
# BiocPackages <- read.table("PackageList/Bioconductorpackages.txt", header = TRUE)
# 
# for (pkg in 1:nrow(BiocPackages)) {
#   if (!requireNamespace(BiocPackages$name[pkg], quietly = TRUE)){
#     BiocManager::install(BiocPackages$name[pkg], ask = FALSE)
#   } else{
#     if (packageVersion(BiocPackages$name[pkg]) < BiocPackages$version[pkg]){
#       BiocManager::install(BiocPackages$name[pkg], ask = FALSE)
#     }
#   }
#   require(as.character(BiocPackages$name[pkg]), character.only = TRUE)
# }