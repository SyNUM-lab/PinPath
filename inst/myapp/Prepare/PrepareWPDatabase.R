# Clear workspace and console
rm(list = ls())
cat("\014")
gc()

# Load packages
library(KEGGgraph)
library(AnnotationDbi)

# Set working directory
setwd("C:/Users/jarno/GitHub/ShinyPath/app/Pathways/")

WPid <- "20251110"
Organisms <- c("Homo_sapiens", "Bos_taurus", "Caenorhabditis_elegans",
               "Mus_musculus", "Rattus_norvegicus")

for (o in 1:length(Organisms)){
  download.file(paste0("https://data.wikipathways.org/current/gmt/wikipathways-",
                       WPid,
                       "-gmt-",
                       Organisms[o],
                       ".gmt"),
                destfile = paste0("C:/Users/jarno/GitHub/ShinyPath/app/Pathways/GMT/wikipathways-",
                                  WPid,
                                  "-gmt-",
                                  Organisms[o],
                                  ".gmt"))
}




ensembl <- useMart("ensembl")
gmt_all <- list()
for (o in Organisms){
  
  # Read GMT file
  gmt <- clusterProfiler::read.gmt.wp(paste0("GMT/wikipathways-", WPid, "-gmt-",
                                             o,
                                             ".gmt"))
  
  # Select biomaRt dataset
  biomaRt_dataset <- switch(o,
                            "Homo_sapiens" = "hsapiens_gene_ensembl" ,
                            "Bos_taurus" = "btaurus_gene_ensembl",
                            "Caenorhabditis_elegans" = "celegans_gene_ensembl",
                            "Mus_musculus" = "mmusculus_gene_ensembl",
                            "Rattus_norvegicus" = "rnorvegicus_gene_ensembl"
  )
  
  # Get annotations
  ensembl_dataset <- useDataset(biomaRt_dataset, mart=ensembl)
  
  if (o == "Homo_sapiens"){
    annotations <- getBM(attributes=c("ensembl_gene_id",
                                      "entrezgene_id",
                                      "hgnc_symbol",
                                      "uniprot_gn_id"
                                      ), 
                         filters = "entrezgene_id",
                         values = gmt$gene,
                         mart = ensembl_dataset)
  } else{
    annotations <- getBM(attributes=c("ensembl_gene_id",
                                      "entrezgene_id",
                                      "external_gene_name",
                                      "uniprot_gn_id"), 
                         filters = "entrezgene_id",
                         values = gmt$gene,
                         mart = ensembl_dataset)
  }
  
  
  annotations$entrezgene_id <- as.character(annotations$entrezgene_id)
  PathwayInfo_WP <- left_join(gmt, annotations, by = c("gene" = "entrezgene_id"))
  colnames(PathwayInfo_WP) <- c("name", "version", "wpid",
                      "species", "ENTREZID", "ENSEMBL", "SYMBOL", "UNIPROT")
  
  save(PathwayInfo_WP, 
       file = paste0("C:/Users/jarno/GitHub/ShinyPath/app/Pathways/WP_", 
                     stringr::str_replace(o, " ", "_"), ".RData"))
}

