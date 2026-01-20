# Clear workspace and console
rm(list = ls())
cat("\014") 
gc()

# Load packages
library(KEGGgraph)
library(AnnotationDbi)

organisms <- c("Homo sapiens", "Bos taurus", "Caenorhabditis elegans", 
               "Mus musculus", "Rattus norvegicus")



for (o in organisms){
  pkg <- switch(o,
                "Homo sapiens" = "org.Hs.eg.db",
                "Bos taurus" = "org.Bt.eg.db",
                "Caenorhabditis elegans" = "org.Ce.eg.db",
                "Mus musculus" = "org.Mm.eg.db",
                "Rattus norvegicus" = "org.Rn.eg.db"
  )
  
  abbr <- switch(o,
                 "Homo sapiens" = "hsa",
                 "Bos taurus" = "bta",
                 "Caenorhabditis elegans" = "cel",
                 "Mus musculus" = "mmu",
                 "Rattus norvegicus" = "rno"
  )
  
  if (!requireNamespace(pkg, quietly = TRUE)){
    BiocManager::install(pkg, ask = FALSE)
  }
  require(as.character(pkg), character.only = TRUE)
  
  
  KEGGann <- unique(AnnotationDbi::select(BiocGenerics::get(pkg), 
                                          columns = c("ENSEMBL", "SYMBOL", "UNIPROT", "PATH"), 
                                          keys = keys(BiocGenerics::get(pkg)), 
                                          keytpe = "ENTREZID"))
  KEGGann <- KEGGann[!is.na(KEGGann$PATH),]
  KEGGann$PATH <- paste0(abbr, KEGGann$PATH)
  
  KEGGids <- unique(KEGGann$PATH)
  
  
  bfc <- BiocFileCache::BiocFileCache()
  
  PathwayNames <- rep(NA, length(KEGGids))
  for (i in 1:length(KEGGids)){
    id <- KEGGids[i]
    tryCatch({
      file_name <- BiocFileCache::bfcrpath(bfc,
                                           paste0("https://rest.kegg.jp/get/",id,"/kgml"))
      doc <- XML::xmlParse(file_name)
      kgml <- XML::xmlToList(doc)
      PathwayNames[i] <- kgml[[length(kgml)]]["title"]
    }, error = function(cond){
      PathwayNames[i] <- NA
    })
  }
  
  names_df <- data.frame("keggid" = KEGGids,
                         "name" = PathwayNames)
  PathwayInfo_KEGG <- inner_join(names_df, KEGGann,
                                 by = c("keggid" = "PATH"))
  PathwayInfo_KEGG$version <- paste0(packageVersion(pkg), 
                                     " (", Sys.Date(), ")")
  PathwayInfo_KEGG$species <- o
  PathwayInfo_KEGG <- PathwayInfo_KEGG[,c("name", "version", "keggid",
                                          "species", "ENTREZID", "ENSEMBL", 
                                          "SYMBOL", "UNIPROT")]
  
  save(PathwayInfo_KEGG, 
       file = paste0("C:/Users/jarno/GitHub/ShinyPath/app/Pathways/KEGG_", 
                     stringr::str_replace(o, " ", "_"), ".RData"))
}



