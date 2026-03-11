library(PinPath)
library(org.Hs.eg.db)
library(BiocFileCache)


test_that("KEGG visualization", {

  lung_expr <- read.csv(system.file("extdata","data-lung-cancer.csv", package="PinPath"),
                        stringsAsFactors = FALSE)

  # Select pathway
  pathway_id <- "hsa05223"
  infile <- BiocFileCache::bfcrpath(BiocFileCache::BiocFileCache(),
                                    paste0("https://rest.kegg.jp/get/",pathway_id,"/kgml"))

  # Plot pathway
  pathVis <- PinPath::drawKGML(
    infile = infile,
    outdir = tempdir(),
    annGenes = "org.Hs.eg.db",
    inputDB = "ENSEMBL",
    featureIDs = lung_expr$GeneID,
    colorVar = lung_expr[,"log2FC"],
    nodeTable = TRUE,
    pathInfo = TRUE,
    legend = TRUE,
    openFile = FALSE) # <-- set to TRUE to open the image automatically

  expect_equal(length(pathVis),4)
})
