library(rWikiPathways)
library(PinPath)
library(org.Hs.eg.db)


test_that("WikiPathways visualization", {
  
  lung_expr <- read.csv(system.file("extdata","data-lung-cancer.csv", package="PinPath"), 
                        stringsAsFactors = FALSE)

  pathVis <- PinPath::drawGPML(
    infile = rWikiPathways::getPathway("WP4255"),
    outdir = tempdir(),
    annGenes = "org.Hs.eg.db",
    inputDB = "ENSEMBL",
    featureIDs = lung_expr$GeneID,
    colorVar = lung_expr[,"log2FC"],
    nodeTable = TRUE,
    pathInfo = TRUE,
    legend = TRUE)
  
  expect_equal(length(pathVis),4)
})


