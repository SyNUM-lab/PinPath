library(rWikiPathways)
library(PinPath)
library(org.Hs.eg.db)


test_that("Custom GPML visualization", {

  lung_expr <- read.csv(system.file("extdata","data-lung-cancer.csv", package="PinPath"),
                        stringsAsFactors = FALSE)
  lung_expr$isSig <- ifelse(lung_expr$P.Value < 0.05, "Yes", "No")

  infile <- system.file("extdata","test-gpml.gpml", package="PinPath")

  pathVis <- PinPath::drawGPML(
    infile = infile,
    outdir = tempdir(),
    annGenes = "org.Hs.eg.db",
    inputDB = "ENSEMBL",
    featureIDs = lung_expr$GeneID,
    colorVar = lung_expr[,c("log2FC", "isSig")],
    nodeTable = TRUE,
    pathInfo = TRUE,
    legend = TRUE)

  expect_equal(length(pathVis),4)
})


