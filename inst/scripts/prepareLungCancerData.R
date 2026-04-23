#' prepareLungCancerData
#' 
#' Prepare lunger cancer example data included in the package.
#' 
#' @return example data csv file
#' @details Differential gene expression analysis statistics as provided by 
#' the [rWikiPathways] package. The dataset compares the gene expression
#' lung cancer biopses versus normal tissue. It contains columns of 
#' gene identifiers  (Ensembl IDs), gene symbols, log2foldchange values, 
#' P-Values and adjusted P-Values.
prepareLungCancerData <- function() {
    lung_expr <- read.csv(system.file(
        "extdata","data-lung-cancer.csv", package="rWikiPathways"),
        stringsAsFactors = FALSE)
    write.csv(lung_expr, file = "data-lung-cancer.csv", row.names = FALSE)
}