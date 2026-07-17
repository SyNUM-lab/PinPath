# ------------------------------------------------------------------------------
#' @title Draw pathway from KGML file
#'
#' @description This function draws a pathway from a KGML file with the option
#'   to map, e.g., expression data onto the pathway diagram.
#' @param infile Input KGML file. This can be a character string of the
#'   KGML file location (e.g., "Downloads/WP42500.KGML").
#' @param outdir (optional) Output directory. The pathway and legend images
#'   will be saved in this directory.
#' @param outname (optional) The file name of the output pathway image.
#'   "svg","png",and "pdf" file extensions are accepted. If no file extension is
#'   specified, the pathway and legend image will be generated in .svg format.
#'   The legend file gets the "legend_" prefix.
#' @param featureIDs (optional) \code{character} vector of gene IDs.
#' @param colorVar (optional) \code{vector} or \code{data.frame} for
#'   coloring the nodes in the pathway. This can be for instance a
#'   \code{data.frame} with the log2FCs and significance in the columns.
#'   The (row) order should match \code{featureIDs}. The color rules and
#'   palettes for the supplied values can be set in the colorList parameter.
#' @param annGenes (optional) \code{character} string of the Bioconductor
#'   annotation package (e.g., org.Hs.eg.db).
#' @param annMetabolites (optional) \code{tibble} or \code{data.frame} with
#'   metabolite mapping information (see metaboliteIDmapping package).
#' @param inputDB (optional) Input gene ID type
#'   (SYMBOL, ENTREZID, ENSEMBL, UNIPROT). This can be a \code{character} vector
#'   of \code{length = 1} (if all gene IDs are of the same type) or of
#'   \code{length = nrow(featureIDs)} (if you want to specify the type per
#'   gene ID).
#' @param colorNames (optional) \code{character} vector with names of the
#'   color variables. If \code{colorNames} is NULL, the column names of the
#'   \code{colorVar} \code{data.frame} will be used.
#' @param colorList (optional) A list with information about the coloring of
#'   the nodes. An example can be generated using the \link{defaultColorList}
#'   function.
#' @param NAvalue (optional) Node color for \code{NA} values.
#' @param legend (optional) Logical (TRUE or FALSE). Should the legend be
#'   plotted?
#' @param nodeTable (optional) Logical (TRUE or FALSE). Should a node table
#'   be returned?
#' @param pathInfo (optional) Logical (TRUE or FALSE). Should pathway
#'   information be returned?
#' @param openFile (optional) Logical (TRUE or FALSE). Should the pathway file
#'   be opened after it has been saved?
#'   This option only works for Windows users.
#' @return A \code{list} with the node table and the file location of the
#'   pathway and legend image.
#' @examples
#'
#' # Load example data
#' lung_expr <- read.csv(system.file(
#'     "extdata","data-lung-cancer.csv", package="PinPath"),
#'     stringsAsFactors = FALSE)
#'
#' # Select pathway
#' pathway_id <- "hsa05223"
#' bfc <- BiocFileCache::BiocFileCache()
#' infile <- BiocFileCache::bfcrpath(bfc,
#' paste0("https://rest.kegg.jp/get/",pathway_id,"/kgml"))
#'
#' # Draw pathway
#' pathVis <- PinPath::drawKGML(
#'     infile = infile,
#'     outdir = tempdir(),
#'     annGenes = "org.Hs.eg.db",
#'     inputDB = "ENSEMBL",
#'     featureIDs = lung_expr$GeneID,
#'     colorVar = lung_expr[,"log2FC"],
#'     nodeTable = TRUE,
#'     legend = TRUE,
#'     openFile = FALSE) # <-- set to TRUE to open the image automatically
#'
#' @export

drawKGML <- function(
        infile,outdir = getwd(),outname = NULL,annGenes = NULL,
        annMetabolites = NULL,inputDB = NULL,featureIDs = NULL,colorVar = NULL,
        colorNames = NULL,colorList = NULL,NAvalue = "#F0F0F0",legend = FALSE,
        nodeTable = FALSE,pathInfo = FALSE,openFile = FALSE){
    # Read and prepare GPML file
    kgml <- XML::xmlToList(XML::xmlParse(infile))
    # Set default values if necessary
    if (is.null(outname)){outname <- .makeOutName_KGML(kgml)}
    if (is.null(colorList) & !is.null(colorVar)){
        colorList <- defaultColorList(colorVar, ColorNames = colorNames)}
    # Get patway image
    image_name <- BiocFileCache::bfcrpath(
        BiocFileCache::BiocFileCache(), kgml[[length(kgml)]]["image"])
    img <- magick::image_read(image_name)
    img <-  magick::image_transparent(img, color = "#BFFFBF")
    # Map colors to entries
    entries_df <- .extractEntries(kgml[names(kgml) == "entry"])
    if (!(
        is.null(featureIDs) | is.null(colorVar) |
        (is.null(annGenes) & is.null(annMetabolites)) | is.null(inputDB))){
        colors_df <- .mapColors(
            nodes_df = entries_df, featureIDs = featureIDs,
            colorVar = colorVar, annGenes = annGenes,
            annMetabolites = data.frame(annMetabolites), inputDB = inputDB,
            colorList = colorList, NAvalue = NAvalue)}else{colors_df <- NULL}
    # Draw pathway
    outfile <- .openFile(
        width = dim(magick::image_data(img))[2],
        height = dim(magick::image_data(img))[3],
        outfile = file.path(outdir, outname))
    .makeKGMLpathway(colors_df,img)
    grDevices::dev.off()
    outputList <- list()
    outputList[["Pathway"]] <- outfile
    if (openFile) {.autoFileOpen(outfile)}
    # Return legend, node table, pathway information
    if (legend & !is.null(colorList)){
        outputList[["Legend"]] <- .exportLegend(outdir, outname, colorList)
    } else{ outputList[["Legend"]] <- NA }
    if (nodeTable & !is.null(colors_df)){
        outputList[["NodeTable"]] <- .returnNodeTable_KGML(colors_df)
    } else{ outputList[["NodeTable"]] <- NA }
    if (pathInfo){
        outputList[["Information"]] <- .returnInformation_KGML(kgml)
    }else{ outputList[["Information"]] <- NA }
    return(outputList)
}


.extractEntries <- function(dataEntries){

    entryFUN <- function(dataEntries){
        data.frame(
            Names = as.character(dataEntries$graphics["name"]),
            ID = strsplit(
                as.character(dataEntries$graphics["name"]),
                ",\\s*")[[1]][1],
            Database = "HGNC",
            KEGG = as.character(dataEntries$.attrs["name"]),
            GraphId1 = as.character(dataEntries$.attrs["id"]),
            GraphType = as.character(dataEntries$.attrs["type"]),
            FgColor = as.character(dataEntries$graphics["fgcolor"]),
            BgColor = as.character(dataEntries$graphics["bgcolor"]),
            NodeType = as.character(dataEntries$graphics["type"]),
            CenterX = as.numeric(dataEntries$graphics["x"]),
            CenterY = as.numeric(dataEntries$graphics["y"]),
            Width = as.numeric(dataEntries$graphics["width"]),
            Height = as.numeric(dataEntries$graphics["height"])
        )
    }
    entries_df <- do.call(rbind, lapply(dataEntries, entryFUN))
    # entries_df$ID<- iconv(entries_df$ID, from = 'UTF-8',
    # to = 'ASCII//TRANSLIT')

    return(entries_df)
}

.makeKGMLpathway <- function(colors_df, img){
    # Draw colors
    if (!is.null(colors_df)){
        graphics::rect(
            xleft = colors_df$CenterX - 0.5 * colors_df$Width,
            ybottom = -1 * (colors_df$CenterY - 0.5 * colors_df$Height),
            xright =  colors_df$CenterX + 0.5 * colors_df$Width,
            ytop =  -1 * (colors_df$CenterY + 0.5 * colors_df$Height),
            col = colors_df$ColorValue,
            border = NA,
            lty = colors_df$LineStyle,
            lwd = colors_df$LineThickness
        )
    }

    # Draw pathway image
    graphics::rasterImage(
        img, 0, -dim(magick::image_data(img))[3],
        dim(magick::image_data(img))[2], 0)
}
