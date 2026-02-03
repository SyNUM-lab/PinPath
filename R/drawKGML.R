# ------------------------------------------------------------------------------
#' @title Draw pathway from KGML file
#'
#' @description This function draws a pathway from a KGML file with the option 
#' to map, e.g., expression data onto the pathway diagram.
#' @param infile Input KGML file. This can be a character string of the 
#' KGML file location (e.g., "Downloads/WP42500.KGML").
#' @param outdir (optional) Output directory. The pathway and legend images 
#' will be saved in this directory.
#' @param outname (optional) The file name of the output pathway image. 
#' "svg","png",and "pdf" file extensions are accepted. If no file extension is 
#' specified, the pathway and legend image will be generated in .svg format.
#' The legend file gets the "legend_" prefix.
#' @param geneIDs (optional) \code{character} vector of gene IDs.
#' @param colorVar (optional) \code{vector} or \code{data.frame} for 
#' coloring the nodes in the pathway. This can be for instance a 
#' \code{data.frame} with the log2FCs and significance in the columns.
#' The (row) order should match \code{geneIDs}. The color rules and palettes 
#' for the supplied values can be set in the colorList parameter.
#' @param annGenes (optional) \code{character} string of the Bioconductor 
#' annotation package (e.g., org.Hs.eg.db).
#' @param annMetabolites (optional) \code{tibble} or \code{data.frame} with 
#' metabolite mapping information (see metaboliteIDmapping package).
#' @param inputDB (optional) Input gene ID type 
#' (SYMBOL, ENTREZID, ENSEMBL, UNIPROT). This can be a \code{character} vector 
#' of \code{length = 1} (if all gene IDs are of the same type) or of 
#' \code{length = nrow(geneIDs)} (if you want to specify the type per gene ID).
#' @param colorNames (optional) \code{character} vector with names of the 
#' color variables. If \code{colorNames} is NULL, the column names of the 
#' \code{colorVar} \code{data.frame} will be used.
#' @param colorList (optional) A list with information about the coloring of 
#' the nodes. An example can be generated using the \link{defaultColorList} 
#' function.
#' @param NAvalue (optional) Node color for \code{NA} values.
#' @param legend (optional) Logical (TRUE or FALSE). Should the legend be 
#' plotted?
#' @param nodeTable (optional) Logical (TRUE or FALSE). Should a node table 
#' be returned?
#' @param pathInfo (optional) Logical (TRUE or FALSE). Should pathway 
#' information be returned?
#' @param openFile (optional) Logical (TRUE or FALSE). Should the pathway file 
#' be opened after it has been saved?
#' @return A \code{list} with the node table and the file location of the 
#' pathway and legend image.
#' @examples
#' 
#'  # Load example data
#'  lung_expr <- read.csv(system.file("extdata",
#'                                   "data-lung-cancer.csv", 
#'                                   package="PinPath"), 
#'                       stringsAsFactors = FALSE)
#' 
#'  # Select pathway
#'  pathway_id <- "hsa05223"
#'  bfc <- BiocFileCache::BiocFileCache()
#'  infile <- BiocFileCache::bfcrpath(bfc, 
#'  paste0("https://rest.kegg.jp/get/",pathway_id,"/kgml"))
#' 
#'  # Draw pathway
#'  pathVis <- PinPath::drawKGML(
#'             infile = infile,
#'             outdir = tempdir(),
#'             annGenes = "org.Hs.eg.db",
#'             inputDB = "ENSEMBL",
#'             geneIDs = lung_expr$GeneID,
#'             colorVar = lung_expr[,"log2FC"],
#'             nodeTable = TRUE,
#'             legend = TRUE)
#' 
#' @export

drawKGML <- function(infile,
                     outdir = getwd(),
                     outname = NULL,
                     annGenes = NULL,
                     annMetabolites = NULL,
                     inputDB = NULL,
                     geneIDs = NULL,
                     colorVar = NULL,
                     colorNames = NULL,
                     colorList = NULL,
                     NAvalue = "#F0F0F0",
                     legend = FALSE,
                     nodeTable = FALSE,
                     pathInfo = FALSE,
                     openFile = TRUE
){
  
  #***********************************************************************#
  # Read and extract info from KGML file
  #***********************************************************************#
  
  # Start with empty output list
  outputList <- list()
  
  # Read KGML file
  doc <- XML::xmlParse(infile)
  kgml <- XML::xmlToList(doc)
  nms <- names(kgml)
  
  # Get pathway name
  PathwayName <- kgml[[length(kgml)]]["title"]
  
  # Get organism
  Organism <- kgml[[length(kgml)]]["org"]
  
  # Get pathway id
  PathwayID <- kgml[[length(kgml)]]["number"]
  
  # Get patway image
  PathwayImage <- kgml[[length(kgml)]]["image"]
  image_name <- BiocFileCache::bfcrpath(BiocFileCache::BiocFileCache(), 
                                        PathwayImage)
  img <- magick::image_read(image_name)
  img <-  magick::image_transparent(img, color = "#BFFFBF")
  
  # Get canvas size
  ImageWidth <- dim(magick::image_data(img))[2]
  ImageHeight <- dim(magick::image_data(img))[3]
  
  #***********************************************************************#
  # Set default values
  #***********************************************************************#
  
  # If output name is not set, give it the name of the pathway
  if (is.null(outname)){
    outname <- paste0(PathwayName,"_",PathwayID, "_",Organism)
    outname <- stringr::str_replace_all(outname, " ", "_")
    outname <- make.names(outname)
  }
  outfile <- paste0(outdir,"/",outname)
  
  # Get file extension
  file_extension <- tolower(tools::file_ext(outname))
  
  # If no color is set, use default color palette
  if (is.null(colorList) & !is.null(colorVar)){
    colorList <- defaultColorList(colorVar, ColorNames = colorNames)
  }
  
  #***********************************************************************#
  # Open file for plotting
  #***********************************************************************#
  
  if (file_extension == "svg"){
    svglite::svglite(outfile, 
                     width = ImageWidth*0.015, 
                     height = ImageHeight*0.015)
  }else if (file_extension == "png"){
    grDevices::png(file = outfile,
                   width = ImageWidth*6,
                   height = ImageHeight*6,
                   units = "px",
                   res = 1200,
                   pointsize = 4)
  }else if (file_extension == "pdf"){
    grDevices::pdf(outfile, 
                   width = ImageWidth*0.015, 
                   height = ImageHeight*0.015)
  }else{
    outfile <- paste0(outfile, ".svg")
    if (file_extension != ""){
      warning("The output file does not have a valid file extension. 
              Generating .svg file instead.")
    }
    svglite::svglite(outfile, 
                     width = ImageWidth*0.015, 
                     height = ImageHeight*0.015)
  }
  
  #***********************************************************************#
  # Make pathway diagram
  #***********************************************************************#
  
  # Set margins
  graphics::par(mar = c(0,0,0,0))
  
  # Make empty canvas
  plot(c(0, ImageWidth),
       c(-1*ImageHeight,0),
       col = "black", axes = FALSE, ann = FALSE)
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$inner, vps$figure, vps$plot)
  
  #=======================================================================#
  # Draw entries
  #=======================================================================#
  
  # Collect entries from KGML file
  dataEntries <- kgml[nms == "entry"]
  
  # Prepare entries
  entries_df <- .prepareEntries(dataEntries)
  
  # Map colors to entries
  if (!(is.null(geneIDs) | 
        is.null(colorVar) | 
        (is.null(annGenes) & is.null(annMetabolites)) | 
        is.null(inputDB))){
    colors_df <- .mapColors(nodes_df = entries_df,
                            geneIDs = geneIDs,
                            colorVar = colorVar,
                            annGenes = annGenes,
                            annMetabolites = data.frame(annMetabolites),
                            inputDB = inputDB,
                            colorList = colorList,
                            NAvalue = NAvalue)
  }
  
  # Draw colors
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
  
  # Draw image
  graphics::rasterImage(img, 0, -ImageHeight, ImageWidth, 0)
  
  #=======================================================================#
  # Export and open plot
  #=======================================================================#
  
  grDevices::dev.off()
  
  # Save file location in output list
  outputList[["Pathway"]] <- outfile
  
  # Open file
  if (openFile) {
    shell(outfile)
  }
  
  #=======================================================================#
  # Make and export legend
  #=======================================================================#
  
  
  if (legend & !is.null(colors_df)){
    
    # Export plot
    if (file_extension == "svg"){
      outfile_legend <- paste0(outdir,"/legend_",outname)
      svglite::svglite(outfile_legend ,
                       width = 5,
                       height = length(colorList) + 1.25)
      .makeLegend(colorList)
      grDevices::dev.off()
    }
    else if (file_extension %in% c("png", "tiff", "pdf")){
      outfile_legend  <-  paste0(outdir,"/legend_",outname)
      grDevices::png(file = outfile_legend,
                     width = 5,
                     height = length(colorList) + 1.25,
                     units = "in",
                     res = 1200,
                     pointsize = 8)
      .makeLegend(colorList)
      grDevices::dev.off()
    }
    else{
      outfile_legend <- paste0(outdir,"/legend_",outname, ".svg")
      svglite::svglite(outfile_legend ,
                       width = 5,
                       height = length(colorList) + 1.25)
      .makeLegend(colorList)
      grDevices::dev.off()
    }
    
    # Save file location in output list
    outputList[["Legend"]] <- outfile_legend
  } else{
    outputList[["Legend"]] <- NA
  }
  
  #=======================================================================#
  # Return node table
  #=======================================================================#
  
  if (nodeTable & !is.null(colors_df)){
    outputTable <- unique(colors_df[!is.na(colors_df$ScaleName),
                                    c("ID", 
                                      "InputId", 
                                      "ScaleName", 
                                      "MapColor")])
    colnames(outputTable) <- c("Node Label", 
                               "ID", 
                               "Scale Name", 
                               "Scale Value")
    outputTable <- outputTable |>
      tidyr::pivot_wider(
        names_from = "Scale Name",
        values_from = "Scale Value",
        values_fn = list
      )
    
    # Save node table in output list
    outputList[["NodeTable"]] <- outputTable
  } else{
    outputList[["NodeTable"]] <- NA
  }
  
  #=======================================================================#
  # Return pathway information
  #=======================================================================#
  
  if (pathInfo){
    outputList[["Information"]] <- c(
      "Name" = as.character(kgml$.attrs["title"]),
      "ID" = paste0(kgml$.attrs["org"], kgml$.attrs["number"]),
      "Link" = as.character(kgml$.attrs["link"]),
      "Description" = ""
    )
  }else{
    outputList[["Information"]] <- NA
  }
  
  return(outputList)
}