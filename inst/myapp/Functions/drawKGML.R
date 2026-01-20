# ------------------------------------------------------------------------------
#' @title Draw pathway from GPML file
#'
#' @description This function draws a pathway from a GPML file with the option to map, e.g.,
#'              expression data onto the pathway diagram.
#'
#' @param id KEGG pathway id.
#' @param outdir output directory.
#' @param outname output name.
#' @param annPkg Bioconductor annotation package.
#' @param inputDB Input gene ID type (SYMBOL, ENTREZID, ENSEMBL, UNIPROT).
#' @param geneIDs Vector of gene IDs.
#' @param colorVar Vector or data frame with the variables used for coloring. 
#'        The order should match Gene IDs.
#' @param colorNames Vector with names of the colors.
#' @param colorList A list with information about the coloring of the nodes.
#' An example can be generated using the defaultColorList() function.
#' @param NAvalue Node color for NA color values.
#' @param legend Should legend be plotted?
#' @param nodeTable Should nodeTable be returned?
#' @param pathInfo Should pathway information be returned?
#' @param openFile Should the pathway file be opened after it has been saved?
#' @return A list with the pathway and legend file location and the node table.
#' @export

drawKGML_app <- function(id,
                     outdir = getwd(),
                     outname = NULL,
                     annPkg = NULL,
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
  # Start with empty output list
  outputList <- list()
  
  #****************************************************************************#
  # Read and extract info from KGML file
  #****************************************************************************#
  # Get KGML file
  bfc <- BiocFileCache::BiocFileCache()
  if (tools::file_ext(id) == "xml"){
    file_name <- id
  }else{
    file_name <- BiocFileCache::bfcrpath(bfc, paste0("https://rest.kegg.jp/get/",id,"/kgml"))
  }
  doc <- XML::xmlParse(file_name)
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
  image_name <- BiocFileCache::bfcrpath(bfc, PathwayImage)
  img <- magick::image_read(image_name)
  img <-  magick::image_transparent(img, color = "#BFFFBF")
  
  # Get canvas size
  ImageWidth = dim(magick::image_data(img))[2]
  ImageHeight = dim(magick::image_data(img))[3]
  
  #****************************************************************************#
  # Set default values
  #****************************************************************************#
  
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
  
  #****************************************************************************#
  # Open file for plotting
  #****************************************************************************#
  
  if (file_extension == "svg"){
    svglite::svglite(outfile, 
                     width = ImageWidth*0.015, 
                     height = ImageHeight*0.015)
  }else if (file_extension == "png"){
    png(file = outfile,
        width = ImageWidth*6,
        height = ImageHeight*6,
        units = "px",
        res = 1200,
        pointsize = 4)
  }else if (file_extension == "pdf"){
    pdf(outfile, 
        width = ImageWidth*0.015, 
        height = ImageHeight*0.015)
  }else{
    outfile <- paste0(outfile, ".svg")
    if (file_extension != ""){
      warning("The output file does not have a valid file extension. Generating .svg file instead.")
    }
    svglite::svglite(outfile, 
                     width = ImageWidth*0.015, 
                     height = ImageHeight*0.015)
  }
  
  #****************************************************************************#
  # Make pathway diagram
  #****************************************************************************#
  
  # Set margins
  par(mar = c(0,0,0,0))
  
  # Make empty canvas
  plot(c(0, ImageWidth),
       c(-1*ImageHeight,0),
       col = "black", axes = FALSE, ann = FALSE)
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$inner, vps$figure, vps$plot)
  
  #==============================================================================#
  # Draw entries
  #==============================================================================#
  
  # Collect entries from KGML file
  dataEntries <- kgml[nms == "entry"]
  
  # Prepare entries
  entries_df <- .prepareEntries(dataEntries)
  
  # Map colors to entries
  if (!(is.null(geneIDs) | is.null(colorVar) | is.null(annPkg) | is.null(inputDB))){
    colors_df <- .mapColors(nodes_df = entries_df,
                            geneIDs = geneIDs,
                            colorVar = colorVar,
                            annPkg = annPkg,
                            inputDB = inputDB,
                            colorList = colorList,
                            NAvalue = NAvalue)
  }
  
  # Draw colors
  rect(
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
  rasterImage(img, 0, -ImageHeight, ImageWidth, 0)
  
  #==============================================================================#
  # Export and open plot
  #==============================================================================#
  
  dev.off()
  
  # Save file location in output list
  outputList[["Pathway"]] <- outfile
  
  # Open file
  if (openFile) {
    shell(outfile)
  }
  
  #==============================================================================#
  # Make and export legend
  #==============================================================================#
  
  
  if (legend & !is.null(colors_df)){
    
    # Export plot
    if (file_extension == "svg"){
      outfile_legend <- paste0(outdir,"/legend_",outname)
      svglite::svglite(outfile_legend ,
                       width = 5,
                       height = length(colorList) + 1.25)
      .makeLegend(colorList)
      dev.off()
    }
    else if (file_extension %in% c("png", "tiff", "pdf")){
      outfile_legend  <-  paste0(outdir,"/legend_",outname)
      png(file = outfile_legend,
          width = 5,
          height = length(colorList) + 1.25,
          units = "in",
          res = 1200,
          pointsize = 8)
      .makeLegend(colorList)
      dev.off()
    }
    else{
      outfile_legend <- paste0(outdir,"/legend_",outname, ".svg")
      svglite::svglite(outfile_legend ,
                       width = 5,
                       height = length(colorList) + 1.25)
      .makeLegend(colorList)
      dev.off()
    }
    
    # Save file location in output list
    outputList[["Legend"]] <- outfile_legend
  } else{
    outputList[["Legend"]] <- NA
  }
  
  #==============================================================================#
  # Return node table
  #==============================================================================#
  
  if (nodeTable & !is.null(colors_df)){
    outputTable <- unique(colors_df[!is.na(colors_df$ScaleName),c("ID", "InputId", "ScaleName", "MapColor")])
    colnames(outputTable) <- c("Node Label", "ID", "Scale Name", "Scale Value")
    outputTable <- outputTable |>
      tidyr::pivot_wider(
        names_from = `Scale Name`,
        values_from = `Scale Value`
      )
    
    # Save node table in output list
    outputList[["NodeTable"]] <- outputTable
  } else{
    outputList[["NodeTable"]] <- NA
  }
  
  #==============================================================================#
  # Return pathway information
  #==============================================================================#
  
  if (pathInfo){
    outputList[["Information"]] <- c("Name" = as.character(kgml$.attrs["title"]),
                                     "ID" = paste0(kgml$.attrs["org"], kgml$.attrs["number"]),
                                     "Link" = as.character(kgml$.attrs["link"]),
                                     "Description" = ""
    )
  }else{
    outputList[["Information"]] <- NA
  }
  
  return(outputList)
}