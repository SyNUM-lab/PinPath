# ------------------------------------------------------------------------------
#' @title Draw pathway from GPML file
#'
#' @description This function draws a pathway from a GPML file with the option to map, e.g.,
#'              expression data onto the pathway diagram.
#'
#' @param id KEGG pathway id.
#' @param outdir (optional) Output directory. The pathway and legend images will be 
#' saved in this directory.
#' @param outname (optional) The file name of the output pathway image. 
#' "svg","png",and "pdf" file extensions are accepted. If no file extension is 
#' specified, the pathway and legend image will be generated in .svg format.
#' The legend file gets the "legend_" prefix.
#' @param geneIDs (optional) \code{character} vector of gene IDs.
#' @param colorVar (optional) \code{vector} or \code{data.frame} for coloring the nodes in the pathway. 
#' This can be for instance a \code{data.frame} with the log2FCs and significance in the columns.
#' The (row) order should match \code{geneIDs}. 
#' The color rules and palettes for the supplied values can be set in the colorList parameter.
#' @param annPkg (optional) \code{character} string of the Bioconductor annotation package (e.g., org.Hs.eg.db).
#' @param inputDB (optional) Input gene ID type (SYMBOL, ENTREZID, ENSEMBL, UNIPROT).
#' This can be a \code{character} vector of \code{length = 1} (if all gene IDs are of the same type) 
#' or of \code{length = nrow(geneIDs)} (if you want to specify the type per gene ID).
#' @param colorNames (optional) \code{character} vector with names of the color variables. 
#' If \code{colorNames} is NULL, the column names of the \code{colorVar} \code{data.frame} will be used.
#' @param colorList (optional) A list with information about the coloring of the nodes.
#' An example can be generated using the \link{defaultColorList} function.
#' @param NAvalue (optional) Node color for \code{NA} values.
#' @param layout (optional) Network layout from igraph.
#' @param unconnectedNodes (optional) Logical (TRUE or FALSE). Should unconnected (isolated) nodes be shown in the network?
#' @param alpha (optional) Transparency of the nodes.
#' @param alpha (optional) Size of the nodes.
#' @param legend (optional) Logical (TRUE or FALSE). Should the legend be plotted?
#' @param pathInfo (optional) Logical (TRUE or FALSE). Should pathway information be returned?
#' @param nodeTable (optional) Logical (TRUE or FALSE). Should a node table be returned?
#' @param openFile (optional) Logical (TRUE or FALSE). Should the pathway file be opened after it has been saved?
#' @return A \code{list} with the node table and the file location of the pathway and legend image.
#' @export

KGML2Network <- function(id,
                         outdir = getwd(),
                         outname = NULL,
                         geneIDs = NULL,
                         colorVar = NULL,
                         annPkg = NULL,
                         inputDB = NULL,
                         colorNames = NULL,
                         colorList = NULL,
                         NAvalue = "#F0F0F0",
                         layout = "nicely",
                         unconnectedNodes = FALSE,
                         alpha = 0.9,
                         nodeSize = 1,
                         legend = FALSE,
                         nodeTable = FALSE,
                         pathInfo = FALSE,
                         openFile = TRUE
){
  
  #****************************************************************************#
  # Read and extract info from KGML file
  #****************************************************************************#
  
  # Start with empty output list
  outputList <- list()
  
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
  # Set the color values of the nodes
  #****************************************************************************#
  
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
    
    # Add colors to nodes
    entries_df <- dplyr::left_join(entries_df, colors_df[, c("GraphId1", "ColorValue", "Scale")],
                                   by = c("GraphId1" = "GraphId1"))
  } else{
    entries_df$ColorValue <- "white"
    entries_df$Scale <- 1
  }
  
  # Change name
  entries_df$name <- entries_df$ID
  entries_df <- entries_df[,c("name", colnames(entries_df)[colnames(entries_df) != "name"])]
  entries_df <- entries_df[!is.na(entries_df$name),]
  
  #****************************************************************************#
  # Prepare data for plotting
  #****************************************************************************#
  
  # Prepare edges for network
  .prepareRelations_network <- function(dataRelations){
    relationsFUN <- function(dataRelations){
      relations_df <- data.frame(
        from = dataRelations$.attrs[1],
        to = dataRelations$.attrs[2],
        type = dataRelations$.attrs[3]
      )
    }
    relations_df <- do.call(rbind, lapply(dataRelations, relationsFUN))
    return(relations_df)
  }
  
  
  
  
  # Prepare edges
  dataRelations <- kgml[nms == "relation"]
  relations_df <- .prepareRelations_network(dataRelations)
  
  # Filter edges for nodes
  relations_df$from <- replace(setNames(relations_df$from,relations_df$from), 
                               entries_df$GraphId1, entries_df$name)[relations_df$from]
  relations_df$to <- replace(setNames(relations_df$to,relations_df$to), 
                             entries_df$GraphId1, entries_df$name)[relations_df$to]
  
  relations_df <- relations_df[(relations_df$from %in% entries_df$name) &
                                 (relations_df$to %in% entries_df$name),]
  
  if (!is.null(colors_df)){
    
  }
  # Collect NA and non-NA scales
  NAdf <- entries_df[is.na(entries_df$Scale),]
  nonNAdf <- entries_df[!is.na(entries_df$Scale),]
  
  # Add each scale as a seperate column
  scales <- unique(entries_df$Scale)
  scales <- scales[!is.na(scales)]
  for (s in scales){
    if (s == 1){
      entries_df_split <- rbind.data.frame(nonNAdf[nonNAdf$Scale == s,-16], NAdf[,-16])
      colnames(entries_df_split)[ncol(entries_df_split)] <- "ColorValue1"
    }else{
      fil <- rbind.data.frame(nonNAdf[nonNAdf$Scale == s,], NAdf)
      entries_df_split <- cbind.data.frame(entries_df_split, fil$ColorValue)
      colnames(entries_df_split)[ncol(entries_df_split)] <- paste0("ColorValue",s)
    }
  }
  
  # Remove duplicated nodes
  entries_df_split <- entries_df_split[!duplicated(entries_df_split$name),]
  
  #****************************************************************************#
  # Make network
  #****************************************************************************#
  
  # Make graph
  graph <- igraph::graph_from_data_frame(relations_df, vertices = entries_df_split)
  
  # Remove self loops
  graph <- igraph::simplify(graph)
  
  # Remove unconnected nodes
  if (!unconnectedNodes){
    isolated <- which(igraph::degree(graph, mode = "total")==0)
    graph <- igraph::delete_vertices(graph, isolated)
  }
  
  # Make basis of network
  g_plot <- ggraph::ggraph(graph, layout = layout) +
    ggraph::geom_edge_link() 
  
  # Add each scale to the network
  for (g in 1:(ncol(entries_df_split)-14)){
    
    loop_input <- paste0("geom_node_split(fill = g_plot@data$ColorValue",g,", alpha = ",alpha,", nCol = ",(ncol(entries_df_split)-14),", iCol = ", g, ", nodeSize = ", nodeSize, ")")
    
    g_plot <- g_plot + eval(parse(text=loop_input))  
  }
  
  # Finalize network
  g_plot <- g_plot +
    ggraph::geom_node_text(ggplot2::aes(label = name), size = 2) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  
  #****************************************************************************#
  # Export plot
  #****************************************************************************#
  
  # Get file extension
  file_extension <- tolower(tools::file_ext(outname))
  
  # Export plot
  if (file_extension == "svg"){
    outfile <-  paste0(outdir,"/",outname)
    svglite::svglite(outfile, 
                     width = 8/nodeSize, 
                     height = 5/nodeSize)
    print(g_plot)
    dev.off()
  }else if (file_extension %in% c("png", "tiff", "pdf")){
    outfile <-  paste0(outdir,"/",outname)
    ggplot2::ggsave(g_plot, file = outfile,
                    width = 8/nodeSize,
                    height = 5/nodeSize,
                    limitsize = FALSE)
  }else{
    if (file_extension != ""){
      warning("The output file does not have a valid file extension. Generating .svg file instead.")
    }
    # Set output file
    outfile <- paste0(outdir,"/",outname,".svg")
    
    svglite::svglite(outfile, 
                     width = 8/nodeSize, 
                     height = 5/nodeSize)
    print(g_plot)
    dev.off()
  }
  
  # Save file location in output list
  outputList[["Pathway"]] <- outfile
  
  # Open file
  if (openFile) {
    shell(outfile)
  }
  
  #****************************************************************************#
  # Make and export legend
  #****************************************************************************#
  
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
                                     "Description" = "")
  }else{
    outputList[["Information"]] <- NA
  }
  
  return(outputList)
}

