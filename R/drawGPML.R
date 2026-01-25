# ------------------------------------------------------------------------------
#' @title Draw pathway from GPML file
#'
#' @description This function draws a pathway from a GPML file with the option 
#' to map, e.g., expression data onto the pathway diagram.
#'
#' @param infile Input GPML file. This can be a character string of the 
#' GPML file location (e.g., "Downloads/WP42500.gpml") or a GPML string 
#' provided by [rWikiPathways::getPathway].
#' @param outdir (optional) Output directory. The pathway and legend images 
#' will be saved in this directory.
#' @param outname (optional) The file name of the output pathway image. 
#' "svg","png",and "pdf" file extensions are accepted. If no file extension is 
#' specified, the pathway and legend image will be generated in .svg format.
#' The legend file gets the "legend_" prefix.
#' @param geneIDs (optional) \code{character} vector of gene IDs.
#' @param colorVar (optional) \code{vector} or \code{data.frame} for coloring 
#' the nodes in the pathway. This can be for instance a \code{data.frame} with 
#' the log2FCs and significance in the columns. The (row) order should match 
#' \code{geneIDs}. The color rules and palettes for the supplied values can be 
#' set in the colorList parameter.
#' @param annGenes (optional) \code{character} string of the Bioconductor 
#' annotation package (e.g., org.Hs.eg.db).
#' @param annMetabolites (optional) \code{tibble} or \code{data.frame} with 
#' metabolite mapping information (see metaboliteIDmapping package).
#' @param inputDB (optional) Input gene ID type (SYMBOL, ENTREZID, ENSEMBL, 
#' UNIPROT).
#' This can be a \code{character} vector of \code{length = 1} (if all gene IDs 
#' are of the same type) or of \code{length = nrow(geneIDs)} (if you want to 
#' specify the type per gene ID).
#' @param colorNames (optional) \code{character} vector with names of the 
#' color variables. If \code{colorNames} is NULL, the column names of the 
#' \code{colorVar} \code{data.frame} will be used.
#' @param colorList (optional) A list with information about the coloring of 
#' the nodes. An example can be generated using the \link{defaultColorList} 
#' function.
#' @param NAvalue (optional) Node color for \code{NA} values.
#' @param legend (optional) Logical (TRUE or FALSE). Should the legend be 
#' plotted?
#' @param nodeTable (optional) Logical (TRUE or FALSE). Should a node table be 
#' returned?
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
#'  infile <- rWikiPathways::getPathway("WP4255")
#' 
#'  # Draw pathway
#'  pathVis <- PinPath::drawGPML(
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

drawGPML <- function(infile,
                     outdir = getwd(),
                     outname = NULL,
                     geneIDs = NULL,
                     colorVar = NULL,
                     annGenes = NULL,
                     annMetabolites = NULL,
                     inputDB = NULL,
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
  
  #************************************************************************#
  # Read and extract info from GPML file
  #************************************************************************#
  
  # Read GPML file
  doc <- XML::xmlParse(xml2::read_xml(infile))
  gpml <- XML::xmlToList(doc)
  
  # Extract the names (e.g., DataNode, Interaction, Group, Label, Shape)
  nms <- names(gpml)
  
  # Get pathway name
  PathwayName <- gpml$.attrs["Name"]
  
  # Get organism
  Organism <- gpml$.attrs["Organism"]
  
  # Get pathway id
  PathwayID <- gpml$.attrs["Version"]
  
  # Get Canvas size
  CanvasSize <- gpml$Graphics
  
  # Filter for graphical elements
  gpml_fil <- gpml[nms %in% c("DataNode", "Shape", "Label", "Interaction",
                              "GraphicalLine", "State", "Group")]
  
  # Give each graphical element a unique ID
  for (l in seq_along(gpml_fil)){
    gpml_fil[[l]]["ID"] <-  paste0("id", l)
  }
  
  #************************************************************************#
  # Set default values
  #************************************************************************#
  
  # If output name is not set, give it the name of the pathway
  if (is.null(outname)){
    outname <- paste0(PathwayName,"_",PathwayID, "_",Organism)
    outname <- stringr::str_replace_all(outname, " ", "_")
    outname <- make.names(outname)
  }
  outfile <-paste0(outdir,"/",outname)
  
  # Get file extension
  file_extension <- tolower(tools::file_ext(outname))
  
  # If no color is set, use default color palette
  if (is.null(colorList) & !is.null(colorVar)){
    colorList <- defaultColorList(colorVar, ColorNames = colorNames)
  }
  
  #************************************************************************#
  # Set order of plotting based on the Z-value
  #************************************************************************#
  
  # For each element in the GPML file, we are going the extract three things:
  # - Element type: Node, interaction, etc.
  # - Z-order
  # - Group reference: the ID of the group element linked to the node
  # - State reference: the ID of the nodes that are linked to a state
  
  ZOrder <- rep(NA, length(gpml_fil))
  Type <- rep(NA, length(gpml_fil))
  GroupRef <- rep(NA, length(gpml_fil))
  StateRef <- rep(NA, length(gpml_fil))
  for (i in seq_along(gpml_fil)){
    
    # Extract element type
    Type[i] <- names(gpml_fil[i])
    
    # Extract Z-order
    if ("Graphics" %in% names(gpml_fil[[i]])){
      if ("ZOrder" %in% names(gpml_fil[[i]][["Graphics"]])){
        ZOrder[i] <- gpml_fil[[i]][["Graphics"]]["ZOrder"]
      }
      if (".attrs" %in% names(gpml_fil[[i]][["Graphics"]])){
        ZOrder[i] <- gpml_fil[[i]][["Graphics"]][[".attrs"]]["ZOrder"]
      }
    }
    
    # Extract group reference
    if(names(gpml_fil[i]) %in% c("DataNode", "Label")){
      GroupRef[i] <- gpml_fil[[i]][[".attrs"]]["GroupRef"]
    }
    if(names(gpml_fil[i]) == "Group"){
      if (".attrs" %in% names(gpml_fil[[i]])){
        GroupRef[i] <- gpml_fil[[i]][[".attrs"]]["GroupId"]
      }else{
        GroupRef[i] <- gpml_fil[[i]][["GroupId"]]
      }
    }
    
    
    # Extract state reference
    if(names(gpml_fil[i]) %in% c("DataNode", "Label")){
      StateRef[i] <- gpml_fil[[i]][[".attrs"]]["GraphId"]
    }
    if(names(gpml_fil[i]) == "State"){
      if (".attrs" %in% names(gpml_fil[[i]])){
        StateRef[i] <- gpml_fil[[i]][[".attrs"]]["GraphRef"]
      }else{
        StateRef[i] <- gpml_fil[[i]][["GraphRef"]]
      }
    }
  }
  
  # Combine extracted values into a data frame
  ZOrder_df <- data.frame(Index = seq_along(gpml_fil),
                          ZOrder = as.numeric(ZOrder),
                          GroupRef = GroupRef,
                          StateRef = StateRef,
                          Type = Type)
  
  # The group elements do not have a defined Z-order.
  # So, we set the order to be before the first node of the group
  # for (i in seq_len(nrow(ZOrder_df))){
  #   if ((is.na(ZOrder_df$ZOrder[i])) & (!is.na(ZOrder_df$GroupRef[i]))){
  # 
  #     nodeZs <- ZOrder_df$ZOrder[ZOrder_df$GroupRef == ZOrder_df$GroupRef[i]]
  #     nodeZs <- nodeZs[!is.na(nodeZs)]
  # 
  #     if (length(nodeZs) > 1){
  #       ZOrder_df$ZOrder[i] <- min(nodeZs, na.rm = TRUE) - 0.5
  #     } else{
  #       ZOrder_df$ZOrder[i] <- -Inf
  #     }
  # 
  #   }
  # }
  
  # The state elements do not have a defined Z-order.
  # So, we set the order to be before the first node of the group
  for (i in seq_len(nrow(ZOrder_df))){
    if ((is.na(ZOrder_df$ZOrder[i])) & (!is.na(ZOrder_df$StateRef[i]))){
      
      nodeZs <- ZOrder_df$ZOrder[ZOrder_df$StateRef == ZOrder_df$StateRef[i]]
      nodeZs <- nodeZs[!is.na(nodeZs)]
      
      if (length(nodeZs) > 0){
        ZOrder_df$ZOrder[i] <- max(nodeZs, na.rm = TRUE) + 0.5
      } else{
        ZOrder_df$ZOrder[i] <- Inf
      }
    }
  }
  
  # The Z-order of the groups is minimal, so they will be drawn first
  ZOrder_df$ZOrder[is.na(ZOrder_df$ZOrder)] <- -Inf
  
  # Sort data frame by Z-order
  ZOrder_df <- dplyr::arrange(ZOrder_df, by = ZOrder)
  
  # Get all elements that could be part of a group
  groupElements_df <- .prepareLabels(gpml_fil[names(gpml_fil) %in% 
                                                c("DataNode", "Label")])
  
  #************************************************************************#
  # Set the color values of the nodes
  #************************************************************************#
  
  # Prepare nodes
  nodes_df_all <- .prepareNodes(gpml_fil[names(gpml_fil) == "DataNode"])
  
  # Map colors to nodes
  colors_df_all <- NULL
  if (!(is.null(geneIDs) | is.null(colorVar) | 
        (is.null(annGenes) & is.null(annMetabolites)) | is.null(inputDB))){
    
    colors_df_all <- .mapColors(nodes_df = nodes_df_all,
                                geneIDs = geneIDs,
                                colorVar = colorVar,
                                annGenes = annGenes,
                                annMetabolites = data.frame(annMetabolites),
                                inputDB = inputDB,
                                colorList = colorList,
                                NAvalue = NAvalue)
  }
  
  #************************************************************************#
  # Open file for plotting
  #************************************************************************#
  
  if (file_extension == "svg"){
    svglite::svglite(outfile, 
                     width = as.numeric(CanvasSize["BoardWidth"])*0.015, 
                     height = as.numeric(CanvasSize["BoardHeight"])*0.015)
  }else if (file_extension == "png"){
    
    w <- as.numeric(CanvasSize["BoardWidth"])*18
    h <- as.numeric(CanvasSize["BoardHeight"])*18
    
    if ((h <= 20000) | (w <= 20000)){
      grDevices::png(file = outfile,
                     width = as.numeric(CanvasSize["BoardWidth"])*18,
                     height = as.numeric(CanvasSize["BoardHeight"])*18,
                     units = "px",
                     res = 1200,
                     pointsize = 12)
    }else{
      
      if (h >= w){
        grDevices::png(file = outfile,
                       width = w * (20000/h),
                       height = 20000,
                       units = "px",
                       res = 1200 * (20000/h),
                       pointsize = 12)
      }
      if (h < w){
        grDevices::png(file = outfile,
                       width = 20000,
                       height = h* (20000/w),
                       units = "px",
                       res = 1200 * (20000/w),
                       pointsize = 12)
      }
      
    }
    
    
    
    
  }else if (file_extension == "pdf"){
    grDevices::pdf(outfile, 
                   width = as.numeric(CanvasSize["BoardWidth"])*0.015, 
                   height = as.numeric(CanvasSize["BoardHeight"])*0.015)
  }else{
    outfile <- paste0(outfile, ".svg")
    if (file_extension != ""){
      warning("The output file does not have a valid file extension. 
              Generating .svg file instead.")
    }
    svglite::svglite(outfile, 
                     width = as.numeric(CanvasSize["BoardWidth"])*0.015, 
                     height = as.numeric(CanvasSize["BoardHeight"])*0.015)
  }
  
  #************************************************************************#
  # Make pathway diagram
  #************************************************************************#
  
  # Set margins and line spacing
  graphics::par(mar = c(0,0,0,0), 
                lheight=0.9)
  
  # Make empty canvas
  plot(c(0, as.numeric(CanvasSize["BoardWidth"])),
       c(-1*as.numeric(CanvasSize["BoardHeight"]),0),
       col = "white", axes = FALSE, ann = FALSE)
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$inner, vps$figure, vps$plot)
  
  # Plot each element in GPML file
  for (i in ZOrder_df$Index){
    
    # Select pathway element
    pathwayElement <- gpml_fil[i]
    
    #======================================================================#
    # Draw nodes
    #======================================================================#
    
    if (names(pathwayElement) == "DataNode"){
      
      if (!(is.null(geneIDs) | 
            is.null(colorVar) | 
            (is.null(annGenes) & is.null(annMetabolites)) | 
            is.null(inputDB))){
        
        # Select color values of pathway element
        colors_df <- colors_df_all[
          colors_df_all$GraphId1 == pathwayElement[[1]]$ID,]
        
        # Draw colors
        if (nrow(colors_df) > 0){
          .drawColors(colors_df)
        }
        
        # Prepare nodes
        nodes_df <- .prepareNodes(pathwayElement)
        
        # Plot nodes
        .drawNodes(nodes_df, colors_df_all)
      } else{
        
        # Prepare nodes
        nodes_df <- .prepareNodes(pathwayElement)
        
        # Plot nodes
        .drawShapes(nodes_df)
      }
    }
    
    #======================================================================#
    # Draw shapes
    #======================================================================#
    
    if (names(pathwayElement) == "Shape"){
      
      # Prepare shapes (non-cell components)
      shapes_df <- .prepareShapes(pathwayElement)
      
      non_cellcomp_df <- shapes_df[!(shapes_df$ShapeType %in% 
                                       c("Mitochondria",
                                         "Sarcoplasmic Reticulum",
                                         "Endoplasmic Reticulum",
                                         "Golgi Apparatus",
                                         "Brace")),]
      
      # Draw shapes (non-cell components)
      if (nrow(non_cellcomp_df) > 0){
        .drawShapes(non_cellcomp_df)
      }
      
      
      # Prepare cell components
      cellcomp_df <- shapes_df[shapes_df$ShapeType %in% 
                                 c("Mitochondria",
                                   "Sarcoplasmic Reticulum",
                                   "Endoplasmic Reticulum",
                                   "Golgi Apparatus"),]
      # Draw cell components
      if (nrow(cellcomp_df) > 0){
        .drawCellComponents(cellcomp_df)
      }
      
      # Prepare braces
      braces_df <- shapes_df[shapes_df$ShapeType == "Brace",]
      
      # Draw braces
      if (nrow(braces_df) > 0){
        .drawBraces(braces_df)
      }
    }
    
    #======================================================================#
    # Draw labels
    #======================================================================#
    
    if (names(pathwayElement) == "Label"){
      
      # Prepare labels
      labels_df <- .prepareLabels(pathwayElement)
      
      # Plot labels
      .drawShapes(labels_df)
    }
    
    #======================================================================#
    # Draw groups
    #======================================================================#
    
    if (names(pathwayElement) == "Group"){
      
      # Get group reference
      if (!is.null(pathwayElement[[1]][["GroupId"]])){
        groupref <- pathwayElement[[1]][["GroupId"]]
      } else{
        groupref <- pathwayElement[[1]]$.attrs["GroupId"]
      }
      
      # Select nodes that are part of the group
      selNodes <- which(groupElements_df$GroupRef == groupref)
      
      # Plot group
      if (length(selNodes) > 0){
        nodes_df_groups <- groupElements_df[selNodes,]
        groups_df <- .prepareGroups(pathwayElement, nodes_df_groups)
        
        # draw groups
        .drawGroups(groups_df)
      }
    }
    
    #======================================================================#
    # Edges
    #======================================================================#
    
    if (names(pathwayElement) == "Interaction" ){
      
      # Prepare edges
      edges_df <- .prepareEdges(pathwayElement, 
                                gpml_fil[names(gpml_fil) %in% 
                                           c("Interaction", "GraphicalLine")])
      
      # draw edges
      if (length(edges_df$lines) > 0){
        for (l in seq_len(nrow(edges_df$lines))){
          .drawEdges(edges_df$lines[l,])
        }
      }
      
      # draw curves
      if (length(edges_df$curves) > 0){
        for (g in unique(edges_df$curves$group)){
          .drawCurves(edges_df$curves[edges_df$curves$group == g,])
        }
      }
    }
    
    #======================================================================#
    # Graphical Line
    #======================================================================#
    
    if (names(pathwayElement) == "GraphicalLine"){
      
      # Prepare edges
      edges_df <- .prepareEdges(pathwayElement)
      
      # draw edges
      if (length(edges_df$lines) > 0){
        for (l in seq_len(nrow(edges_df$lines))){
          .drawEdges(edges_df$lines[l,])
        }
      }
      
      # draw curves
      if (length(edges_df$curves) > 0){
        for (g in unique(edges_df$curves$group)){
          .drawCurves(edges_df$curves[edges_df$curves$group == g,])
        }
      }
    }
    
    #======================================================================#
    # States
    #======================================================================#
    if (names(pathwayElement) == "State"){
      
      # Prepare states
      states_df <- .prepareStates(pathwayElement, nodes_df_all)
      
      # Draw states
      .drawStates(states_df)
      
    }
  } 
  
  #======================================================================#
  # Export and open plot
  #======================================================================#
  
  grDevices::dev.off()
  
  # Save file location in output list
  outputList[["Pathway"]] <- outfile
  
  # Open file
  if (openFile) {
    shell(outfile)
  }
  
  #======================================================================#
  # Make and export legend
  #======================================================================#
  
  if (legend & !is.null(colors_df_all)){
    
    # Export plot
    if (file_extension == "svg"){
      outfile_legend <- paste0(outdir,"/legend_",outname)
      svglite::svglite(outfile_legend ,
                       width = 5,
                       height = length(colorList) + 1.25)
      .makeLegend(colorList)
      grDevices::dev.off()
    }
    else if (file_extension == "pdf"){
      outfile_legend <- paste0(outdir,"/legend_",outname)
      grDevices::pdf(outfile_legend ,
                     width = 5,
                     height = length(colorList) + 1.25)
      .makeLegend(colorList)
      grDevices::dev.off()
    }
    else if (file_extension == "png"){
      outfile_legend  <-  paste0(outdir,"/legend_",outname)
      grDevices::png(file = outfile_legend,
                     width = 5,
                     height = length(colorList) + 1.25,
                     units = "in",
                     res = 1200,
                     pointsize = 12)
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
  
  #======================================================================#
  # Return node table
  #======================================================================#
  
  if (nodeTable & !is.null(colors_df_all)){
    outputTable <- unique(colors_df_all[!is.na(colors_df_all$ScaleName),
                                        c("Label", 
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
        values_from = "Scale Value"
      )
    
    # Save node table in output list
    outputList[["NodeTable"]] <- outputTable
  } else{
    outputList[["NodeTable"]] <- NA
  }
  
  #======================================================================#
  # Return pathway information
  #======================================================================#
  
  if (pathInfo){
    outputList[["Information"]] <- c(
      "Name" = as.character(gpml$.attrs["Name"]),
      "ID" = as.character(gpml$.attrs["Version"]),
      "Link" = paste0("https://www.wikipathways.org/pathways/",
                      stringr::str_split(gpml$.attrs["Version"], "_")[[1]][1],
                      ".html"),
      "Description" = gpml$Comment$text)
  }else{
    outputList[["Information"]] <- NA
  }
  
  return(outputList)
}
