# ------------------------------------------------------------------------------
#' @title Draw pathway from GPML file
#'
#' @description This function draws a pathway from a GPML file with the option
#'   to map, e.g., expression data onto the pathway diagram.
#'
#' @param infile Input GPML file. This can be a character string of the
#'   GPML file location (e.g., "Downloads/WP42500.gpml") or a GPML string
#'   provided by [rWikiPathways::getPathway].
#' @param outdir (optional) Output directory. The pathway and legend images
#'   will be saved in this directory.
#' @param outname (optional) The file name of the output pathway image.
#'   "svg","png",and "pdf" file extensions are accepted. If no file extension is
#'   specified, the pathway and legend image will be generated in .svg format.
#'   The legend file gets the "legend_" prefix.
#' @param featureIDs (optional) \code{character} vector of feature IDs.
#' @param colorVar (optional) \code{vector} or \code{data.frame} for coloring
#'   the nodes in the pathway. This can be for instance a \code{data.frame} with
#'   the log2FCs and significance in the columns. The (row) order should match
#'   \code{featureIDs}. The color rules and palettes for the supplied values
#'   can be set in the colorList parameter.
#' @param annGenes (optional) \code{character} string of the Bioconductor
#'   annotation package (e.g., org.Hs.eg.db).
#' @param annMetabolites (optional) \code{tibble} or \code{data.frame} with
#'   metabolite mapping information (see metaboliteIDmapping package).
#' @param inputDB (optional) Input gene ID type (SYMBOL, ENTREZID, ENSEMBL,
#'   UNIPROT). This can be a \code{character} vector of \code{length = 1}
#'   (if all gene IDs
#'   are of the same type) or of \code{length = nrow(featureIDs)} (if you want
#'   to specify the type per gene ID).
#' @param colorNames (optional) \code{character} vector with names of the
#'   color variables. If \code{colorNames} is NULL, the column names of the
#'   \code{colorVar} \code{data.frame} will be used.
#' @param colorList (optional) A list with information about the coloring of
#'   the nodes. An example can be generated using the \link{defaultColorList}
#'   function.
#' @param NAvalue (optional) Node color for \code{NA} values.
#' @param legend (optional) Logical (TRUE or FALSE). Should the legend be
#'   plotted?
#' @param nodeTable (optional) Logical (TRUE or FALSE). Should a node table be
#'   returned?
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
#' infile <- rWikiPathways::getPathway("WP4255")
#'
#' # Draw pathway
#' pathVis <- PinPath::drawGPML(
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
drawGPML <- function(
        infile,outdir = getwd(),outname = NULL,featureIDs = NULL,
        colorVar = NULL,annGenes = NULL,annMetabolites = NULL,inputDB = NULL,
        colorNames = NULL,colorList = NULL,NAvalue = "#F0F0F0",legend = FALSE,
        nodeTable = FALSE,pathInfo = FALSE,openFile = FALSE){
    # Read and prepare GPML file
    gpml <- XML::xmlToList(XML::xmlParse(xml2::read_xml(infile)))
    gpml_fil <- .prepareGPML(gpml)
    # Set default values if necessary
    if (is.null(outname)){outname <- .makeOutName(gpml)}
    if (is.null(colorList) & !is.null(colorVar)){
        colorList <- defaultColorList(colorVar, ColorNames = colorNames)}
    # Set order of plotting based on the Z-value
    ZOrder_df <- .setZorder_GPML(gpml_fil)
    # Get all elements that could be part of a group
    groupElements_df <- .prepareLabels(
        gpml_fil[names(gpml_fil) %in% c("DataNode", "Label")])
    # Prepare nodes
    nodes_df_all <- .prepareNodes(gpml_fil[names(gpml_fil) == "DataNode"])
    # Map colors to nodes
    plotColor <- !(
        is.null(featureIDs) | is.null(colorVar) |
            (is.null(annGenes) & is.null(annMetabolites)) | is.null(inputDB))
    colors_df_all <- NULL
    if (plotColor){colors_df_all <- .mapColors(
        nodes_df = nodes_df_all, annGenes, data.frame(annMetabolites), inputDB,
        featureIDs, colorVar, colorList, NAvalue)}
    # Make pathway diagram
    outfile <- .openFile(
        gpml$Graphics["BoardWidth"], gpml$Graphics["BoardHeight"],
        file.path(outdir, outname))
    for (i in ZOrder_df$Index){
        .drawElement(
            gpml_fil[i], plotColor, gpml_fil, groupElements_df, nodes_df_all,
            colors_df_all)}
    grDevices::dev.off()
    outputList <- list()
    outputList[["Pathway"]] <- outfile
    if (openFile) {.autoFileOpen(outputList[["Pathway"]])}
    # Return legend, node table, pathway information
    if (legend & !is.null(colors_df_all)){
        outputList[["Legend"]] <- .exportLegend(outdir, outname, colorList)
    } else{ outputList[["Legend"]] <- NA }
    if (nodeTable & !is.null(colors_df_all)){
        outputList[["NodeTable"]] <- .returnNodeTable(colors_df_all)
    } else{ outputList[["NodeTable"]] <- NA }
    if (pathInfo){outputList[["Information"]] <- .returnInformation(gpml)
    }else{ outputList[["Information"]] <- NA }
    return(outputList)
}


.prepareGPML <- function(gpml){
    # Filter for graphical elements
    gpml_fil <- gpml[names(gpml) %in% c(
        "DataNode", "Shape", "Label", "Interaction", "GraphicalLine", "State",
        "Group")]

    # Give each graphical element a unique ID
    for (l in seq_along(gpml_fil)){
        gpml_fil[[l]]["ID"] <-  paste0("id", l)
    }
    return(gpml_fil)
}


.setZorder_GPML <- function(gpml_fil){
    ZOrder <- rep(NA, length(gpml_fil))
    Type <- rep(NA, length(gpml_fil))
    GroupRef <- rep(NA, length(gpml_fil))
    StateRef <- rep(NA, length(gpml_fil))
    for (i in seq_along(gpml_fil)){

        # Extract element type
        # Element type: Node, interaction, etc.
        Type[i] <- names(gpml_fil[i])

        # Extract Z-order
        ZOrder[i] <- .extractZorder(gpml_fil[i])

        # Extract group reference
        # Group reference: the ID of the group element linked to the node
        GroupRef[i] <- .extractGroupRef(gpml_fil[i])

        # Extract state reference
        # State reference: the ID of the nodes that are linked to a state
        StateRef[i] <- .extractStateRef(gpml_fil[i])
    }

    # Combine extracted values into a data frame
    ZOrder_df <- data.frame(
        Index = seq_along(gpml_fil),
        ZOrder = as.numeric(ZOrder),
        GroupRef = GroupRef,
        StateRef = StateRef,
        Type = Type)

    # The state elements do not have a defined Z-order.
    # So, we set the order to be before the node
    ZOrder_df <- .setStateZ(ZOrder_df)

    # The Z-order of the groups is minimal, so they will be drawn first
    ZOrder_df <- .setGroupZ(ZOrder_df)

    # Sort data frame by Z-order
    ZOrder_df <- dplyr::arrange(ZOrder_df, by = ZOrder)
    return(ZOrder_df)
}


.extractZorder <- function(gpml_element){
    ZOrder_i <- NA
    if ("Graphics" %in% names(gpml_element[[1]])){
        if ("ZOrder" %in% names(gpml_element[[1]][["Graphics"]])){
            ZOrder_i <- gpml_element[[1]][["Graphics"]]["ZOrder"]
        }
        if (".attrs" %in% names(gpml_element[[1]][["Graphics"]])){
            ZOrder_i <- gpml_element[[1]][["Graphics"]][[".attrs"]]["ZOrder"]
        }
    }

    return(ZOrder_i)
}


.extractGroupRef <- function(gpml_element){
    GroupRef_i <- NA
    if(names(gpml_element) %in% c("DataNode", "Label")){
        GroupRef_i <- gpml_element[[1]][[".attrs"]]["GroupRef"]
    }
    if(names(gpml_element) == "Group"){
        if (".attrs" %in% names(gpml_element[[1]])){
            GroupRef_i <- gpml_element[[1]][[".attrs"]]["GroupId"]
        }else{
            GroupRef_i <- gpml_element[[1]][["GroupId"]]
        }
    }
    return(GroupRef_i)
}


.extractStateRef <- function(gpml_element){
    StateRef_i <- NA
    if(names(gpml_element) %in% c("DataNode", "Label")){
        StateRef_i <- gpml_element[[1]][[".attrs"]]["GraphId"]
    }
    if(names(gpml_element) == "State"){
        if (".attrs" %in% names(gpml_element[[1]])){
            StateRef_i <- gpml_element[[1]][[".attrs"]]["GraphRef"]
        }else{
            StateRef_i <- gpml_element[[1]][["GraphRef"]]
        }
    }
    return(StateRef_i)
}


.setStateZ <- function(ZOrder_df){
    for (i in seq_len(nrow(ZOrder_df))){
        if ((is.na(ZOrder_df$ZOrder[i])) & (!is.na(ZOrder_df$StateRef[i]))){

            nodeZs <- ZOrder_df$ZOrder[
                ZOrder_df$StateRef == ZOrder_df$StateRef[i]]
            nodeZs <- nodeZs[!is.na(nodeZs)]

            if (length(nodeZs) > 0){
                ZOrder_df$ZOrder[i] <- max(nodeZs, na.rm = TRUE) + 0.5
            } else{
                ZOrder_df$ZOrder[i] <- Inf
            }
        }
    }
    return(ZOrder_df)
}


.setGroupZ <- function(ZOrder_df){

    # The group elements do not have a defined Z-order.
    # So, we set the order to be before the first node of the group
    # for (i in seq_len(nrow(ZOrder_df))){
    #   if ((is.na(ZOrder_df$ZOrder[i])) & (!is.na(ZOrder_df$GroupRef[i]))){
    #
    #     nodeZs <- ZOrder_df$ZOrder[
    #        ZOrder_df$GroupRef == ZOrder_df$GroupRef[i]]
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
    ZOrder_df$ZOrder[is.na(ZOrder_df$ZOrder)] <- -Inf
    return(ZOrder_df)
}


.openFile <- function(width, height, outfile){
    width <- as.numeric(width)
    height <- as.numeric(height)
    file_extension <- tolower(tools::file_ext(outfile))
    if (file_extension == "svg"){
        svglite::svglite(
            outfile,
            width = width*0.015, height = height*0.015)
    }else if (file_extension == "png"){
        if ((height <= 1100) | (width <= 1100)){
            grDevices::png(
                file = outfile,
                width = width*0.015, height = height*0.015,
                units = "in", res = 600, pointsize = 12)
        }else{
            warning("PNG cannot be generated, so SVG is generated instead.")
            outfile <- paste0(outfile, ".svg")
            svglite::svglite(
                outfile, width = width*0.015, height = height*0.015)
        }
    }else if (file_extension == "pdf"){
        grDevices::pdf(outfile, width = width*0.015, height = height*0.015)
    }else{
        outfile <- paste0(outfile, ".svg")
        if (file_extension != ""){
            warning(
                "The output file does not have a valid file extension.
                Generating SVG file instead.")
        }
        svglite::svglite(outfile,width = width*0.015,height = height*0.015)
    }
    graphics::par(mar = c(0,0,0,0), lheight=0.9)
    plot(c(0, width), c(-1*height,0), col = "white", axes = FALSE, ann = FALSE)
    vps <- gridBase::baseViewports()
    grid::pushViewport(vps$inner, vps$figure, vps$plot)
    return(outfile)
}


.drawElement <- function(
        pathwayElement, plotColor, gpml_fil,
        groupElements_df, nodes_df_all, colors_df_all){

    if (names(pathwayElement) == "DataNode"){
        .plotNodes(pathwayElement, plotColor, colors_df_all)
    }

    if (names(pathwayElement) == "Shape"){
        .plotShapes(pathwayElement)
    }

    if (names(pathwayElement) == "Label"){
        .plotLabels(pathwayElement)
    }

    if (names(pathwayElement) == "Group"){
        .plotGroups(pathwayElement, groupElements_df)
    }

    if (names(pathwayElement) %in% c("Interaction", "GraphicalLine")){
        .plotInteractions(pathwayElement, gpml_fil)
    }

    if (names(pathwayElement) == "State"){
        .plotStates(pathwayElement, nodes_df_all)
    }
}


.plotNodes <- function(pathwayElement, plotColor, colors_df_all){

    # Prepare nodes
    nodes_df <- .prepareNodes(pathwayElement)

    # Plot colored node
    if (plotColor){
        colors_df <- colors_df_all[
            colors_df_all$GraphId1 == pathwayElement[[1]]$ID,]
        if (nrow(colors_df) > 0){.drawColors(colors_df)}
        .drawNodes(nodes_df, colors_df_all)

        # Plot uncolored node
    } else{
        .drawShapes(nodes_df)
    }
}


.plotShapes <- function(pathwayElement){
    shapes_df <- .prepareShapes(pathwayElement)

    # Split shapes in cell and non-cell components
    cellcomp <- c(
        "Mitochondria", "Sarcoplasmic Reticulum", "Endoplasmic Reticulum",
        "Golgi Apparatus", "Brace")
    non_cellcomp_df <- shapes_df[!(shapes_df$ShapeType %in% cellcomp),]
    cellcomp_df <- shapes_df[shapes_df$ShapeType %in% cellcomp,]

    # Draw non-cell components
    if (nrow(non_cellcomp_df) > 0){
        .drawShapes(non_cellcomp_df)
    }

    # Draw cell components
    if (nrow(cellcomp_df) > 0){
        .drawCellComponents(cellcomp_df)
    }

    # Draw braces
    braces_df <- shapes_df[shapes_df$ShapeType == "Brace",]
    if (nrow(braces_df) > 0){.drawBraces(braces_df)}
}


.plotLabels <- function(pathwayElement){

    # Prepare labels
    labels_df <- .prepareLabels(pathwayElement)

    # Draw labels
    .drawShapes(labels_df)
}


.plotGroups <- function(pathwayElement, groupElements_df){
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


.plotInteractions <- function(pathwayElement, gpml_fil){
    # Prepare edges
    edges_df <- .prepareEdges(
        pathwayElement,
        gpml_fil[names(gpml_fil) %in% c(
            "Interaction", "GraphicalLine")])

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


.plotStates <- function(pathwayElement, nodes_df_all){

    # Prepare states
    states_df <- .prepareStates(pathwayElement, nodes_df_all)

    # Draw states
    .drawStates(states_df)
}


.exportLegend <- function(outdir, outname, colorList){
    file_extension <- tolower(tools::file_ext(outname))

    # Export plot
    if (file_extension == "svg"){
        outfile_legend <- file.path(outdir, paste0("legend_", outname))
        svglite::svglite(
            outfile_legend,
            width = 5, height = length(colorList) + 1.25)
        .makeLegend(colorList)
        grDevices::dev.off()
    }
    else if (file_extension == "pdf"){
        outfile_legend <- file.path(outdir, paste0("legend_", outname))
        grDevices::pdf(
            outfile_legend ,
            width = 5, height = length(colorList) + 1.25)
        .makeLegend(colorList)
        grDevices::dev.off()
    }
    else if (file_extension == "png"){
        outfile_legend <- file.path(outdir, paste0("legend_", outname))
        grDevices::png(
            file = outfile_legend,
            width = 5, height = length(colorList) + 1.25,
            units = "in", res = 1200, pointsize = 12)
        .makeLegend(colorList)
        grDevices::dev.off()
    }
    else{
        outfile_legend <- file.path(outdir, paste0("legend_", outname, ".svg"))
        svglite::svglite(
            outfile_legend ,
            width = 5, height = length(colorList) + 1.25)
        .makeLegend(colorList)
        grDevices::dev.off()
    }
    return(outfile_legend)
}


.returnNodeTable <- function(colors_df_all){

    outputTable <- unique(colors_df_all[
        !is.na(colors_df_all$ScaleName),
        c("Label", "InputId", "ScaleName", "MapColor")])

    colnames(outputTable) <- c(
        "Node Label",
        "ID",
        "Scale Name",
        "Scale Value")
    outputTable <- outputTable |>
        tidyr::pivot_wider(
            names_from = "Scale Name",
            values_from = "Scale Value",
            values_fn = list
        )
}


.makeOutName <- function(gpml, network = FALSE){

    # Extract information from GPML file
    PathwayName <- gpml$.attrs["Name"]
    Organism <- gpml$.attrs["Organism"]
    PathwayID <- gpml$.attrs["Version"]

    # Combine information into name
    outname <- paste0(PathwayName,"_",PathwayID, "_",Organism)
    outname <- stringr::str_replace_all(outname, " ", "_")
    if(network){outname <- paste0("network_", outname)}
    outname <- make.names(outname)
}


.returnInformation <- function(gpml){
    info <- tryCatch({
        c(
            "Name" = as.character(gpml$.attrs["Name"]),
            "ID" = as.character(gpml$.attrs["Version"]),
            "Link" = paste0(
                "https://www.wikipathways.org/pathways/",
                stringr::str_split(gpml$.attrs["Version"], "_")[[1]][1],
                ".html"),
            "Description" = gpml[
                which(names(gpml) == "Comment")][
                    which.max(
                        nchar(gpml[which(names(gpml) == "Comment")])
                    )][[1]][[1]])
    },error = function(cond){NA})
    return(info)
}

.autoFileOpen <- function(path){
    if ((.Platform$OS.type == "windows")&
        (exists("shell", mode="function"))){
        shell(path)
    }else{
        warning("\n\n
    Automatic opening of the pathway image is only supported on Windows;
    please open the file manually on other operating systems.")
    }
}
