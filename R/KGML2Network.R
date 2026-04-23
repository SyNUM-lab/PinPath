# ------------------------------------------------------------------------------
#' @title Draw network from KGML file
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
#' @param featureIDs (optional) \code{character} vector of gene IDs.
#' @param colorVar (optional) \code{vector} or \code{data.frame} for coloring
#' the nodes in the pathway. This can be for instance a \code{data.frame} with
#' the log2FCs and significance in the columns. The (row) order should match
#' \code{featureIDs}.  The color rules and palettes for the supplied values can
#' be set in the colorList parameter.
#' @param annGenes (optional) \code{character} string of the Bioconductor
#' annotation package (e.g., org.Hs.eg.db).
#' @param annMetabolites (optional) \code{tibble} or \code{data.frame} with
#' metabolite mapping information (see metaboliteIDmapping package).
#' @param inputDB (optional) Input gene ID type
#' (SYMBOL, ENTREZID, ENSEMBL, UNIPROT). This can be a \code{character}
#' vector of \code{length = 1} (if all gene IDs are of the same type) or of
#' \code{length = nrow(featureIDs)} (if you want to specify the type
#' per gene ID).
#' @param colorNames (optional) \code{character} vector with names of the
#' color variables. If \code{colorNames} is NULL, the column names of the
#' \code{colorVar} \code{data.frame} will be used.
#' @param colorList (optional) A list with information about the coloring of
#' the nodes. An example can be generated using the \link{defaultColorList}
#' function.
#' @param NAvalue (optional) Node color for \code{NA} values.
#' @param layout (optional) Network layout from igraph.
#' @param unconnectedNodes (optional) Logical (TRUE or FALSE).
#' Should unconnected (isolated) nodes be shown in the network?
#' @param alpha (optional) Transparency of the nodes.
#' @param nodeSize (optional) Size of the nodes.
#' @param legend (optional) Logical (TRUE or FALSE).
#' Should the legend be plotted?
#' @param nodeTable (optional) Logical (TRUE or FALSE).
#' Should a node table be returned?
#' @param pathInfo (optional) Logical (TRUE or FALSE).
#' Should pathway information be returned?
#' @param openFile (optional) Logical (TRUE or FALSE).
#' Should the pathway file be opened after it has been saved?
#' This option only works for Windows users.
#' @return A \code{list} with the node table and the file location of the
#' pathway and legend image.
#' @importFrom rlang .data
#' @examples
#'
#'  # Load example data
#'  lung_expr <- read.csv(system.file(
#'      "extdata","data-lung-cancer.csv", package="PinPath"),
#'      stringsAsFactors = FALSE)
#'
#'  # Select pathway
#'  pathway_id <- "hsa05223"
#'  bfc <- BiocFileCache::BiocFileCache()
#'  infile <- BiocFileCache::bfcrpath(bfc,
#'  paste0("https://rest.kegg.jp/get/",pathway_id,"/kgml"))
#'
#'  # Draw pathway
#'  pathVis <- PinPath::KGML2Network(
#'      infile = infile,
#'      outdir = tempdir(),
#'      annGenes = "org.Hs.eg.db",
#'      inputDB = "ENSEMBL",
#'      featureIDs = lung_expr$GeneID,
#'      colorVar = lung_expr[,"log2FC"],
#'      nodeTable = TRUE,
#'      legend = TRUE,
#'      openFile = FALSE) # <-- set to TRUE to open the image automatically
#'
#' @export

KGML2Network <- function(
        infile,outdir = getwd(),outname = NULL,featureIDs = NULL,
        colorVar = NULL,annGenes = NULL,annMetabolites = NULL,inputDB = NULL,
        colorNames = NULL,colorList = NULL,NAvalue = "#F0F0F0",
        layout = "nicely",unconnectedNodes = FALSE,alpha = 0.9,nodeSize = 1,
        legend = FALSE,nodeTable = FALSE,pathInfo = FALSE,openFile = FALSE){
    # Read KGML file
    kgml <- XML::xmlToList(XML::xmlParse(infile))

    # Set default values if necessary
    if (is.null(outname)){outname <- .makeOutName_KGML(kgml, network = TRUE)}
    if (is.null(colorList) & !is.null(colorVar)){
        colorList <- defaultColorList(colorVar, ColorNames = colorNames)}

    # Prepare entries
    df <- .prepareEntries_network(
        kgml, featureIDs, colorVar, annGenes, annMetabolites, inputDB,
        colorList, NAvalue)
    entries_df <- df[[1]]
    colors_df <- df[[2]]
    # Prepare relations
    relations_df <- .prepareRelations_network(kgml, entries_df)
    # split entries
    entries_df_split <- .splitEntries_network(entries_df)

    # Make network
    g_plot <- .makeNetwork_KGML(
        relations_df, entries_df_split, unconnectedNodes, layout, nodeSize,
        alpha)
    outfile <- .exportNetwork(g_plot, outdir, outname, nodeSize)
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



.makeOutName_KGML <- function(kgml, network = FALSE){
    # Get pathway name
    PathwayName <- kgml[[length(kgml)]]["title"]

    # Get organism
    Organism <- kgml[[length(kgml)]]["org"]

    # Get pathway id
    PathwayID <- kgml[[length(kgml)]]["number"]

    # Make name
    outname <- paste0(PathwayName,"_",PathwayID, "_",Organism)
    outname <- stringr::str_replace_all(outname, " ", "_")
    if(network){outname <- paste0("network_", outname)}
    outname <- make.names(outname)
    return(outname)
}


.prepareEntries_network <- function(
        kgml, featureIDs, colorVar, annGenes, annMetabolites, inputDB,
        colorList, NAvalue){

    # Collect entries from KGML file
    dataEntries <- kgml[names(kgml) == "entry"]
    entries_df <- .extractEntries(dataEntries)

    # Map colors to entries
    if (!(
        is.null(featureIDs) |
        is.null(colorVar) |
        (is.null(annGenes) & is.null(annMetabolites)) |
        is.null(inputDB))){
        colors_df <- .mapColors(
            nodes_df = entries_df,
            featureIDs = featureIDs,
            colorVar = colorVar,
            annGenes = annGenes,
            annMetabolites = data.frame(annMetabolites),
            inputDB = inputDB,
            colorList = colorList,
            NAvalue = NAvalue)

        # Add colors to nodes
        entries_df <- dplyr::left_join(
            entries_df,
            colors_df[, c("GraphId1", "ColorValue","Scale")],
            by = c("GraphId1" = "GraphId1"))
    } else{
        entries_df$ColorValue <- "white"
        entries_df$Scale <- 1
    }

    # Change name
    entries_df$name <- entries_df$ID
    entries_df <- entries_df[, c("name", colnames(entries_df)[
        colnames(entries_df) != "name"])]
    entries_df <- entries_df[!is.na(entries_df$name),]
    return(list(entries_df,colors_df))
}



.prepareRelations_network <- function(kgml, entries_df){
    dataRelations <- kgml[names(kgml) == "relation"]

    relationsFUN <- function(dataRelations){
        relations_df <- data.frame(
            from = dataRelations$.attrs[1],
            to = dataRelations$.attrs[2],
            type = dataRelations$.attrs[3]
        )
    }
    relations_df <- do.call(rbind, lapply(dataRelations, relationsFUN))

    # Filter relations for entries
    relations_df$from <- replace(
        stats::setNames(relations_df$from,
                        relations_df$from),
        entries_df$GraphId1,
        entries_df$name)[relations_df$from]
    relations_df$to <- replace(
        stats::setNames(relations_df$to,
                        relations_df$to),
        entries_df$GraphId1,
        entries_df$name)[relations_df$to]

    relations_df <- relations_df[
        (relations_df$from %in% entries_df$name) &
            (relations_df$to %in% entries_df$name),]
    return(relations_df)
}


.splitEntries_network <- function(entries_df){
    # Collect NA and non-NA scales
    NAdf <- entries_df[is.na(entries_df$Scale),]
    nonNAdf <- entries_df[!is.na(entries_df$Scale),]

    # Add each scale as a separate column
    scales <- unique(entries_df$Scale)
    scales <- scales[!is.na(scales)]

    if (length(scales) > 0){
        for (s in scales){
            if (s == 1){
                entries_df_split <- rbind.data.frame(
                    nonNAdf[nonNAdf$Scale == s,-16],
                    NAdf[,-16])
                colnames(entries_df_split)[
                    ncol(entries_df_split)] <- "ColorValue1"
            }else{
                fil <- rbind.data.frame(nonNAdf[nonNAdf$Scale == s,], NAdf)
                entries_df_split <- cbind.data.frame(
                    entries_df_split,
                    fil$ColorValue)
                colnames(entries_df_split)[
                    ncol(entries_df_split)] <- paste0("ColorValue",s)
            }
        }
        # Remove duplicated nodes
        dupIds <- sum(
            BiocGenerics::duplicated(entries_df_split$name[
                !BiocGenerics::duplicated(entries_df_split[
                    ,(ncol(entries_df_split)-1):ncol(entries_df_split)
                ])]))
        if (dupIds > 0){warning(
            "There duplicated feature IDs. The KGML2Network function
                only plots the values associated with the first feature ID.")}
    }

    if (length(scales) == 0){
        entries_df_split <- NAdf[-16]
        colnames(entries_df_split)[ncol(entries_df_split)] <- "ColorValue1"}

    entries_df_split <- entries_df_split[
        !BiocGenerics::duplicated(entries_df_split$name),]
    return(entries_df_split)
}


.makeNetwork_KGML <- function(
        relations_df, entries_df_split,
        unconnectedNodes, layout, nodeSize, alpha){
    # Make graph
    graph <- igraph::graph_from_data_frame(
        relations_df,
        vertices = entries_df_split)

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
    for (g in seq_len((ncol(entries_df_split)-14))){

        loop_input <- paste0(
            ".geom_node_split(fill = g_plot@data$ColorValue",
            g,", alpha = ",alpha,
            ", nCol = ",(ncol(entries_df_split)-14),
            ", iCol = ", g, ", nodeSize = ", nodeSize, ")")

        g_plot <- g_plot + eval(parse(text=loop_input))
    }

    # Finalize network
    g_plot <- g_plot +
        .geom_node_split(
            linewidth = 0.3,
            alpha = 0, color = "lightgrey",
            nCol = 1, iCol = 1, nodeSize = nodeSize) +
        ggraph::geom_node_text(ggplot2::aes(label = .data$name), size = 2) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none")
    return(g_plot)
}

.returnNodeTable_KGML <- function(colors_df){
    outputTable <- unique(colors_df[
        !is.na(colors_df$ScaleName),
        c("ID", "InputId", "ScaleName", "MapColor")])
    colnames(outputTable) <- c(
        "Node Label",
        "ID",
        "Scale Name",
        "Scale Value")
    outputTable <- outputTable |>
        tidyr::pivot_wider(
            names_from = "Scale Name",
            values_from = "Scale Value",
            values_fn = list)
    return(outputTable)
}


.returnInformation_KGML <- function(kgml){
    info <- c(
        "Name" = as.character(kgml$.attrs["title"]),
        "ID" = paste0(kgml$.attrs["org"], kgml$.attrs["number"]),
        "Link" = as.character(kgml$.attrs["link"]),
        "Description" = "")
    return(info)
}
