# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting edges
#'
#' @description This function makes a data frame for plotting edges.
#' @param dataEdges A GPML list filtered for edges.
#' @return A data frame for plotting edges.
#' @noRd

# Prepare edges for network
.prepareEdges_network <- function(dataEdges){
    edgeFUN <- function(dataEdges){
        endPoint <- sum(names(dataEdges$Graphics) == "Point")
        edges_df <- data.frame(
            GraphRef1 = as.character(
                dataEdges$Graphics[[1]]["GraphRef"]),
            GraphRef2 = as.character(
                dataEdges$Graphics[[endPoint]]["GraphRef"])
        )
    }
    edges_df <- do.call(rbind, lapply(dataEdges, edgeFUN))
    edges_df <- edges_df[
        !is.na(edges_df$GraphRef1) & !is.na(edges_df$GraphRef2),]
    colnames(edges_df) <- c("from", "to")
    return(edges_df)
}

# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting nodes
#'
#' @description This function makes a data frame for plotting nodes.
#' @param dataNodes A GPML list filtered for nodes.
#' @return A data frame for plotting nodes.
#' @noRd

# Prepare nodes for network
.prepareNodes_network <- function(dataNodes){
    nodeFUN <- function(dataNodes){
        data.frame(
            GraphId = as.character(dataNodes$.attrs["GraphId"]),
            GraphId1 = dataNodes$ID,
            Label = as.character(stringr::str_remove_all(
                dataNodes$.attrs["TextLabel"],"\\n+$")),
            NodeType = as.character(dataNodes$.attrs["Type"]),
            Database = as.character(dataNodes$Xref["Database"]),
            GroupRef = as.character(dataNodes$.attrs["GroupRef"]),
            ID = as.character(dataNodes$Xref["ID"])

        )
    }
    nodes_df <- do.call(rbind, lapply(dataNodes, nodeFUN))
    #nodes_df <- nodes_df[!is.na(nodes_df$GraphId),]
    return(nodes_df)
}


# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting groups
#'
#' @description This function makes a data frame for plotting groups.
#' @param dataGroups A GPML list filtered for groups.
#' @return A data frame for plotting groups.
#' @noRd

# Prepare groups for network
.prepareGroups_network <- function(dataGroups){
    groupFUN <- function(dataGroups){
        data.frame(
            GroupId = as.character(dataGroups["GroupId"]),
            GraphId = as.character(dataGroups["GraphId"])
        )
    }
    groups_df <- do.call(rbind, lapply(dataGroups, groupFUN))
    return(groups_df)
}
# ------------------------------------------------------------------------------
#' @title Show nodes as rectangles vertically splitted by color scale.
#'
#' @description This geom allows for plotting nodes as vertically splitted
#' rectangles.
#' @inheritParams ggraph::geom_node_text
#' @param nCol The number of columns the node should be split in.
#' @param iCol The node column index of current variable.
#' @param nodeSize The size of the nodes.
#' @return Rectangular nodes vertically splitted by color scale.
#' @importFrom rlang .data
#' @noRd

# Plot nodes in network
.geom_node_split <- function(
        mapping=NULL, data=NULL,
        position='identity',
        show.legend=NA, nCol = 1,
        iCol = 1, nodeSize = 1, ...) {
    mapping1 <- mapping
    mapping_temp1 <- mapping

    raw_mapping1 <- ggplot2::aes(
        xmin=.data$x-0.5*(0.06*nodeSize*(max(.data$x) - min(.data$x))) +
            ((0.06*nodeSize*(max(.data$x) - min(.data$x)))/nCol)*(iCol-1),
        ymin=.data$y-nodeSize*0.018*(max(.data$y) - min(.data$y)),
        xmax=.data$x-0.5*(0.06*nodeSize*(max(.data$x) - min(.data$x))) +
            ((0.06*nodeSize*(max(.data$x) - min(.data$x)))/nCol)*(iCol),
        ymax=.data$y+nodeSize*0.018*(max(.data$y) - min(.data$y)))

    mapping1 <- c(
        as.list(mapping_temp1),
        raw_mapping1[!names(raw_mapping1) %in% names(mapping_temp1)])
    class(mapping1) <- "uneval"

    ggplot2::layer(
        data=data,
        mapping=mapping1,
        stat=ggraph::StatFilter,
        geom=ggplot2::GeomRect,
        position=position,
        show.legend=show.legend,
        inherit.aes=FALSE,
        params=list(na.rm=FALSE, ...)
    )
}

# ------------------------------------------------------------------------------
#' @title Draw network from GPML file
#'
#' @description This function draws a network from a GPML file with the option
#' to map, e.g., expression data onto the network diagram.
#' @param infile Input GPML file. This can be a character string of the GPML
#' file location (e.g., "Downloads/WP42500.gpml") or a GPML string provided by
#' [rWikiPathways::getPathway].
#' @param outdir (optional) Output directory. The pathway and legend images
#' will be saved in this directory.
#' @param outname (optional) The file name of the output pathway image.
#' "svg","png",and "pdf" file extensions are accepted. If no file extension is
#' specified, the pathway and legend image will be generated in .svg format.
#' The legend file gets the "legend_" prefix.
#' @param featureIDs (optional) \code{character} vector of gene IDs.
#' @param colorVar (optional) \code{vector} or \code{data.frame} for
#' coloring the nodes in the pathway. This can be for instance a
#' \code{data.frame} with the log2FCs and significance in the columns.
#' The (row) order should match \code{featureIDs}. The color rules and palettes
#' for the supplied values can be set in the colorList parameter.
#' @param annGenes (optional) \code{character} string of the Bioconductor
#' annotation package (e.g., org.Hs.eg.db).
#' @param annMetabolites (optional) \code{tibble} or \code{data.frame} with
#' metabolite mapping information (see metaboliteIDmapping package).
#' @param inputDB (optional) Input gene ID type
#' (SYMBOL, ENTREZID, ENSEMBL, UNIPROT).
#' This can be a \code{character} vector of \code{length = 1}
#' (if all gene IDs are of the same type) or of
#' \code{length = nrow(featureIDs)}
#' (if you want to specify the type per gene ID).
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
#'  infile <- rWikiPathways::getPathway("WP4255")
#'
#'  # Draw pathway
#'  pathVis <- PinPath::GPML2Network(
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


GPML2Network <- function(
        infile,outdir = getwd(),outname = NULL,featureIDs = NULL,
        colorVar = NULL,annGenes = NULL,annMetabolites = NULL,inputDB = NULL,
        colorNames = NULL,colorList = NULL,NAvalue = "#F0F0F0",
        layout = "nicely",unconnectedNodes = FALSE,alpha = 0.9,nodeSize = 1,
        legend = FALSE,nodeTable = FALSE,pathInfo = FALSE,openFile = FALSE){
    # Read and prepare GPML file
    gpml <- XML::xmlToList(XML::xmlParse(xml2::read_xml(infile)))
    gpml_fil <- .prepareGPML(gpml)
    # Set default values if necessary
    if (is.null(outname)){ outname <- .makeOutName(gpml, network = TRUE)}
    if (is.null(colorList) & !is.null(colorVar)){
        colorList <- defaultColorList(colorVar, ColorNames = colorNames)}
    # Extract nodes
    dataNodes <- gpml_fil[names(gpml_fil) == "DataNode"]
    nodes_df_temp <- .prepareNodes_network(dataNodes)
    # Extract group information
    dataGroups <- gpml_fil[names(gpml_fil) == "Group"]
    groups_df <- .prepareGroups_network(dataGroups)
    # Prepare nodes for plotting
    df <- .allNodes_network(
        dataNodes, nodes_df_temp, groups_df, featureIDs, colorVar, annGenes,
        annMetabolites, inputDB, colorList, NAvalue)
    nodes_df <- df[[1]]
    colors_df <- df[[2]]
    # Prepare edges for plotting
    edges_df <- .allEdges_network(gpml_fil, nodes_df, groups_df)
    # Split nodes for multiple color scales
    nodes_df_split <- .splitNodes_network(nodes_df)

    # Make network
    g_plot <- .makeNetwork(
        edges_df, nodes_df_split, unconnectedNodes, layout, nodeSize, alpha)
    outfile <- .exportNetwork(g_plot, outdir, outname, nodeSize)
    outputList <- list()
    outputList[["Pathway"]] <- outfile
    if (openFile) {.autoFileOpen(outputList[["Pathway"]])}

    # Return legend, node table, pathway information
    if (legend & !is.null(colorList)){
        outputList[["Legend"]] <- .exportLegend(outdir, outname, colorList)
    } else{ outputList[["Legend"]] <- NA }
    if (nodeTable & !is.null(colors_df)){
        outputList[["NodeTable"]] <- .returnNodeTable(colors_df)
    } else{ outputList[["NodeTable"]] <- NA }
    if (pathInfo){
        outputList[["Information"]] <- .returnInformation(gpml)
    }else{ outputList[["Information"]] <- NA }
    return(outputList)
}


.exportNetwork <- function(g_plot, outdir, outname, nodeSize){
    # Get file extension
    file_extension <- tolower(tools::file_ext(outname))

    # Export plot
    if (file_extension == "svg"){
        outfile <-  file.path(outdir, outname)
        svglite::svglite(
            outfile,
            width = 13.3/nodeSize,
            height = 8.3/nodeSize)
        plot(g_plot)
        grDevices::dev.off()
    }else if (file_extension %in% c("png", "tiff", "pdf")){
        outfile <-  file.path(outdir, outname)
        ggplot2::ggsave(
            g_plot, file = outfile,
            width = 13.3/nodeSize,
            height = 8.3/nodeSize,
            limitsize = FALSE)
    }else{
        if (file_extension != ""){
            warning(
                "The output file does not have a valid file extension.
                Generating .svg file instead.")
        }
        # Set output file
        outfile <- file.path(outdir, paste0(outname,".svg"))

        svglite::svglite(
            outfile,
            width = 13.3/nodeSize,
            height = 8.3/nodeSize)
        plot(g_plot)
        grDevices::dev.off()
    }
    return(outfile)
}

.makeNetwork <- function(
        edges_df, nodes_df_split, unconnectedNodes, layout, nodeSize, alpha){
    # Make graph
    graph_full <- igraph::graph_from_data_frame(
        edges_df,
        vertices = nodes_df_split)
    # Remove self loops
    graph <- igraph::simplify(graph_full, remove.multiple = FALSE)
    # Remove unconnected nodes
    if (!unconnectedNodes){
        isolated <- which(igraph::degree(graph, mode = "total")==0)
        graph <- igraph::delete_vertices(graph, isolated)
    }

    # Make basis of network
    g_plot <- ggraph::ggraph(graph, layout = layout) +
        ggraph::geom_edge_link(ggplot2::aes(color = .data$type))
    # Add each scale to the network
    for (g in seq_len(ncol(nodes_df_split)-8)){
        loop_input <- paste0(
            ".geom_node_split(ggplot2::aes(alpha = NodeType),
            fill = g_plot@data$ColorValue",g,
            ",nCol = ",(ncol(nodes_df_split)-8),
            ", iCol = ", g,
            ", nodeSize = ", nodeSize, ")")
        g_plot <- g_plot + eval(parse(text=loop_input))
    }
    # Finalize network
    g_plot <- g_plot +
        .geom_node_split(
            ggplot2::aes(linewidth = .data$NodeType),
            alpha = 0, color = "lightgrey",
            nCol = 1, iCol = 1, nodeSize = nodeSize) +
        ggraph::geom_node_text(
            ggplot2::aes(label = .data$name,alpha = .data$NodeType),
            size = 2) +
        ggplot2::scale_alpha_manual(values = stats::setNames(
            c(alpha,0),c("nonGroup", "Group"))) +
        ggraph::scale_edge_color_manual(values = stats::setNames(
            c("black", "lightgrey"),
            c("node_node", "node_group"))) +
        ggplot2::scale_linewidth_manual(values = stats::setNames(
            c(0.3,0),c("nonGroup", "Group"))) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none")
    return(g_plot)
}


.allNodes_network <- function(
        dataNodes, nodes_df_temp, groups_df, featureIDs, colorVar, annGenes,
        annMetabolites,inputDB, colorList, NAvalue){
    # Map colors to nodes
    colors_df <- NULL
    if (!(
        is.null(featureIDs) | is.null(colorVar) |
        (is.null(annGenes) & is.null(annMetabolites)) | is.null(inputDB))){
        colors_df <- .mapColors(
            nodes_df = .prepareNodes(dataNodes), featureIDs = featureIDs,
            colorVar = colorVar, annGenes = annGenes,
            annMetabolites = data.frame(annMetabolites), inputDB = inputDB,
            colorList = colorList,NAvalue = NAvalue)
        # Add colors to nodes
        nodes_df <- dplyr::left_join(
            nodes_df_temp, colors_df[, c("GraphId1", "ColorValue","Scale")],
            by = c("GraphId1" = "GraphId1"))
    } else{
        nodes_df$ColorValue <- "white"
        nodes_df$Scale <- 1}
    # Change name
    nodes_df$name <- nodes_df$Label
    nodes_df <- nodes_df[,c(
        "name",colnames(nodes_df)[colnames(nodes_df) != "name"])]
    # Collect node-to-group link
    node2group <- nodes_df[, c("GraphId", "name", "GroupRef")]
    node2group <- node2group[!is.na(node2group$GroupRef),]
    node2group <- node2group[!duplicated(node2group),]

    # Give groups unique names
    group_ids <- unique(node2group$GroupRef)
    group_names <- rep(NA, length(group_ids))
    graph_ids<- rep(NA, length(group_ids))
    for (g in seq_along(group_ids)){
        group_names[g] <- paste(
            sort(node2group$name[node2group$GroupRef == group_ids[g]]),
            collapse = "_")
        graph_ids[g] <- groups_df$GraphId[groups_df$GroupId == group_ids[g]][1]
    }
    # Combine groups with node information
    if (length(group_names) > 0){
        nodes_df <- rbind.data.frame(
            nodes_df, data.frame(
                name = group_names, GraphId = graph_ids, GraphId1 = graph_ids,
                Label = group_names, NodeType = "Group", Database = NA,
                GroupRef = NA, ID = NA, ColorValue = "black", Scale = NA))}
    nodes_df$NodeType <- ifelse(
        nodes_df$NodeType  == "Group", "Group", "nonGroup")
    return(list(nodes_df,colors_df))
}

.allEdges_network <- function(gpml_fil, nodes_df, groups_df){
    # Prepare edges
    dataEdges <- gpml_fil[names(gpml_fil) %in% c(
        "Interaction",
        "GraphicalLine")]
    edges_df_temp <- .prepareEdges_network(dataEdges)

    if (sum(nodes_df$NodeType == "Group") > 0){
        group_edges_df <- nodes_df[
            nodes_df$NodeType  == "nonGroup",
            c("GraphId", "GroupRef")]
        group_edges_df <- dplyr::inner_join(
            group_edges_df, groups_df,
            by = c("GroupRef" = "GroupId"))[c(1,3)]
        colnames(group_edges_df) <- c("from", "to")
        edges_df <- rbind.data.frame(edges_df_temp, group_edges_df)
        edges_df$type <- c(
            rep("node_node", nrow(edges_df_temp)),
            rep("node_group", nrow(group_edges_df)))
    }else{
        edges_df <- edges_df_temp
        edges_df$type <- "node_node"
    }


    # Filter edges for nodes
    edges_df$from <- replace(
        stats::setNames(edges_df$from,edges_df$from),
        nodes_df$GraphId, nodes_df$Label)[edges_df$from]
    edges_df$to <- replace(
        stats::setNames(edges_df$to,edges_df$to),
        nodes_df$GraphId, nodes_df$Label)[edges_df$to]

    edges_df <- edges_df[(
        edges_df$from %in% nodes_df$name) & (edges_df$to %in% nodes_df$name),]

    return(edges_df)
}

.splitNodes_network <- function(nodes_df){
    # Collect NA and non-NA scales
    NAdf <- nodes_df[is.na(nodes_df$Scale),]
    nonNAdf <- nodes_df[!is.na(nodes_df$Scale),]

    # Add each scale as a seperate column
    scales <- unique(nodes_df$Scale)
    scales <- scales[!is.na(scales)]

    if (length(scales) > 0){
        for (s in scales){
            if (s == 1){
                nodes_df_split <- rbind.data.frame(
                    nonNAdf[nonNAdf$Scale == s,-10],
                    NAdf[,-10])
                colnames(nodes_df_split)[ncol(nodes_df_split)] <- "ColorValue1"
            }else{
                fil <- rbind.data.frame(nonNAdf[nonNAdf$Scale == s,], NAdf)
                nodes_df_split <- cbind.data.frame(
                    nodes_df_split,
                    fil$ColorValue)
                colnames(nodes_df_split)[ncol(nodes_df_split)] <- paste0(
                    "ColorValue",s)
            }
        }

        # Remove duplicated nodes
        dupIds <- sum(
            BiocGenerics::duplicated(nodes_df_split$name[
                !BiocGenerics::duplicated(
                    nodes_df_split[,9:ncol(nodes_df_split)])]))
        if (dupIds > 0){
            warning(
                "There duplicated feature IDs. The GPML2Network function
                only plots the values associated with the first feature ID.")
        }
    }

    if (length(scales) == 0){
        nodes_df_split <- NAdf[-10]
        colnames(nodes_df_split)[ncol(nodes_df_split)] <- "ColorValue1"
    }

    nodes_df_split <- nodes_df_split[
        !BiocGenerics::duplicated(nodes_df_split$name),]

    return(nodes_df_split)
}
