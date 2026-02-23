# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting groups
#'
#' @description This function makes a data frame for plotting groups.
#' @param dataGroups A GPML list filtered for groups.
#' @param nodes_df_groups A data frame with information about the nodes of
#' the group.
#' @return A data frame for plotting groups.
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @noRd

.prepareGroups <- function(dataGroups, nodes_df_groups){

    # Collect X and Y coordinates of the nodes in the group
    groups_df <- data.frame(
        GroupRef = nodes_df_groups$GroupRef,
        minY = as.numeric(nodes_df_groups$CenterY) -
            0.5*as.numeric(nodes_df_groups$Height),
        maxY = as.numeric(nodes_df_groups$CenterY) +
            0.5*as.numeric(nodes_df_groups$Height),
        minX = as.numeric(nodes_df_groups$CenterX) -
            0.5*as.numeric(nodes_df_groups$Width),
        maxX = as.numeric(nodes_df_groups$CenterX) +
            0.5*as.numeric(nodes_df_groups$Width)
    )
    groups_df <- groups_df[!is.na(groups_df$GroupRef),]

    if (!is.null(groups_df)){

        # Get the most extreme X and Y coordinates of the nodes in the group
        groups_df <- groups_df %>%
            dplyr::group_by(.data$GroupRef) %>%
            dplyr::mutate(maxX1 = max(.data$maxX)) %>%
            dplyr::mutate(minX1 = min(.data$minX)) %>%
            dplyr::mutate(maxY1 = max(.data$maxY)) %>%
            dplyr::mutate(minY1 = min(.data$minY))
        groups_df <- unique(groups_df[,c(1,6,7,8,9)])

        # Get group IDs
        GroupIds <- unlist(lapply(
            dataGroups,
            function(x){
                unlist(x)[stringr::str_detect(names(unlist(x)),"GroupId")]}))
        groups_df_all <- list()

        # Make data frame for plotting groups
        groups_df_all[[1]] <- .complexGroup(dataGroups, groups_df, GroupIds)
        groups_df_all[[2]] <- .emptyGroup(dataGroups, groups_df, GroupIds)
        groups_df_all[[3]] <- .pathwayGroup(dataGroups, groups_df, GroupIds)
        return(groups_df_all)
    }
}

.complexGroup <- function(dataGroups, groups_df, GroupIds){
    complex <- unlist(lapply(
        dataGroups,
        function(x){
            sum(stringr::str_detect(unlist(x), "Complex") &
                    stringr::str_detect(names(unlist(x)), "Style"))>0
        }))
    if (sum(complex) > 0){
        groups_df_complex <- do.call(
            rbind,
            apply(
                groups_df[groups_df$GroupRef %in% GroupIds[complex],],
                1,
                .makeComplexGroup)
        )
        groups_df_complex$Style <- "Complex"
        return(groups_df_complex)
    } else{return(NA)}
}

.emptyGroup <- function(dataGroups, groups_df, GroupIds){
    group <- unlist(
        lapply(
            dataGroups,
            function(x){
                sum(stringr::str_detect(names(unlist(x)), "Style")) == 0
            }
        ))
    if (sum(group) > 0){
        groups_df_empty <- do.call(
            rbind,
            apply(
                groups_df[groups_df$GroupRef %in% GroupIds[group],],
                1,
                .makeEmptyGroup)
        )
        groups_df_empty$Style <- "Empty"
        return(groups_df_empty)
    } else{return(NA)}
}

.pathwayGroup <- function(dataGroups, groups_df, GroupIds){
    pathway <- unlist(lapply(
        dataGroups,
        function(x){
            sum(stringr::str_detect(unlist(x), "Pathway") &
                    stringr::str_detect(names(unlist(x)), "Style"))>0
        }))
    if (sum(pathway) > 0){
        groups_df_pathway <- do.call(
            rbind,
            apply(
                groups_df[groups_df$GroupRef %in% GroupIds[pathway],],
                1,
                .makeEmptyGroup)
        )
        groups_df_pathway$Style <- "Empty"
        return(groups_df_pathway)
    } else{return(NA)}
}

.makeComplexGroup <- function(groups_row){
    maxX <- as.numeric(groups_row[2])
    minX <- as.numeric(groups_row[3])
    maxY <- as.numeric(groups_row[4])
    minY <- as.numeric(groups_row[5])

    temp <- data.frame(
        id = as.character(groups_row[1]),
        x = c(
            maxX + 10,maxX + 10,
            maxX, minX,
            minX - 10, minX - 10,
            minX, maxX),
        y = c(
            maxY, minY,
            minY - 10, minY - 10,
            minY, maxY,
            maxY + 10, maxY + 10)
    )
    return(temp)
}

.makeEmptyGroup <- function(groups_row){
    maxX <- as.numeric(groups_row[2])
    minX <- as.numeric(groups_row[3])
    maxY <- as.numeric(groups_row[4])
    minY <- as.numeric(groups_row[5])

    temp <- data.frame(
        id = as.character(groups_row[1]),
        xmin = minX-10,
        xmax = maxX+10,
        ymin = -minY+10,
        ymax = -maxY-10
    )
    return(temp)
}

# ------------------------------------------------------------------------------
#' @title Draw groups
#'
#' @description This function adds groups to the pathway image.
#' @param groups_df A data frame with group information, as generated by the
#' .prepareGroups() function.
#' @return A plot with groups.
#' @noRd

.drawGroups <- function(groups_df){
    if (sum(!is.na(groups_df[[1]]))>0){
        plotDF <- groups_df[[1]]

        graphics::polygon(
            x = plotDF$x,
            y = -1 *plotDF$y,
            col = "#F7F7EF",
            border = "#737373")

    }
    if (sum(!is.na(groups_df[[2]]))>0){

        plotDF <- groups_df[[2]]

        graphics::rect(
            xleft = plotDF$xmin,
            ybottom = plotDF$ymin,
            xright =  plotDF$xmax,
            ytop =  plotDF$ymax,
            col = "#F7F7EF",
            border = "#BDBDBD",
            lty = "dashed")
    }
    if (sum(!is.na(groups_df[[3]]))>0){

        plotDF <- groups_df[[3]]

        graphics::rect(
            xleft = plotDF$xmin,
            ybottom = plotDF$ymin,
            xright =  plotDF$xmax,
            ytop =  plotDF$ymax,
            col = "#E5FFE5",
            border = "#BDBDBD",
            lty = "dashed")
    }
}


