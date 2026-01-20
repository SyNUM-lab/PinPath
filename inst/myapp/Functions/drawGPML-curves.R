# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting curved edges
#'
#' @description This function makes a data frame for plotting curved edges.
#' @param df data frame with information about curved edges.
#' @return A data frame for plotting curved edges.

.prepareCurve <- function(df){
  
  # Curved edges are special elbow edges, so first prepare elbow edges
  df <- .prepareElbow(df)
  
  # Get all unique edges
  edges <- unique(df$GraphId)
  edges <- edges[!is.na(edges)]
  
  # for each unique edge, we want to seperate it into subedges
  # Each subedge consist of a starting point, a mid point, and an end point.
  # These points are needed to fit a bezier curve.
  df_all <- NULL
  for (e in 1:length(edges)){
    
    # Get the information about a single edge
    curve_df <- df[df$GraphId == edges[e],]
    curve_df <- curve_df[!((curve_df$X1 == curve_df$X2) & (curve_df$Y1 == curve_df$Y2)),]
    
    # Get all X and Y coordinates defining the path of edge
    X_all <- c(curve_df$X1[1],curve_df$X2)
    Y_all <- c(curve_df$Y1[1],curve_df$Y2)
    
    # We remove redundant X and Y coordinates, e.g., when two points describe 
    # the same direction
    remove_index <- c(which(sapply(1:(length(X_all)-1), function(n) sum(duplicated(X_all)[c(n,n+1)]))==2),
                      which(sapply(1:(length(Y_all)-1), function(n) sum(duplicated(Y_all)[c(n,n+1)]))==2))
    
    if (length(remove_index) > 0){
      X_all <- X_all[-remove_index]
      Y_all <- Y_all[-remove_index]
    }
    
    # Collect the coordinates of the corners
    X_corners <- X_all[-c(1,length(X_all))]
    Y_corners <- Y_all[-c(1,length(Y_all))]
    
    # Collect the coordinates of the start and end points
    X_start <- X_all[1]
    X_end <- X_all[length(X_all)]
    Y_start <- Y_all[1]
    Y_end <- Y_all[length(X_all)]
    
    # The mid points are in-between the corner points
    if (length(X_corners) > 1){
      X_mid <- sapply(1:(length(X_corners)-1), function(n) mean(X_corners[c(n,n+1)]))
      Y_mid <- sapply(1:(length(Y_corners)-1), function(n) mean(Y_corners[c(n,n+1)])) 
      
      # Collect all relevant X and Y coordinates for plotting the curved edges
      X_points <- c(X_start, X_mid[!is.na(X_mid)], X_end)
      Y_points <- c(Y_start, Y_mid[!is.na(Y_mid)], Y_end)
    } else{
      
      # Collect all relevant X and Y coordinates for plotting the curved edges
      X_points <- c(X_start, X_end)
      Y_points <- c(Y_start, Y_end)
    }
    
    
    
    # The last thing to do, is to separate the X and Y coordinates into sub-edges
    # Each curve can only be fitted to a sub-edge consisting of the start, mid, and end
    for (i in 1:(length(X_points) - 1)){
      
      # The arrow end and arrow type is by default "none"
      arrowEnd <- "none"
      arrowType <- "none"
      
      # If the arrow end is last, only the last sub-edge should get the arrow
      if (("last" %in% curve_df$ArrowEnd) & (i == (length(X_points) - 1))){
        arrowEnd <- "last"
        arrowType <- curve_df$ArrowType[curve_df$ArrowEnd == arrowEnd][1]
      }
      
      # If the arrow is first, only the first sub-edge should get the arrow
      if (("first" %in% curve_df$ArrowEnd) & (i == 1)){
        arrowEnd <- "first"
        arrowType <- curve_df$ArrowType[curve_df$ArrowEnd == arrowEnd][1]
      }
      
      # Combine the sub-edge data into a data frame
      df_subedge <- data.frame(x = c(X_points[i], rep(X_corners[i],2), X_points[i+1]),
                               y = c(Y_points[i], rep(Y_corners[i],2), Y_points[i+1]),
                               edge = edges[e],
                               subedge = i,
                               group = paste0(edges[e], "_", i),
                               color = curve_df$Color[1],
                               arrowEnd = arrowEnd,
                               arrowType = arrowType,
                               linewidth = curve_df$LineThickness[1],
                               linetype = curve_df$LineStyle[1]
      )
      
      # Combine the data of all edges and sub-edges
      df_all <- rbind.data.frame(df_all, df_subedge)
    }
  }
  
  # The "broken" linetype should be "dashed" linetype to be consistent with
  # ggplot2 naming conventions
  df_all$linetype[df_all$linetype == "broken"] <- "dashed"
  
  # Return data frame for plotting the curved edges
  return(df_all)
}


# ------------------------------------------------------------------------------
#' @title Draw curved edges
#'
#' @description This function adds curved edges to the pathway image.
#' @param df data frame with information about curved edges.
#' @return A plot with curved edges.

.drawCurves <- function(df){
  
  
  #============================================================================#
  # Edges without arrow head (`none`, `mim-necessary-stimulation`)
  #============================================================================#
  
  if (sum(df$arrowType == "none"  |
          df$arrowType == "mim-necessary-stimulation") > 0){
    
    # Filter for edges without arrow head
    plotDF <- df[df$arrowType == "none" |
                   df$arrowType == "mim-necessary-stimulation",]
    
    # Draw edges
    grid::grid.bezier(x = plotDF$x,
                      y = -1*plotDF$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF$color[1], 
                                    lwd = plotDF$linewidth[1], 
                                    lty = plotDF$linetype[1]))
    
  }
  
  
  #============================================================================#
  # Filled black arrow head (`Arrow`, `mim-conversion`)
  #============================================================================#
  
  if (sum(df$arrowType == "Arrow" |
          df$arrowType == "mim-conversion") > 0){
    
    
    plotDF_temp <- df[df$arrowType == "Arrow" |
                        df$arrowType == "mim-conversion",]
    
    # Get all edge groups. Each edge group corresponds to a single bezier curve
    groups <- unique(plotDF_temp$group)


    # Offset is the distance at which the arrow head should be attached to the
    # main body of the arrow
    offset <- 5

    plotDF_main <- NULL
    plotDF_end <- NULL
    for (i in 1:length(groups)){

      temp_main <- plotDF_temp[plotDF_temp$group == groups[i],]

      if ("last" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[nrow(temp_main)-1]
        x2 <- temp_main$x[nrow(temp_main)]
        y1 <- temp_main$y[nrow(temp_main)-1]
        y2 <- temp_main$y[nrow(temp_main)]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[nrow(temp_main)] <- temp_main$x[nrow(temp_main)] + x_offset
        temp_main$y[nrow(temp_main)] <- temp_main$y[nrow(temp_main)] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(X2 = x2,
                               X1 = temp_main$x[nrow(temp_main)],
                               Y2 = y2,
                               Y1 = temp_main$y[nrow(temp_main)],
                               ArrowEnd = temp_main$arrowEnd[nrow(temp_main)],
                               ArrowType = temp_main$arrowType[nrow(temp_main)],
                               LineThickness = temp_main$linewidth[nrow(temp_main)],
                               LineType = temp_main$linetype[nrow(temp_main)],
                               Color = temp_main$color[nrow(temp_main)]
        )
      }
      if ("first" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[1]
        x2 <- temp_main$x[2]
        y1 <- temp_main$y[1]
        y2 <- temp_main$y[2]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[1] <- temp_main$x[1] - x_offset
        temp_main$y[1] <- temp_main$y[1] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(X1 = x1,
                               X2 = temp_main$x[1],
                               Y1 = y1,
                               Y2 = temp_main$y[1],
                               ArrowEnd = temp_main$arrowEnd[1],
                               ArrowType = temp_main$arrowType[1],
                               LineThickness = temp_main$linewidth[1],
                               LineType = temp_main$linetype[1],
                               Color = temp_main$color[1]
        )
      }
      plotDF_main <- rbind.data.frame(plotDF_main,temp_main)
      plotDF_end <- rbind.data.frame(plotDF_end,temp_end)
    }

    # Draw edges
    grid::grid.bezier(x = plotDF_main$x,
                      y = -1*plotDF_main$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF_main$color[1], 
                                    lwd = plotDF_main$linewidth[1], 
                                    lty = plotDF_main$linetype[1]))
    shape::Arrows(x0 = plotDF_end$X1, x1 = plotDF_end$X2,
                  y0 = -1*plotDF_end$Y1, y1 = -1*plotDF_end$Y2, 
                  code = ifelse(plotDF_end$ArrowEnd == "first", 1, 2), 
                  arr.type = "triangle",
                  arr.length = 0.3,
                  arr.width = 0.3,
                  lcol = plotDF_end$Color, 
                  arr.col = plotDF_end$Color, 
                  col = plotDF_end$Color,
                  lwd = plotDF_end$LineThickness,
                  arr.adj = 1)
  }
  
  
  
  #============================================================================#
  # Open arrow head (`mim-binding`, `mim-modification`)
  #============================================================================#
  
  if (sum(df$arrowType == "mim-binding"|
          df$arrowType == "mim-modification") > 0){
    
    
    plotDF_temp <- df[df$arrowType == "mim-binding"|
                        df$arrowType == "mim-modification",]
    
    # Get all edge groups. Each edge group corresponds to a single bezier curve
    groups <- unique(plotDF_temp$group)

    # Offset is the distance at which the arrow head should be attached to the
    # main body of the arrow
    offset <- 5

    plotDF_main <- NULL
    plotDF_end <- NULL
    for (i in 1:length(groups)){

      temp_main <- plotDF_temp[plotDF_temp$group == groups[i],]

      if ("last" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[nrow(temp_main)-1]
        x2 <- temp_main$x[nrow(temp_main)]
        y1 <- temp_main$y[nrow(temp_main)-1]
        y2 <- temp_main$y[nrow(temp_main)]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[nrow(temp_main)] <- temp_main$x[nrow(temp_main)] + x_offset
        temp_main$y[nrow(temp_main)] <- temp_main$y[nrow(temp_main)] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(X2 = x2,
                               X1 = temp_main$x[nrow(temp_main)],
                               Y2 = y2,
                               Y1 = temp_main$y[nrow(temp_main)],
                               ArrowEnd = temp_main$arrowEnd[nrow(temp_main)],
                               ArrowType = temp_main$arrowType[nrow(temp_main)],
                               LineThickness = temp_main$linewidth[nrow(temp_main)],
                               LineType = temp_main$linetype[nrow(temp_main)],
                               Color = temp_main$color[nrow(temp_main)]
        )
      }
      if ("first" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[1]
        x2 <- temp_main$x[2]
        y1 <- temp_main$y[1]
        y2 <- temp_main$y[2]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[1] <- temp_main$x[1] - x_offset
        temp_main$y[1] <- temp_main$y[1] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(X1 = x1,
                               X2 = temp_main$x[1],
                               Y1 = y1,
                               Y2 = temp_main$y[1],
                               ArrowEnd = temp_main$arrowEnd[1],
                               ArrowType = temp_main$arrowType[1],
                               LineThickness = temp_main$linewidth[1],
                               LineType = temp_main$linetype[1],
                               Color = temp_main$color[1]
        )
      }
      plotDF_main <- rbind.data.frame(plotDF_main,temp_main)
      plotDF_end <- rbind.data.frame(plotDF_end,temp_end)
    }
    
    # Draw edges
    grid::grid.bezier(x = plotDF_main$x,
                      y = -1*plotDF_main$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF_main$color[1], 
                                    lwd = plotDF_main$linewidth[1], 
                                    lty = plotDF_main$linetype[1]))
    shape::Arrows(x0 = plotDF_end$X1, x1 = plotDF_end$X2,
                  y0 = -1*plotDF_end$Y1, y1 = -1*plotDF_end$Y2, 
                  code = ifelse(plotDF_end$ArrowEnd == "first", 1, 2), 
                  arr.type = "simple",
                  arr.length = 0.3,
                  arr.width = 0.3,
                  lcol = plotDF_end$Color, 
                  arr.col = plotDF_end$Color, 
                  col = plotDF_end$Color, 
                  lwd = plotDF_end$LineThickness,
                  arr.adj = 1)
  }
  
  
  #============================================================================#
  # Filled white arrow (`mim-stimulation`)
  #============================================================================#
  
  if (sum(df$arrowType == "mim-stimulation") > 0){
    
    plotDF_temp <- df[df$arrowType == "mim-stimulation",]
    
    # Get all edge groups. Each edge group corresponds to a single bezier curve
    groups <- unique(plotDF_temp$group)

    # Offset is the distance at which the arrow head should be attached to the
    # main body of the arrow
    offset <- 5

    plotDF_main <- NULL
    plotDF_end <- NULL
    for (i in 1:length(groups)){

      temp_main <- plotDF_temp[plotDF_temp$group == groups[i],]

      if ("last" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[nrow(temp_main)-1]
        x2 <- temp_main$x[nrow(temp_main)]
        y1 <- temp_main$y[nrow(temp_main)-1]
        y2 <- temp_main$y[nrow(temp_main)]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[nrow(temp_main)] <- temp_main$x[nrow(temp_main)] + x_offset
        temp_main$y[nrow(temp_main)] <- temp_main$y[nrow(temp_main)] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(X2 = x2,
                               X1 = temp_main$x[nrow(temp_main)],
                               Y2 = y2,
                               Y1 = temp_main$y[nrow(temp_main)],
                               ArrowEnd = temp_main$arrowEnd[nrow(temp_main)],
                               ArrowType = temp_main$arrowType[nrow(temp_main)],
                               LineThickness = temp_main$linewidth[nrow(temp_main)],
                               LineType = temp_main$linetype[nrow(temp_main)],
                               Color = temp_main$color[nrow(temp_main)]
        )
      }
      if ("first" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[1]
        x2 <- temp_main$x[2]
        y1 <- temp_main$y[1]
        y2 <- temp_main$y[2]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[1] <- temp_main$x[1] - x_offset
        temp_main$y[1] <- temp_main$y[1] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(X1 = x1,
                               X2 = temp_main$x[1],
                               Y1 = y1,
                               Y2 = temp_main$y[1],
                               ArrowEnd = temp_main$arrowEnd[1],
                               ArrowType = temp_main$arrowType[1],
                               LineThickness = temp_main$linewidth[1],
                               LineType = temp_main$linetype[1],
                               Color = temp_main$color[1]
        )
      }
      plotDF_main <- rbind.data.frame(plotDF_main,temp_main)
      plotDF_end <- rbind.data.frame(plotDF_end,temp_end)
    }
    
    # Draw edges
    grid::grid.bezier(x = plotDF_main$x,
                      y = -1*plotDF_main$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF_main$color[1], 
                                    lwd = plotDF_main$linewidth[1], 
                                    lty = plotDF_main$linetype[1]))
    shape::Arrows(x0 = plotDF_end$X1, x1 = plotDF_end$X2,
                  y0 = -1*plotDF_end$Y1, y1 = -1*plotDF_end$Y2, 
                  code = ifelse(plotDF_end$ArrowEnd == "first", 1, 2), 
                  arr.type = "triangle",
                  arr.length = 0.3,
                  arr.width = 0.3,
                  col = plotDF_end$Color,
                  lcol = plotDF_end$Color, 
                  arr.col = "white", 
                  lwd = plotDF_end$LineThickness,
                  arr.adj = 1)
  }
  
  
  #============================================================================#
  # T-bar (`mim-inhibition`, `T-bar`)
  #============================================================================#
  
  if (sum(df$arrowType == "mim-inhibition" |
          df$arrowType == "TBar") > 0){
    
    plotDF_temp <- df[df$arrowType == "mim-inhibition" |
                        df$arrowType == "TBar",]
    
    # Get all edge groups. Each edge group corresponds to a single bezier curve
    groups <- unique(plotDF_temp$group)

    # Offset is the distance at which the arrow head should be attached to the
    # main body of the arrow
    offset <- 5

    # Gap is the distance that should be present between the end of the arrow
    # and the node
    gap <- 10

    plotDF_main <- NULL
    plotDF_end <- NULL
    for (i in 1:length(groups)){

      temp_main <- plotDF_temp[plotDF_temp$group == groups[i],]

      if ("last" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[nrow(temp_main)-1]
        x2 <- temp_main$x[nrow(temp_main)]
        y1 <- temp_main$y[nrow(temp_main)-1]
        y2 <- temp_main$y[nrow(temp_main)]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        x_gap <- gap*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))
        y_gap <- gap*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[nrow(temp_main)] <- temp_main$x[nrow(temp_main)] + x_offset + x_gap
        temp_main$y[nrow(temp_main)] <- temp_main$y[nrow(temp_main)] + y_offset + y_gap

        # Collect plotting information for the arrow head
        temp_end <- data.frame(X2 = x2 + x_gap,
                               X1 = temp_main$x[nrow(temp_main)],
                               Y2 = y2 + y_gap,
                               Y1 = temp_main$y[nrow(temp_main)],
                               ArrowEnd = temp_main$arrowEnd[nrow(temp_main)],
                               ArrowType = temp_main$arrowType[nrow(temp_main)],
                               LineThickness = temp_main$linewidth[nrow(temp_main)],
                               LineType = temp_main$linetype[nrow(temp_main)],
                               Color = temp_main$color[nrow(temp_main)]
        )
      }
      if ("first" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[1]
        x2 <- temp_main$x[2]
        y1 <- temp_main$y[1]
        y2 <- temp_main$y[2]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        x_gap<- gap*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))
        y_gap <- gap*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[1] <- temp_main$x[1] - (x_offset+x_gap)
        temp_main$y[1] <- temp_main$y[1] + (y_offset+y_gap)

        # Collect plotting information for the arrow head
        temp_end <- data.frame(X1 = x1 - x_gap,
                               X2 = temp_main$x[1],
                               Y1 = y1 + y_gap,
                               Y2 = temp_main$y[1],
                               ArrowEnd = temp_main$arrowEnd[1],
                               ArrowType = temp_main$arrowType[1],
                               LineThickness = temp_main$linewidth[1],
                               LineType = temp_main$linetype[1],
                               Color = temp_main$color[1]
        )
      }
      plotDF_main <- rbind.data.frame(plotDF_main,temp_main)
      plotDF_end <- rbind.data.frame(plotDF_end,temp_end)
    }
    
    # Draw edges
    grid::grid.bezier(x = plotDF_main$x,
                      y = -1*plotDF_main$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF_main$color[1], 
                                    lwd = plotDF_main$linewidth[1], 
                                    lty = plotDF_main$linetype[1]))
    shape::Arrows(x0 = plotDF_end$X1, x1 = plotDF_end$X2,
                  y0 = -1*plotDF_end$Y1, y1 = -1*plotDF_end$Y2, 
                  code = ifelse(plotDF_end$ArrowEnd == "first", 1, 2), 
                  arr.type = "T",
                  arr.width = 0.4,
                  arr.length = 0.5,
                  lcol = plotDF_end$Color,
                  arr.col = plotDF_end$Color,
                  col = plotDF_end$Color,
                  lwd = plotDF_end$LineThickness,
                  arr.adj = 1)
  }
  
  #============================================================================#
  # Filled white circle (`mim-catalysis`)
  #============================================================================#
  
  if (sum(df$arrowType == "mim-catalysis") > 0){
    
    plotDF_temp <- df[df$arrowType == "mim-catalysis",]
    
    # Get all edge groups. Each edge group corresponds to a single bezier curve
    groups <- unique(plotDF_temp$group)

    # Offset is the distance at which the arrow head should be attached to the
    # main body of the arrow
    offset <- 5

    plotDF_main <- NULL
    plotDF_end <- NULL
    for (i in 1:length(groups)){

      temp_main <- plotDF_temp[plotDF_temp$group == groups[i],]

      if ("last" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[nrow(temp_main)-1]
        x2 <- temp_main$x[nrow(temp_main)]
        y1 <- temp_main$y[nrow(temp_main)-1]
        y2 <- temp_main$y[nrow(temp_main)]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[nrow(temp_main)] <- temp_main$x[nrow(temp_main)] + x_offset
        temp_main$y[nrow(temp_main)] <- temp_main$y[nrow(temp_main)] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(Xpoint =  temp_main$x[nrow(temp_main)],
                               Ypoint = temp_main$y[nrow(temp_main)],
                               ArrowEnd = temp_main$arrowEnd[nrow(temp_main)],
                               ArrowType = temp_main$arrowType[nrow(temp_main)],
                               LineThickness = temp_main$linewidth[nrow(temp_main)],
                               LineType = temp_main$linetype[nrow(temp_main)],
                               Color = temp_main$color[nrow(temp_main)]
        )
      }
      if ("first" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[1]
        x2 <- temp_main$x[2]
        y1 <- temp_main$y[1]
        y2 <- temp_main$y[2]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[1] <- temp_main$x[1] - x_offset
        temp_main$y[1] <- temp_main$y[1] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(Xpoint = temp_main$x[1],
                               Ypoint = temp_main$y[1],
                               ArrowEnd = temp_main$arrowEnd[1],
                               ArrowType = temp_main$arrowType[1],
                               LineThickness = temp_main$linewidth[1],
                               LineType = temp_main$linetype[1],
                               Color = temp_main$color[1]
        )
      }
      plotDF_main <- rbind.data.frame(plotDF_main,temp_main)
      plotDF_end <- rbind.data.frame(plotDF_end,temp_end)
    }

    # Draw edges
    grid::grid.bezier(x = plotDF_main$x,
                      y = -1*plotDF_main$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF_main$color[1], 
                                    lwd = plotDF_main$linewidth[1], 
                                    lty = plotDF_main$linetype[1]))
    points(x = plotDF_end$Xpoint,
         y = -1*plotDF_end$Ypoint,
         pch = 21,
         cex = 2,
         bg = "white",
         col = plotDF_main$color[1])
  }
  
  
  #============================================================================#
  # Filled white square (`min-covalent-bond`)
  #============================================================================#
  
  if (sum(df$arrowType == "mim-covalent-bond") > 0){
    
    plotDF_temp <- df[df$arrowType == "mim-covalent-bond",]
    
    # Get all edge groups. Each edge group corresponds to a single bezier curve
    groups <- unique(plotDF_temp$group)

    # Offset is the distance at which the arrow head should be attached to the
    # main body of the arrow
    offset <- 5

    plotDF_main <- NULL
    plotDF_end <- NULL
    for (i in 1:length(groups)){

      temp_main <- plotDF_temp[plotDF_temp$group == groups[i],]

      if ("last" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[nrow(temp_main)-1]
        x2 <- temp_main$x[nrow(temp_main)]
        y1 <- temp_main$y[nrow(temp_main)-1]
        y2 <- temp_main$y[nrow(temp_main)]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[nrow(temp_main)] <- temp_main$x[nrow(temp_main)] + x_offset
        temp_main$y[nrow(temp_main)] <- temp_main$y[nrow(temp_main)] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(Xpoint =  temp_main$x[nrow(temp_main)],
                               Ypoint = temp_main$y[nrow(temp_main)],
                               ArrowEnd = temp_main$arrowEnd[nrow(temp_main)],
                               ArrowType = temp_main$arrowType[nrow(temp_main)],
                               LineThickness = temp_main$linewidth[nrow(temp_main)],
                               LineType = temp_main$linetype[nrow(temp_main)],
                               Color = temp_main$color[nrow(temp_main)]
        )
      }
      if ("first" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[1]
        x2 <- temp_main$x[2]
        y1 <- temp_main$y[1]
        y2 <- temp_main$y[2]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[1] <- temp_main$x[1] - x_offset
        temp_main$y[1] <- temp_main$y[1] + y_offset

        # Collect plotting information for the arrow head
        temp_end <- data.frame(Xpoint = temp_main$x[1],
                               Ypoint = temp_main$y[1],
                               ArrowEnd = temp_main$arrowEnd[1],
                               ArrowType = temp_main$arrowType[1],
                               LineThickness = temp_main$linewidth[1],
                               LineType = temp_main$linetype[1],
                               Color = temp_main$color[1]
        )
      }
      plotDF_main <- rbind.data.frame(plotDF_main,temp_main)
      plotDF_end <- rbind.data.frame(plotDF_end,temp_end)
    }

    # Draw edges
    grid::grid.bezier(x = plotDF_main$x,
                      y = -1*plotDF_main$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF_main$color[1], 
                                    lwd = plotDF_main$linewidth[1], 
                                    lty = plotDF_main$linetype[1]))
    points(x = plotDF_end$Xpoint,
           y = -1*plotDF_end$Ypoint,
           pch = 22,
           cex = 2,
           bg = "white",
           col = plotDF_main$color[1])
  }
  
  
  
  #============================================================================#
  # Straight line with gap at the end (`mim-gap`)
  #============================================================================#
  
  if (sum(df$arrowType == "mim-gap") > 0){
    
    plotDF_temp <- df[df$arrowType == "mim-gap",]
    
    # Get all edge groups. Each edge group corresponds to a single bezier curve
    groups <- unique(plotDF_temp$group)

    # Gap is the distance that should be present between the end of the arrow
    # and the node
    gap <- 10

    plotDF_main <- NULL
    plotDF_end <- NULL
    for (i in 1:length(groups)){

      temp_main <- plotDF_temp[plotDF_temp$group == groups[i],]

      if ("last" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[nrow(temp_main)-1]
        x2 <- temp_main$x[nrow(temp_main)]
        y1 <- temp_main$y[nrow(temp_main)-1]
        y2 <- temp_main$y[nrow(temp_main)]

        # Calculate gaps
        x_gap <- gap*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_gap <- gap*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[nrow(temp_main)] <- temp_main$x[nrow(temp_main)] + x_gap
        temp_main$y[nrow(temp_main)] <- temp_main$y[nrow(temp_main)] + y_gap
      }

      if ("first" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[1]
        x2 <- temp_main$x[2]
        y1 <- temp_main$y[1]
        y2 <- temp_main$y[2]

        # Calculate offsets
        x_gap <- gap*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_gap <- gap*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        temp_main$x[1] <- temp_main$x[1] - x_gap
        temp_main$y[1] <- temp_main$y[1] + y_gap
      }

      plotDF_main <- rbind.data.frame(plotDF_main,temp_main)
    }

    # Draw edges
    grid::grid.bezier(x = plotDF_main$x,
                      y = -1*plotDF_main$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF_main$color[1], 
                                    lwd = plotDF_main$linewidth[1], 
                                    lty = plotDF_main$linetype[1]))
  }
  
  
  #============================================================================#
  # `mim-cleavage`
  #============================================================================#
  
  
  if (sum(df$arrowType == "mim-cleavage")> 0){
    
    plotDF_temp<- df[df$arrowType == "mim-cleavage",]
    
    # Get all edge groups. Each edge group corresponds to a single bezier curve
    groups <- unique(plotDF_temp$group)

    # Offset is the distance from the end node to the start of the arrow head
    offset <- 15

    # Deviation is the length of the orthogonal distance of the arrow head
    deviation <- 12

    plotDF_main <- NULL
    plotDF_orth <- NULL
    plotDF_diag <- NULL
    for (i in 1:length(groups)){
      temp_main <- plotDF_temp[plotDF_temp$group == groups[i],]

      if ("last" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[nrow(temp_main)-1]
        x2 <- temp_main$x[nrow(temp_main)]
        y1 <- temp_main$y[nrow(temp_main)-1]
        y2 <- temp_main$y[nrow(temp_main)]

        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        # Main part of the line
        temp_main$x[nrow(temp_main)] <- temp_main$x[nrow(temp_main)] + x_offset
        temp_main$y[nrow(temp_main)] <- temp_main$y[nrow(temp_main)] + y_offset

        # Orthogonal part of the line
        rotated_coords <- t(.rotation_matrix(-0.5*pi) %*% c(deviation*((x1-x2)/(abs(x1-x2) + abs(y1-y2))),
                                                            deviation*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))
        ))

        temp_orth <- data.frame(X1 = temp_main$x[nrow(temp_main)],
                                X2 = temp_main$x[nrow(temp_main)] + rotated_coords[1,1],
                                Y1 = temp_main$y[nrow(temp_main)],
                                Y2 = temp_main$y[nrow(temp_main)] + rotated_coords[1,2],
                                LineThickness = temp_main$linewidth[nrow(temp_main)],
                                LineType = temp_main$linetype[nrow(temp_main)],
                                Color = temp_main$color[nrow(temp_main)])

        # Diagonal part of the line
        temp_diag <- data.frame(X2 =  x2,
                                X1 = temp_main$x[nrow(temp_main)] + rotated_coords[1,1],
                                Y2 = y2,
                                Y1 = temp_main$y[nrow(temp_main)] + rotated_coords[1,2],
                                LineThickness = temp_main$linewidth[nrow(temp_main)],
                                LineType = temp_main$linetype[nrow(temp_main)],
                                Color = temp_main$color[nrow(temp_main)])

      }
      if ("first" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[1]
        x2 <- temp_main$x[2]
        y1 <- temp_main$y[1]
        y2 <- temp_main$y[2]


        # Calculate offsets
        x_offset <- offset*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_offset <- offset*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        # Main part of the line
        temp_main$x[1] <- temp_main$x[1] + x_offset
        temp_main$y[1] <- temp_main$y[1] + y_offset

        # Orthogonal part of the line
        rotated_coords <- t(.rotation_matrix(-0.5*pi) %*% c(deviation*((x1-x2)/(abs(x1-x2) + abs(y1-y2))),
                                                            deviation*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))
        ))

        temp_orth <- data.frame(X1 = temp_main$x[1],
                                X2 = temp_main$x[1] + rotated_coords[1,1],
                                Y1 = temp_main$y[1],
                                Y2 = temp_main$y[1] + rotated_coords[1,2],
                                LineThickness = temp_g$linewidth[1],
                                LineType = temp_g$linetype[1],
                                Color = temp_g$color[1])

        # Diagonal part of the line
        temp_diag <- data.frame(X2 =  x2,
                                X1 = temp_main$x[1] + rotated_coords[1,1],
                                Y2 = y2,
                                Y1 = temp_main$y[n1] + rotated_coords[1,2],
                                LineThickness = temp_main$linewidth[1],
                                LineType = temp_main$linetype[1],
                                Color = temp_main$color[1])
      }
      plotDF_main <- rbind.data.frame(plotDF_main,temp_main)
      plotDF_orth <- rbind.data.frame(plotDF_orth,temp_orth)
      plotDF_diag <- rbind.data.frame(plotDF_diag,temp_diag)
    }

    # Draw edges
    grid::grid.bezier(x = plotDF_main$x,
                      y = -1*plotDF_main$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF_main$color[1], 
                                    lwd = plotDF_main$linewidth[1], 
                                    lty = plotDF_main$linetype[1]))
    arrows(x0 = plotDF_orth$X1, x1 = plotDF_orth$X2,
           y0 = -1*plotDF_orth$Y1, y1 = -1*plotDF_orth$Y2, 
           length = 0, 
           col = plotDF_orth$Color, 
           lty = plotDF_orth$LineStyle, 
           lwd = plotDF_orth$LineThickness)
    arrows(x0 = plotDF_diag$X1, x1 = plotDF_diag$X2,
           y0 = -1*plotDF_diag$Y1, y1 = -1*plotDF_diag$Y2, 
           length = 0, 
           col = plotDF_diag$Color, 
           lty = plotDF_diag$LineStyle, 
           lwd = plotDF_diag$LineThickness)
  }
  
  #============================================================================#
  # `mim-transcription-translation`
  #============================================================================#
  
  if (sum(df$arrowType == "mim-transcription-translation")> 0){
    
    plotDF_temp<- df[df$arrowType == "mim-transcription-translation",]
    
    # Get all edge groups. Each edge group corresponds to a single bezier curve
    groups <- unique(plotDF_temp$group)

    # Gap is the distance between node and main line
    gap <- 15

    # extend is how much the main line should extend beyond the gap
    extend <- 5

    # Deviation is the length of the orthogonal distance of the arrow head
    deviation <- 10

    plotDF_main <- NULL
    plotDF_orth <- NULL
    plotDF_end <- NULL
    for (i in 1:length(groups)){
      temp_main <- plotDF_temp[plotDF_temp$group == groups[i],]

      if ("last" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[nrow(temp_main)-1]
        x2 <- temp_main$x[nrow(temp_main)]
        y1 <- temp_main$y[nrow(temp_main)-1]
        y2 <- temp_main$y[nrow(temp_main)]

        # Calculate gaps
        x_gap <- gap*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        x_extend <- extend*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_gap <- gap*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))
        y_extend <- extend*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        # Orthogonal part of the line
        rotated_coords <- t(.rotation_matrix(-0.5*pi) %*% c(deviation*((x1-x2)/(abs(x1-x2) + abs(y1-y2))),
                                                            deviation*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))
        ))

        temp_orth <- data.frame(X1 = temp_main$x[nrow(temp_main)] + x_gap,
                                X2 = temp_main$x[nrow(temp_main)] + x_gap - rotated_coords[1,1],
                                Y1 = temp_main$y[nrow(temp_main)] + y_gap,
                                Y2 = temp_main$y[nrow(temp_main)] + y_gap - rotated_coords[1,2],
                                LineThickness = temp_main$linewidth[nrow(temp_main)],
                                LineType = temp_main$linetype[nrow(temp_main)],
                                Color = temp_main$color[nrow(temp_main)])

        # End part of the line
        temp_end <- data.frame(X2 =  x2 - rotated_coords[1,1],
                                X1 = temp_main$x[nrow(temp_main)] + x_gap - rotated_coords[1,1],
                                Y2 = y2 - rotated_coords[1,2],
                                Y1 = temp_main$y[nrow(temp_main)] + y_gap - rotated_coords[1,2],
                                LineThickness = temp_main$linewidth[nrow(temp_main)],
                                LineType = temp_main$linetype[nrow(temp_main)],
                                Color = temp_main$color[nrow(temp_main)])

        # Main part of the line
        temp_main$x[nrow(temp_main)] <- temp_main$x[nrow(temp_main)] + x_gap - x_extend
        temp_main$y[nrow(temp_main)] <- temp_main$y[nrow(temp_main)] + y_gap - y_extend

      }
      if ("first" %in% temp_main$arrowEnd){

        # Collect X and Y coordinates in separate vectors
        x1 <- temp_main$x[1]
        x2 <- temp_main$x[2]
        y1 <- temp_main$y[1]
        y2 <- temp_main$y[2]

        # Calculate offsets
        x_gap <- gap*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        x_extend <- extend*((x1-x2)/(abs(x1-x2) + abs(y1-y2)))
        y_gap <- gap*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))
        y_extend <- extend*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))

        # Orthogonal part of the line
        rotated_coords <- t(.rotation_matrix(-0.5*pi) %*% c(deviation*((x1-x2)/(abs(x1-x2) + abs(y1-y2))),
                                                            deviation*((y1-y2)/(abs(x1-x2) + abs(y1-y2)))
        ))

        temp_orth <- data.frame(X1 = temp_main$x[1] + x_gap,
                                X2 = temp_main$x[1] + x_gap - rotated_coords[1,1],
                                Y1 = temp_main$y[1] + y_gap,
                                Y2 = temp_main$y[1] + y_gap - rotated_coords[1,2],
                                LineThickness = temp_main$linewidth[1],
                                LineType = temp_main$linetype[1],
                                Color = temp_main$color[1])

        # End part of the line
        temp_end<- data.frame(X2 =  x2 - rotated_coords[1,1],
                                X1 = temp_main$x[1] + x_gap - rotated_coords[1,1],
                                Y2 = y2 - rotated_coords[1,2],
                                Y1 = temp_main$y[1] + y_gap - rotated_coords[1,2],
                                LineThickness = temp_main$linewidth[1],
                                LineType = temp_main$linetype[1],
                                Color = temp_main$color[1])

        # Main part of the line
        temp_main$x[1] <- temp_main$x[1] + x_gap - x_extend
        temp_main$y[1] <- temp_main$y[1] + y_gap - y_extend
      }
      plotDF_main <- rbind.data.frame(plotDF_main,temp_main)
      plotDF_orth <- rbind.data.frame(plotDF_orth,temp_orth)
      plotDF_end <- rbind.data.frame(plotDF_end,temp_end)

    }
    
    # Draw edges
    grid::grid.bezier(x = plotDF_main$x,
                      y = -1*plotDF_main$y,
                      default.units = "native",
                      gp=grid::gpar(col= plotDF_main$color[1], 
                                    lwd = plotDF_main$linewidth[1], 
                                    lty = plotDF_main$linetype[1]))
    arrows(x0 = plotDF_orth$X1, x1 = plotDF_orth$X2,
           y0 = -1*plotDF_orth$Y1, y1 = -1*plotDF_orth$Y2, 
           length = 0, 
           col = plotDF_orth$Color, 
           lty = plotDF_orth$LineStyle, 
           lwd = plotDF_orth$LineThickness)
    shape::Arrows(x0 = plotDF_end$X1, x1 = plotDF_end$X2,
                  y0 = -1*plotDF_end$Y1, y1 = -1*plotDF_end$Y2, 
                  code = 2, 
                  arr.type = "triangle",
                  arr.length = 0.2,
                  arr.width = 0.2,
                  col = plotDF_end$Color,
                  lcol = plotDF_end$Color, 
                  arr.col = "white", 
                  lwd = plotDF_end$LineThickness,
                  arr.adj = 1)
    
  }
}