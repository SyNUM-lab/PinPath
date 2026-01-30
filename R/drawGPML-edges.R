# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting edges
#'
#' @description This function makes a data frame for plotting edges.
#' @param dataEdges A GPML list object filtered for a specific edge.
#' @param dataEdges_all All entries of GPML edge list.
#' @return A data frame for plotting edges.
#' @noRd

.prepareEdges <- function(dataEdges, dataEdges_all){
  
  # Get graph IDs from the anchor points
  anchorIds <- lapply(dataEdges, function(x){
    if ("Anchor" %in% names(x$Graphics)){
      anchors <- unlist(x$Graphics[names(x$Graphics) == "Anchor"])
      anchors <- anchors[stringr::str_detect(names(anchors),"GraphId")]
      return(anchors)
    } else{
      return(NA)
    }
    
  })
  
  # Prepare data frame
  edges_df <- do.call(rbind, 
                      lapply(dataEdges, 
                             function(x){
                               .edgeFUN(x, dataEdges_all, anchorIds)
                             }
                      )
  )
  
  # Relative X and Y positions should be either 0 or 1
  # edges_df$Xrel1 <- sign(edges_df$Xrel1)*round(abs(edges_df$Xrel1)-0.01)
  # edges_df$Xrel2 <- sign(edges_df$Xrel2)*round(abs(edges_df$Xrel2)-0.01)
  # edges_df$Yrel1 <- sign(edges_df$Yrel1)*round(abs(edges_df$Yrel1)-0.01)
  # edges_df$Yrel2 <- sign(edges_df$Yrel2)*round(abs(edges_df$Yrel2)-0.01)
  
  # Prepare straight edges
  if (nrow(edges_df[edges_df$ConnectorType == "Straight" |
                    edges_df$ConnectorType == "Segmented",]) > 0){
    
    edges_df_straight <- edges_df[
      edges_df$ConnectorType == "Straight" |
        edges_df$ConnectorType == "Segmented",]
    
    edges_df_straight$LineStyle[
      edges_df_straight$LineStyle == "broken"] <- "dashed"
    
  } else{
    edges_df_straight <- NULL
  }
  
  # Prepare elbow edges
  if (nrow(edges_df[edges_df$ConnectorType == "Elbow",]) > 0){
    edges_df_elbow <- .prepareElbow(
      edges_df[edges_df$ConnectorType == "Elbow",])
  } else{
    edges_df_elbow <- NULL
  }
  
  # Prepare curved edges
  if (nrow(edges_df[edges_df$ConnectorType == "Curved",]) > 0){
    if (sum((edges_df$X1 - edges_df$X2)==0 | (edges_df$Y1 - edges_df$Y2)==0)>0){
      edges_df_straight <- edges_df[
        (edges_df$ConnectorType == "Curved") &
          ((edges_df$X1 - edges_df$X2)==0 | (edges_df$Y1 - edges_df$Y2)==0),]
      edges_df_straight$LineStyle[
        edges_df_straight$LineStyle == "broken"] <- "dashed"
      
      edges_df_curved <- .prepareCurve(
        edges_df[
          (edges_df$ConnectorType == "Curved") &
            ((edges_df$X1 - edges_df$X2)!=0 & 
               (edges_df$Y1 - edges_df$Y2)!=0),]
        )
      
    }else{
      edges_df_curved <- .prepareCurve(
        edges_df[edges_df$ConnectorType == "Curved",])
    }
  } else{
    edges_df_curved <- NULL
  }
  
  # Combine straight and elbow edges
  edges_df_all <- rbind.data.frame(edges_df_straight, edges_df_elbow)
  
  # If arrow head is different on both sides, we need to draw them as two 
  # separate arrows
  if (sum(edges_df_all$ArrowType == "Different")>0){
    edges_df_dif1 <- edges_df_all[edges_df_all$ArrowType == "Different",]
    edges_df_dif1$ArrowType <- edges_df_dif1$ArrowHead1
    edges_df_dif2 <- edges_df_dif1
    edges_df_dif2$ArrowType <- edges_df_dif2$ArrowHead2
    edges_df_dif <- rbind.data.frame(edges_df_dif1, edges_df_dif2)
    edges_df_all <- rbind.data.frame(
      edges_df_all[edges_df_all$ArrowType != "Different",],
      edges_df_dif)
  }
  
  # If arrow head is different on both sides, we need to draw them as two 
  # separate arrows
  if (sum(edges_df_all$ArrowEnd == "both")>0){
    edges_df_dif1 <- edges_df_all[edges_df_all$ArrowEnd == "both",]
    edges_df_dif1$ArrowEnd <- "first"
    edges_df_dif2 <- edges_df_dif1
    edges_df_dif2$ArrowEnd <- "last"
    edges_df_dif <- rbind.data.frame(edges_df_dif1, edges_df_dif2)
    edges_df_all <- rbind.data.frame(
      edges_df_all[edges_df_all$ArrowEnd != "both",],
      edges_df_dif)
  }
  
  # Return data frame
  return(list(lines = edges_df_all, curves = edges_df_curved))
}

# ------------------------------------------------------------------------------
#' @title Extract edge information from GPML list
#'
#' @description This function extracts the information from the GPML list and 
#' put it into a data.frame
#' @param dataEdges single entry of GPML edge list.
#' @param dataEdges_all all entries of GPML edge list.
#' @param anchorIds vector with graph IDs from anchor points
#' @return data.frame with edge information.
#' @noRd

.edgeFUN <- function(dataEdges, dataEdges_all, anchorIds){
  
  # There are two types of elbow edges:
  # 1) Elbow edges that follow the default path.
  # 2) Elbow edges that follow a custom path set by the GPML creator.
  
  #======================================================================#
  # Custom elbow edges
  #======================================================================#
  # When there are more than two coordinates for an edge, we know it is a 
  # custom elbow edge
  if (sum(names(dataEdges$Graphics) == "Point") > 2){
    
    # We need to define the path between each of the given coordinates.
    # This path exist of starting coordinates (X1 and Y1) and end 
    # coordinates (X2 and Y2)
    edges_df <- NULL
    for (p in seq_len(sum(names(dataEdges$Graphics) == "Point")-1)){
      
      # For the starting point we need to determine the relative X and Y 
      # positions. The relative X and Y positions of the other points 
      # follow from this.
      if (p == 1){
        
        # Collect the information about the starting point of the first edge
        temp_edges1 <- data.frame(
          X1 = as.numeric(dataEdges$Graphics[[p]]["X"]),
          Y1 = as.numeric(dataEdges$Graphics[[p]]["Y"]),
          GraphRef1 = as.character(dataEdges$Graphics[[p]]["GraphRef"]),
          Xrel1 = ifelse(is.na(as.numeric(dataEdges$Graphics[[p]]["RelX"])),
                         -1, as.numeric(dataEdges$Graphics[[p]]["RelX"])),
          Yrel1 = ifelse(is.na(as.numeric(dataEdges$Graphics[[p]]["RelY"])),
                         0, as.numeric(dataEdges$Graphics[[p]]["RelY"])),
          ArrowHead1 = as.character(dataEdges$Graphics[[p]]["ArrowHead"]))
        
        # Relative X and Y positions should be either 0 or 1
        temp_edges1$Xrel1 <- sign(
          temp_edges1$Xrel1)*round(abs(temp_edges1$Xrel1)-0.01)
        temp_edges1$Yrel1 <- sign(
          temp_edges1$Yrel1)*round(abs(temp_edges1$Yrel1)-0.01)
        
        # If the relative position of X and Y is both zero
        # the edge is probably attached to another edge.
        # So, we need to determine the relative X and Y position from 
        # the attached edge
        if ((temp_edges1$Xrel1 == 0) & (temp_edges1$Yrel1 == 0)){
          
          # Get the edge(s) to which the edge is attached
          attachedEdges <- as.numeric(
            which(
              unlist(
                lapply(anchorIds, function(x) temp_edges1$GraphRef1 %in% x)
              )
            )
          )
          
          if(length(attachedEdges) != 0){
            
            # Calculate the delta X and Y, to determine whether the 
            # attachment point is in Y or X direction
            Obj <- dataEdges_all[[attachedEdges]]$Graphics
            Obj <- Obj[names(Obj) == "Point"]
            dX <- as.numeric(Obj[[1]]["X"]) - 
              as.numeric(Obj[[length(Obj)]]["X"])
            dY <- as.numeric(Obj[[1]]["Y"]) - 
              as.numeric(Obj[[length(Obj)]]["Y"])
            
            # If the attached edge moves predominantly in Y direction, 
            # the attachment point should be in the X direction
            if (abs(dY) > abs(dX)){
              if (temp_edges1$X1 > as.numeric(dataEdges$Graphics[[p+1]]["X"])){
                temp_edges1$Xrel1 <- -1
              } else{
                temp_edges1$Xrel1 <- 1 
              }
            }
            # If the attached edge moves predominantly in X direction, 
            # the attachment point should be in the Y direction
            if (abs(dY) <= abs(dX)){
              if (temp_edges1$Y1 > as.numeric(dataEdges$Graphics[[p+1]]["Y"])){
                temp_edges1$Yrel1 <- -1
              } else{
                temp_edges1$Yrel1 <- 1 
              }
            }
          }else{
            if (temp_edges1$Y1 > as.numeric(dataEdges$Graphics[[p+1]]["Y"])){
              temp_edges1$Yrel1 <- -1
            } else{
              temp_edges1$Yrel1 <- 1
            }
          }
          
        }
        
        # Collect the information about the end point of the first edge
        temp_edges2 <- data.frame(
          X2 = as.numeric(dataEdges$Graphics[[p+1]]["X"]),
          Y2 = as.numeric(dataEdges$Graphics[[p+1]]["Y"]),
          GraphRef2 = as.character(dataEdges$Graphics[[p+1]]["GraphRef"]),
          Xrel2 = -1*floor(abs(temp_edges1$Yrel1))*
            sign(as.numeric(dataEdges$Graphics[[p+1]]["X"]) - temp_edges1$X1),
          Yrel2 = -1*floor(abs(temp_edges1$Xrel1))*
            sign(as.numeric(dataEdges$Graphics[[p+1]]["Y"]) - temp_edges1$Y1),
          ArrowHead2 = as.character(dataEdges$Graphics[[p+1]]["ArrowHead"]),
          LineStyle = tolower(as.character(
            ifelse(is.na(dataEdges$Graphics$.attrs["LineStyle"]),
                   "solid", 
                   dataEdges$Graphics$.attrs["LineStyle"]))),
          Color = as.character(ifelse(
            is.na(dataEdges$Graphics$.attrs["Color"]),
            "#000000", 
            paste0("#",dataEdges$Graphics$.attrs["Color"]))),
          ConnectorType = as.character(ifelse(
            is.na(dataEdges$Graphics$.attrs["ConnectorType"]),
            "Straight", 
            dataEdges$Graphics$.attrs["ConnectorType"])),
          LineThickness = as.numeric(ifelse(
            is.na(dataEdges$Graphics$.attrs["LineThickness"]),
            1, 
            dataEdges$Graphics$.attrs["LineThickness"])),
          Force = TRUE
        )
        
        # Combine data from the starting (temp_edges1) and 
        # ending point (temp_edges2) from the first edge
        temp_edges <- cbind.data.frame(temp_edges1, temp_edges2)
        
        # For the coordinates after the first edge, the relative X and Y 
        # positions can be determined from the edge before
      } else{
        
        # Collect the information about the starting point
        temp_edges1 <- data.frame(
          X1 = as.numeric(dataEdges$Graphics[[p]]["X"]),
          Y1 = as.numeric(dataEdges$Graphics[[p]]["Y"]),
          GraphRef1 = as.character(dataEdges$Graphics[[p]]["GraphRef"]),
          Xrel1 = -1*edges_df$Xrel2[p-1],
          Yrel1 = -1*edges_df$Yrel2[p-1],
          ArrowHead1 = as.character(dataEdges$Graphics[[p]]["ArrowHead"]))
        
        # Collect the information about the end point
        temp_edges2 <- data.frame(
          X2 = as.numeric(dataEdges$Graphics[[p+1]]["X"]),
          Y2 = as.numeric(dataEdges$Graphics[[p+1]]["Y"]),
          GraphRef2 = as.character(dataEdges$Graphics[[p+1]]["GraphRef"]),
          Xrel2 = -1*floor(abs(temp_edges1$Yrel1))*
            sign(as.numeric(dataEdges$Graphics[[p+1]]["X"])- temp_edges1$X1),
          Yrel2 = -1*floor(abs(temp_edges1$Xrel1))*
            sign(as.numeric(dataEdges$Graphics[[p+1]]["Y"])- temp_edges1$Y1),
          ArrowHead2 = as.character(dataEdges$Graphics[[p+1]]["ArrowHead"]),
          LineStyle = tolower(as.character(ifelse(
            is.na(dataEdges$Graphics$.attrs["LineStyle"]),
            "solid", 
            dataEdges$Graphics$.attrs["LineStyle"]))),
          Color = as.character(ifelse(
            is.na(dataEdges$Graphics$.attrs["Color"]),
            "#000000", 
            paste0("#",dataEdges$Graphics$.attrs["Color"]))),
          ConnectorType = as.character(ifelse(
            is.na(dataEdges$Graphics$.attrs["ConnectorType"]),
            "Straight", 
            dataEdges$Graphics$.attrs["ConnectorType"])),
          LineThickness = as.numeric(ifelse(
            is.na(dataEdges$Graphics$.attrs["LineThickness"]),
            1, 
            dataEdges$Graphics$.attrs["LineThickness"])),
          Force = TRUE
        )
        
        # Combine data from the starting (temp_edges1) 
        # and ending point (temp_edges2) 
        temp_edges <- cbind.data.frame(temp_edges1, temp_edges2)
      }
      
      
      # Now we have the add information about the arrows to the data frame
      
      # Arrow head on both sides
      if(!is.na(temp_edges$ArrowHead1) & !is.na(temp_edges$ArrowHead2)){
        temp_edges$ArrowEnd <- "both"
        temp_edges$ArrowType <- ifelse(
          temp_edges$ArrowHead1 == temp_edges$ArrowHead2,
          temp_edges$ArrowHead1, "Different")
      }
      
      # Arrow head at the end of the edge
      if(is.na(temp_edges$ArrowHead1) & !is.na(temp_edges$ArrowHead2)){
        temp_edges$ArrowEnd <- "last"
        temp_edges$ArrowType <- temp_edges$ArrowHead2
      }
      
      # Arrow head at the start of the edge
      if(!is.na(temp_edges$ArrowHead1) & is.na(temp_edges$ArrowHead2)){
        temp_edges$ArrowEnd <- "first"
        temp_edges$ArrowType <- temp_edges$ArrowHead1
      }
      
      # No arrow head
      if(is.na(temp_edges$ArrowHead1) & is.na(temp_edges$ArrowHead2)){
        temp_edges$ArrowEnd <- "none"
        temp_edges$ArrowType <- "none"
      }
      
      # Combine the information of all edges into a single data frame
      edges_df <- rbind.data.frame(edges_df, temp_edges)
      
    } # EO for-loop
    
    #======================================================================#
    # Default elbow edges
    #======================================================================#
    
  } else{
    
    # Collect the information from the dataEdges object
    edges_df <- data.frame(
      X1 = as.numeric(dataEdges$Graphics[[1]]["X"]),
      Y1 = as.numeric(dataEdges$Graphics[[1]]["Y"]),
      GraphRef1 = as.character(dataEdges$Graphics[[1]]["GraphRef"]),
      Xrel1 = ifelse(is.na(as.numeric(dataEdges$Graphics[[1]]["RelX"])),
                     -1, as.numeric(dataEdges$Graphics[[1]]["RelX"])),
      Yrel1 = ifelse(is.na(as.numeric(dataEdges$Graphics[[1]]["RelY"])),
                     0, as.numeric(dataEdges$Graphics[[1]]["RelY"])),
      ArrowHead1 = as.character(dataEdges$Graphics[[1]]["ArrowHead"]),
      
      X2 = as.numeric(dataEdges$Graphics[[2]]["X"]),
      Y2 = as.numeric(dataEdges$Graphics[[2]]["Y"]),
      GraphRef2 = as.character(dataEdges$Graphics[[2]]["GraphRef"]),
      Xrel2 = ifelse(is.na(as.numeric(dataEdges$Graphics[[2]]["RelX"])),
                     1, as.numeric(dataEdges$Graphics[[2]]["RelX"])),
      Yrel2 = ifelse(is.na(as.numeric(dataEdges$Graphics[[2]]["RelY"])),
                     0, as.numeric(dataEdges$Graphics[[2]]["RelY"])),
      ArrowHead2 = as.character(dataEdges$Graphics[[2]]["ArrowHead"]),
      LineStyle = tolower(as.character(ifelse(
        is.na(dataEdges$Graphics$.attrs["LineStyle"]),
        "solid",
        dataEdges$Graphics$.attrs["LineStyle"]))),
      Color = as.character(ifelse(
        is.na(dataEdges$Graphics$.attrs["Color"]),
        "#000000", 
        paste0("#",dataEdges$Graphics$.attrs["Color"]))),
      ConnectorType = as.character(ifelse(
        is.na(dataEdges$Graphics$.attrs["ConnectorType"]),
        "Straight", 
        dataEdges$Graphics$.attrs["ConnectorType"])),
      LineThickness = as.numeric(ifelse(
        is.na(dataEdges$Graphics$.attrs["LineThickness"]),
        1,
        dataEdges$Graphics$.attrs["LineThickness"])),
      Force = FALSE
    )
    
    # Relative X and Y positions should be either 0 or 1
    edges_df$Xrel1 <- sign(edges_df$Xrel1)*round(abs(edges_df$Xrel1)-0.01)
    edges_df$Xrel2 <- sign(edges_df$Xrel2)*round(abs(edges_df$Xrel2)-0.01)
    edges_df$Yrel1 <- sign(edges_df$Yrel1)*round(abs(edges_df$Yrel1)-0.01)
    edges_df$Yrel2 <- sign(edges_df$Yrel2)*round(abs(edges_df$Yrel2)-0.01)
    
    # If the relative position of starting X (Xrel1) and Y (Yrel1) are
    # both zero the edge is probably attached to another edge.
    # So, we need to determine the relative X and Y position from 
    # the attached edge
    if ((edges_df$Xrel1 == 0) & (edges_df$Yrel1 == 0)){
      
      # Get the edge(s) to which the edge is attached
      attachedEdges <- as.numeric(
        which(
          unlist(
            lapply(anchorIds, function(x) edges_df$GraphRef1 %in% x)
          )
        )
      )
      
      if(length(attachedEdges) != 0){
        
        # Calculate the delta X and Y, to determine whether the 
        # attachment point is in Y or X direction
        Obj <- dataEdges_all[[attachedEdges]]$Graphics
        Obj <- Obj[names(Obj) == "Point"]
        dX <- as.numeric(Obj[[1]]["X"]) - as.numeric(Obj[[length(Obj)]]["X"])
        dY <- as.numeric(Obj[[1]]["Y"]) - as.numeric(Obj[[length(Obj)]]["Y"])
        
        # If the attached edge moves predominantly in Y direction, 
        # the attachment point should be in the X direction
        if (abs(dY) > abs(dX)){
          if (edges_df$X1 > edges_df$X2){
            edges_df$Xrel1 <- -1
          } else{
            edges_df$Xrel1 <- 1 
          }
        }
        
        # If the attached edge moves predominantly in X direction, 
        # the attachment point should be in the Y direction
        if (abs(dY) <= abs(dX)){
          if (edges_df$Y1 > edges_df$Y2){
            edges_df$Yrel1 <- -1
          } else{
            edges_df$Yrel1 <- 1 
          }
        }
      } else{
        if (edges_df$Y1 > edges_df$Y2){
          edges_df$Yrel1 <- -1
        } else{
          edges_df$Yrel1 <- 1 
        }
      }
    }
    
    # If the relative position of ending X (Xrel2) and Y (Yrel2) is both zero
    # the edge is probably attached to another edge.
    # So, we need to determine the relative X and Y position from 
    # the attached edge
    if ((edges_df$Xrel2 == 0) & (edges_df$Yrel2 == 0)){
      
      # Get the edge(s) to which the edge is attached
      attachedEdges <- as.numeric(
        which(
          unlist(
            lapply(anchorIds, function(x) edges_df$GraphRef2 %in% x)
          )
        )
      )
      
      if(length(attachedEdges) != 0){
        
        # Calculate the delta X and Y, to determine whether the 
        # attachment point is in Y or X direction
        Obj <- dataEdges_all[[attachedEdges]]$Graphics
        Obj <- Obj[names(Obj) == "Point"]
        dX <- as.numeric(Obj[[1]]["X"]) - as.numeric(Obj[[length(Obj)]]["X"])
        dY <- as.numeric(Obj[[1]]["Y"]) - as.numeric(Obj[[length(Obj)]]["Y"])
        
        # If the attached edge moves predominantly in Y direction, 
        # the attachment point should be in the X direction
        if (abs(dY) > abs(dX)){
          if (edges_df$X2 > edges_df$X1){
            edges_df$Xrel2 <- -1
          } else{
            edges_df$Xrel2 <- 1 
          }
        }
        
        # If the attached edge moves predominantly in X direction, 
        # the attachment point should be in the Y direction
        if (abs(dY) <= abs(dX)){
          if (edges_df$Y2 > edges_df$Y1){
            edges_df$Yrel2 <- -1
          } else{
            edges_df$Yrel2 <- 1 
          }
        }
      }else{
        if (edges_df$Y1 > edges_df$Y2){
          edges_df$Yrel2 <- -1
        } else{
          edges_df$Yrel2 <- 1 
        }
      }
      
    }
    
    # Add arrow head to edge information
    
    # Arrow head on both sides
    if(!is.na(edges_df$ArrowHead1) & !is.na(edges_df$ArrowHead2)){
      
      if (edges_df$ArrowHead1 == edges_df$ArrowHead2){
        edges_df$ArrowEnd <- "both"
        edges_df$ArrowType <- edges_df$ArrowHead1 
      } else{
        edges_df$ArrowEnd <- "both"
        edges_df$ArrowType <- "Different" 
      }
      
    }
    
    # Arrow head at the end
    if(is.na(edges_df$ArrowHead1) & !is.na(edges_df$ArrowHead2)){
      edges_df$ArrowEnd <- "last"
      edges_df$ArrowType <- edges_df$ArrowHead2
    }
    
    # Arrow head at the start
    if(!is.na(edges_df$ArrowHead1) & is.na(edges_df$ArrowHead2)){
      edges_df$ArrowEnd <- "first"
      edges_df$ArrowType <- edges_df$ArrowHead1
    }
    
    # No arrow head
    if(is.na(edges_df$ArrowHead1) & is.na(edges_df$ArrowHead2)){
      edges_df$ArrowEnd <- "none"
      edges_df$ArrowType <- "none"
    }
  }
  
  # If an object has no is no graph Id, set it to a random number
  edges_df$GraphId <- dataEdges$ID
  
  # return data frame
  return(edges_df)
}


# ------------------------------------------------------------------------------
#' @title Draw straight edges
#'
#' @description This function adds straight edges to the pathway image.
#' @param df A data frame with edge information, as generated by the 
#' .prepareEdges() function.
#' @return A plot with straight edges.
#' @noRd

.drawEdges <- function(df){
  
  #========================================================================#
  # Edges without arrow head (`none`, `mim-necessary-stimulation`)
  #========================================================================#
  
  if (sum(df$ArrowType == "none"  |
          df$ArrowType == "mim-necessary-stimulation") > 0){
    
    # Filter for edges without arrow head
    plotDF <- df[df$ArrowType == "none" |
                   df$ArrowType == "mim-necessary-stimulation",]
    
    # Draw edges
    graphics::arrows(x0 = plotDF$X1, x1 = plotDF$X2,
                     y0 = -1*plotDF$Y1, y1 = -1*plotDF$Y2, 
                     length = 0, 
                     col = plotDF$Color, 
                     lty = plotDF$LineStyle, 
                     lwd = plotDF$LineThickness)
    
  }
  
  #========================================================================#
  # Filled black arrow head (`Arrow`, `mim-conversion`)
  #========================================================================#
  
  if (sum(df$ArrowType == "Arrow"  |
          df$ArrowType == "mim-conversion") > 0){
    
    # Filter for edges with filled black arrow
    plotDF_temp <- df[df$ArrowType == "Arrow"  |
                        df$ArrowType == "mim-conversion",]
    
    # We will plot the main body of the edge separately from the
    # arrow head, because the arrow head looks weird when the line is dashed.
    
    # Collect X and Y coordinates in separate vectors
    x1 <- plotDF_temp$X1
    x2 <- plotDF_temp$X2
    y1 <- plotDF_temp$Y1
    y2 <- plotDF_temp$Y2
    
    # Offset is the distance at which the arrow head should be attached to the
    # main body of the arrow
    offset <- 5
    
    plotDF <- NULL
    for (a in seq_len(nrow(plotDF_temp))){
      
      if (plotDF_temp$ArrowEnd[a] == "first"){
        
        Xstart <- x1[a]-offset*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                                 abs(y1[a]-y2[a])))
        Xend <- x2[a]
        X1 <- x1[a]
        X2 <- Xstart
        Ystart <- -1*(y1[a]-offset*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                     abs(y1[a]-y2[a]))))
        Yend <- -y2[a]
        Y1 <- -y1[a]
        Y2 <- Ystart
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, X1, X2,
            Ystart, Yend, Y1, Y2
          )
        )
      }
      
      if (plotDF_temp$ArrowEnd[a] == "last"){
        
        Xstart <- x1[a]
        Xend <- x2[a]-offset*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                               abs(y1[a]-y2[a])))
        X1 <- Xend
        X2 <- x2[a]
        Ystart <- -y1[a]
        Yend <- -1*(y2[a]-offset*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                   abs(y1[a]-y2[a]))))
        Y1 <- Yend
        Y2 <- -y2[a]
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, X1, X2,
            Ystart, Yend, Y1, Y2
          )
        )
      }
    }
    
    # Draw edges
    graphics::arrows(x0 = plotDF$Xstart, x1 = plotDF$Xend,
                     y0 = plotDF$Ystart, y1 = plotDF$Yend, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    shape::Arrows(x0 = plotDF$X1, x1 = plotDF$X2,
                  y0 = plotDF$Y1, y1 = plotDF$Y2, 
                  code = ifelse(plotDF_temp$ArrowEnd == "first", 1, 2), 
                  arr.type = "triangle",
                  arr.length = 0.3,
                  arr.width = 0.3,
                  col = plotDF_temp$Color,
                  lcol = plotDF_temp$Color, 
                  arr.col = plotDF_temp$Color, 
                  lwd = plotDF_temp$LineThickness,
                  arr.adj = 1)
  }
  
  #========================================================================#
  # Open arrow head (`mim-binding`, `mim-modification`)
  #========================================================================#
  
  if (sum(df$ArrowType == "mim-binding"|
          df$ArrowType == "mim-modification") > 0){
    
    
    plotDF_temp <- df[df$ArrowType == "mim-binding"|
                        df$ArrowType == "mim-modification",]
    
    
    # We will plot the main body of the edge separately from the
    # arrow head, because the arrow head looks weird when the line is dashed.
    
    # Collect X and Y coordinates in separate vectors
    x1 <- plotDF_temp$X1
    x2 <- plotDF_temp$X2
    y1 <- plotDF_temp$Y1
    y2 <- plotDF_temp$Y2
    
    # Offset is the distance at which the arrow head should be attached to 
    # the main body of the arrow
    offset <- 5
    
    plotDF <- NULL
    for (a in seq_len(nrow(plotDF_temp))){
      
      if (plotDF_temp$ArrowEnd[a] == "first"){
        
        Xstart <- x1[a]-offset*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                                 abs(y1[a]-y2[a])))
        Xend <- x2[a]
        X1 <- x1[a]
        X2 <- Xstart
        Ystart <- -1*(y1[a]-offset*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                     abs(y1[a]-y2[a]))))
        Yend <- -y2[a]
        Y1 <- -y1[a]
        Y2 <- Ystart
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, X1, X2,
            Ystart, Yend, Y1, Y2
          )
        )
      }
      
      if (plotDF_temp$ArrowEnd[a] == "last"){
        
        Xstart <- x1[a]
        Xend <- x2[a]-offset*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                               abs(y1[a]-y2[a])))
        X1 <- Xend
        X2 <- x2[a]
        Ystart <- -y1[a]
        Yend <- -1*(y2[a]-offset*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                   abs(y1[a]-y2[a]))))
        Y1 <- Yend
        Y2 <- -y2[a]
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, X1, X2,
            Ystart, Yend, Y1, Y2
          )
        )
        
      }
    }
    
    # Draw edges
    graphics::arrows(x0 = plotDF$Xstart, x1 = plotDF$Xend,
                     y0 = plotDF$Ystart, y1 = plotDF$Yend, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    shape::Arrows(x0 = plotDF$X1, x1 = plotDF$X2,
                  y0 = plotDF$Y1, y1 = plotDF$Y2, 
                  code = ifelse(plotDF_temp$ArrowEnd == "first", 1, 2), 
                  arr.type = "simple",
                  arr.length = 0.3,
                  arr.width = 0.3,
                  col = plotDF_temp$Color,
                  lcol = plotDF_temp$Color, 
                  arr.col = plotDF_temp$Color,  
                  lwd = plotDF_temp$LineThickness,
                  arr.adj = 1)
  }
  
  #========================================================================#
  # Filled white arrow (`mim-stimulation`)
  #========================================================================#
  
  if (sum(df$ArrowType == "mim-stimulation")>0){
    
    
    plotDF_temp <- df[df$ArrowType == "mim-stimulation",]
    
    # We will plot the main body of the edge separately from the
    # arrow head, because the arrow head looks weird when the line is dashed.
    
    # Collect X and Y coordinates in separate vectors
    x1 <- plotDF_temp$X1
    x2 <- plotDF_temp$X2
    y1 <- plotDF_temp$Y1
    y2 <- plotDF_temp$Y2
    
    # Offset is the distance at which the arrow head should be attached to the
    # main body of the arrow
    offset <- 5
    
    plotDF <- NULL
    for (a in seq_len(nrow(plotDF_temp))){
      
      if (plotDF_temp$ArrowEnd[a] == "first"){
        
        Xstart <- x1[a]-offset*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                                 abs(y1[a]-y2[a])))
        Xend <- x2[a]
        X1 <- x1[a]
        X2 <- Xstart
        Ystart <- -1*(y1[a]-offset*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                     abs(y1[a]-y2[a]))))
        Yend <- -y2[a]
        Y1 <- -y1[a]
        Y2 <- Ystart
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, X1, X2,
            Ystart, Yend, Y1, Y2
          )
        )
      }
      
      if (plotDF_temp$ArrowEnd[a] == "last"){
        
        Xstart <- x1[a]
        Xend <- x2[a]-offset*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                               abs(y1[a]-y2[a])))
        X1 <- Xend
        X2 <- x2[a]
        Ystart <- -y1[a]
        Yend <- -1*(y2[a]-offset*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                   abs(y1[a]-y2[a]))))
        Y1 <- Yend
        Y2 <- -y2[a]
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, X1, X2,
            Ystart, Yend, Y1, Y2
          )
        )
        
      }
    }
    
    # Draw edges
    graphics::arrows(x0 = plotDF$Xstart, x1 = plotDF$Xend,
                     y0 = plotDF$Ystart, y1 = plotDF$Yend, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    shape::Arrows(x0 = plotDF$X1, x1 = plotDF$X2,
                  y0 = plotDF$Y1, y1 = plotDF$Y2, 
                  code = ifelse(plotDF_temp$ArrowEnd == "first", 1, 2), 
                  arr.type = "triangle",
                  arr.length = 0.3,
                  arr.width = 0.3,
                  col = plotDF_temp$Color,
                  lcol = plotDF_temp$Color, 
                  arr.col = "white", 
                  lwd = plotDF_temp$LineThickness,
                  arr.adj = 1)
  }
  
  
  #========================================================================#
  # T-bar (`mim-inhibition`, `T-bar`)
  #========================================================================#
  
  if (sum(df$ArrowType == "mim-inhibition" |
          df$ArrowType == "TBar") > 0){
    
    plotDF_temp <- df[df$ArrowType == "mim-inhibition" |
                        df$ArrowType == "TBar",]
    # We will plot the main body of the edge separately from the
    # arrow head, because the arrow head looks weird when the line is dashed.
    
    # Collect X and Y coordinates in separate vectors
    x1 <- plotDF_temp$X1
    x2 <- plotDF_temp$X2
    y1 <- plotDF_temp$Y1
    y2 <- plotDF_temp$Y2
    
    # Offset is the distance at which the arrow head should be attached to the
    # main body of the arrow
    offset <- 5
    
    # Gap is the distance that should be present between the end of the arrow
    # and the node
    gap <- 10
    
    plotDF <- NULL
    for (a in seq_len(nrow(plotDF_temp))){
      
      if (plotDF_temp$ArrowEnd[a] == "first"){
        
        Xstart <- x1[a]-(offset+gap)*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                                       abs(y1[a]-y2[a])))
        Xend <- x2[a]
        X1 <- x1[a]-(gap)*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                            abs(y1[a]-y2[a])))
        X2 <- Xstart
        Ystart <- -1*(y1[a]-(offset+gap)*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                           abs(y1[a]-y2[a]))))
        Yend <- -y2[a]
        Y1 <- -1*(y1[a]-(gap)*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                abs(y1[a]-y2[a]))))
        Y2 <- Ystart
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, X1, X2,
            Ystart, Yend, Y1, Y2
          )
        )
      }
      
      if (plotDF_temp$ArrowEnd[a] == "last"){
        
        Xstart <- x1[a]
        Xend <- x2[a]-(offset+gap)*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                                     abs(y1[a]-y2[a])))
        X1 <- Xend
        X2 <- x2[a]-(gap)*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                            abs(y1[a]-y2[a])))
        Ystart <- -y1[a]
        Yend <- -1*(y2[a]-(offset+gap)*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                         abs(y1[a]-y2[a]))))
        Y1 <- Yend
        Y2 <- -1*(y2[a]-(gap)*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                abs(y1[a]-y2[a]))))
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, X1, X2,
            Ystart, Yend, Y1, Y2
          )
        )
        
      }
    }
    
    # Draw edges
    graphics::arrows(x0 = plotDF$Xstart, x1 = plotDF$Xend,
                     y0 = plotDF$Ystart, y1 = plotDF$Yend, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    shape::Arrows(x0 = plotDF$X1, x1 = plotDF$X2,
                  y0 = plotDF$Y1, y1 = plotDF$Y2, 
                  code = ifelse(plotDF_temp$ArrowEnd == "first", 1, 2), 
                  arr.width = 0.4,
                  arr.length = 0.5,
                  arr.type = "T",
                  lcol = plotDF_temp$Color, 
                  arr.col = plotDF_temp$Color, 
                  col = plotDF_temp$Color,
                  lwd = plotDF_temp$LineThickness,
                  arr.adj = 1)
  }
  
  #========================================================================#
  # Filled white circle (`mim-catalysis`)
  #========================================================================#
  
  if (sum(df$ArrowType == "mim-catalysis") > 0){
    
    plotDF_temp <- df[df$ArrowType == "mim-catalysis",]
    
    # We will plot the main body of the edge separately from the
    # arrow head, because the arrow head looks weird when the line is dashed.
    
    # Collect X and Y coordinates in separate vectors
    x1 <- plotDF_temp$X1
    x2 <- plotDF_temp$X2
    y1 <- plotDF_temp$Y1
    y2 <- plotDF_temp$Y2
    
    # Offset is the distance at which the arrow head should be attached to 
    # the main body of the arrow
    offset <- 5
    
    plotDF <- NULL
    for (a in seq_len(nrow(plotDF_temp))){
      
      if (plotDF_temp$ArrowEnd[a] == "first"){
        Xstart <- x1[a]-offset*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                                 abs(y1[a]-y2[a])))
        Xend <- x2[a]
        Xpoint <- Xstart
        
        Ystart <- -1*(y1[a]-offset*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                     abs(y1[a]-y2[a]))))
        Yend <- -y2[a]
        Ypoint <- Ystart
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, Xpoint,
            Ystart, Yend, Ypoint
          )
        )
        
      }
      if (plotDF_temp$ArrowEnd[a] == "last"){
        Xstart <- x1[a]
        Xend <- x2[a]-offset*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                               abs(y1[a]-y2[a])))
        Xpoint <- Xend
        
        Ystart <- -y1[a]
        Yend <- -1*(y2[a]-offset*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                   abs(y1[a]-y2[a]))))
        Ypoint <- Yend
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, Xpoint,
            Ystart, Yend, Ypoint
          )
        )
        
      }
    }
    
    # Draw edges
    graphics::arrows(x0 = plotDF$Xstart, x1 = plotDF$Xend,
                     y0 = plotDF$Ystart, y1 = plotDF$Yend, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    graphics::points(x = plotDF$Xpoint,
                     y = plotDF$Ypoint,
                     pch = 21,
                     cex = 2,
                     bg = "white",
                     col = plotDF_temp$Color[1])
  }
  
  #========================================================================#
  # Filled white square (`min-covalent-bond`)
  #========================================================================#
  
  if (sum(df$ArrowType == "min-covalent-bond") > 0){
    
    plotDF_temp <- df[df$ArrowType == "mim-covalent-bond",]
    
    # We will plot the main body of the edge separately from the
    # arrow head, because the arrow head looks weird when the line is dashed.
    
    # Collect X and Y coordinates in separate vectors
    x1 <- plotDF_temp$X1
    x2 <- plotDF_temp$X2
    y1 <- plotDF_temp$Y1
    y2 <- plotDF_temp$Y2
    
    # Offset is the distance at which the arrow head should be attached to 
    # the main body of the arrow
    offset <- 5
    
    plotDF <- NULL
    for (a in seq_len(nrow(plotDF_temp))){
      
      if (plotDF_temp$ArrowEnd[a] == "first"){
        Xstart <- x1[a]-offset*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                                 abs(y1[a]-y2[a])))
        Xend <- x2[a]
        Xpoint <- Xstart
        
        Ystart <- -1*(y1[a]-offset*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                     abs(y1[a]-y2[a]))))
        Yend <- -y2[a]
        Ypoint <- Ystart
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, Xpoint,
            Ystart, Yend, Ypoint
          )
        )
        
      }
      if (plotDF_temp$ArrowEnd[a] == "last"){
        Xstart <- x1[a]
        Xend <- x2[a]-offset*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                               abs(y1[a]-y2[a])))
        Xpoint <- Xend
        
        Ystart <- -y1[a]
        Yend <- -1*(y2[a]-offset*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) +
                                                   abs(y1[a]-y2[a]))))
        Ypoint <- Yend
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, Xpoint,
            Ystart, Yend, Ypoint
          )
        )
        
      }
    }
    
    # Draw edges
    graphics::arrows(x0 = plotDF$Xstart, x1 = plotDF$Xend,
                     y0 = plotDF$Ystart, y1 = plotDF$Yend, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    graphics::points(x = plotDF$Xpoint,
                     y = plotDF$Ypoint,
                     pch = 22,
                     cex = 2,
                     bg = "white",
                     col = plotDF_temp$Color[1])
    
  }
  
  #========================================================================#
  #  Straight line with gap at the end (`mim-gap`)
  #========================================================================#
  
  if (sum(df$ArrowType == "mim-gap") > 0){
    
    plotDF_temp <- df[df$ArrowType == "mim-gap",]
    
    # We will plot the main body of the edge separately from the 
    # arrow head, because the arrow head looks weird when the line is dashed.
    
    # Collect X and Y coordinates in separate vectors
    x1 <- plotDF_temp$X1
    x2 <- plotDF_temp$X2
    y1 <- plotDF_temp$Y1
    y2 <- plotDF_temp$Y2
    
    # Gap is the distance that should be present between the end of the arrow 
    # and the node
    gap <- 10
    
    plotDF <- NULL
    for (a in seq_len(nrow(plotDF_temp))){
      
      if (plotDF_temp$ArrowEnd[a] == "first"){
        Xstart <- x1[a]-gap*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                              abs(y1[a]-y2[a])))
        Xend <- x2[a]
        
        Ystart <- -1*(y1[a]-gap*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                  abs(y1[a]-y2[a]))))
        Yend <- -y2[a]
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend,
            Ystart, Yend
          )
        )
        
      }
      if (plotDF_temp$ArrowEnd[a] == "last"){
        Xstart <- x1[a]
        Xend <- x2[a]-gap*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                            abs(y1[a]-y2[a])))
        
        Ystart <- -y1[a]
        Yend <- -1*(y2[a]-gap*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                abs(y1[a]-y2[a]))))
        
        plotDF <- rbind.data.frame(
          plotDF,
          data.frame(
            Xstart, Xend, 
            Ystart, Yend
          )
        )
        
      }
    }
    
    # Draw edges
    graphics::arrows(x0 = plotDF$Xstart, x1 = plotDF$Xend,
                     y0 = plotDF$Ystart, y1 = plotDF$Yend, 
                     col = plotDF_temp$Color, code = 0,
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness
    )
  }
  
  #========================================================================#
  # `mim-cleavage`
  #========================================================================#
  
  
  if (sum(df$ArrowType == "mim-cleavage")> 0){
    
    plotDF_temp <- df[df$ArrowType == "mim-cleavage",]
    
    # We will plot the main body of the edge separately from the 
    # arrow head, because the arrow head looks weird when the line is dashed.
    
    # Collect X and Y coordinates in separate vectors
    x1 <- plotDF_temp$X1
    x2 <- plotDF_temp$X2
    y1 <- plotDF_temp$Y1
    y2 <- plotDF_temp$Y2
    
    # Offset is the distance from the end node to the start of the arrow head
    offset <- 15
    
    # Deviation is the length of the orthogonal distance of the arrow head
    deviation <- 12
    
    
    plotDF_main <- NULL
    plotDF_orth <- NULL
    plotDF_diag <- NULL
    for (a in seq_len(nrow(plotDF_temp))){
      
      if (plotDF_temp$ArrowEnd[a] == "first"){
        
        # Get main part of line
        Xstart <- x1[a]-offset*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                                 abs(y1[a]-y2[a])))
        Xend <- x2[a]
        Ystart <- -1*(y1[a]-offset*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                     abs(y1[a]-y2[a]))))
        Yend <- -y2[a]
        
        plotDF_main <- rbind.data.frame(
          plotDF_main,
          data.frame(
            X1 = Xstart, 
            X2 = Xend,
            Y1 = Ystart, 
            Y2 = Yend
          )
        )
        
        # Get orthogonal part of the line
        rotated_coords <- t(
          .rotation_matrix(-0.5*pi) %*% 
            c(
              deviation*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + abs(y1[a]-y2[a]))),
              -deviation*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + abs(y1[a]-y2[a])))
            )
        )
        
        Xorth_end <- rotated_coords[1,1] + Xstart
        Yorth_end <- rotated_coords[1,2] + Ystart
        
        plotDF_orth <- rbind.data.frame(
          plotDF_orth,
          data.frame(
            X1 = Xstart, 
            X2 = Xorth_end,
            Y1 = Ystart, 
            Y2 = Yorth_end
          )
        )
        
        # Get diagonal part of the line
        Xdiag_end <- x1[a]
        Ydiag_end <- -1*y1[a]
        
        plotDF_diag <- rbind.data.frame(
          plotDF_diag,
          data.frame(
            X1 = Xorth_end, 
            X2 = Xdiag_end,
            Y1 = Yorth_end, 
            Y2 = Ydiag_end
          )
        )
      }
      if (plotDF_temp$ArrowEnd[a] == "last"){
        
        # Get main part of line
        Xstart <- x1[a]
        Xend <- x2[a]-offset*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                               abs(y1[a]-y2[a])))
        Ystart <- -y1[a]
        Yend <- -1*(y2[a]-offset*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                   abs(y1[a]-y2[a]))))
        
        plotDF_main <- rbind.data.frame(
          plotDF_main,
          data.frame(
            X1 = Xstart, 
            X2 = Xend,
            Y1 = Ystart, 
            Y2 = Yend
          )
        )
        
        # Get orthogonal part of the line
        rotated_coords <- t(
          .rotation_matrix(-0.5*pi) %*% 
            c(
              deviation*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + abs(y1[a]-y2[a]))),
              -deviation*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + abs(y1[a]-y2[a])))
            )
        )
        Xorth_end <- rotated_coords[1,1] + Xend
        Yorth_end <- rotated_coords[1,2] + Yend
        
        plotDF_orth <- rbind.data.frame(
          plotDF_orth,
          data.frame(
            X1 = Xend, 
            X2 = Xorth_end,
            Y1 = Yend, 
            Y2 = Yorth_end
          )
        )
        
        # Get diagonal part of the line
        Xdiag_end <- x2[a]
        Ydiag_end <- -1*y2[a]
        
        plotDF_diag <- rbind.data.frame(
          plotDF_diag,
          data.frame(
            X1 = Xorth_end, 
            X2 = Xdiag_end,
            Y1 = Yorth_end, 
            Y2 = Ydiag_end
          )
        )
      }
    }
    
    # Draw edges
    graphics::arrows(x0 = plotDF_main$X1, x1 = plotDF_main$X2,
                     y0 = plotDF_main$Y1, y1 = plotDF_main$Y2, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    graphics::arrows(x0 = plotDF_orth$X1, x1 = plotDF_orth$X2,
                     y0 = plotDF_orth$Y1, y1 = plotDF_orth$Y2, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    graphics::arrows(x0 = plotDF_diag$X1, x1 = plotDF_diag$X2,
                     y0 = plotDF_diag$Y1, y1 = plotDF_diag$Y2, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
  }
  
  #========================================================================#
  # `mim-transcription-translation`
  #========================================================================#
  
  if (sum(df$ArrowType == "mim-transcription-translation")> 0){
    
    plotDF_temp <- df[df$ArrowType == "mim-transcription-translation",]
    x1 <- plotDF_temp$X1
    x2 <- plotDF_temp$X2
    y1 <- plotDF_temp$Y1
    y2 <- plotDF_temp$Y2
    
    # Gap is the distance between node and main line
    gap <- 15
    
    # extend is how much the main line should extend beyond the gap
    extend <- 5
    
    # Deviation is the length of the orthogonal distance of the arrow head
    deviation <- 10
    
    plotDF_main <- NULL
    plotDF_orth <- NULL
    plotDF_end <- NULL
    for (a in seq_len(nrow(plotDF_temp))){
      
      if (plotDF_temp$ArrowEnd[a] == "first"){
        
        # Get main part of line
        Xstart <- x1[a]-(gap-extend)*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                                       abs(y1[a]-y2[a])))
        Xend <- x2[a]
        Ystart <- -1*(y1[a]-(gap-extend)*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                           abs(y1[a]-y2[a]))))
        Yend <- -y2[a]
        
        plotDF_main <- rbind.data.frame(
          plotDF_main,
          data.frame(
            X1 = Xstart, 
            X2 = Xend,
            Y1 = Ystart, 
            Y2 = Yend
          )
        )
        
        # Get orthogonal part of the line
        rotated_coords <- t(
          .rotation_matrix(-0.5*pi) %*% 
            c(
              deviation*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + abs(y1[a]-y2[a]))),
              -deviation*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + abs(y1[a]-y2[a])))
            )
        )
        
        Xstart <- x1[a]-(gap)*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + 
                                                abs(y1[a]-y2[a])))
        Ystart <- -1*(y1[a]-(gap)*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + 
                                                    abs(y1[a]-y2[a]))))
        Xorth_end <- Xstart - rotated_coords[1,1]
        Yorth_end <- Ystart - rotated_coords[1,2]
        
        plotDF_orth <- rbind.data.frame(
          plotDF_orth,
          data.frame(
            X1 = Xstart, 
            X2 = Xorth_end,
            Y1 = Ystart, 
            Y2 = Yorth_end
          )
        )
        
        # Get end part of the line
        Xend_end <- x1[a] - rotated_coords[1,1]
        Yend_end <- -1*y1[a] - rotated_coords[1,2]
        
        plotDF_end <- rbind.data.frame(
          plotDF_end,
          data.frame(
            X1 = Xorth_end, 
            X2 = Xend_end,
            Y1 = Yorth_end, 
            Y2 = Yend_end
          )
        )
        
      }
      if (plotDF_temp$ArrowEnd[a] == "last"){
        
        # Get main part of line
        Xstart <- x1[a]
        Xend <- x2[a]-(gap-extend)*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                                     abs(y1[a]-y2[a])))
        Ystart <- -y1[a]
        Yend <- -1*(y2[a]-(gap-extend)*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                         abs(y1[a]-y2[a]))))
        
        plotDF_main <- rbind.data.frame(
          plotDF_main,
          data.frame(
            X1 = Xstart, 
            X2 = Xend,
            Y1 = Ystart, 
            Y2 = Yend
          )
        )
        
        # Get orthogonal part of the line
        rotated_coords <- t(
          .rotation_matrix(-0.5*pi) %*% 
            c(
              deviation*((x1[a]-x2[a])/(abs(x1[a]-x2[a]) + abs(y1[a]-y2[a]))),
              -deviation*((y1[a]-y2[a])/(abs(x1[a]-x2[a]) + abs(y1[a]-y2[a])))
            )
        )
        
        Xstart <- x2[a]-(gap)*((x2[a]-x1[a])/(abs(x1[a]-x2[a]) + 
                                                abs(y1[a]-y2[a])))
        Ystart <- -1*(y2[a]-(gap)*((y2[a]-y1[a])/(abs(x1[a]-x2[a]) + 
                                                    abs(y1[a]-y2[a]))))
        Xorth_end <- Xstart + rotated_coords[1,1]
        Yorth_end <- Ystart + rotated_coords[1,2]
        
        plotDF_orth <- rbind.data.frame(
          plotDF_orth,
          data.frame(
            X1 = Xstart, 
            X2 = Xorth_end,
            Y1 = Ystart, 
            Y2 = Yorth_end
          )
        )
        
        # Get end part of the line
        Xend_end <- x2[a] + rotated_coords[1,1]
        Yend_end <- -1*y2[a] + rotated_coords[1,2]
        
        plotDF_end <- rbind.data.frame(
          plotDF_end,
          data.frame(
            X1 = Xorth_end, 
            X2 = Xend_end,
            Y1 = Yorth_end, 
            Y2 = Yend_end
          )
        )
      }
    }
    
    # Draw edges
    graphics::arrows(x0 = plotDF_main$X1, x1 = plotDF_main$X2,
                     y0 = plotDF_main$Y1, y1 = plotDF_main$Y2, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    graphics::arrows(x0 = plotDF_orth$X1, x1 = plotDF_orth$X2,
                     y0 = plotDF_orth$Y1, y1 = plotDF_orth$Y2, 
                     length = 0, 
                     col = plotDF_temp$Color, 
                     lty = plotDF_temp$LineStyle, 
                     lwd = plotDF_temp$LineThickness)
    shape::Arrows(x0 = plotDF_end$X1, x1 = plotDF_end$X2,
                  y0 = plotDF_end$Y1, y1 = plotDF_end$Y2, 
                  code = 2, 
                  arr.type = "triangle",
                  arr.length = 0.2, 
                  arr.width = 0.2,
                  col = plotDF_temp$Color,
                  lcol = plotDF_temp$Color, 
                  arr.col = "white", 
                  lwd = plotDF_temp$LineThickness,
                  arr.adj = 1)
    
  }
}

