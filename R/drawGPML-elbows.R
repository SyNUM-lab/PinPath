# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting elbow edges
#'
#' @description This function makes a data frame for plotting elbow edges.
#' @param df A data frame with information about elbow edges.
#' @return A data frame for plotting elbow edges.

.prepareElbow <- function(df){
  
  # For each row (i.e., elbow edge), we need to split the elbow edge into
  # Multiple straight lines for plotting.
  
  plotDF <- NULL
  for (i in 1:nrow(df)){
    
    # Set relative X and Y positions to -1, 0, or 1. 
    df$Xrel1[i] <- ifelse(abs(df$Xrel1[i]) != 1,
                          0, df$Xrel1[i])
    df$Xrel2[i] <- ifelse(abs(df$Xrel2[i]) != 1,
                          0, df$Xrel2[i])
    df$Yrel1[i] <- ifelse(abs(df$Yrel1[i]) != 1,
                          0, df$Yrel1[i])
    df$Yrel2[i] <- ifelse(abs(df$Yrel2[i]) != 1,
                          0, df$Yrel2[i])
    
    # There are two types of elbow edges:
    # 1) Elbow edges that follow the default path.
    # 2) Elbow edges that follow a custom path set by the GPML creator.
    
    #==========================================================================#
    # Custom edges
    #==========================================================================#
    
    if (df$Force[i]){
      
      #------------------------------------------------------------------------#
      # If only the starting point is in the X direction
      #------------------------------------------------------------------------#
      if (abs(df$Xrel1[i]) == 1){
        
        xs <- df$X1[i]
        ys <- df$Y1[i]
        
        # Move first in X direction
        xi <- df$X2[i]
        yi <- df$Y1[i]
        
        xe <- df$X2[i]
        ye <- df$Y2[i]
        
      }
      
      
      #------------------------------------------------------------------------#
      # If only the end point is in the X direction
      #------------------------------------------------------------------------#
      if (abs(df$Xrel2[i]) == 1){
        
        xs <- df$X1[i]
        ys <- df$Y1[i]
        
        # Move first in Y direction
        xi <- df$X1[i]
        yi <- df$Y2[i]
        
        xe <- df$X2[i]
        ye <- df$Y2[i]
        
      }
      
      #------------------------------------------------------------------------#
      # If neither/both starting point and end point are in the X direction
      #------------------------------------------------------------------------#
      if ((abs(df$Xrel2[i]) != 1) & (abs(df$Xrel1[i]) != 1)){
        
        xs <- df$X1[i]
        ys <- df$Y1[i]
        
        # Move first in Y direction
        xi <- df$X1[i]
        yi <- df$Y2[i]
        
        xe <- df$X2[i]
        ye <- df$Y2[i]
        
      }
      
      # Combine the straight edges of each elbow edge into a temporary data frame
      if (df$ArrowEnd[i] == "last"){
        temp <- data.frame(X1 = c(xs,xi),
                           Y1 = c(ys,yi),
                           GraphRef1 = df$GraphRef1[i],
                           Xrel1 = df$Xrel1[i],
                           Yrel1 = df$Yrel1[i],
                           ArrowHead1 = df$ArrowHead1[i],
                           X2 = c(xi, xe),
                           Y2 = c(yi, ye),
                           GraphRef2 = df$GraphRef2[i],
                           Xrel2 = df$Xrel2[i],
                           Yrel2 = df$Yrel2[i],
                           ArrowHead2 = df$ArrowHead2[i],
                           LineStyle = df$LineStyle[i],
                           Color = df$Color[i],
                           ConnectorType = df$ConnectorType[i],
                           LineThickness = df$LineThickness[i],
                           Force = df$Force[i],
                           ArrowEnd = c("none", "last"),
                           ArrowType = c("none", df$ArrowType[i]),
                           GraphId = df$GraphId[i])
      }
      if (df$ArrowEnd[i] == "first"){
        temp <- data.frame(X1 = c(xs,xi),
                           Y1 = c(ys,yi),
                           GraphRef1 = df$GraphRef1[i],
                           Xrel1 = df$Xrel1[i],
                           Yrel1 = df$Yrel1[i],
                           ArrowHead1 = df$ArrowHead1[i],
                           X2 = c(xi, xe),
                           Y2 = c(yi, ye),
                           GraphRef2 = df$GraphRef2[i],
                           Xrel2 = df$Xrel2[i],
                           Yrel2 = df$Yrel2[i],
                           ArrowHead2 = df$ArrowHead2[i],
                           LineStyle = df$LineStyle[i],
                           Color = df$Color[i],
                           ConnectorType = df$ConnectorType[i],
                           LineThickness = df$LineThickness[i],
                           Force = df$Force[i],
                           ArrowEnd = c("first", "none"),
                           ArrowType = c(df$ArrowType[i], "none"),
                           GraphId = df$GraphId[i])
      }
      if (df$ArrowEnd[i] == "none"){
        temp <- data.frame(X1 = c(xs,xi),
                           Y1 = c(ys,yi),
                           GraphRef1 = df$GraphRef1[i],
                           Xrel1 = df$Xrel1[i],
                           Yrel1 = df$Yrel1[i],
                           ArrowHead1 = df$ArrowHead1[i],
                           X2 = c(xi, xe),
                           Y2 = c(yi, ye),
                           GraphRef2 = df$GraphRef2[i],
                           Xrel2 = df$Xrel2[i],
                           Yrel2 = df$Yrel2[i],
                           ArrowHead2 = df$ArrowHead2[i],
                           LineStyle = df$LineStyle[i],
                           Color = df$Color[i],
                           ConnectorType = df$ConnectorType[i],
                           LineThickness = df$LineThickness[i],
                           Force = df$Force[i],
                           ArrowEnd = "none",
                           ArrowType = "none",
                           GraphId = df$GraphId[i])
      }
      
      
      
      #==========================================================================#
      # Default edges
      #==========================================================================# 
    } else{
      padding <- 17 # Default distance an edge moves away from a node
      minMove <- 5  # Minimum required movement of an edge
      
      
      # Add padding to starting and end point
      xs1 <- df$X1[i]
      xs2 <- df$X1[i] + ifelse(abs(df$Xrel1[i]) == 1, 
                               padding*sign(df$Xrel1[i]),0)
      
      xe1 <- df$X2[i]
      xe2 <- df$X2[i] + ifelse(abs(df$Xrel2[i]) == 1, 
                               padding*sign(df$Xrel2[i]),0)
      
      ys1 <- df$Y1[i]
      ys2 <- df$Y1[i] + ifelse(abs(df$Yrel1[i]) == 1, 
                               padding*sign(df$Yrel1[i]),0)
      
      ye1 <- df$Y2[i]
      ye2 <- df$Y2[i] + ifelse(abs(df$Yrel2[i]) == 1, 
                               padding*sign(df$Yrel2[i]),0)
      
      
      #------------------------------------------------------------------------#
      # If both attachment points are in Y direction
      #------------------------------------------------------------------------#
      if ((ys1-ys2 != 0) & (ye1-ye2 != 0)){
        
        # Both ends cannot move in y-direction
        if ((sign(ys1-ys2) != sign(ys2-ye2)) &
            sign(ye1-ye2) != sign(ye2-ys2)){
          
          # Keep y the same
          yi1 <- ys2
          
          # Move halfway in x-direction
          xi1 <- xs2 + 0.5*ifelse(0.5*(xe2-xs2)>minMove,
                                  0.5*(xe2-xs2), minMove)
          
          # Keep y the same
          yi2 <- ye2
          
          # move halfway in x-direction
          xi2 <- xi1
          
          
        }
        
        # Both ends can move in y-direction
        if ((sign(ys1-ys2) == sign(ys2-ye2)) &
            sign(ye1-ye2) == sign(ye2-ys2)){
          
          # Keep y the same
          yi1 <- ys2
          
          # Move in x-direction
          xi1 <- xe2
          
          # Move in y-direction
          yi2 <- ys2
          
          # Keep x the same
          xi2 <- xe2
          
          
        }
        
        # Only end point can move in y direction
        if ((sign(ys1-ys2) != sign(ys2-ye2)) &
            sign(ye1-ye2) == sign(ye2-ys2)){
          
          # Keep y the same
          yi1 <- ys2
          
          # Move in x direction
          xi1 <- xe2
          
          # Move in y direction
          yi2 <- ys2
          
          # Keep x the same
          xi2 <- xe2
          
        }
        
        # Only start point can move in y direction
        if ((sign(ys1-ys2) == sign(ys2-ye2)) &
            sign(ye1-ye2) != sign(ye2-ys2)){
          
          # Move in y direction
          yi1 <- ye2
          
          # Keep x the same
          xi1 <- xs2
          
          
          # Keep y the same
          yi2 <- ye2
          
          # Move in x direction
          xi2 <- xs2
          
        }
      }
      
      #------------------------------------------------------------------------#
      # If both attachment points are in X direction
      #------------------------------------------------------------------------#
      if ((xs1-xs2 != 0) & (xe1-xe2 != 0)){
        
        # Both ends cannot move in x-direction
        if ((sign(xs1-xs2) != sign(xs2-xe2)) &
            sign(xe1-xe2) != sign(xe2-xs2)){
          
          # Keep x the same
          xi1 <- xs2
          
          # Move halfway in y-direction
          yi1 <- ys2 + 0.5*ifelse(0.5*(ye2-ys2)>minMove,
                                  0.5*(ye2-ys2), minMove)
          
          # Keep x the same
          xi2 <- xe2
          
          # move halfway in y-direction
          yi2 <- yi1
          
          
        }
        
        # Both ends can move in x-direction
        if ((sign(xs1-xs2) == sign(xs2-xe2)) &
            sign(xe1-xe2) == sign(xe2-xs2)){
          
          # Keep x the same
          xi1 <- xs2
          
          # Move in y-direction
          yi1 <- ye2
          
          # Move in x-direction
          xi2 <- xs2
          
          # Keep y the same
          yi2 <- ye2
          
          
        }
        
        # Only end point can move in x direction
        if ((sign(xs1-xs2) != sign(xs2-xe2)) &
            sign(xe1-xe2) == sign(xe2-xs2)){
          
          # Keep x the same
          xi1 <- xs2
          
          # Move in y direction
          yi1 <- ye2
          
          # Move in x direction
          xi2 <- xs2
          
          # Keep y the same
          yi2 <- ye2
          
        }
        
        # Only start point can move in x direction
        if ((sign(xs1-xs2) == sign(xs2-xe2)) &
            sign(xe1-xe2) != sign(xe2-xs2)){
          
          # Move in x direction
          xi1 <- xe2
          
          # Keep y the same
          yi1 <- ys2
          
          
          # Keep x the same
          xi2 <- xe2
          
          # Move in y direction
          yi2 <- ys2
          
        }
      }
      
      
      #------------------------------------------------------------------------#
      # If only starting point is in X direction
      #------------------------------------------------------------------------#
      if ((xs1-xs2 != 0) & (xe1-xe2 == 0)){
        
        # Starting point cannot move in x-direction
        if ((sign(xs1-xs2) != sign(xs2-xe2))){
          
          # Keep x the same
          xi1 <- xs2
          
          # Move in y direction
          yi1 <- ye2
          
          # Move in x direction
          xi2 <- xs2
          
          # Keep y the same
          yi2 <- ye2
          
        }
        
        # End point cannot move in y-direction
        if ((sign(ye1-ye2) != sign(ye2-ys2))){
          
          # Keep x the same
          xi1 <- xs2
          
          # Move in y direction
          yi1 <- ye2
          
          # Move in x direction
          xi2 <- xs2
          
          # Keep y the same
          yi2 <- ye2
          
        }
        
        # Both ends can move in their direction
        if ((sign(xs1-xs2) == sign(xs2-xe2)) &
            sign(ye1-ye2) == sign(ye2-ys2)){
          
          # Move in x-direction
          xi1 <- xe2
          
          # Keep y the same
          yi1 <- ys2
          
          # Keep x the same
          xi2 <- xe2
          
          # Move in y direction
          yi2 <- ys2
          
          
        }
        
      }
      
      
      #------------------------------------------------------------------------#
      # If only end point is in X direction
      #------------------------------------------------------------------------#
      if ((xs1-xs2 == 0) & (xe1-xe2 != 0)){
        
        # End point cannot move in x-direction
        if ((sign(xe1-xe2) != sign(xe2-xs2))){
          
          # Move in x direction
          xi1 <- xe2
          
          # Keep y the same
          yi1 <- ys2
          
          # Keep x the same
          xi2 <- xe2
          
          # Move in y direction
          yi2 <- ys2
          
        }
        
        # Start point cannot move in y-direction
        if ((sign(ys1-ys2) != sign(ys2-ye2))){
          
          # Move in x direction
          xi1 <- xe2
          
          # Keep y the same
          yi1 <- ys2
          
          # Keep x the same
          xi2 <- xe2
          
          # Move in y direction
          yi2 <- ys2
          
          
        }
        
        # Both ends can move in their direction
        if ((sign(ys1-ys2) == sign(ys2-ye2)) &
            sign(xe1-xe2) == sign(xe2-xs2)){
          
          # Move in y-direction
          yi1 <- ye2
          
          # Keep x the same
          xi1 <- xs2
          
          # Keep y the same
          yi2 <- ye2
          
          # Move in x direction
          xi2 <- xs2
          
          
        }
        
      }
      
      # Combine the straight edges of each elbow edge into a temporary data frame
      if (df$ArrowEnd[i] == "last"){
        temp <- data.frame(X1 = c(xs1,xs2,xi1,xi2,xe2),
                           Y1 = c(ys1,ys2,yi1,yi2,ye2),
                           GraphRef1 = df$GraphRef1[i],
                           Xrel1 = df$Xrel1[i],
                           Yrel1 = df$Yrel1[i],
                           ArrowHead1 = df$ArrowHead1[i],
                           X2 = c(xs2,xi1,xi2,xe2,xe1),
                           Y2 = c(ys2,yi1,yi2,ye2,ye1),
                           GraphRef2 = df$GraphRef2[i],
                           Xrel2 = df$Xrel2[i],
                           Yrel2 = df$Yrel2[i],
                           ArrowHead2 = df$ArrowHead2[i],
                           LineStyle = df$LineStyle[i],
                           Color = df$Color[i],
                           ConnectorType = df$ConnectorType[i],
                           LineThickness = df$LineThickness[i],
                           Force = df$Force[i],
                           ArrowEnd = c("none", "none", "none", "none", "last"),
                           ArrowType = c("none", "none", "none", "none", df$ArrowType[i]),
                           GraphId = df$GraphId[i])
      }
      if (df$ArrowEnd[i] == "first"){
        temp <- data.frame(X1 = c(xs1,xs2,xi1,xi2,xe2),
                           Y1 = c(ys1,ys2,yi1,yi2,ye2),
                           GraphRef1 = df$GraphRef1[i],
                           Xrel1 = df$Xrel1[i],
                           Yrel1 = df$Yrel1[i],
                           ArrowHead1 = df$ArrowHead1[i],
                           X2 = c(xs2,xi1,xi2,xe2,xe1),
                           Y2 = c(ys2,yi1,yi2,ye2,ye1),
                           GraphRef2 = df$GraphRef2[i],
                           Xrel2 = df$Xrel2[i],
                           Yrel2 = df$Yrel2[i],
                           ArrowHead2 = df$ArrowHead2[i],
                           LineStyle = df$LineStyle[i],
                           Color = df$Color[i],
                           ConnectorType = df$ConnectorType[i],
                           LineThickness = df$LineThickness[i],
                           Force = df$Force[i],
                           ArrowEnd = c("first", "none", "none", "none", "none"),
                           ArrowType = c(df$ArrowType[i], "none", "none", "none", "none"),
                           GraphId = df$GraphId[i])
      }
      if (df$ArrowEnd[i] == "none"){
        temp <- data.frame(X1 = c(xs1,xs2,xi1,xi2,xe2),
                           Y1 = c(ys1,ys2,yi1,yi2,ye2),
                           GraphRef1 = df$GraphRef1[i],
                           Xrel1 = df$Xrel1[i],
                           Yrel1 = df$Yrel1[i],
                           ArrowHead1 = df$ArrowHead1[i],
                           X2 = c(xs2,xi1,xi2,xe2,xe1),
                           Y2 = c(ys2,yi1,yi2,ye2,ye1),
                           GraphRef2 = df$GraphRef2[i],
                           Xrel2 = df$Xrel2[i],
                           Yrel2 = df$Yrel2[i],
                           ArrowHead2 = df$ArrowHead2[i],
                           LineStyle = df$LineStyle[i],
                           Color = df$Color[i],
                           ConnectorType = df$ConnectorType[i],
                           LineThickness = df$LineThickness[i],
                           Force = df$Force[i],
                           ArrowEnd = "none",
                           ArrowType = "none",
                           GraphId = df$GraphId[i])
      }
      if (df$ArrowEnd[i] == "both"){
        temp <- data.frame(X1 = c(xs1,xs2,xi1,xi2,xe2),
                           Y1 = c(ys1,ys2,yi1,yi2,ye2),
                           GraphRef1 = df$GraphRef1[i],
                           Xrel1 = df$Xrel1[i],
                           Yrel1 = df$Yrel1[i],
                           ArrowHead1 = df$ArrowHead1[i],
                           X2 = c(xs2,xi1,xi2,xe2,xe1),
                           Y2 = c(ys2,yi1,yi2,ye2,ye1),
                           GraphRef2 = df$GraphRef2[i],
                           Xrel2 = df$Xrel2[i],
                           Yrel2 = df$Yrel2[i],
                           ArrowHead2 = df$ArrowHead2[i],
                           LineStyle = df$LineStyle[i],
                           Color = df$Color[i],
                           ConnectorType = df$ConnectorType[i],
                           LineThickness = df$LineThickness[i],
                           Force = df$Force[i],
                           ArrowEnd = c("first", "none", "none", "none", "last"),
                           ArrowType = c(df$ArrowHead1[i], "none", "none", "none", df$ArrowHead1[i]),
                           GraphId = df$GraphId[i])
      }
    } # EO Default edges
    
    # Combine temporary data frames into a single combined data frame for plotting
    plotDF <- rbind.data.frame(plotDF, temp)
    
  }
  
  # The "broken" linetype should be "dashed" linetype to be consistent with
  # ggplot2 naming conventions
  plotDF$LineStyle[plotDF$LineStyle == "broken"] <- "dashed"
  
  return(plotDF)
}

