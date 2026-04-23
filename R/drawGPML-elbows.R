# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting elbow edges
#'
#' @description This function makes a data frame for plotting elbow edges.
#' @param df A data frame with information about elbow edges.
#' @return A data frame for plotting elbow edges.
#' @noRd

.prepareElbow <- function(df){

    # For each row (i.e., elbow edge), we need to split the elbow edge into
    # Multiple straight lines for plotting.

    plotDF <- NULL
    for (i in seq_len(nrow(df))){

        # Set relative X and Y positions to -1, 0, or 1.
        df$Xrel1[i] <- ifelse(abs(df$Xrel1[i]) != 1,0, df$Xrel1[i])
        df$Xrel2[i] <- ifelse(abs(df$Xrel2[i]) != 1,0, df$Xrel2[i])
        df$Yrel1[i] <- ifelse(abs(df$Yrel1[i]) != 1,0, df$Yrel1[i])
        df$Yrel2[i] <- ifelse(abs(df$Yrel2[i]) != 1,0, df$Yrel2[i])

        # There are two types of elbow edges:
        # 1) Elbow edges that follow the default path.
        # 2) Elbow edges that follow a custom path set by the GPML creator.


        if (df$Force[i]){
            temp <- .prepareCustomElbow(df, i)
        } else{
            temp <- .prepareDefaultElbow(df, i)
        }

        # Combine temporary data frames into a single combined data frame for
        # plotting
        plotDF <- rbind.data.frame(plotDF, temp)

    }

    # The "broken" linetype should be "dashed" linetype to be consistent with
    # naming conventions
    plotDF$LineStyle[plotDF$LineStyle == "broken"] <- "dashed"
    return(plotDF)
}


.prepareCustomElbow <- function(df, i){

    # If only the starting point is in the X direction
    if (abs(df$Xrel1[i]) == 1){
        xs <- df$X1[i]
        ys <- df$Y1[i]
        xi <- df$X2[i]
        yi <- df$Y1[i] # Move first in X direction
        xe <- df$X2[i]
        ye <- df$Y2[i]}

    # If only the end point is in the X direction
    if (abs(df$Xrel2[i]) == 1){
        xs <- df$X1[i]
        ys <- df$Y1[i]
        xi <- df$X1[i]
        yi <- df$Y2[i] # Move first in Y direction
        xe <- df$X2[i]
        ye <- df$Y2[i]}

    # If neither/both starting point and end point are in the X direction
    if ((abs(df$Xrel2[i]) != 1) & (abs(df$Xrel1[i]) != 1)){
        xs <- df$X1[i]
        ys <- df$Y1[i]
        xi <- df$X1[i]
        yi <- df$Y2[i] # Move first in Y direction
        xe <- df$X2[i]
        ye <- df$Y2[i]}

    # Combine the straight edges of each elbow edge into a temporary data frame
    temp <- .prepareCustomElbow_df(df, i, xs, xi, xe, ys, yi, ye)
    return(temp)
}

.prepareCustomElbow_df <- function(df, i, xs, xi, xe, ys, yi, ye){
    if (df$ArrowEnd[i] == "last"){
        temp <- data.frame(
            X1 = c(xs,xi), Y1 = c(ys,yi), GraphRef1 = df$GraphRef1[i],
            Xrel1 = df$Xrel1[i], Yrel1 = df$Yrel1[i],
            ArrowHead1 = df$ArrowHead1[i], X2 = c(xi, xe), Y2 = c(yi, ye),
            GraphRef2 = df$GraphRef2[i],
            Xrel2 = df$Xrel2[i], Yrel2 = df$Yrel2[i],
            ArrowHead2 = df$ArrowHead2[i], LineStyle = df$LineStyle[i],
            Color = df$Color[i], ConnectorType = df$ConnectorType[i],
            LineThickness = df$LineThickness[i], Force = df$Force[i],
            ArrowEnd = c("none", "last"),
            ArrowType = c("none", df$ArrowType[i]), GraphId = df$GraphId[i])
    }
    if (df$ArrowEnd[i] == "first"){
        temp <- data.frame(
            X1 = c(xs,xi), Y1 = c(ys,yi),GraphRef1 = df$GraphRef1[i],
            Xrel1 = df$Xrel1[i], Yrel1 = df$Yrel1[i],
            ArrowHead1 = df$ArrowHead1[i],X2 = c(xi, xe), Y2 = c(yi, ye),
            GraphRef2 = df$GraphRef2[i],
            Xrel2 = df$Xrel2[i], Yrel2 = df$Yrel2[i],
            ArrowHead2 = df$ArrowHead2[i], LineStyle = df$LineStyle[i],
            Color = df$Color[i], ConnectorType = df$ConnectorType[i],
            LineThickness = df$LineThickness[i],Force = df$Force[i],
            ArrowEnd = c("first", "none"),
            ArrowType = c(df$ArrowType[i], "none"), GraphId = df$GraphId[i])
    }
    if (df$ArrowEnd[i] == "none"){
        temp <- data.frame(
            X1 = c(xs,xi), Y1 = c(ys,yi), GraphRef1 = df$GraphRef1[i],
            Xrel1 = df$Xrel1[i],Yrel1 = df$Yrel1[i],
            ArrowHead1 = df$ArrowHead1[i], X2 = c(xi, xe),Y2 = c(yi, ye),
            GraphRef2 = df$GraphRef2[i],
            Xrel2 = df$Xrel2[i],Yrel2 = df$Yrel2[i],
            ArrowHead2 = df$ArrowHead2[i],LineStyle = df$LineStyle[i],
            Color = df$Color[i],ConnectorType = df$ConnectorType[i],
            LineThickness = df$LineThickness[i],Force = df$Force[i],
            ArrowEnd = "none",ArrowType = "none", GraphId = df$GraphId[i])
    }
    return(temp)
}

.prepareDefaultElbow <- function(df, i){
    padding <- 17 # Default distance an edge moves away from a node
    minMove <- 5  # Minimum required movement of an edge

    # Add padding to starting and end point
    xs1 <- df$X1[i]
    xs2 <- df$X1[i] + ifelse(abs(df$Xrel1[i]) == 1,padding*sign(df$Xrel1[i]),0)
    xe1 <- df$X2[i]
    xe2 <- df$X2[i] + ifelse(abs(df$Xrel2[i]) == 1,padding*sign(df$Xrel2[i]),0)
    ys1 <- df$Y1[i]
    ys2 <- df$Y1[i] + ifelse(abs(df$Yrel1[i]) == 1,padding*sign(df$Yrel1[i]),0)
    ye1 <- df$Y2[i]
    ye2 <- df$Y2[i] + ifelse(abs(df$Yrel2[i]) == 1,padding*sign(df$Yrel2[i]),0)

    # If both attachment points are in Y direction
    if ((ys1-ys2 != 0) & (ye1-ye2 != 0)){
        param <- .coords_yy(xs1,xs2,xe1,xe2,ys1,ys2,ye1,ye2,minMove)
    }
    # If both attachment points are in X direction
    if ((xs1-xs2 != 0) & (xe1-xe2 != 0)){
        param <- .coords_xx(xs1,xs2,xe1,xe2,ys1,ys2,ye1,ye2,minMove)
    }
    # If only starting point is in X direction
    if ((xs1-xs2 != 0) & (xe1-xe2 == 0)){
        param <- .coords_xy(xs1,xs2,xe1,xe2,ys1,ys2,ye1,ye2)
    }
    # If only end point is in X direction
    if ((xs1-xs2 == 0) & (xe1-xe2 != 0)){
        param <- .coords_yx(xs1,xs2,xe1,xe2,ys1,ys2,ye1,ye2)
    }
    xs1 <- param[[1]]
    xs2 <- param[[2]]
    xi1 <- param[[3]]
    xi2 <- param[[4]]
    xe1 <- param[[5]]
    xe2 <- param[[6]]
    ys1 <- param[[7]]
    ys2 <- param[[8]]
    yi1 <- param[[9]]
    yi2 <- param[[10]]
    ye1 <- param[[11]]
    ye2 <- param[[12]]

    # Combine the straight edges of each elbow edge into a temporary data frame
    temp <- .prepareDefaultElbow_df(
        df,i,xs1,xs2,xi1,xi2,xe1, xe2,ys1,ys2,yi1,yi2,ye1,ye2)
    return(temp)
}

.coords_yy <- function(xs1,xs2,xe1,xe2,ys1,ys2,ye1,ye2,minMove){
    # Both ends cannot move in y-direction
    if ((sign(ys1-ys2) != sign(ys2-ye2)) & sign(ye1-ye2) != sign(ye2-ys2)){
        yi1 <- ys2 # Keep y the same
        xi1 <- xs2 + 0.5*ifelse(
            0.5*(xe2-xs2)>minMove,
            0.5*(xe2-xs2), minMove) # Move halfway in x-direction
        yi2 <- ye2 # Keep y the same
        xi2 <- xi1 # move halfway in x-direction
    }
    # Both ends can move in y-direction
    if ((sign(ys1-ys2) == sign(ys2-ye2)) & sign(ye1-ye2) == sign(ye2-ys2)){
        yi1 <- ys2  # Keep y the same
        xi1 <- xe2 # Move in x-direction
        yi2 <- ys2 # Move in y-direction
        xi2 <- xe2 # Keep x the same
    }
    # Only end point can move in y direction
    if ((sign(ys1-ys2) != sign(ys2-ye2)) & sign(ye1-ye2) == sign(ye2-ys2)){
        yi1 <- ys2 # Keep y the same
        xi1 <- xe2 # Move in x direction
        yi2 <- ys2 # Move in y direction
        xi2 <- xe2 # Keep x the same
    }
    # Only start point can move in y direction
    if ((sign(ys1-ys2) == sign(ys2-ye2)) & sign(ye1-ye2) != sign(ye2-ys2)){
        yi1 <- ye2 # Move in y direction
        xi1 <- xs2 # Keep x the same
        yi2 <- ye2 # Keep y the same
        xi2 <- xs2 # Move in x direction
    }
    return(list(xs1,xs2,xi1,xi2,xe1, xe2,ys1,ys2,yi1,yi2,ye1,ye2))
}

.coords_xx <- function(xs1,xs2,xe1,xe2,ys1,ys2,ye1,ye2,minMove){
    # Both ends cannot move in x-direction
    if ((sign(xs1-xs2) != sign(xs2-xe2)) & sign(xe1-xe2) != sign(xe2-xs2)){
        xi1 <- xs2 # Keep x the same
        yi1 <- ys2 + 0.5*ifelse(
            0.5*(ye2-ys2)>minMove,
            0.5*(ye2-ys2), minMove) # Move halfway in y-direction
        xi2 <- xe2 # Keep x the same
        yi2 <- yi1 # Move halfway in y-direction
    }
    # Both ends can move in x-direction
    if ((sign(xs1-xs2) == sign(xs2-xe2)) & sign(xe1-xe2) == sign(xe2-xs2)){
        xi1 <- xs2 # Keep x the same
        yi1 <- ye2 # Move in y-direction
        xi2 <- xs2 # Move in x-direction
        yi2 <- ye2 # Keep y the same
    }
    # Only end point can move in x direction
    if ((sign(xs1-xs2) != sign(xs2-xe2)) & sign(xe1-xe2) == sign(xe2-xs2)){
        xi1 <- xs2  # Keep x the same
        yi1 <- ye2 # Move in y direction
        xi2 <- xs2 # Move in x direction
        yi2 <- ye2 # Keep y the same
    }
    # Only start point can move in x direction
    if ((sign(xs1-xs2) == sign(xs2-xe2)) & sign(xe1-xe2) != sign(xe2-xs2)){
        xi1 <- xe2 # Move in x direction
        yi1 <- ys2 # Keep y the same
        xi2 <- xe2 # Keep x the same
        yi2 <- ys2 # Move in y direction
    }
    return(list(xs1,xs2,xi1,xi2,xe1, xe2,ys1,ys2,yi1,yi2,ye1,ye2))
}

.coords_xy <- function(xs1,xs2,xe1,xe2,ys1,ys2,ye1,ye2){
    # Starting point cannot move in X direction
    if ((sign(xs1-xs2) != sign(xs2-xe2))){
        xi1 <- xs2 # Keep x the same
        yi1 <- ye2 # Move in y direction
        xi2 <- xs2 # Move in x direction
        yi2 <- ye2 # Keep y the same
    }
    # End point cannot move in y-direction
    if ((sign(ye1-ye2) != sign(ye2-ys2))){
        xi1 <- xs2 # Keep x the same
        yi1 <- ye2 # Move in y direction
        xi2 <- xs2 # Move in x direction
        yi2 <- ye2 # Keep y the same
    }
    # Both ends can move in their direction
    if ((sign(xs1-xs2) == sign(xs2-xe2)) &
        sign(ye1-ye2) == sign(ye2-ys2)){
        xi1 <- xe2 # Move in x-direction
        yi1 <- ys2 # Keep y the same
        xi2 <- xe2 # Keep x the same
        yi2 <- ys2 # Move in y direction
    }
    return(list(xs1,xs2,xi1,xi2,xe1, xe2,ys1,ys2,yi1,yi2,ye1,ye2))
}

.coords_yx <- function(xs1,xs2,xe1,xe2,ys1,ys2,ye1,ye2){
    # End point cannot move in x-direction
    if ((sign(xe1-xe2) != sign(xe2-xs2))){
        xi1 <- xe2 # Move in x direction
        yi1 <- ys2 # Keep y the same
        xi2 <- xe2 # Keep x the same
        yi2 <- ys2 # Move in y direction
    }
    # Start point cannot move in y-direction
    if ((sign(ys1-ys2) != sign(ys2-ye2))){
        xi1 <- xe2 # Move in x direction
        yi1 <- ys2 # Keep y the same
        xi2 <- xe2 # Keep x the same
        yi2 <- ys2 # Move in y direction
    }
    # Both ends can move in their direction
    if ((sign(ys1-ys2) == sign(ys2-ye2)) &
        sign(xe1-xe2) == sign(xe2-xs2)){
        yi1 <- ye2 # Move in y-direction
        xi1 <- xs2 # Keep x the same
        yi2 <- ye2 # Keep y the same
        xi2 <- xs2 # Move in x direction
    }
    return(list(xs1,xs2,xi1,xi2,xe1, xe2,ys1,ys2,yi1,yi2,ye1,ye2))
}

.prepareDefaultElbow_df <- function(
        df,i,xs1,xs2,xi1,xi2,xe1,xe2,ys1,ys2,yi1,yi2,ye1,ye2){

    if (df$ArrowEnd[i] == "last"){
        temp <- .prepareDefaultElbow_df_last(
            df,i,xs1,xs2,xi1,xi2,xe1,xe2,ys1,ys2,yi1,yi2,ye1,ye2)
    }
    if (df$ArrowEnd[i] == "first"){
        temp <- .prepareDefaultElbow_df_first(
            df,i,xs1,xs2,xi1,xi2,xe1,xe2,ys1,ys2,yi1,yi2,ye1,ye2)
    }

    if (df$ArrowEnd[i] == "none"){
        temp <- .prepareDefaultElbow_df_none(
            df,i,xs1,xs2,xi1,xi2,xe1,xe2,ys1,ys2,yi1,yi2,ye1,ye2)
    }
    if (df$ArrowEnd[i] == "both"){
        temp <- .prepareDefaultElbow_df_both(
            df,i,xs1,xs2,xi1,xi2,xe1,xe2,ys1,ys2,yi1,yi2,ye1,ye2)
    }
    return(temp)
}

.prepareDefaultElbow_df_last <- function(
        df,i,xs1,xs2,xi1,xi2,xe1,xe2,ys1,ys2,yi1,yi2,ye1,ye2){
    temp <- data.frame(
        X1 = c(xs1,xs2,xi1,xi2,xe2), Y1 = c(ys1,ys2,yi1,yi2,ye2),
        GraphRef1 = df$GraphRef1[i],
        Xrel1 = df$Xrel1[i], Yrel1 = df$Yrel1[i],
        ArrowHead1 = df$ArrowHead1[i],
        X2 = c(xs2,xi1,xi2,xe2,xe1), Y2 = c(ys2,yi1,yi2,ye2,ye1),
        GraphRef2 = df$GraphRef2[i],
        Xrel2 = df$Xrel2[i], Yrel2 = df$Yrel2[i],
        ArrowHead2 = df$ArrowHead2[i], LineStyle = df$LineStyle[i],
        Color = df$Color[i], ConnectorType = df$ConnectorType[i],
        LineThickness = df$LineThickness[i], Force = df$Force[i],
        ArrowEnd = c("none", "none", "none", "none", "last"),
        ArrowType = c("none", "none", "none", "none", df$ArrowType[i]),
        GraphId = df$GraphId[i])
    return(temp)
}

.prepareDefaultElbow_df_first <- function(
        df,i,xs1,xs2,xi1,xi2,xe1,xe2,ys1,ys2,yi1,yi2,ye1,ye2){
    temp <- data.frame(
        X1 = c(xs1,xs2,xi1,xi2,xe2), Y1 = c(ys1,ys2,yi1,yi2,ye2),
        GraphRef1 = df$GraphRef1[i],
        Xrel1 = df$Xrel1[i], Yrel1 = df$Yrel1[i],
        ArrowHead1 = df$ArrowHead1[i],
        X2 = c(xs2,xi1,xi2,xe2,xe1), Y2 = c(ys2,yi1,yi2,ye2,ye1),
        GraphRef2 = df$GraphRef2[i],
        Xrel2 = df$Xrel2[i], Yrel2 = df$Yrel2[i],
        ArrowHead2 = df$ArrowHead2[i], LineStyle = df$LineStyle[i],
        Color = df$Color[i], ConnectorType = df$ConnectorType[i],
        LineThickness = df$LineThickness[i], Force = df$Force[i],
        ArrowEnd = c("first", "none", "none", "none", "none"),
        ArrowType = c(df$ArrowType[i], "none", "none", "none", "none"),
        GraphId = df$GraphId[i])
    return(temp)
}

.prepareDefaultElbow_df_none <- function(
        df,i,xs1,xs2,xi1,xi2,xe1,xe2,ys1,ys2,yi1,yi2,ye1,ye2){
    temp <- data.frame(
        X1 = c(xs1,xs2,xi1,xi2,xe2), Y1 = c(ys1,ys2,yi1,yi2,ye2),
        GraphRef1 = df$GraphRef1[i],
        Xrel1 = df$Xrel1[i], Yrel1 = df$Yrel1[i],
        ArrowHead1 = df$ArrowHead1[i],
        X2 = c(xs2,xi1,xi2,xe2,xe1),Y2 = c(ys2,yi1,yi2,ye2,ye1),
        GraphRef2 = df$GraphRef2[i],
        Xrel2 = df$Xrel2[i],Yrel2 = df$Yrel2[i],
        ArrowHead2 = df$ArrowHead2[i],LineStyle = df$LineStyle[i],
        Color = df$Color[i],ConnectorType = df$ConnectorType[i],
        LineThickness = df$LineThickness[i],Force = df$Force[i],
        ArrowEnd = "none",ArrowType = "none",GraphId = df$GraphId[i])
    return(temp)
}

.prepareDefaultElbow_df_both <- function(
        df,i,xs1,xs2,xi1,xi2,xe1,xe2,ys1,ys2,yi1,yi2,ye1,ye2){
    temp <- data.frame(
        X1 = c(xs1,xs2,xi1,xi2,xe2),Y1 = c(ys1,ys2,yi1,yi2,ye2),
        GraphRef1 = df$GraphRef1[i],
        Xrel1 = df$Xrel1[i],Yrel1 = df$Yrel1[i],
        ArrowHead1 = df$ArrowHead1[i],
        X2 = c(xs2,xi1,xi2,xe2,xe1),Y2 = c(ys2,yi1,yi2,ye2,ye1),
        GraphRef2 = df$GraphRef2[i],
        Xrel2 = df$Xrel2[i],Yrel2 = df$Yrel2[i],
        ArrowHead2 = df$ArrowHead2[i],LineStyle = df$LineStyle[i],
        Color = df$Color[i],ConnectorType = df$ConnectorType[i],
        LineThickness = df$LineThickness[i],Force = df$Force[i],
        ArrowEnd = c("first", "none", "none", "none", "last"),
        ArrowType = c(
            df$ArrowHead1[i], "none", "none", "none", df$ArrowHead1[i]),
        GraphId = df$GraphId[i])
    return(temp)
}
