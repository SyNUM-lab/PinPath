# ------------------------------------------------------------------------------
#' @title Make default colorList
#'
#' @description This function makes default list that can be used to set the
#' node colors in the pathway diagram.
#' @param ColorVar \code{vector} or \code{data.frame} for coloring the nodes
#' in the pathway. This can be for instance a \code{data.frame} with the
#' log2FCs and significance in the columns.
#' @param ColorNames (optional) \code{character} vector with names of the
#' color variables. These will be used to set the names in the legend.
#' If \code{colorNames} is NULL, the column names of the \code{colorVar}
#' \code{data.frame} will be used.
#' @return A list that can be provided to \link{drawGPML} to set the node
#' colors in the pathway diagram.
#' @details For both divergent and sequential color scales,
#' values outside the specified range (\code{minVal} to \code{maxVal})
#' are clipped and mapped to the colors corresponding to the
#' minimum (\code{minCol}) or maximum (\code{maxCol}).
#' @examples
#'
#' # Load example data
#'  lung_expr <- read.csv(system.file(
#'      "extdata","data-lung-cancer.csv", package="PinPath"),
#'      stringsAsFactors = FALSE)
#'
#' # Set significance as a binary variable
#' lung_expr$Significant <- ifelse(lung_expr$adj.P.Value < 0.05, "Yes", "No")
#'
#' # Make default color list
#' colorList <- PinPath::defaultColorList(
#' lung_expr[,c("log2FC", "Significant")])
#'
#' @export

defaultColorList <- function(ColorVar, ColorNames = NULL){

    # Convert ColorVar to data frame
    if (!is.data.frame(ColorVar)){
        ColorVar <- data.frame(Color = ColorVar)}

    # Set color names
    if ((!is.null(ColorNames)) & (ncol(ColorVar) == length(ColorNames))){
        colnames(ColorVar) <- ColorNames
    }
    if ((!is.null(ColorNames)) & (ncol(ColorVar) != length(ColorNames))){
        warning(
            "The number of color names is different than the number of
            color variables. Default color names will be used instead.")
    }

    # For each color variable, add color information
    colorList <- list()
    for (c in seq_len(ncol(ColorVar))){
        if (is.numeric(ColorVar[,c])){

            # Divergent color scale
            if ((min(ColorVar[,c], na.rm = TRUE) < 0) &
                (max(ColorVar[,c], na.rm = TRUE) > 0)){
                colorList[[c]] <- defaultDivergentScale(ColorVar, c)}

            # Sequential color scale
            if ((min(ColorVar[,c], na.rm = TRUE) >= 0) |
                (max(ColorVar[,c], na.rm = TRUE) <= 0)){
                colorList[[c]] <- defaultSequentialScale(ColorVar, c)}
        }

        # Qualitative color scale
        if (!is.numeric(ColorVar[,c])){
            colorList[[c]] <- defaultQualitativeScale(ColorVar, c)}
    }
    names(colorList) <- colnames(ColorVar)
    return(colorList)
}


defaultDivergentScale <- function(ColorVar, c){
    # Since we want to color scale to be symmetric, we set the
    # absolute min and max value to the same max absolute value
    max_absolute_value <- max(
        abs(as.numeric(stats::quantile(
            ColorVar[,c], 0.9,
            na.rm = TRUE))),
        abs(as.numeric(stats::quantile(
            ColorVar[,c], 0.1,
            na.rm = TRUE)))
    )

    outList <- list(
        ScaleName = colnames(ColorVar)[c],
        ScaleType = "Divergent",
        ColorVal = c(
            "MinVal" = -1*max_absolute_value,
            "MidVal" = 0,
            "MaxVal" = max_absolute_value),
        Color = c(
            "MinCol" = "#5754FF",
            "MidCol" = "white",
            "MaxCol" = "#FF4845"))
    return(outList)
}

defaultSequentialScale <- function(ColorVar, c){
    outList <- list(
        ScaleName = colnames(ColorVar)[c],
        ScaleType = "Sequential",
        ColorVal = c(
            "MinVal" = as.numeric(
                stats::quantile(
                    ColorVar[,c],
                    0.1,
                    na.rm = TRUE)),
            "MaxVal" = as.numeric(
                stats::quantile(
                    ColorVar[,c],
                    0.9,
                    na.rm = TRUE))),
        Color = c(
            "MinCol" = "#DADAEB",
            "MaxCol" = "#54278F"))
    return(outList)
}

defaultQualitativeScale <- function(ColorVar, c){
    if (length(unique(ColorVar[,c])) == 2){
        outList <- list(
            ScaleName = colnames(ColorVar)[c],
            ScaleType = "Qualitative",
            Color = stats::setNames(
                c("green", "yellow"),
                unique(ColorVar[,c]))
        )
    } else{
        outList <- list(
            ScaleName = colnames(ColorVar)[c],
            ScaleType = "Qualitative",
            Color = stats::setNames(
                grDevices::rainbow(length(unique(ColorVar[,c]))),
                unique(ColorVar[,c]))
        )
    }
    return(outList)
}
