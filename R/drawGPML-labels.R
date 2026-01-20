# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting labels
#'
#' @description This function makes a data frame for plotting labels.
#' @param dataLabels  A GPML list filtered for labels.
#' @return A data frame for plotting labels.

.prepareLabels <- function(dataLabels){
  
  labelFUN <- function(dataLabels){
    data.frame(
      CenterX = as.numeric(dataLabels$Graphics["CenterX"]),
      CenterY = as.numeric(dataLabels$Graphics["CenterY"]),
      Width = as.numeric(dataLabels$Graphics["Width"]),
      Height = as.numeric(dataLabels$Graphics["Height"]),
      ZOrder = as.numeric(dataLabels$Graphics["ZOrder"]),
      FillColor = ifelse(is.na(as.character(dataLabels$Graphics["FillColor"])), "white",
                         paste0("#",as.character(dataLabels$Graphics["FillColor"]))),
      Alpha = 1,
      Color = ifelse(is.na(as.character(dataLabels$Graphics["Color"])), "black",
                     paste0("#",as.character(dataLabels$Graphics["Color"]))),
      Label = as.character(stringr::str_remove_all(dataLabels$.attrs["TextLabel"],"\\n+$")),
      FontSize = as.numeric(dataLabels$Graphics["FontSize"]),
      FontWeight = tolower(as.character(dataLabels$Graphics["FontWeight"])),
      FontStyle = tolower(as.character(dataLabels$Graphics["FontStyle"])),
      Valign = ifelse(as.character(dataLabels$Graphics["Valign"]) == "Middle", 0.5,
                      ifelse(as.character(dataLabels$Graphics["Valign"]) == "Top", 1,0)), 
      Align = ifelse(as.character(dataLabels$Graphics["Align"]) == "Middle", 0.5,
                     ifelse(as.character(dataLabels$Graphics["Align"]) == "Right", 1,0)), 
      ShapeType = as.character(dataLabels$Graphics["ShapeType"]),
      Rotation = 0,
      LineThickness = as.numeric(ifelse(is.na(dataLabels$Graphics["LineThickness"]),
                                        1, dataLabels$Graphics["LineThickness"]))/2,
      LineStyle = as.character(ifelse(is.na(dataLabels$Graphics["LineStyle"]),
                                      "solid", tolower(dataLabels$Graphics["LineStyle"]))),
      nLine = ifelse(sum(stringr::str_detect(unlist(dataLabels[which(names(dataLabels) == "Attribute")]), "Double")) == 0, "Single", "Double"),
      GroupRef = as.character(dataLabels$.attrs["GroupRef"]),
      GraphId = as.character(dataLabels$.attrs["GraphId"])
    )
  }
  labels_df <- do.call(rbind, lapply(dataLabels, labelFUN))
  #labels_df$Label <- iconv(labels_df$Label, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  # Change font face
  labels_df$FontFace <- "plain"
  labels_df$FontFace[(!is.na(labels_df$FontWeight) & is.na(labels_df$FontStyle))] <- labels_df$FontWeight[(!is.na(labels_df$FontWeight) & is.na(labels_df$FontStyle))]
  labels_df$FontFace[(is.na(labels_df$FontWeight) & !is.na(labels_df$FontStyle))] <- labels_df$FontStyle[(is.na(labels_df$FontWeight) & !is.na(labels_df$FontStyle))]
  labels_df$FontFace[((labels_df$FontWeight == "bold") & (labels_df$FontStyle == "italic")) |
                       ((labels_df$FontWeight == "italic") & (labels_df$FontStyle == "bold"))] <- "bold.italic"
  
  # Change line style
  labels_df$LineStyle <- ifelse(labels_df$LineStyle == "broken",
                                "dashed", "solid")
  
  return(labels_df)
}



