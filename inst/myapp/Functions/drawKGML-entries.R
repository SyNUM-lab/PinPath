# ------------------------------------------------------------------------------
#' @title Prepare data frame for plotting entries
#'
#' @description This function makes a data frame for plotting entries.
#' @param dataEntries A KGML list filtered for entries.
#' @return A data frame for plotting entries.

.prepareEntries <- function(dataEntries){
  
  entryFUN <- function(dataEntries){
    data.frame(Names = as.character(dataEntries$graphics["name"]),
               ID = strsplit(as.character(dataEntries$graphics["name"]), ",\\s*")[[1]][1],
               Database = "HGNC",
               KEGG = as.character(dataEntries$.attrs["name"]),
               GraphId1 = as.character(dataEntries$.attrs["id"]),
               GraphType = as.character(dataEntries$.attrs["type"]),
               FgColor = as.character(dataEntries$graphics["fgcolor"]),
               BgColor = as.character(dataEntries$graphics["bgcolor"]),
               NodeType = as.character(dataEntries$graphics["type"]),
               CenterX = as.numeric(dataEntries$graphics["x"]),
               CenterY = as.numeric(dataEntries$graphics["y"]),
               Width = as.numeric(dataEntries$graphics["width"]),
               Height = as.numeric(dataEntries$graphics["height"])
    )
  }
  entries_df <- do.call(rbind, lapply(dataEntries, entryFUN))
  #entries_df$ID<- iconv(entries_df$ID, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  return(entries_df)
}