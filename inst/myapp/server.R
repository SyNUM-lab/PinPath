# Increase connection size to allow for bigger data uploads
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# Start server
server <- function(input, output, session){
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  # Set options for data upload
  options(shiny.maxRequestSize=125*1024^10)
  
  # Hide panels
  hideTab("navbar", target = "panel2")
  hideTab("navbar", target = "panel3")
  
  
  
  observe({
    
    ###########################################################################
    
    # Data upload
    
    ###########################################################################
    
    output$download_example <- downloadHandler(
      filename = "statTable.csv",
      content = function(file){
        file.copy("Data/statTable.csv", file)
      }
    )
    
    # Go to information tab
    observeEvent(input$info,{
      updateNavbarPage(session, "navbar",
                       selected = "panel4")
    })
    
    
    # Make list for reactive values
    rv <- reactiveValues()
    
    # Go to next tab
    observeEvent(input$home_next, {
      
      req(input$upload_table$datapath)
      shinybusy::show_modal_spinner(text = "Reading data...",
                                    color="#2c3e50")
      rv$statTable <- as.data.frame(data.table::fread(input$upload_table$datapath))
      shinybusy::remove_modal_spinner()
      
      if (length(rv$statTable) > 0){
        
        # Show next tab
        showTab("navbar", target = "panel2")
        
        # Go next tab
        updateNavbarPage(session, "navbar",
                         selected = "panel2")
      } else{
        
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error!",
          text = "Please upload statistics table",
          type = "error")
      }
      
    })
    
    observeEvent(input$example,{
      shinybusy::show_modal_spinner(text = "Reading data...",
                                    color="#2c3e50")
      load("Data/statTable.RData")
      rv$statTable <- statTable#as.data.frame(data.table::fread("Data/topTable_RTT - WT.csv"))
      rm(statTable)
      
      shinybusy::remove_modal_spinner()
      # Show next tab
      showTab("navbar", target = "panel2")
      
      # Go next tab
      updateNavbarPage(session, "navbar",
                       selected = "panel2")
    })
    
    ###########################################################################
    
    # Gene Info
    
    ###########################################################################
    
    # Set tables to zero
    output$statTable_view <- renderDataTable(NULL)
    
    # Print expression table
    observe({
      req(rv$statTable)
      
      output$statTable_view <- DT::renderDataTable({
        return(rv$statTable)
      }, options = list(pageLength = 6,
                        dom = "tp"))
      
      output$UI_settings <- renderUI({
        tagList(
          h4(span("Gene ID column",
                  tags$span(
                    icon(
                      name = "question-circle",
                    ) 
                  ) |>
                    prompter::add_prompt(
                      message = "Which column of the statistics table contains the gene IDs?",
                      position = "right",
                      rounded = TRUE,
                      size = "large",
                      type = "info")
          )),
          selectInput(inputId = "GeneID_column",
                      label = NULL,
                      choices = colnames(rv$statTable),
                      multiple = FALSE),
          
          conditionalPanel(
            condition = "input.TypeInColumn",
            h4(span("Gene ID type column",
                    tags$span(
                      icon(
                        name = "question-circle",
                      ) 
                    ) |>
                      prompter::add_prompt(
                        message = "Which column of the statistics table contains the gene ID types?",
                        position = "right",
                        rounded = TRUE,
                        size = "large",
                        type = "info")
            )),
            selectInput(inputId = "GeneID_type_column",
                        label = NULL,
                        choices = colnames(rv$statTable),
                        multiple = FALSE),
          ),
          conditionalPanel(
            condition = "!input.TypeInColumn",
            h4(span("Gene ID type",
                    tags$span(
                      icon(
                        name = "question-circle",
                      ) 
                    ) |>
                      prompter::add_prompt(
                        message = "Which gene ID type is used?",
                        position = "right",
                        rounded = TRUE,
                        size = "large",
                        type = "info")
            )),
            selectInput(inputId = "GeneID_type",
                        label = NULL,
                        choices = c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT"),
                        multiple = FALSE,
                        selected = whichID(rv$statTable[,input$GeneID_column]))
          ),
          shinyWidgets::materialSwitch(
            inputId = "TypeInColumn",
            label = "Gene ID type in column",
            value = FALSE
          ),
          
          h4(span("Organism",
                  tags$span(
                    icon(
                      name = "question-circle",
                    ) 
                  ) |>
                    prompter::add_prompt(
                      message = "From which organism is the data? 
                      This information will be used for mapping the gene IDs to the pathways.",
                      position = "right",
                      rounded = TRUE,
                      size = "large",
                      type = "info")
          )),
          selectInput(inputId = "organism",
                      label = NULL,
                      choices = c("Homo sapiens", "Bos taurus", "Caenorhabditis elegans", 
                                  "Mus musculus", "Rattus norvegicus"),
                      multiple = FALSE),
         
          h4(span("Remove/add columns",
                  tags$span(
                    icon(
                      name = "question-circle",
                    ) 
                  ) |>
                    prompter::add_prompt(
                      message = "Click to remove (-) or add (+) columns to the 
                      uploaded statistics table.",
                      position = "right",
                      rounded = TRUE,
                      size = "large",
                      type = "info")
          )),
          actionButton("removeColumn",
                       label = NULL,
                       icon = icon("minus")),
          actionButton("addColumn",
                       label = NULL,
                       icon = icon("plus")),
          
          hr(),
          shinyWidgets::actionBttn(inputId = "details_back",
                                   label = "Back",
                                   style = "simple",
                                   color = "warning",
                                   icon = icon("arrow-left")),
          shinyWidgets::actionBttn(inputId = "details_next",
                                   label = "Next",
                                   style = "simple",
                                   color = "warning",
                                   icon = icon("arrow-right"))
          
        )
      })
    })
    
    #**************************************************************************#
    # Add  columns
    #**************************************************************************#
    
    # Make modal
    observeEvent(input$addColumn, {
      showModal(modalDialog(
        fluidPage(
          h2(strong("Add columns")),
          hr(),
          h4("New column name"),
          textInput(
            inputId = "addColumn_name",
            label = NULL,
            width = "80%"
          ),
          h4("Make rules"),
          textAreaInput(
            inputId = "addColumn_rules",
            label = NULL,
            width = "80%",
            height = "100px"
          ),
          h5(
          actionLink(inputId = "addColumn_example1", 
                     label = "Example 1"),
          strong(" | "),
          actionLink(inputId = "addColumn_example2", 
                     label = "Example 2"),
          strong(" | "),
          actionLink(inputId = "addColumn_info", 
                     label = "How to make rules?")
          ),
          br(),
          br(),
          actionButton("addColumn_ok",
                       label = "Add column",
                       icon = icon("plus")),
          actionButton(inputId = "addColumn_cancel",
                       label = "Cancel",
                       icon = icon("xmark"))
        ), easyClose = TRUE, size = "l", footer = NULL
        
      ))
    })
    
    observeEvent(input$addColumn_example1,{
      updateTextInput(
        session,
        inputId = "addColumn_name",
        label = NULL,
        value = "Significant"
      )
      
      updateTextAreaInput(
        session,
        inputId = "addColumn_rules",
        label = NULL,
        value = 'Yes: `p-value` < 0.05\nNo: `p-value` >= 0.05'
      )
    })
    
    observeEvent(input$addColumn_example2,{
      updateTextInput(
        session,
        inputId = "addColumn_name",
        label = NULL,
        value = "Change"
      )
      
      updateTextAreaInput(
        session,
        inputId = "addColumn_rules",
        label = NULL,
        value = 'Up: (`Significant` == "Yes") & (`log2FC` > 0)\nDown: (`Significant` == "Yes") & (`log2FC` < 0)\nUnchanged: `Significant` == "No"'
      )
    })
    
    
    observeEvent(input$addColumn_info,{
      removeModal()
      updateNavbarPage(session, "navbar",
                       selected = "panel4")
      
    })
    
    # Add column to statistics table
    observeEvent(input$addColumn_ok,{
      tryCatch({
        
        if (stringr::str_detect(input$addColumn_rules, ":")){
          # Set new column to NA values
          newColumn <- rep(NA, nrow(rv$statTable))
          
          # A new line indicates a new rule
          all_rules <- stringr::str_split(input$addColumn_rules, "\n")[[1]]
          
          # Add labels to new column
          for (r in 1:length(all_rules)){
            rule <- all_rules[r]
            if (nchar(stringr::str_remove_all(rule, " "))>0){
              rule_label <- stringr::str_split(rule, ":")[[1]][1]
              rule_true <- stringr::str_split(rule, ":")[[1]][2]
              
              varExtract <- stringr::str_extract_all(rule_true, "`(.*?)`")[[1]]
              for (v in 1:length(varExtract)){
                rule_true <- stringr::str_replace_all(rule_true, varExtract[[v]], paste0("rv$statTable$",varExtract[[v]]))
              }
              newColumn[eval(parse(text = rule_true))] <- rule_label
            }
          }
        }
        
        if (!stringr::str_detect(input$addColumn_rules, ":")){
          
          # A new line indicates a new rule
          all_rules <- stringr::str_split(input$addColumn_rules, "\n")[[1]]
          
          # Add labels to new column
          for (r in 1:length(all_rules)){
            rule <- all_rules[r]
            if (nchar(stringr::str_remove_all(rule, " "))>0){
              rule_true <- rule
              varExtract <- stringr::str_extract_all(rule_true, "`(.*?)`")[[1]]
              for (v in 1:length(varExtract)){
                rule_true <- stringr::str_replace_all(rule_true, varExtract[[v]], paste0("rv$statTable$",varExtract[[v]]))
              }
              newColumn <- eval(parse(text = rule_true))
            }
          }
        }
        
        # Add new column to statistics table
        eval(parse(text = paste0("rv$statTable$",input$addColumn_name, "<- newColumn")))
        
        # Remove model
        removeModal()
        
        # Show success message
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Success",
          text = "Great! A column has been added to the statistics table.",
          type = "success")
      }, error = function(cond){
        
        # Show error message
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error",
          text = "Oops...something went wrong!",
          type = "error")
      })
    })
    
    # Remove modal
    observeEvent(input$addColumn_cancel,{
      removeModal()
    })
    
    #**************************************************************************#
    # Remove columns
    #**************************************************************************#
    
    # Make modal
    observeEvent(input$removeColumn, {
      showModal(modalDialog(
        fluidPage(
          h2(strong("Remove columns")),
          hr(),
          h4("Which column(s) do you want to remove?"),
          selectInput(inputId = "removeColumn_name",
                      label = NULL,
                      choices = colnames(rv$statTable),
                      multiple = TRUE),
          actionButton("removeColumn_ok",
                       label = "Remove column(s)",
                       icon = icon("minus")),
          actionButton(inputId = "removeColumn_cancel",
                       label = "Cancel",
                       icon = icon("xmark"))
        ), easyClose = TRUE, size = "l", footer = NULL
        
      ))
    })
    
    # Add column to statistics table
    observeEvent(input$removeColumn_ok,{
      tryCatch({
        if (length(input$removeColumn_name) > 0){
          rv$statTable <- rv$statTable[,-which(colnames(rv$statTable) %in% input$removeColumn_name)]
        }
        hideTab("navbar", target = "panel3")
        rv$pathwayInfo <- NULL
        removeModal()
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Success",
          text = "Great! A column has been removed from the statistics table.",
          type = "success")
      }, error = function(cond){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error",
          text = "Oops...something went wrong!",
          type = "error")
      })
    })
    observeEvent(input$removeColumn_cancel,{
      removeModal()
    })
    
    
    # Save statistics table
    observe({
      req(rv$statTable)
      output$save_statTable <- downloadHandler(
        filename = "StatTable.csv",
        content = function(file){
          write.csv(rv$statTable, file = file,
                    row.names = FALSE)
        }
      )
    })
    
    #**************************************************************************#
    # Change tab
    #**************************************************************************#
    
    # Go to previous tab
    observeEvent(input$details_back,{
      # Show next tab
      hideTab("navbar", target = "panel2")
      
      # Go next tab
      updateNavbarPage(session, "navbar",
                       selected = "panel1")
    })
    
    # Go to next tab
    observeEvent(input$details_next, {
      
      rv$GeneID_column <- input$GeneID_column
      if (input$TypeInColumn){
        rv$GeneID_type <-  rv$statTable[,input$GeneID_type_column]
      } else{
        rv$GeneID_type <-  input$GeneID_type
        
      }
      rv$organism <- input$organism
      
      shinybusy::show_modal_spinner(text = "Loading...",
                                    color="#2c3e50")
      if ((length(rv$GeneID_column) > 0) &
          (length(rv$GeneID_type) > 0) &
          (length(rv$organism) > 0)){
        
        # Load pathway information
        load(paste0("Pathways/WP_",stringr::str_replace(rv$organism, " ", "_"),".RData"))
        rv$PathwayInfo_WP <- PathwayInfo_WP
        rv$pathwayInfo[["WikiPathways"]] <- setNames(PathwayInfo_WP$wpid,
                                                     paste0(PathwayInfo_WP$name, " (", PathwayInfo_WP$wpid, ")"))
        
        load(paste0("Pathways/KEGG_",stringr::str_replace(rv$organism, " ", "_"),".RData"))
        rv$PathwayInfo_KEGG <- PathwayInfo_KEGG
        rv$pathwayInfo[["KEGG"]] <- setNames(PathwayInfo_KEGG$keggid,
                                             paste0(PathwayInfo_KEGG$name, " (", PathwayInfo_KEGG$keggid, ")"))
        
        rm(PathwayInfo_WP)
        rm(PathwayInfo_KEGG)
        
        # Load annotation package
        pkg <- switch(rv$organism,
                      "Homo sapiens" = "org.Hs.eg.db",
                      "Bos taurus" = "org.Bt.eg.db",
                      "Caenorhabditis elegans" = "org.Ce.eg.db",
                      "Mus musculus" = "org.Mm.eg.db",
                      "Rattus norvegicus" = "org.Rn.eg.db"
        )
        if (!requireNamespace(pkg, quietly = TRUE)){
          BiocManager::install(pkg, ask = FALSE)
        }
        
        # Show next tab
        showTab("navbar", target = "panel3")
        
        # Go next tab
        updateNavbarPage(session, "navbar",
                         selected = "panel3")
        
        
      } else{
        
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error!",
          text = "Unvalid selection",
          type = "error")
      }
      shinybusy::remove_modal_spinner()
      
    })
    
    
    ###########################################################################
    
    # Make figure
    
    ###########################################################################
    
    # Pathway settings
    observe({
      req(rv$statTable)
      updateSelectizeInput(session, "color_column", 
                           choices = setdiff(colnames(rv$statTable), rv$GeneID_column), 
                           selected = colnames(rv$statTable)[2],
                           server = TRUE)
    })
    
    observe({
      req(rv$pathwayInfo)
      req(input$collection)
      
      if (input$collection != "Custom"){
        
        updateSelectizeInput(session, "pathway", 
                             choices = rv$pathwayInfo[[input$collection]], 
                             server = TRUE)
      }
    })
    
    # Pathway statistics
    rv$pathStats <- list()
    output$pathStats_view <- DT::renderDataTable(NULL)
    observe({
      req(rv$pathStats[[input$collection]])
      output$pathStats_view <- DT::renderDataTable({
        return(rv$pathStats[[input$collection]])
      },selection = list(mode = "single"),
      options = list(pageLength = 3),
      #dom = "tp"),
      rownames= FALSE)
    })
    
    # Update selectize input
    observeEvent(input$pathStats_view_rows_selected, {
      req(input$pathStats_view_rows_selected)
      
      rv$pathway <- rv$pathStats[[input$collection]]$ID[input$pathStats_view_rows_selected]
      
      updateSelectizeInput(session, "pathway", 
                           choices = rv$pathwayInfo[[input$collection]],
                           selected = rv$pathway,
                           server = TRUE)
    }, ignoreInit = TRUE)
    
    observe({
      if (input$collection == "Custom"){
        rv$pathway <- input$pathway_file$datapath
      }
    }
    )
    # update data table
    observeEvent(input$pathway, {
      req(input$pathway)
      req(input$collection)
      if (input$collection != "Custom"){
        rv$pathway <- input$pathway
      }
      
      
      
      proxy <- dataTableProxy("pathStats_view")
      selectRows(proxy, which(rv$pathStats[[input$collection]]$ID == rv$pathway))
    }, ignoreInit = TRUE)
    
    
    # Draw pathway
    observe({
      req(rv$organism)
      req(input$color_column)
      req(input$collection)
      req(rv$pathway)
      req(rv$colorList)
      req(sum(input$asNetwork))
      req(input$network_ok+1)
      file_dir <- tempdir()
      
      pkg <- switch(rv$organism,
                    "Homo sapiens" = "org.Hs.eg.db",
                    "Bos taurus" = "org.Bt.eg.db",
                    "Caenorhabditis elegans" = "org.Ce.eg.db",
                    "Mus musculus" = "org.Mm.eg.db",
                    "Rattus norvegicus" = "org.Rn.eg.db"
      )
      
      tryCatch({
        set.seed(123)
        if (length(input$color_column) == 1){
          temp <- data.frame(rv$statTable[,input$color_column])
          colnames(temp) <- input$color_column
        }else{
          temp <- rv$statTable[,input$color_column]
        }
        if (input$collection == "WikiPathways"){
          if (!input$asNetwork){
            rv$outPath <- drawGPML_app(
              infile = rWikiPathways::getPathway(rv$pathway),
              outdir = file_dir,
              outname = "pathway.svg",
              annPkg = pkg,
              inputDB = rv$GeneID_type,
              geneIDs = rv$statTable[,rv$GeneID_column],
              colorVar = temp,
              colorNames = NULL,
              colorList = rv$colorList,
              NAvalue = rv$NAvalue,
              legend = TRUE,
              nodeTable = TRUE,
              pathInfo = TRUE,
              openFile = FALSE
            )
          }
          if (input$asNetwork){
            rv$outPath <- GPML2Network(
              infile = rWikiPathways::getPathway(rv$pathway),
              outdir = file_dir,
              outname = "pathway.svg",
              annPkg = pkg,
              inputDB = rv$GeneID_type,
              geneIDs = rv$statTable[,rv$GeneID_column],
              colorVar = temp,
              colorNames = NULL,
              colorList = rv$colorList,
              NAvalue = rv$NAvalue,
              layout = input$layout,
              unconnectedNodes = input$unconnectedNodes,
              alpha = input$alpha,
              nodeSize = input$nodeSize,
              legend = TRUE,
              nodeTable = TRUE,
              pathInfo = TRUE,
              openFile = FALSE
            )
          }
        }
        
        if (input$collection == "KEGG"){
          if (!input$asNetwork){
            rv$outPath <- drawKGML_app(
              id = rv$pathway,
              outdir = file_dir,
              outname = "pathway.svg",
              annPkg = pkg,
              inputDB = rv$GeneID_type,
              geneIDs = rv$statTable[,rv$GeneID_column],
              colorVar = temp,
              colorNames = NULL,
              colorList = rv$colorList,
              NAvalue = rv$NAvalue,
              legend = TRUE,
              nodeTable = TRUE,
              pathInfo = TRUE,
              openFile = FALSE
            )
          }
          if (input$asNetwork){
            rv$outPath <- KGML2Network(
              id = rv$pathway,
              outdir = file_dir,
              outname = "pathway.svg",
              annPkg = pkg,
              inputDB = rv$GeneID_type,
              geneIDs = rv$statTable[,rv$GeneID_column],
              colorVar = temp,
              colorNames = NULL,
              colorList = rv$colorList,
              NAvalue = rv$NAvalue,
              layout = input$layout,
              unconnectedNodes = input$unconnectedNodes,
              alpha = input$alpha,
              nodeSize = input$nodeSize,
              legend = TRUE,
              nodeTable = TRUE,
              pathInfo = TRUE,
              openFile = FALSE
            )
          }
          
        }
        if (input$collection == "Custom"){
          if (tools::file_ext(rv$pathway) == "gpml"){
            if (!input$asNetwork){
              rv$outPath <- drawGPML_app(
                infile = rv$pathway,
                outdir = file_dir,
                outname = "pathway.svg",
                annPkg = pkg,
                inputDB = rv$GeneID_type,
                geneIDs = rv$statTable[,rv$GeneID_column],
                colorVar = temp,
                colorNames = NULL,
                colorList = rv$colorList,
                NAvalue = "#F0F0F0",
                legend = TRUE,
                nodeTable = TRUE,
                pathInfo = TRUE,
                openFile = FALSE
              )
            }
            if (input$asNetwork){
              rv$outPath <- GPML2Network(
                infile = rv$pathway,
                outdir = file_dir,
                outname = "pathway.svg",
                annPkg = pkg,
                inputDB = rv$GeneID_type,
                geneIDs = rv$statTable[,rv$GeneID_column],
                colorVar = temp,
                colorNames = NULL,
                colorList = rv$colorList,
                NAvalue = rv$NAvalue,
                layout = input$layout,
                unconnectedNodes = input$unconnectedNodes,
                alpha = input$alpha,
                nodeSize = input$nodeSize,
                legend = TRUE,
                nodeTable = TRUE,
                pathInfo = TRUE,
                openFile = FALSE
              )
            }
          }
          if (tools::file_ext(rv$pathway) == "xml"){
            if (!input$asNetwork){
              rv$outPath <- drawKGML_app(
                id = rv$pathway,
                outdir = file_dir,
                outname = "pathway.svg",
                annPkg = pkg,
                inputDB = rv$GeneID_type,
                geneIDs = rv$statTable[,rv$GeneID_column],
                colorVar = temp,
                colorNames = NULL,
                colorList = rv$colorList,
                NAvalue = rv$NAvalue,
                legend = TRUE,
                nodeTable = TRUE,
                pathInfo = TRUE,
                openFile = FALSE
              )
            }
            if (input$asNetwork){
              rv$outPath <- KGML2Network(
                id = rv$pathway,
                outdir = file_dir,
                outname = "pathway.svg",
                annPkg = pkg,
                inputDB = rv$GeneID_type,
                geneIDs = rv$statTable[,rv$GeneID_column],
                colorVar = temp,
                colorNames = NULL,
                colorList = rv$colorList,
                NAvalue = rv$NAvalue,
                layout = input$layout,
                unconnectedNodes = input$unconnectedNodes,
                alpha = input$alpha,
                nodeSize = input$nodeSize,
                legend = TRUE,
                nodeTable = TRUE,
                pathInfo = TRUE,
                openFile = FALSE
              )
            }
          } 

        }
      }, error = function(cond){
        outPath <- list()
        
        outPath[["Pathway"]] <- "Data/nullfile.svg" 
        outPath[["Legend"]] <- "Data/nullfile.svg" 
        outPath[["NodeTable"]] <- NULL
        rv$outPath <- outPath
      })
    })
    
    #**************************************************************************#
    # Outputs
    #**************************************************************************#
    
    # Show pathway
    observe({
      
      output$pathwayImage <- renderImage({
        req(input$collection)
        req(rv$pathway)
        req(input$pathway_width)
        req(rv$outPath)
        req(rv$colorList)
        req(sum(input$asNetwork))
        req(input$network_ok+1)
        
        svg <- read_xml(rv$outPath[["Pathway"]])
        width <- as.numeric(gsub("[^0-9.]", "", xml_attr(svg, "width")))
        height <- as.numeric(gsub("[^0-9.]", "", xml_attr(svg, "height")))
        
        if (width/height > 0.7){
          w <- paste0(input$pathway_width, "%")
          h <- "auto"
        } else{
          w <- "auto"
          h <- paste0(input$pathway_width*1.5, "%")
        }
        list(src = rv$outPath[["Pathway"]],
             contentType = 'image/svg+xml',
             width = w,
             height = h,
             alt = "Pathway diagram")
        
        
        
        
      }, deleteFile = FALSE)
      
    })
    
    # Show legend
    observe({
      output$legendImage <- renderImage({
        req(input$collection)
        req(rv$pathway)
        req(rv$outPath)
        req(rv$colorList)
        req(rv$NAvalue)
        req(input$network_ok+1)
        
        list(src = rv$outPath[["Legend"]],
             contentType = 'image/svg+xml',
             width = paste0(input$pathway_width, "%"),
             alt = "Pathway legend")
        
      }, deleteFile = FALSE)
      
    })
    
    # Show node table
    observe({
      req(rv$pathway)
      req(rv$outPath)
      req(rv$colorList)
      output$outputTable <- DT::renderDataTable(rv$outPath[["NodeTable"]])
    })
    
    observe({
      req(rv$outPath)
      output$infoTable <- DT::renderDataTable({
        out <- data.frame(c(as.character(rv$outPath[["Information"]]["Name"]),
                            as.character(rv$outPath[["Information"]]["ID"]),
                            paste0("<a href='",rv$outPath[["Information"]]["Link"],
                                   "' target='_blank'>", rv$outPath[["Information"]]["Link"], "</a>")
        )
        )
        if (nrow(out) == 3){
          rownames(out) <- c("Name",
                             "ID",
                             "Link")
          colnames(out) <- "Pathway information"
          return(out)
        } else
          return(NULL)
      },
      options = list(pageLength = 3, dom = "t"),
      rownames = TRUE,
      escape = FALSE)
      
      output$pathway_info <- renderUI({
        fluidPage(
          DT::dataTableOutput("infoTable"),
          br(),
          markdown(
            rv$outPath[["Information"]]["Description"]
          )
        )
      })
    })
    
    #**************************************************************************#
    # Downloads
    #**************************************************************************#
    
    # Save pathway modal
    observeEvent(input$save_pathway, {
      showModal(modalDialog(
        fluidPage(
          h4("What do you want to save?"),
          checkboxGroupInput(
            inputId = "downloads",
            label = NULL, 
            choices = c("Pathway", "Legend", "Node table"),
            selected = "Pathway",
            inline = TRUE
          ),
          hr(),
          downloadButton('download_pathway', 
                         'Save'),
          actionButton(inputId = "download_cancel",
                       label = "Cancel",
                       icon = icon("xmark"))
        ), easyClose = TRUE, size = "m", footer = NULL
      ))
    })
    
    # Close modal
    observeEvent(input$download_cancel, {
      removeModal()
    })
    
    # Download pathway
    observe({
      req(input$downloads)
      downloads <- input$downloads
      
      if (length(downloads) > 0){
        download_files <- rep(NA, length(downloads))
        
        if ("Pathway" %in% input$downloads){
          download_files[downloads == "Pathway"] <- rv$outPath[["Pathway"]]
        }
        if ("Legend" %in% input$downloads){
          download_files[downloads == "Legend"] <- rv$outPath[["Legend"]]
        }
        if ("Node table" %in% input$downloads){
          write.csv(rv$outPath[["NodeTable"]],
                    file = paste0(dirname(rv$outPath[["Pathway"]]),"/","NodeTable.csv"),
                    row.names = FALSE)
          download_files[downloads == "Node table"] <- paste0(dirname(rv$outPath[["Pathway"]]),"/","NodeTable.csv")
        }
        
        
        if (length(download_files) == 1){
          output$download_pathway <- downloadHandler(
            filename = basename(download_files),
            content = function(file){
              file.copy(download_files, file)
            }
          )
        }
        if (length(downloads) > 1){
          output$download_pathway <- downloadHandler(
            filename = "shinyPath.zip",
            content = function(file){
              setwd(dirname(download_files))
              zip(zipfile = file, files = basename(download_files))
            })
        }
      }
    })
    
    
    #**************************************************************************#
    # Statistics
    #**************************************************************************#
    output$UI_stat_column <- renderUI({
      cols <- apply(rv$statTable, 2, function(x) length(table(x)))
      selectInput(inputId = "stat_column",
                  label = "Column",
                  choices = names(cols)[(cols > 1) & (cols < 6)],
                  multiple = FALSE)
    })
    
    output$UI_stat_column_pos <- renderUI({
      selectInput(inputId = "stat_column_pos",
                  label = "Label",
                  choices = unique(rv$statTable[,input$stat_column]),
                  multiple = FALSE)
    })
    
    # Save pathway modal
    observeEvent(input$calculate_statistics, {
      showModal(modalDialog(
        fluidPage(
          h2(strong("Overrepresentation analysis")),
          hr(),
          h4(span("Pathway collection",
                  tags$span(
                    icon(
                      name = "question-circle",
                    ) 
                  ) |>
                    prompter::add_prompt(
                      message = "For which pathway collection do you want to perform 
                      overrepresentation analysis?",
                      position = "right",
                      rounded = TRUE,
                      size = "large",
                      type = "info")
          )),
          shinyWidgets::pickerInput(
            inputId = "stat_collection",
            label = NULL,
            choices = c("WikiPathways", "KEGG"),
            selected = input$collection,
            multiple = TRUE),
          br(),
          h4(span("Select genes",
                  tags$span(
                    icon(
                      name = "question-circle",
                    ) 
                  ) |>
                    prompter::add_prompt(
                      message = "For the overrepresentation analysis, we 
                      need to know which genes are considered differentially expressed. 
                      Here you can select which column indicates whether or not a gene is differentially expressed. 
                      The genes with the selected label are considered differentially expressed.",
                      position = "right",
                      rounded = TRUE,
                      size = "large",
                      type = "info")
          )),
          fluidRow(
            column(6,
                   uiOutput("UI_stat_column")),
            column(6,
                   uiOutput("UI_stat_column_pos"))
          ),
          hr(),
          actionButton(inputId = "statistics_ok",
                       label = "Calculate",
                       icon = icon("rotate")),
          actionButton(inputId = "statistics_cancel",
                       label = "Cancel",
                       icon = icon("xmark"))
        ), easyClose = TRUE, size = "m", footer = NULL
      ))
    })
    
    # Close modal
    observeEvent(input$statistics_cancel, {
      removeModal()
    })
    
    # Calculate statistics
    observeEvent(input$statistics_ok, {
      
      shinybusy::show_modal_spinner(text = "Calculating statistics...",
                                    color="#2c3e50")
      if ("WikiPathways" %in% input$stat_collection){
        ORAresults <- clusterProfiler::enricher(
          gene = rv$statTable[rv$statTable[,input$stat_column] == input$stat_column_pos,rv$GeneID_column],
          pvalueCutoff = Inf,
          pAdjustMethod = "BH",
          universe = rv$statTable[,rv$GeneID_column],
          minGSSize = -Inf,
          maxGSSize = Inf,
          qvalueCutoff = Inf,
          gson = NULL,
          TERM2GENE = rv$PathwayInfo_WP[,c("wpid", rv$GeneID_type)],
          TERM2NAME = rv$PathwayInfo_WP[,c("wpid", "name")]
        )
        
        pathStats <- ORAresults@result[,c("ID", "Description", "GeneRatio",
                                          "BgRatio", "pvalue", "p.adjust")]
        pathStats$pvalue <- signif(pathStats$pvalue, 3)
        pathStats$`p.adjust`<- signif(pathStats$`p.adjust`, 3)
        colnames(pathStats) <- c("ID", "Description", "GeneRatio",
                                 "BgRatio", "p-value", "adj. p-value")
        
        rv$pathStats[["WikiPathways"]] <- pathStats
      }
      
      if ("KEGG" %in% input$stat_collection){
        ORAresults <- clusterProfiler::enricher(
          gene = rv$statTable[rv$statTable[,input$stat_column] == input$stat_column_pos,rv$GeneID_column],
          pvalueCutoff = Inf,
          pAdjustMethod = "BH",
          universe = rv$statTable[,rv$GeneID_column],
          minGSSize = -Inf,
          maxGSSize = Inf,
          qvalueCutoff = Inf,
          gson = NULL,
          TERM2GENE = rv$PathwayInfo_KEGG[,c("keggid", rv$GeneID_type)],
          TERM2NAME = rv$PathwayInfo_KEGG[,c("keggid", "name")]
        )
        pathStats <- ORAresults@result[,c("ID", "Description", "GeneRatio",
                                          "BgRatio", "pvalue", "p.adjust")]
        pathStats$pvalue <- signif(pathStats$pvalue, 3)
        pathStats$`p.adjust`<- signif(pathStats$`p.adjust`, 3)
        
        colnames(pathStats) <- c("ID", "Description", "GeneRatio",
                                 "BgRatio", "p-value", "adj. p-value")
        
        rv$pathStats[["KEGG"]] <- pathStats
      }
      
      shinybusy::remove_modal_spinner()
      removeModal()
    })
    
    #**************************************************************************#
    # Color settings
    #**************************************************************************#
    
    # Make modal
    observeEvent(input$setColors, {
      showModal(modalDialog(
        fluidPage(
          h4("Column for coloring"),
          uiOutput("UI_color_var"),
          hr(),
          h4("Set color"),
          uiOutput("UI_scaleType"),
          uiOutput("UI_colorSettings"),
          hr(),
          h4("NA values"),
          uiOutput("UI_NAvalue"),
          hr(),
          actionButton(inputId = "color_ok",
                       label = "Change color!",
                       icon = icon("palette")),
          actionButton(inputId = "color_cancel",
                       label = "Cancel",
                       icon = icon("xmark"))
        ), easyClose = TRUE, size = "l", footer = NULL
        
      ))
    })
    
    
    observe({
      req(rv$statTable)
      req(input$color_column)
      
      # Color variable
      output$UI_color_var <- renderUI({
        tagList(
          selectizeInput(inputId = "color_var",
                         label = NULL,
                         choices = input$color_column,
                         selected = input$color_column[1],
                         multiple = FALSE)
        )
      })
      
      # Color list
      if (length(setdiff(input$color_column, colnames(rv$statTable)))==0){
        rv$colorList <- defaultColorList_app(rv$statTable[,input$color_column],
                                             ColorNames = input$color_column)
      }
      
      rv$NAvalue <- "#F0F0F0"
      
    })
    
    # Scale type
    observe({
      req(input$color_column)
      req(input$color_var)
      
      if (length(which(input$color_column == input$color_var)) > 0){
        output$UI_scaleType <- renderUI({
          tagList(
            radioButtons(inputId = "scaleType",
                         label = NULL,
                         choices = c("Divergent", "Sequential", "Qualitative"),
                         selected =  rv$colorList[[which(input$color_column == input$color_var)]][["ScaleType"]],
                         inline = TRUE)
          )
        })
      }
    })
    
    
    # Set scale type
    observe({
      req(input$color_column)
      req(input$color_var)
      req(input$scaleType )
      req(rv$colorList)
      
      # Set color
      if (length(which(input$color_column == input$color_var)) > 0){
        rv$Color_default <- rv$colorList[[which(input$color_column == input$color_var)]]$Color
        rv$ColorVal_default <- rv$colorList[[which(input$color_column == input$color_var)]]$ColorVal
        
        output$UI_colorSettings <- renderUI({
          if (input$scaleType == "Divergent"){
            tagList(
              fluidRow(
                column(2, align = "right",
                       h5("Minimum:")),
                column(2,
                       numericInput(
                         inputId = "minVal_divergent",
                         label = NULL,
                         value = as.numeric(rv$ColorVal_default["MinVal"]))
                ),
                column(2,
                       colourpicker::colourInput(
                         inputId = "minCol_divergent",
                         label = NULL,
                         value = rv$Color_default["MinCol"],
                         showColour = "both")
                )
              ),
              fluidRow(
                column(2, align = "right",
                       h5("Middle:")),
                column(2,
                       numericInput(
                         inputId = "midVal_divergent",
                         label = NULL,
                         value = as.numeric(rv$ColorVal_default["MidVal"]))
                ),
                column(2,
                       colourpicker::colourInput(
                         inputId = "midCol_divergent",
                         label = NULL,
                         value = rv$Color_default["MidCol"],
                         showColour = "both")
                )
              ),
              fluidRow(
                column(2, align = "right",
                       h5("Maximum:")),
                column(2,
                       numericInput(
                         inputId = "maxVal_divergent",
                         label = NULL,
                         value = as.numeric(rv$ColorVal_default["MaxVal"]))
                ),
                column(2,
                       colourpicker::colourInput(
                         inputId = "maxCol_divergent",
                         label = NULL,
                         value = rv$Color_default["MaxCol"],
                         showColour = "both")
                )
              )
            )
          } else if (input$scaleType == "Sequential"){
            
            tagList(
              fluidRow(
                column(2, align = "right",
                       h5("Minimum:")),
                column(2,
                       numericInput(
                         inputId = "minVal_sequential",
                         label = NULL,
                         value = as.numeric(rv$ColorVal_default["MinVal"]))
                ),
                column(2,
                       colourpicker::colourInput(
                         inputId = "minCol_sequential",
                         label = NULL,
                         value = rv$Color_default["MinCol"],
                         showColour = "both")
                )
              ),
              fluidRow(
                column(2, align = "right",
                       h5("Maximum:")),
                column(2,
                       numericInput(
                         inputId = "maxVal_sequential",
                         label = NULL,
                         value = as.numeric(rv$ColorVal_default["MaxVal"]))
                ),
                column(2,
                       colourpicker::colourInput(
                         inputId = "maxCol_sequential",
                         label = NULL,
                         value = rv$Color_default["MaxCol"],
                         showColour = "both")
                )
              )
            )
          } else if (input$scaleType == "Qualitative"){
            ColorVar_temp <- rv$statTable[,input$color_var]
            ColorVar_temp <- ColorVar_temp[!is.na(ColorVar_temp)]
            if (length(unique(ColorVar_temp)) == 1){
              tagList(
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col1_qualitative",
                           label = NULL,
                           value = rv$Color_default[1],
                           showColour = "both")
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[1]))
                )
              )
            } else if (length(unique(ColorVar_temp)) == 2){
              tagList(
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col1_qualitative",
                           label = NULL,
                           value = rv$Color_default[1],
                           showColour = "both")
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[1]))
                ),
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col2_qualitative",
                           label = NULL,
                           value = rv$Color_default[2],
                           showColour = "both")
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[2]))
                )
              )
            } else if (length(unique(ColorVar_temp)) == 3){
              tagList(
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col1_qualitative",
                           label = NULL,
                           value = rv$Color_default[1],
                           showColour = "both")
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[1]))
                ),
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col2_qualitative",
                           label = NULL,
                           value = rv$Color_default[2],
                           showColour = "both")
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[2]))
                ),
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col3_qualitative",
                           label = NULL,
                           value = rv$Color_default[3],
                           showColour = "both")
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[3]))
                )
              )
            } else if (length(unique(ColorVar_temp)) == 4){
              tagList(
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col1_qualitative",
                           label = NULL,
                           value = rv$Color_default[1],
                           showColour = "both")
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[1]))
                ),
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col2_qualitative",
                           label = NULL,
                           value = rv$Color_default[2],
                           showColour = "both")
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[2]))
                ),
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col3_qualitative",
                           label = NULL,
                           value = rv$Color_default[3],
                           showColour = "both")
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[3]))
                ),
                fluidRow(
                  column(2,
                         colourpicker::colourInput(
                           inputId = "col4_qualitative",
                           label = NULL,
                           value = rv$Color_default[4],
                           showColour = "both"),
                  ),
                  column(10,
                         h5(unique(ColorVar_temp)[4]))
                )
              )
            } else if (length(unique(ColorVar_temp)) > 4){
              tagList(
                h4("There are too many unique values!")
              )
            }
          }
        }) # EO renderUI
      }
    })
    
    observe({
      req(rv$NAvalue)
      output$UI_NAvalue <- renderUI({
        tagList(
          fluidRow(
            column(12,
                   colourpicker::colourInput(
                     inputId = "NAvalue",
                     label = NULL,
                     value = rv$NAvalue,
                     showColour = "both"),
            )
          )
        )
      })
    })
    
    rv$NAvalue_temp <- NULL
    rv$colorList_temp <- list()
    observe({
      req(input$color_column)
      req(input$color_var)
      req(input$scaleType)
      
      if (length(which(input$color_column == input$color_var)) > 0){
        if (input$scaleType == "Divergent"){
          req(input$minVal_divergent)
          rv$colorList_temp[[which(input$color_column == input$color_var)]] <- list(
            ScaleName = input$color_var,
            ScaleType = input$scaleType,
            ColorVal = c(
              "MinVal" = input$minVal_divergent,
              "MidVal" = input$midVal_divergent,
              "MaxVal" = input$maxVal_divergent
            ),
            Color = c(
              "MinCol" = input$minCol_divergent,
              "MidCol" = input$midCol_divergent,
              "MaxCol" = input$maxCol_divergent
            )
          )
        } else if (input$scaleType == "Sequential"){
          req(input$minVal_sequential)
          rv$colorList_temp[[which(input$color_column == input$color_var)]] <- list(
            ScaleName = input$color_var,
            ScaleType = input$scaleType,
            ColorVal = c(
              "MinVal" = input$minVal_sequential,
              "MaxVal" = input$maxVal_sequential
            ),
            Color = c(
              "MinCol" = input$minCol_sequential,
              "MaxCol" = input$maxCol_sequential
            )
          )
        } else if (input$scaleType == "Qualitative"){
          req(input$col1_qualitative)
          color_vec <- c(
            input$col1_qualitative,
            input$col2_qualitative,
            input$col3_qualitative,
            input$col4_qualitative
          )
          
          if (length(color_vec) == length(unique(rv$statTable[,input$color_var]))){
            rv$colorList_temp[[which(input$color_column == input$color_var)]] <- list(
              ScaleName = input$color_var,
              ScaleType = input$scaleType,
              Color = setNames(color_vec, unique(rv$statTable[,input$color_var]))
            )
          }
        }
      }
      
      rv$NAvalue_temp <- input$NAvalue
    })
    
    # Set color
    observeEvent(input$color_ok,{
      
      for (s in 1:length(rv$colorList)){
        if (!is.null(rv$colorList_temp[[s]])){
          rv$colorList[[s]] <- rv$colorList_temp[[s]]
        }
      }
      
      if (!is.null(rv$NAvalue_temp)){
        rv$NAvalue <- rv$NAvalue_temp
      }
      removeModal()
    })
    observeEvent(input$color_cancel,{
      removeModal()
    })
    
  }) # EO observe
}