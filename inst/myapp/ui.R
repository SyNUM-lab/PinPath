ui <- tagList(
  # Set the style of the UI
  tags$head(tags$style(HTML(
    "
    .my_style_1{ 
      background-image: url(background.png);
      background-size: cover;
      background-repeat: no-repeat;
      background-attachment: fixed;
      background-position: center center;
      min-height: 100vh;
      margin-top: -20px; 
      margin-right: 0; 
      padding: 0;
      display: flex;
      align-items: center;
    }
                            
    .pretty input:checked~.state.p-success label:after, .pretty.p-toggle .state.p-success label:after {
      background-color: black!important;
    }
                           
    .dropbtn {
      background-color: #798D8F;
      color: white;
      padding: 0;
      display: flex;
      font-size: 28px;
      border: none;
      border-radius: 50%;
      cursor: pointer;
      width: 50px;
      height: 50px;
      align-items: center;
      justify-content: center;
      line-height = 1;
    }

    .dropdown {
      position: relative;
      display: inline-block;
    }

    .dropdown-content {
      display: none;
      position: absolute;
      background-color: #f9f9f9;
      min-width: 250px;
      box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
      padding: 12px;
      z-index: 1;
      border-radius: 4px;
    }

    .dropdown:hover .dropdown-content {
      display: block;
    }
    "))),
  
  
  # Set theme
  fluidPage(theme = shinythemes::shinytheme("flatly"),
            
            # This allow for pop-up messages to show up
            shinyWidgets::useSweetAlert(),
            
            # This allows for insertion of information boxes in the app
            prompter::use_prompt(),
            
            navbarPage(
              title = div(img(src="logo.png",
                              style="margin-top: -20px;
                               padding-right:5px;
                               padding-left:5px;
                               padding-bottom:10px;
                              padding-top:10px",
                              height = 50)),
              windowTitle = "shinyPath",
              id = "navbar",
              
              ###########################################################################
              
              # Data upload
              
              ###########################################################################
              tabPanel("Home", 
                       value = "home_panel", 
                       icon = icon("fas fa-home"),
                       class = "my_style_1",
                       
                       # Set spacing between top of page and text box
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       
                       # Make text box
                       fluidRow(
                         column(4, offset = 4, 
                                align = "center", 
                                style = "background-color:rgba(44, 62, 80, 0.8); border-radius: 15px;",
                                
                                br(),
                                # h1(strong(span(style = "color:#FFFFFF",
                                #                "Welcome to"))),
                                img(src = "logo_welcome.png", width = "70%"),
                                h4(span(style = "color:#FFFFFF", 
                                        "Start by uploading your statistics table",
                                        tags$span(
                                          icon(
                                            name = "question-circle",
                                          ) 
                                        ) |>
                                          prompter::add_prompt(message = "The statistics table should have at least two columns:\n\n 
                                                               (1) a column with gene/protein IDs and  
                                                               (2) a column that can be used for coloring (e.g., logFCs or p-values). 
                                                               Click on 'Download example' or 'Run example' to see how a statistics table can look like.",
                                                               position = "right",
                                                               rounded = TRUE,
                                                               size = "large",
                                                               type = "info")
                                )),
                                br(),
                                fileInput(inputId = "upload_table",
                                          label = NULL,
                                          accept = c(".csv", ".tsv", ".txt"),
                                          placeholder = "Select .csv/.tsv/.txt file"),
                                
                                # Action button: start by clicking
                                shinyWidgets::actionBttn(inputId = "info",
                                                         label = "Information",
                                                         style = "simple",
                                                         color = "warning",
                                                         icon = icon("circle-info")),
                                
                                shinyWidgets::actionBttn(inputId = "home_next",
                                                         label = "Start",
                                                         style = "simple",
                                                         color = "warning",
                                                         icon = icon("arrow-right")),
                                br(),
                                #br(),
                                h5(span(style = "color:#FFFFFF",                                
                                        downloadLink(outputId = "download_example", 
                                                     label = "Download example",
                                                     style = "color:#C6DBEF"),
                                        strong(" | "),
                                        actionLink(inputId = "example", 
                                                   label = "Run example",
                                                   style = "color:#C6DBEF"))),
                                
                                
                                # Line breaks
                                br(),
                                #br(),
                                h6(align = "left", 
                                   style = "color:lightgrey",
                                   "PinPath v0.1.0 | CC-BY 4.0"),
                                
                         ) # EO column
                       ), # EO fluidRow
                       br(),
                       br()
              ),
              
              ###########################################################################
              
              # Gene info
              
              ###########################################################################
              tabPanel("Gene info", value = "panel2", 
                       icon = icon("plus"),
                       sidebarPanel(width = 3,
                                    uiOutput("UI_settings")
                       ),
                       mainPanel(width = 9,
                                 DT::dataTableOutput(outputId = "statTable_view") %>% 
                                   shinycssloaders::withSpinner(color="#2c3e50"),
                                 downloadButton('save_statTable', 
                                                'Save')
                       )
              ),
              
              
              ###########################################################################
              
              # Data upload
              
              ###########################################################################
              tabPanel("Pathway", value = "panel3", 
                       icon = icon("diagram-project"),
                       sidebarPanel(width = 3,
                                    h4("Pathway"),
                                    shinyWidgets::pickerInput(
                                      inputId = "collection",
                                      label = NULL,
                                      choices = c("WikiPathways", "KEGG", "Custom")),
                                    
                                    conditionalPanel(
                                      condition = "input.collection != `Custom`",
                                      selectizeInput(inputId = "pathway",
                                                     label = NULL,
                                                     choices = NULL,
                                                     multiple = FALSE),
                                    ),
                                    conditionalPanel(
                                      condition = "input.collection == `Custom`",
                                      fileInput(inputId = "pathway_file",
                                                label = NULL,
                                                accept = c(".gpml", ".xml"),
                                                placeholder = "Select GPML/KGML file")
                                    ),
                                    shinyWidgets::materialSwitch(
                                      inputId = "asNetwork",
                                      label = "Network view",
                                      value = FALSE
                                    ),
                                    
                                    br(),
                                    h4("Color"),
                                    selectizeInput(inputId = "color_column",
                                                   label = NULL,
                                                   choices = NULL,
                                                   multiple = TRUE,
                                                   options = list(plugins = "remove_button")),
                                    
                                    actionButton(inputId = "setColors",
                                                 label = "Color palette",
                                                 icon = icon("palette")),
                                    br(),br(),
                                    h4("Zoom"),
                                    sliderInput(inputId = "pathway_width",
                                                label = NULL,
                                                value = 100,
                                                step = 10,
                                                min = 10,
                                                max = 300),
                                    hr(),
                                    
                                    shinyWidgets::actionBttn(inputId = "save_pathway",
                                                             label = "Save",
                                                             icon = icon("download"),
                                                             style = "simple",
                                                             color = "warning"),
                                    
                                    shinyWidgets::actionBttn(inputId = "calculate_statistics",
                                                             label = "Statistics",
                                                             icon = icon("fa-solid fa-chart-bar"),
                                                             style = "simple",
                                                             color = "warning")
                                    
                       ),
                       mainPanel(width = 9,
                                 tabsetPanel(
                                   tabPanel("Pathway",
                                            br(),
                                            DT::dataTableOutput(outputId = "pathStats_view") %>% 
                                              shinycssloaders::withSpinner(color="#2c3e50"),
                                            conditionalPanel(
                                              condition = "input.asNetwork",
                                              tagList(
                                                tags$div(class = "dropdown",
                                                         tags$button(class = "dropbtn", icon("cog")),
                                                         tags$div(class = "dropdown-content",
                                                                  h5(strong("Transparency (0-1)")),
                                                                  numericInput(inputId = "alpha",
                                                                               label = NULL,
                                                                               value = 0.9,
                                                                               min = 0,
                                                                               max = 1),
                                                                  h5(strong("Node size (0-5)")),
                                                                  numericInput(inputId = "nodeSize",
                                                                               label = NULL,
                                                                               value = 1,
                                                                               min = 0,
                                                                               max = 5),
                                                                  h5(strong("Network layout")),
                                                                  selectInput(inputId = "layout",
                                                                              label = NULL,
                                                                              choices = c("nicely",
                                                                                          "graphopt",
                                                                                          "lgl",
                                                                                          "kk",
                                                                                          "gem",
                                                                                          "fr",
                                                                                          "dh",
                                                                                          "randomly",
                                                                                          "grid",
                                                                                          "star",
                                                                                          "circle"),
                                                                              selected = "nicely"),
                                                                  h5(strong("Show unconnected nodes")),
                                                                  shinyWidgets::materialSwitch(
                                                                    inputId = "unconnectedNodes",
                                                                    label = NULL,
                                                                    value = FALSE
                                                                  ),
                                                                  hr(),
                                                                  actionButton("network_ok",
                                                                               label = "Apply")
                                                                  
                                                         )
                                                )
                                              )
                                            ),
                                            
                                            imageOutput("pathwayImage") %>% 
                                              shinycssloaders::withSpinner(color="#2c3e50")
                                   ),
                                   tabPanel("Legend",
                                            br(),
                                            imageOutput("legendImage") %>% 
                                              shinycssloaders::withSpinner(color="#2c3e50"),
                                   ),
                                   tabPanel("Node info",
                                            br(),
                                            DT::dataTableOutput("outputTable") %>% 
                                              shinycssloaders::withSpinner(color="#2c3e50")
                                   ),
                                   tabPanel("Pathway info",
                                            br(),
                                            uiOutput("pathway_info") %>% 
                                              shinycssloaders::withSpinner(color="#2c3e50")
                                   ),
                                 ) #EO tabsetPanel
                       ) #EO mainPanel
                       
              ),
              
              ###########################################################################
              
              # Information
              
              ###########################################################################
              tabPanel("Information", value = "panel4", 
                       icon = icon("circle-info"),
                       mainPanel(
                         includeMarkdown("www/info.md")
                       )
              )
              
            
  ) # EO navbarPage
) # EO fluidPage
) # EO tagList