# lib ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, shinyjs, ctmm, ggplot2)
# increase the uploading file size limit to 30M
options(shiny.maxRequestSize = 30*1024^2)
# util for pretty printing summary
short_summary <- function(l) {
  lines <- vector(mode = "character", length = length(l))
  n <- names(l)
  for (i in seq_along(l)) {
    lines[i] <- paste0("- ", n[i], ": ", paste0(l[[i]], collapse = ", "), "\n")
  }
  return(paste0(lines, collapse = ""))
}
# header ----
header <- dashboardHeader(title = "Animal Movement",
            dropdownMenu(type = "messages",
               messageItem(
                 from = "ctmm team",
                 message = "About ctmm",
                 href = "https://cran.r-project.org/web/packages/ctmm/index.html"),
               messageItem(
                 from = "Documentation",
                 message = "View Documentation and Source",
                 icon = icon("question"),
                 href = "https://github.com/xhdong-umd/ctmm-shiny-prototype"),
               messageItem(
                 from = "Issues",
                 message = "Report Issues Here.",
                 icon = icon("life-ring"),
                 ## time = "2014-12-01",
                 href = "https://github.com/xhdong-umd/ctmm-shiny-prototype/issues")))
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    # match tabItem
    menuItem("Intro", tabName = "intro", icon = icon("book")),
    menuItem("Upload", tabName = "upload", icon = icon("upload")),
    menuItem("Time-lag", tabName = "timelag", icon = icon("line-chart")),
    menuItem("Model", tabName = "model", icon = icon("hourglass-start")),
    menuItem("Home Range", tabName = "homerange", icon = icon("map-o")),
    menuItem("Report", tabName = "report", icon = icon("file-text-o"))
    
  )
)
# boxes ----
upload_box <- box(title = "Upload your MoveBank format data",
                  status = "info", solidHeader = TRUE,
                  useShinyjs(),
                  radioButtons('load_option', 'Load Data From',
                               c("Upload File" = 'upload',
                                 "Use ctmm Bufflo Data" = 'ctmm'), selected = "upload"
                  ),
                  fileInput('file1', label = "",
                            accept = c('text/csv',
                                       'text/comma-separated-values,text/plain',
                                       '.csv')))
data_summary_box <- box(title = "Data Summary - 1st animal", status = "primary",
                        solidHeader = TRUE, 
                        verbatimTextOutput("data_summary"))
data_plot_box <- tabBox(title = "Data Plot",
                        id = "plottabs", height = "450px", width = 12,
                        tabPanel("Basic Plot", plotOutput("data_plot_basic")),
                        tabPanel("ggplot2", plotOutput("data_plot_gg")))
vario_plot_box_1 <- box(title = "Variogram with up to 50% lag",
                        status = "primary", solidHeader = TRUE,
                        plotOutput("vario_plot_1"))
vario_plot_box_2 <- box(title = "Variogram with minimal lag",
                        status = "primary", solidHeader = TRUE,
                        plotOutput("vario_plot_2"))
vario_plot_box_3 <- box(title = "Variogram with Zoom",
                        status = "info", solidHeader = TRUE, width = 12,
                        sidebarPanel(sliderInput("zoom", "Zoom", 
                                                 min = 0.001, max = 1, 
                                                 value = 0.01)),
                        mainPanel(plotOutput("vario_plot_3")))
# TODO plot 3 also have a button to use user selected parameters for next step
# TOO a button to auto guess parameters for next step
# explain the result source, then print summary
model_summary_box <- box(title = "Model Summary", status = "info",
                         solidHeader = TRUE, width = 12,
                         verbatimTextOutput("model_summary"))
model_plot_box_1 <- box(title = "Variogram with model for up to 50% lag",
                        status = "primary", solidHeader = TRUE,
                        plotOutput("model_plot_1"))
model_plot_box_2 <- box(title = "Variogram with model for minimal lag",
                        status = "primary", solidHeader = TRUE,
                        plotOutput("model_plot_2"))
range_summary_box <- box(title = "Home Range Estimation", status = "info",
                       solidHeader = TRUE, width = 12,
                       verbatimTextOutput("range_summary"))
range_plot_box <- tabBox(title = "Home Range Estimation plot",
                        id = "rangeplottabs", height = "450px", width = 12,
                        tabPanel("Basic Plot", plotOutput("range_plot_basic")),
                        tabPanel("ggplot2", plotOutput("range_plot_gg")))
# body ----
body <- dashboardBody(
  # match menuItem
  tabItems(
    tabItem(tabName = "intro", fluidPage(includeMarkdown("workflow1.md"))),
    tabItem(tabName = "upload",
            fluidRow(upload_box, data_summary_box), 
            fluidRow(data_plot_box)), 
    tabItem(tabName = "timelag",
            fluidRow(vario_plot_box_1, vario_plot_box_2),
            fluidRow(vario_plot_box_3)),
    tabItem(tabName = "model",
            fluidRow(model_summary_box),
            fluidRow(model_plot_box_1, model_plot_box_2)),
    tabItem(tabName = "homerange",
            fluidRow(range_summary_box),
            fluidRow(range_plot_box)),
    tabItem(tabName = "report", fluidPage(includeMarkdown("workflow1.md")))
  )
)
# assemble UI
ui <- dashboardPage(header, sidebar, body,skin = "green")
# server ----
server <- function(input, output) { 
  # load data
  # TODO only taking first animal now. and as.telemetry return a list if multiple animal found.
  datasetInput <- reactive({
    if (input$load_option == "ctmm") {
      data("buffalo")
      buffalo[[1]]
    } else if (input$load_option == "upload") {
      inFile <- input$file1
      if (!is.null(inFile)) {
        tele <- as.telemetry(inFile$datapath)
        # ifelse will have Error in NextMethod("[") : 'NextMethod' called from an anonymous function
        if (class(tele) == "list") {
          tele[[1]]
        } else {
          tele
        }
      }
    } 
  })
  # toggle browse button if use ctmm data
  observeEvent(input$load_option, {
    toggleState(id = "file1", condition = (input$load_option == "upload"))
  })
  # data summary
  output$data_summary <- renderPrint({
    animal_1 <- datasetInput()
    if (is.null(animal_1))
      cat("No data loaded yet")
    else{
      cat(short_summary(summary(animal_1)))
    }
  })
  # data plot
  output$data_plot_basic <- renderPlot({
    animal_1 <- datasetInput()
    if (!is.null(animal_1))
      plot(animal_1)
  })
  output$data_plot_gg <- renderPlot({
    animal_1 <- datasetInput()
    if (!is.null(animal_1))
      ggplot(data = animal_1, aes(x, y)) + 
        geom_point(color = "red", shape = 1, size = 0.8) +
        labs(x = "x (meters)", y = "y (meters)") +
        ggtitle(paste0("animal: ", animal_1@info$identity)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_fixed()
  })
  # vario
  vg.animal_1 <- reactive({
    animal_1 <- datasetInput()
    variogram(animal_1)
  })
  output$vario_plot_1 <- renderPlot({plot(vg.animal_1())})
  output$vario_plot_2 <- renderPlot({plot(vg.animal_1(), fraction = 0.1)})
  output$vario_plot_3 <- renderPlot({plot(vg.animal_1(), fraction = input$zoom)})
  # model selection, right now with all default parameter, no user selection
  selected_model <- reactive({
    animal_1 <- datasetInput()
    guessed <- ctmm.guess(animal_1, interactive = FALSE)
    withProgress(ctmm.select(animal_1, CTMM = guessed), 
                 message = "Fitting models to find the best fit")
  })
  output$model_summary <- renderPrint({
    fitted.mod <- selected_model()
    if (is.null(fitted.mod))
      cat("No model selected yet")
    else{
      summary(fitted.mod)
    }
  })
  
}

shinyApp(ui, server)
