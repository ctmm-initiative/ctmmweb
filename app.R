# lib ----
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(shiny, shinydashboard, shinyjs, ctmm, ggplot2, markdown)
# pacman seemed not working with shinyapps.io, good for local version though
# debug flag
# debug <- TRUE
# if (debug) {
#   options(shiny.reactlog = TRUE) 
# }
library(shiny)
library(shinydashboard)
# DT should be after shiny to override dataTable in shiny
library(DT)
library(ctmm)
library(ggplot2)
library(markdown)
library(data.table)
library(lubridate)
# increase the uploading file size limit to 30M
options(shiny.maxRequestSize = 30*1024^2)
# options(shiny.trace = TRUE)
source("helpers.R")
# header ----
header <- dashboardHeader(title = "Animal Movement"
            # dropdownMenu(type = "messages",
            #    messageItem(
            #      from = "ctmm team",
            #      message = "About ctmm",
            #      href = "https://cran.r-project.org/web/packages/ctmm/index.html"),
            #    messageItem(
            #      from = "Documentation",
            #      message = "View Documentation and Source",
            #      icon = icon("question"),
            #      href = "https://github.com/xhdong-umd/ctmm-shiny-prototype"),
            #    messageItem(
            #      from = "Issues",
            #      message = "Report Issues Here.",
            #      icon = icon("life-ring"),
            #      ## time = "2014-12-01",
            #      href = "https://github.com/xhdong-umd/ctmm-shiny-prototype/issues"))
            )
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs", 
    # match tabItem
    menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("Variogram", tabName = "timelag", icon = icon("line-chart")),
    menuItem("Model", tabName = "model", icon = icon("hourglass-start")),
    menuItem("Home Range", tabName = "homerange", icon = icon("map-o")),
    menuItem("Report", tabName = "report", icon = icon("file-text-o")),
    menuItem("Help", tabName = "intro", icon = icon("question"))
  ), 
  tags$br(), tags$br(), tags$br(), tags$br(), 
  radioButtons('load_option', "Load Movebank format data",
               c("Bufflo Data in ctmm" = 'ctmm',
                 "Movebank format file" = 'upload'), selected = "upload"
  ),
  fileInput('file1', label = "",
            accept = c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv'))
)
# boxes ----
# upload_box <- box(title = "Load data",
#                   status = "info", solidHeader = TRUE, width = 4,
#                   useShinyjs(),
#                   radioButtons('load_option', NULL,
#                                c("Use Bufflo Data in ctmm" = 'ctmm',
#                                  "Upload Movebank format file" = 'upload'), selected = "upload"
#                   ),
#                   fileInput('file1', label = "",
#                             accept = c('text/csv',
#                                        'text/comma-separated-values,text/plain',
#                                        '.csv')))
data_summary_box <- box(title = "Data Summary", status = "primary",
                        solidHeader = TRUE, width = 12,
                        # verbatimTextOutput("data_summary")
                        fluidRow(column(12, DT::dataTableOutput('data_summary'))),
                        fluidRow(column(6,actionButton(
                          "batch", 
                          "Batch process all selected")),
                          column(6,  actionButton("single", "Analyze single selected"))
                         
                          )
                        )
data_plot_box <- tabBox(title = "Data Plot", id = "plottabs", 
                  height = "450px", width = 12, 
                  tabPanel("ggplot2", plotOutput("data_plot_gg")), 
                  tabPanel("Basic Plot", plotOutput("data_plot_basic"))
                 )
vario_plot_box_1 <- box(title = "Variogram zoomed in for 50% Time-lag",
                        status = "primary", solidHeader = TRUE,
                        plotOutput("vario_plot_1"))
vario_plot_box_2 <- box(title = "Variogram zoomed in 10% Time-lag",
                        status = "primary", solidHeader = TRUE,
                        plotOutput("vario_plot_2"))
#                                sliderInput("zoom2", "Log10(fraction)", 
#                                            min = -3, max = 0, step = 0.1, 
#                                            value = log10(0.5))
#                                ))
                        # sidebarPanel(sliderInput("zoom2", "Log10(fraction)", 
                        #                          min = -3, max = 0, step = 0.1, 
                        #                          value = log10(0.5))),
                        # mainPanel(plotOutput("vario_plot_2")))
vario_plot_box_3 <- box(title = "Variogram with Zoom selection",
                           status = "info", solidHeader = TRUE, width = 12,
                           sidebarPanel(h4("Zoom in: 0.1% - 100%"),
                             sliderInput("zoom", "Log10(percentage)", 
                                                    min = -3, max = 0, step = 0.1, 
                                                    value = log10(0.5))),
                           mainPanel(plotOutput("vario_plot_3")))

# vario_plot_zoom_box <- box(title = "Variogram with Zoom",
#                         status = "info", solidHeader = TRUE, width = 12,
#                         sidebarPanel(sliderInput("zoom", "Log10(fraction)", 
#                                                  min = -3, max = 0, step = 0.1, 
#                                                  value = log10(0.5)),
#                                      actionButton('snapBtn', 'Snapshot')),
#                         mainPanel(plotOutput("vario_plot_1")))
# vario_plot_static_box <- box(title = "Variogram snapshots", status = "primary",
#                              solidHeader = TRUE, width = 12,
#                              tags$div(id = 'variosnapshots'))
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
  includeCSS("www/styles.css"),
  # match menuItem
  tabItems(
    tabItem(tabName = "intro", fluidPage(includeMarkdown("workflow1.md"))),
    tabItem(tabName = "data",
            fluidRow(data_summary_box), 
            # fluidRow(upload_box, data_summary_box), 
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
server <- function(input, output, session) {
  # upload area behavior ----
  # clicking browse button switch mode automatically
  observeEvent(input$file1, {
    updateRadioButtons(session, "load_option", selected = "upload")
    updateTabItems(session, "tabs", "data")
  })
  # radio button and browse all should switch page to data
  observeEvent(input$load_option, {
    updateTabItems(session, "tabs", "data")
  })
  
  
  # load data ----
  # return the telemetry obj which could be an obj or obj list
  # only use this in basic plot or models expecting tele obj or tele obj list
  # every reference of this need to check null before it initialized, like in merged_data
  datasetInput <- reactive({
    if (input$load_option == "ctmm") {
      data("buffalo")
      buffalo
    } else if (input$load_option == "upload") {
      inFile <- input$file1
      if (!is.null(inFile)) {
        as.telemetry(inFile$datapath)
      }
    } 
  })

  # merge obj list into data frame with identity column, easier for ggplot and summary
  merged_data <- reactive({
    # need to avoid call function in null input before it initialize
    tele_objs <- datasetInput()
    # if (is.null(tele_objs)) {
    #   return(NULL)
    # } else {
    #   merge_animals(tele_objs)
    # }
    merge_animals(tele_objs)
  })
  # data summary ----
  output$data_summary <- DT::renderDataTable(
    merged_data()$info_print
  )
  # outputOptions(output, "data_summary", priority = 10)
  # data plot
  output$data_plot_basic <- renderPlot({
    tele_objs <- datasetInput()
    # if (is.null(tele_objs)) {
    #   return(NULL)
    # } else {
    #   # summaries <- merge_animals(tele_objs)$summaries
    #   # if (!is.null(tele_objs))
    #     plot(tele_objs, col = rainbow(length(tele_objs)))
    #     # if added legend, the reactive value only return the plot, once switched to basic plot it will lost legend. no legend now, just use basic plot as a backup verification.
    #     # legend("top", summaries$identity, horiz = TRUE,
    #     #        fill = rainbow(length(tele_objs)))
    # }
    validate(need(!is.null(tele_objs), ""))
    plot(tele_objs, col = rainbow(length(tele_objs)))
    
    
  })
  # ggplot locations ----
  output$data_plot_gg <- renderPlot({
    merged <- merged_data()
    validate(need(!is.null(merged), ""))
    # if (!is.null(merged)) {
      animals <- merged$data
      ggplot(data = animals, aes(x, y, color = id)) + 
        geom_point(size = 0.01, alpha = 0.8) +
        labs(x = "x (meters)", y = "y (meters)") +
        coord_fixed() +
        scale_colour_hue(c = 90, l = 60) +
        theme(legend.position = "top", 
              legend.direction = "horizontal",
              legend.key.size = unit(2.5, "mm")) +
        guides(colour = guide_legend(override.aes = list(size = 2)))
    # }
  })
  # prerender ggplot plot if it didn't block too much and save some time. could turn this on if ggplot is slow.
  # outputOptions(output, "data_plot_gg", suspendWhenHidden = FALSE)
  # vario ----
  vg.animal_1 <- reactive({
    animal_1 <- datasetInput()
    variogram(animal_1)
  })
  output$vario_plot_1 <- renderPlot({plot(vg.animal_1())})
  output$vario_plot_2 <- renderPlot({plot(vg.animal_1(), fraction = 0.1)})
  output$vario_plot_3 <- renderPlot({plot(vg.animal_1(), fraction = 10 ^ input$zoom, main = sprintf("%2.1f%s", (10 ^ input$zoom) * 100, "% of Total Time-lag" ))})
  # # take snapshot of variogram
  # observeEvent(input$snapBtn, {
  #   btn <- input$snapBtn
  #   insertUI(
  #     selector = '#varioholder',
  #     ## wrap element in a div with id for ease of removal
  #     ui = plotOutput(paste0("vario_plot_", btn))
  #   )})
  # model selection ----
  # right now with all default parameter, no user selection
  selected_model <- reactive({
    # debug
    # if (debug) {
    #   cat(file = stderr(), "fitting models\n")
    # }
    animal_1 <- datasetInput()
    guessed <- ctmm.guess(animal_1, interactive = FALSE)
    withProgress(ctmm.select(animal_1, CTMM = guessed), 
                 message = "Fitting models to find the best ...")
  })
  # observe({
  #   print("observing")
  #   invisible(selected_model())
  # })
  output$model_summary <- renderPrint({
    fitted.mod <- selected_model()
    # TODO use validate
    if (is.null(fitted.mod))
      cat("No model selected yet")
    else{
      summary(fitted.mod)
    }
  })
  # try calculating model early
  # outputOptions(output, "model_summary", 
  #               suspendWhenHidden = FALSE, priority = 2)
  output$model_plot_1 <- renderPlot({
    ouf <- selected_model()
    plot(vg.animal_1(), CTMM = ouf, col.CTMM = "#1b9e77")
  })
  output$model_plot_2 <- renderPlot({
    ouf <- selected_model()
    plot(vg.animal_1(),
         CTMM = ouf,
         col.CTMM = "#1b9e77",
         fraction = 0.1)
  })
  # home range ----
  akde.animal_1 <- reactive({
    # debug
    # if (debug) {
    #   cat(file = stderr(), "akde running\n")
    # }
    animal_1 <- datasetInput()
    ouf <- selected_model()
    withProgress(akde(animal_1,CTMM = ouf), message = "Calculating home range ...")
  })
  output$range_summary <- renderPrint({
    akde1 <- akde.animal_1()
    # TODO use validate
    if (is.null(akde1))
      cat("No model selected yet")
    else{
      summary(akde1)
    }
  })
  output$range_plot_basic <- renderPlot({
    plot(datasetInput(), UD = akde.animal_1())
  })
}

shinyApp(ui, server)
