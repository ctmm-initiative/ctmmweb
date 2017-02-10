# local deployment ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, DT, ctmm, ggplot2, scales, gridExtra, data.table, markdown)
# shinyapps.io deployment vvvvvv
# library(shiny)
# library(shinydashboard)
# library(DT) # DT should be after shiny to override dataTable in shiny
# library(ctmm)
# library(ggplot2)
# library(scales)
# library(gridExtra)
# library(data.table)
# library(markdown)
# shinyapps.io deployment  ^^^^^^^
# increase the uploading file size limit to 30M
options(shiny.maxRequestSize = 30*1024^2)
# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)
# plot heights ----
height_location_box <- "800px"
height_plot_loc <- 730
height_plot_3 <- 640
height_hist_box <- "350px"
height_hist <- 280
height_hist_subset_box <- "200px"
height_hist_subset <- 150
height_selected_loc_box <- "400px"
height_selected_loc <- 380
# global variables across pages
# selected_animal_no <- 1
source("helpers.R")
header <- dashboardHeader(title = "Animal Movement")
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs", 
    # match tabItem
    menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("Subset", tabName = "subset", icon = icon("pie-chart")),
    menuItem("Variogram", tabName = "timelag", icon = icon("line-chart")),
    menuItem("Model", tabName = "model", icon = icon("hourglass-start")),
    menuItem("Home Range", tabName = "homerange", icon = icon("map-o")),
    menuItem("Report", tabName = "report", icon = icon("file-text-o")),
    menuItem("Help", tabName = "intro", icon = icon("question"))
  )
)
# p1. data boxes ----
upload_box <- box(title = "Data Source",
                  status = "info", solidHeader = TRUE, width = 6,
                  radioButtons('load_option', NULL,
                     c("Use Bufflo Data in ctmm" = 'ctmm',
                       "Upload Movebank format file" = 'upload'), 
                     selected = "upload"),
                  fileInput('file1', label = "",
                      accept = c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv')))
action_data_box <- box(title = "Select and Analyze",
                       status = "warning", solidHeader = TRUE, width = 6,
                       tags$br(),
                       fluidRow(column(5, offset = 1, actionButton("selected", "Analyze single selected"))),  tags$br(), tags$br(), tags$br(),
                       fluidRow(column(5, offset = 1, actionButton("batch", "Batch process all selected"))), tags$br())
data_summary_box <- box(title = "Data Summary", status = "primary",
    solidHeader = TRUE, width = 12,
    fluidRow(column(12, DT::dataTableOutput('data_summary'))))
location_plot_box <- tabBox(title = "Animal Locations", 
      id = "location_plot_tabs", 
      height = height_location_box, width = 12, 
      tabPanel("1. Overview", plotOutput("location_plot_gg",
                                         dblclick = "plot1_dblclick",
                                         brush = brushOpts(
                                           id = "plot1_brush",
                                           resetOnNew = TRUE
                                         ))), 
      tabPanel("2. Facet", plotOutput("location_plot_facet_fixed")), 
      tabPanel("3. Individuals", 
        fluidRow(column(6, offset = 3,
                        sliderInput("zoom_ratio", "Zoom Into Portion of Plots", 
                    min = 0.01, max = 1, value = 1))),
        plotOutput("location_plot_individual")),
      tabPanel("4. Basic Plot", plotOutput("location_plot_basic")))
# data_plot_facet_box <- tabBox(title = "Data Plot facet", 
#                               id = "facet_tabs", width = 12,
#      tabPanel("Fixed scale", plotOutput("data_plot_gg_facet_fixed")), 
#      tabPanel("Free scale", plotOutput("data_plot_gg_facet_free")) )
histogram_facet_box <- box(title = "5. Sampling Time", 
                         status = "primary", solidHeader = TRUE, 
                         width = 12, height = height_hist_box, 
                         plotOutput("histogram_facet"))
# p2. subset boxes ----
selected_summary_box <- box(title = "Selected Animal",
              status = "info", solidHeader = TRUE, 
              width = 12,
              fluidRow(column(12, DT::dataTableOutput('selected_summary'))))
histogram_subsetting_box <- box(title = "6. Select Time Range", 
                         status = "primary", solidHeader = TRUE, 
                         width = 12, height = height_hist_subset_box, 
                         fluidRow(column(2, offset = 9, actionButton("add_time",
                              "Add to Selections"))),
                         plotOutput("histogram_subsetting")
                         )
selected_plot_box <- box(title = "7. Selected Locations", 
                         status = "primary", solidHeader = TRUE, 
                         width = 12, height = height_selected_loc_box, 
                         plotOutput("selected_loc"))
selected_ranges_box <- box(title = "Selected Time Ranges",
                status = "primary", solidHeader = TRUE, width = 12,
                fluidRow(column(3, offset = 9, actionButton("analyze", "Analyze")), 
                         column(12, DT::dataTableOutput('selected_ranges'))))
# p3. variogram boxes ----
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
            # fluidRow(data_summary_box), 
            fluidRow(upload_box, action_data_box),
            fluidRow(data_summary_box),
            fluidRow(location_plot_box),
            fluidRow(histogram_facet_box)
            ), 
    tabItem(tabName = "subset",
            fluidRow(selected_summary_box,
                     histogram_subsetting_box,
                     selected_plot_box,
                     selected_ranges_box)), 
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
  # clicking browse button switch mode automatically
  observeEvent(input$file1, {
    updateRadioButtons(session, "load_option", selected = "upload")
    # updateTabItems(session, "tabs", "data")
  })
  # radio button and browse all should switch page to data
  # observeEvent(input$load_option, {
  #   updateTabItems(session, "tabs", "data")
  # })
  # p1. data ----
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
    merge_animals(tele_objs)
  })
  # data summary ----
  # have this warning in beginning.
  # Warning: Error in <-: replacement has length zero
  output$data_summary <- DT::renderDataTable({
    merged <- merged_data()
    # cannot test the merged$info_print, have to test merged directly, because null value don't have the infor_print item
    validate(need(!is.null(merged), ""))
    datatable(merged$info_print) %>%
    formatStyle('Identity', target = 'row',
                color =
                  styleEqual(merged$info_print$Identity,
                             hue_pal()(nrow(merged$info_print)))
    )}
    # merged_data()$info_print
  )
  # 3. location basic plot
  output$location_plot_basic <- renderPlot({
    tele_objs <- datasetInput()
    validate(need(!is.null(tele_objs), ""))
    plot(tele_objs, col = rainbow(length(tele_objs)))
  })
  # selected id and color ----
  selection <- reactive({
    id_vec <- merged_data()$info_print[, Identity]
    color_vec <- hue_pal()(length(id_vec))
    # table can be sorted, but always return row number in column 1
    selected_ids <- id_vec[input$data_summary_rows_selected]
    selected_colors <- color_vec[id_vec %in% selected_ids]
    if (length(selected_ids) == 0) {
      # select all when there is no selection
      return(list(ids = id_vec, colors = color_vec))
    } else {
      return(list(ids = selected_ids, colors = selected_colors))
    }
  })
  # output$debug <- renderPrint(selected_rows())
  # single selected ----
  values <- reactiveValues()
  values$selected_animal_no <- 1
  observeEvent(input$selected, {
    if (length(input$data_summary_rows_selected) != 1) {
      showNotification("Please select single Animal.", type = "error")
    } else {
      values$selected_animal_no <- input$data_summary_rows_selected
      updateTabItems(session, "tabs", "subset")
    }
  })
  # plot 1. overview ----
  values$ranges <- c(x = NULL, y = NULL)
  # ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      values$ranges$x <- c(brush$xmin, brush$xmax)
      values$ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      values$ranges$x <- NULL
      values$ranges$y <- NULL
    }
  })
  output$location_plot_gg <- renderPlot({
    merged <- merged_data()
    validate(need(!is.null(merged), ""))
    animals <- merged$data
    ggplot(data = animals, aes(x, y)) + 
      geom_point(size = 0.1, alpha = 0.6, colour = "gray") +
      geom_point(size = 0.1, alpha = 0.7, data = animals[identity %in% selection()$ids], aes(colour = id)) + 
      # coord_cartesian(xlim = values$ranges$x, ylim = values$ranges$y) +
      coord_fixed(xlim = values$ranges$x, ylim = values$ranges$y) +
      # coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) + 
      scale_color_manual(values = selection()$colors) +
      labs(x = "x (meters)", y = "y (meters)") +
      theme(legend.position = "top",
            legend.direction = "horizontal") +
      bigger_theme + bigger_key
  }, height = height_plot_loc, width = "auto")
  # plot 2. facet ----
  output$location_plot_facet_fixed <- renderPlot({
    merged <- merged_data()
    validate(need(!is.null(merged), ""))
    animals <- merged$data
    ggplot(data = animals, aes(x, y)) + 
      geom_point(size = 0.1, alpha = 1/3, data = animals, aes(colour = id)) +
      labs(x = "x (meters)", y = "y (meters)") +
      facet_grid(id ~ .) + 
      coord_fixed() +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
  }, height = height_plot_loc, width = "auto")
  # plot 3. individuals ----
  output$location_plot_individual <- renderPlot({
    merged <- merged_data()
    validate(need(!is.null(merged), ""))
    animals <- merged$data
    new_ranges <- get_ranges(animals)
    id_vector <- merged$info_print$Identity
    color_vec <- hue_pal()(length(id_vector))
    g_list <- vector("list", length = length(id_vector))
    for (i in seq_along(id_vector)) {
      data_i <- animals[identity == id_vector[i]]
      new_ranges_i <- new_ranges[identity == id_vector[i]]
      g_list[[i]] <- ggplot(data = data_i, aes(x, y)) +
        geom_point(size = 0.1, alpha = 1/3, color = color_vec[i]) +
        labs(title = id_vector[i], x = "x (meters)", y = "y (meters)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_fixed(xlim = zoom_in_range(new_ranges_i$x_start, 
                                         new_ranges_i$x_end,
                                         input$zoom_ratio), 
                    ylim = zoom_in_range(new_ranges_i$y_start, 
                                         new_ranges_i$y_end,
                                         input$zoom_ratio),
                    expand = FALSE) 
      # no bigger theme and key here since no key involved. bigger theme could mess up the axis labels too.
    }
    grid.arrange(grobs = g_list)
  }, height = height_plot_3, width = "auto")
  # plot 5. histogram facet ----
  output$histogram_facet <- renderPlot({
    merged <- merged_data()
    validate(need(!is.null(merged), ""))
    animals <- merged$data
    ggplot(data = animals, aes(x = timestamp, fill = id)) +
      geom_histogram(bins = 60) +
      facet_grid(id ~ .) +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key 
  }, height = height_hist, width = "auto")  
  # p2. subset ----
    output$selected_summary <- DT::renderDataTable({
    merged <- merged_data()
    # cannot test the merged$info_print, have to test merged directly, because null value don't have the infor_print item
    validate(need(!is.null(merged), ""))
    dt <- merged$info_print[values$selected_animal_no]
    color_vec <- hue_pal()(nrow(merged$info_print))
    selected_color <- color_vec[values$selected_animal_no]
    datatable(dt, options = list(dom = 't', ordering = FALSE)) %>%
    formatStyle('Identity', target = 'row',
                color =
                  styleEqual(dt$Identity,
                             selected_color)
    )}
    # merged_data()$info_print
  )
  # variogram ----
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
