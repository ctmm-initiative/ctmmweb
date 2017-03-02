# local deployment ----
if (!require("pacman")) install.packages("pacman")
# to make sure github packages was picked up, even we loaded ctmm in p_load_gh, still add ctmm in p_load.
# and load gh directly may fail if there is CRAN version installed and didn't get uninstalled properly in one run.
pacman::p_load_gh("ctmm-initiative/ctmm@a24eeab591c7b00a28406a9972a26878507a43a1")
pacman::p_load(shiny, shinydashboard, DT, ctmm, ggplot2, scales, gridExtra, data.table, lubridate, markdown)
# shinyapps.io deployment vvvvvv
# to make this work need to have github version ctmm installed and use library call, then deployment will try to find the github version? check this next time need deployment.
# library(shiny)
# library(shinydashboard)
# library(DT) # DT should be after shiny to override dataTable in shiny
# library(ctmm)
# library(ggplot2)
# library(scales)
# library(gridExtra)
# library(data.table)
# library(lubridate)
# library(markdown)
# shinyapps.io deployment  ^^^^^^^
# increase the uploading file size limit to 30M
options(shiny.maxRequestSize = 200*1024^2)
# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)
# UI style constants ----
height_location_box <- "800px"
height_plot_loc <- 730
height_plot_3 <- 640
# sampling time
height_hist_box <- "350px"
height_hist <- 280
# time subsetting
# not setting the box height make arrange multiple items easier.
height_hist_subset_box <- "310px"
height_hist_subset_output <- "150px"
# height_selected_loc_box <- "480px"
# height_selected_loc <- 480
page_action_style <- "background-color: #FFEB3B;"
# info box blue #00c0ef
source("helpers.R")
header <- dashboardHeader(title = "Animal Movement")
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # match tabItem
    menuItem("Import Data", tabName = "import", icon = icon("upload")),
    menuItem("Visualization", tabName = "plots", icon = icon("line-chart")),
    menuItem("Time Subsetting", tabName = "subset", icon = icon("pie-chart")),
    menuItem("Visual Diagnostics", tabName = "visual", icon = icon("stethoscope")),
    menuItem("Model Fitting", tabName = "model", icon = icon("hourglass-start")),
    menuItem("Home Range", tabName = "homerange", icon = icon("map-o")),
    menuItem("Work Report", tabName = "report", icon = icon("file-text-o")),
    menuItem("Help", tabName = "intro", icon = icon("question"))
  )
)
# p1. data boxes ----
upload_box <- box(title = "Data Source",
                  status = "info", solidHeader = TRUE, width = 8,
                  radioButtons('load_option', NULL,
                     c("Use Bufflo Data in ctmm" = 'ctmm',
                       "Upload Movebank format file" = 'upload'),
                     selected = "upload"),
                  fileInput('file1', label = "",
                      accept = c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv')))
action_data_box <- box(title = "Select and Analyze",
                       status = "warning", solidHeader = TRUE, width = 4,
                       tags$br(),
                       fluidRow(column(12, actionButton("selected",
                                              "Analyze single",
                                              icon = icon("arrow-right"),
                                              width = "100%",
                                              style = page_action_style))),
                       tags$br(), tags$br(), tags$br(),
                       fluidRow(column(12, actionButton("batch",
                                              "Batch process",
                                              icon = icon("tasks"),
                                              width = "100%",
                                              style = page_action_style))),
                       tags$br())
data_summary_box <- box(title = "Data Summary", status = "primary",
    solidHeader = TRUE, width = 12,
    fluidRow(column(12, DT::dataTableOutput('data_summary'))))
location_plot_box <- tabBox(title = "Animal Locations",
      id = "location_plot_tabs",
      height = height_location_box, width = 12,
      tabPanel("1. Overview", plotOutput("location_plot_gg",
                                         dblclick = "overview_dblclick",
                                         brush = brushOpts(
                                           id = "overview_brush",
                                           resetOnNew = TRUE
                                         ))),
      tabPanel("2. Facet", plotOutput("location_plot_facet_fixed")),
      tabPanel("3. Individuals",
        fluidRow(column(10, offset = 1,
                        sliderInput("include_level", "Zoom Into Portion of Plots",
                    min = 0.85, max = 1, value = 1, step = 0.005, width = "100%"))),
        plotOutput("location_plot_individual")),
      tabPanel("4. Basic Plot", plotOutput("location_plot_basic")))
histogram_facet_box <- box(title = "Sampling Time",
                         status = "primary", solidHeader = TRUE,
                         width = 12, height = height_hist_box,
                         plotOutput("histogram_facet"))
# p2. subset boxes ----
# histogram need to wrapped in column and fluidrow to avoid out of border, which disabled the brush
histogram_subsetting_box <- box(title = "Select Time Range",
         status = "info", solidHeader = TRUE, width = 12,
         height = height_hist_subset_box,
         fluidRow(column(6, offset = 2,
                         sliderInput("bin_count", "Color Bins",
                                     min = 2, max = 20, value = 7, step = 1))),
         fluidRow(column(12, plotOutput("histogram_subsetting",
                                        height = height_hist_subset_output,
                    brush = brushOpts(
                      id = "histo_sub_brush",
                      direction = "x",
                      stroke = "purple",
                      fill = "blue",
                      resetOnNew = TRUE)))))
current_range_box <- box(title = "Current Time Range",
                         status = "primary", solidHeader = TRUE,
                         width = 12,
                         fluidRow(column(10, DT::dataTableOutput("current_range")),
                                  column(2, br(), br(),
                                         actionButton("add_time",
                                                      "Add", icon = icon("plus")))))
selected_plot_box <- box(title = "Locations in Selected Time Range",
                         status = "primary", solidHeader = TRUE,
                         width = 12,
                         # height = height_selected_loc_box,
                         plotOutput("selected_loc"))
selected_ranges_box <- box(title = "Selected Time Ranges",
                           status = "primary", solidHeader = TRUE, width = 12,
                           column(2, offset = 10, actionButton("reset", "Reset",
                                                               icon = icon("times"))),
                           DT::dataTableOutput('selected_ranges'))
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
    tabItem(tabName = "import", fluidRow(upload_box)),
    tabItem(tabName = "plots",
            fluidRow(data_summary_box),
            fluidRow(action_data_box),
            fluidRow(location_plot_box),
            fluidRow(histogram_facet_box)
            ),
    tabItem(tabName = "subset",
            fluidRow(histogram_subsetting_box,
                     current_range_box,
                     selected_plot_box,
                     selected_ranges_box)),
    tabItem(tabName = "visual",
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
  input_data <- reactive({
    if (input$load_option == "ctmm") {
      data("buffalo")
      buffalo
    } else if (input$load_option == "upload") {
      # we can add message here for debugging. checking null in the source should remove the needs of all the null check later because the ractive value stop here
      # validate(need(!is.null(inFile), ""))
      req(input$file1)
      as.telemetry(input$file1$datapath)
    }
  })
  # merge obj list into data frame with identity column, easier for ggplot and summary
  merge_data <- reactive({
    merge_animals(input_data())
  })
  # 1.3 data summary ----
  output$data_summary <- DT::renderDataTable({
    info <- merge_data()$info_print
    datatable(info) %>%
    formatStyle('Identity', target = 'row',
                color =
                  styleEqual(info$Identity,
                             hue_pal()(nrow(info)))
    )}
  )
  # 1.4.4 location basic plot
  output$location_plot_basic <- renderPlot({
    tele_objs <- input_data()
    plot(tele_objs, col = rainbow(length(tele_objs)))
  })
  # selected ids and color ----
  select_animal <- reactive({
    id_vec <- merge_data()$info_print[, Identity]
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
  # single selected ----
  # values got updated in observeEvent need this format.
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
  # 1.4.1 overview plot ----
  values$ranges <- c(x = NULL, y = NULL)
  observeEvent(input$overview_dblclick, {
    brush <- input$overview_brush
    if (!is.null(brush)) {
      values$ranges$x <- c(brush$xmin, brush$xmax)
      values$ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      values$ranges$x <- NULL
      values$ranges$y <- NULL
    }
  })
  output$location_plot_gg <- renderPlot({
    animals <- merge_data()$data
    ggplot(data = animals, aes(x, y)) +
      geom_point(size = 0.1, alpha = 0.6, colour = "gray") +
      geom_point(size = 0.1, alpha = 0.7, data = animals[identity %in% select_animal()$ids], aes(colour = id)) +
      coord_fixed(xlim = values$ranges$x, ylim = values$ranges$y) +
      scale_color_manual(values = select_animal()$colors) +
      labs(x = "x (meters)", y = "y (meters)") +
      theme(legend.position = "top",
            legend.direction = "horizontal") +
      bigger_theme + bigger_key
  }, height = height_plot_loc, width = "auto")
  # 1.4.2 facet ----
  output$location_plot_facet_fixed <- renderPlot({
    animals <- merge_data()$data
    ggplot(data = animals, aes(x, y)) +
      geom_point(size = 0.1, alpha = 1/3, data = animals, aes(colour = id)) +
      labs(x = "x (meters)", y = "y (meters)") +
      facet_grid(id ~ .) +
      coord_fixed() +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
  }, height = height_plot_loc, width = "auto")
  # 1.4.3 individuals ----
  output$location_plot_individual <- renderPlot({
    merged <- merge_data()
    animals <- merged$data
    new_ranges <- get_ranges_quantile(input_data(), animals, input$include_level)
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
        # coord_fixed(xlim = zoom_in_range(new_ranges_i$x_start,
        #                                  new_ranges_i$x_end,
        #                                  input$zoom_ratio),
        #             ylim = zoom_in_range(new_ranges_i$y_start,
        #                                  new_ranges_i$y_end,
        #                                  input$zoom_ratio),
        #             expand = FALSE)
        coord_fixed(xlim = c(new_ranges_i$x_start, new_ranges_i$x_end),
                    ylim = c(new_ranges_i$y_start, new_ranges_i$y_end),
                    expand = FALSE)
      # no bigger theme and key here since no key involved. bigger theme could mess up the axis labels too.
    }
    grid.arrange(grobs = g_list)
  }, height = height_plot_3, width = "auto")
  # 1.5 histogram facet ----
  output$histogram_facet <- renderPlot({
    animals <- merge_data()$data
    ggplot(data = animals, aes(x = timestamp, fill = id)) +
      geom_histogram(bins = 60) +
      facet_grid(id ~ .) +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
  }, height = height_hist, width = "auto")
  # p2. subset ----
  # actually should not color by page 1 color because we will rainbow color by time
  output$selected_summary <- DT::renderDataTable({
    info <- merge_data()$info_print
    dt <- info[values$selected_animal_no]
    datatable(dt, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
    }
  )
  # selected animal data and color bins
  # when putting brush in same reactive value, every brush selection updated the whole value which update the histogram then reset brush.
  color_bin_animal <- reactive({
    bin_count <- input$bin_count
    merged <- merge_data()
    animals <- merged$data
    id_vector <- merged$info_print$Identity
    color_vec <- hue_pal()(bin_count)
    data_i <- animals[identity == id_vector[values$selected_animal_no]]
    data_i[, color_bin_factor := cut(timestamp, bin_count)]
    color_bin_start_vec_time <- ymd_hms(levels(data_i$color_bin_factor))
    color_bin_breaks <- c(color_bin_start_vec_time, data_i[t == max(t), timestamp])
    return(list(data = data_i,
                color_vec = color_vec,
                color_bin_start_vec_time = color_bin_start_vec_time,
                color_bin_breaks = color_bin_breaks))
  })
  # 2.1 histogram subsetting ----
  output$histogram_subsetting <- renderPlot({
    animal_binned <- color_bin_animal()
    ggplot(data = animal_binned$data, aes(x = timestamp)) +
      geom_histogram(breaks = as.numeric(animal_binned$color_bin_breaks), fill = animal_binned$color_vec) +
      scale_x_datetime(breaks = animal_binned$color_bin_breaks,
                       labels = date_format("%Y-%m-%d %H:%M:%S")) +
      ggtitle(animal_binned$data[1, identity]) +
      center_title +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # brush selection and matching color bins
  select_time_range <- reactive({
    animal_binned <- color_bin_animal()
    if (is.null(input$histo_sub_brush)) {
      select_start <- animal_binned$data[t == min(t), timestamp]
      select_end <- animal_binned$data[t == max(t), timestamp]
    } else {
      # brush value in seconds
      select_start <- as_datetime(input$histo_sub_brush$xmin)
      select_end <- as_datetime(input$histo_sub_brush$xmax)
    }
    select_length <- select_end - select_start
    select_start_bin <- findInterval(select_start, animal_binned$color_bin_start_vec_time)
    select_end_bin <- findInterval(select_end, animal_binned$color_bin_start_vec_time)
    selected_color <- animal_binned$color_vec[select_start_bin:select_end_bin]
    return(list(select_start = select_start, select_end = select_end,
                select_length = select_length,
                selected_color = selected_color))
  })
  # 2.2 current range ----
  output$current_range <- DT::renderDataTable({
    dt <- data.frame(start = select_time_range()$select_start,
               end = select_time_range()$select_end,
               length = select_time_range()$select_length)
    datatable(dt, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
      formatStyle(1, target = 'row', color = "#00c0ef")
  })
  # 2.3 selected locations ----
  output$selected_loc <- renderPlot({
    animal_binned <- color_bin_animal()
    time_range <- select_time_range()
    ggplot(data = animal_binned$data, aes(x, y)) +
      geom_point(size = 0.01, alpha = 0.5, colour = "gray") +
      geom_point(size = 0.01, alpha = 0.9,
                 data = animal_binned$data[timestamp >= time_range$select_start &
                                             timestamp <= time_range$select_end],
                 aes(colour = color_bin_factor)) +
      scale_colour_manual(values = time_range$selected_color) +
      labs(x = "x (meters)", y = "y (meters)") +
      coord_fixed() +
      theme(legend.position = "top",
            legend.direction = "horizontal") +
      bigger_key
  })
  # 2.4 time range table ----
  empty_ranges <- data.frame(start = NULL, end = NULL, length = NULL)
  values$selected_time_ranges <- empty_ranges
  observeEvent(input$add_time, {
    l <- list(values$selected_time_ranges,
              data.frame(start = select_time_range()$select_start,
                         end = select_time_range()$select_end,
                         length = select_time_range()$select_length))
    values$selected_time_ranges <- rbindlist(l)
  })
  observeEvent(input$reset, {
    values$selected_time_ranges <- empty_ranges
  })
  # selected_times
  output$selected_ranges <- DT::renderDataTable({
    time_range <- select_time_range()
    # dt <- data.frame(start = time_range$select_start, end = time_range$select_end,
    #            length = time_range$select_length)
    datatable(values$selected_time_ranges, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  # p3. variogram ----
  vg.animal_1 <- reactive({
    animal_1 <- input_data()
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
    animal_1 <- input_data()
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
    animal_1 <- input_data()
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
    plot(input_data(), UD = akde.animal_1())
  })
}

shinyApp(ui, server)
