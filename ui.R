header <- dashboardHeader(title = "Animal Movement")
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # match tabItem
    # menuItem("Data", tabName = "data", icon = icon("database"),
    #          menuSubItem("Import Data", tabName = "import", icon = icon("upload")),
    #          ),
    menuItem("Import Data", tabName = "import", icon = icon("upload")),
    menuItem("Visualization", tabName = "plots", icon = icon("line-chart")),
    # menuItem("Filter and Subset", tabName = "subset_filter", icon = icon("database"),
    #          menuSubItem("Filter Outliers", tabName = "filter", icon = icon("filter")),
    #          menuSubItem("Time Subsetting", tabName = "subset", icon = icon("pie-chart"))
    #          ),
    menuItem("Filter Outliers", tabName = "filter", icon = icon("filter")),
    menuItem("Time Subsetting", tabName = "subset", icon = icon("pie-chart")),
    menuItem("Visual Diagnostics", tabName = "visual", icon = icon("stethoscope")),
    menuItem("Model Fitting", tabName = "model", icon = icon("hourglass-start")),
    menuItem("Home Range", tabName = "homerange", icon = icon("map-o")),
    menuItem("Work Report", tabName = "report", icon = icon("file-text-o")),
    menuItem("Help", tabName = "intro", icon = icon("question"))
  )
)
# p1. import ----
upload_box <- box(title = "Local Data Import",
                  height = styles$height_data_import_box,
                  status = "info", solidHeader = TRUE, width = 6,
                  radioButtons('load_option', NULL,
                               c("Use Bufflo Data in ctmm" = 'ctmm',
                                 "Upload Movebank format file" = 'upload'),
                               selected = "upload"
                  ),
                  tags$style("input[type='radio']+span{font-weight: 600;font-size: small;}"),
                  fileInput('file1', label = "")
)
movebank_login_box <- box(title = "Movebank Login",
                          status = "warning", solidHeader = TRUE, width = 6,
                          fluidRow(column(12,
                                          textInput("user", "User Name"),
                                          passwordInput("pass", label = "Password")),
                                   column(5, actionButton("login", "Login",
                                                  icon = icon("sign-in"),
                                                  style = styles$page_action)),
                                   column(5, offset = 2,
                                          help_button("login")
                                          )
                          )
)
movebank_studies_box <- box(title = "Movebank Studies",
                            status = "primary", solidHeader = TRUE, width = 12,
                            fluidRow(column(9, verbatimTextOutput("all_studies_stat")),
                                     column(3, checkboxInput("data_manager",
                                                             "Only show I'm data manager"))),
                            fluidRow(column(12, DT::dataTableOutput('studies')))
)
movebank_study_detail_box <- box(title = "Selected Study Detail",
                                 status = "primary", solidHeader = TRUE, width = 12,
                                 fluidRow(column(3,
                                                 actionButton("download",
                                            "Download",
                                               icon = icon("cloud-download"),
                                               style = styles$page_action)),
                                          column(4, offset = 1,
                                                 uiOutput("open_study")),
                                          column(3, offset = 1,
                                                 help_button("download")
                                                 )),
                                 hr(),
                                 fluidRow(column(12, DT::dataTableOutput("study_detail"))))
movebank_study_preview_box <- box(title = "Selected Study Data",
                                  status = "primary", solidHeader = TRUE, width = 12,
                                  fluidRow(column(3, downloadButton("save", "Save",
                                              icon = icon("floppy-o"),
                                              style = styles$page_action)),
                                           column(3, offset = 6,
                                                  actionButton("import",
                                           "Import",
                                               icon = icon("arrow-right"),
                                               style = styles$page_action))),
                                  hr(),
                                  fluidRow(column(12, verbatimTextOutput("study_data_response"))),
                                  fluidRow(column(12, DT::dataTableOutput('study_preview')))
)
# p2. plots ----
data_summary_box <- box(title = "Individuals", status = "info",
                        solidHeader = TRUE, width = 12,
                        fluidRow(column(12, DT::dataTableOutput('individuals'))),
                        br(),
                        fluidRow(column(3, actionButton("outlier",
                                              "Filter Outliers",
                                              icon = icon("filter"),
                                              style = styles$page_action)),
                                 column(4, offset = 1, radioButtons("time_unit",
                                                "Time range in:",
                                                c("Seconds" = "secs",
                                                  "Normal unit" = "normal"),
                                                selected = "normal",
                                                inline = TRUE)),
                                 column(3, offset = 1,
                                        actionButton("time_subset",
                                                 "Time Subsetting",
                                                 icon = icon("pie-chart"),
                                                 style = styles$page_action))
                        ))
# relying naming convention here. use plot id with postfix for event name.
location_plot_box <- tabBox(title = "Animal Locations",
                            id = "location_plot_tabs",
                            height = styles$height_location_box, width = 12,
  tabPanel("1. Overview",
   fluidRow(column(5, offset = 2,
                  sliderInput("point_size_1", "Size of points in plot",
                              min = 0.05, max = 1, value = 0.1, step = 0.05,
                              width = "100%")),
            column(5, offset = 0, br(),
                   checkboxInput("overlay_all",
                                 h4("Current page in background"),
                                 value = TRUE))),
   plotOutput("location_plot_gg",
              dblclick = "location_plot_gg_dblclick",
              brush = brushOpts(id = "location_plot_gg_brush",
                                resetOnNew = TRUE))
                    ),
  tabPanel("2. Facet", plotOutput("location_plot_facet_fixed")),
  tabPanel("3. Individuals",
           fluidRow(column(5, offset = 1,
            sliderInput("include_level", "Zoom Into Portion of Plots",
              min = 0.9, max = 1, value = 1, step = 0.001,
              width = "100%")),
            column(4, offset = 1,
                   sliderInput("point_size_3", "Size of points in plot",
                               min = 0.05, max = 0.5, value = 0.1, step = 0.05,
                               width = "100%"))),
           plotOutput("location_plot_individual")),
  tabPanel("4. Basic Plot", plotOutput("location_plot_basic")))
histogram_facet_box <- box(title = "Sampling Time",
                           height = styles$height_hist_box,
                           status = "primary", solidHeader = TRUE, width = 12,
                           plotOutput("histogram_facet"))
# p3. outlier ----
outlier_filter_box <- tabBox(title = "Outlier Detection",
                       id = "outlier_filter_tabs", width = 12,
  # p3.a distance ----
  tabPanel("Distance to center",
    fluidRow(column(4, offset = 1, sliderInput("distance_his_bins",
                                               "Histogram Bins",
                                  min = 2, max = 20, value = 7, step = 1)),
             column(4, offset = 0, sliderInput("distance_his_y_limit",
                                   "Limit y axis",
                                   min = 10, max = 50, value = 20, step = 1)),
             column(2, offset = 1, br(), br(), help_button("outlier_distance"))),
    fluidRow(column(12, plotOutput("distance_histogram",
                                   brush = brushOpts(
                                     id = "distance_his_brush",
                                     direction = "x",
                                     stroke = "purple",
                                     fill = "blue",
                                     resetOnNew = TRUE)))),
    fluidRow(column(4, offset = 1, sliderInput("distance_point_size",
                                               "Selected Point Size",
                           min = 0.1, max = 2, value = 1, step = 0.1)),
             column(4, offset = 0, sliderInput("distance_alpha",
                                               "Selected Point Alpha ",
                           min = 0.1, max = 1, value = 1, step = 0.1))),
    fluidRow(column(12, plotOutput("distance_outlier_plot",                                                      dblclick = "distance_outlier_plot_dblclick",
                                   brush = brushOpts(
                                     id = "distance_outlier_plot_brush",
                                     resetOnNew = TRUE
                                   )))),
    fluidRow(column(9, h4("Points in Range")),
             column(3, offset = 0,
                    actionButton("remove_distance_selected",
                                 "Remove Selected",
                                 icon = icon("trash-o"),
                                 style = styles$page_action))),
    hr(),
    fluidRow(column(12,
                    DT::dataTableOutput("points_in_distance_range")))
    # fluidRow(column(3, offset = 9,
    #                 actionButton("remove_distance_selected",
    #                              "Remove Selected",
    #                              icon = icon("trash-o"),
    #                              style = styles$page_action)))
    ),
  # p3.b speed ----
  tabPanel("Speed",
    fluidRow(column(4, offset = 1, sliderInput("speed_his_bins",
                                               "Histogram Bins",
                             min = 2, max = 20, value = 7, step = 1)),
             column(4, offset = 0, sliderInput("speed_his_y_limit",
                                  "Limit y axis",
                                  min = 10, max = 50, value = 20, step = 1)),
             column(2, offset = 1, br(), br(), help_button("outlier_speed"))),
    fluidRow(column(12, plotOutput("speed_histogram",
                              brush = brushOpts(
                                id = "speed_his_brush",
                                direction = "x",
                                stroke = "purple",
                                fill = "blue",
                                resetOnNew = TRUE)))),
    fluidRow(column(4, offset = 1, sliderInput("speed_point_size",
                                               "Selected Point Size",
                          min = 0.1, max = 2, value = 1, step = 0.1)),
             column(3, offset = 0, sliderInput("speed_alpha",
                                               "Selected Point Alpha ",
                          min = 0.1, max = 1, value = 1, step = 0.1)),
             column(4, checkboxGroupInput("selected_details",
                        "More Details for Selected Points:",
                        c("Draw Path Around" = "draw_speed_path",
                          "Label Row Number in Path" = "add_label")
                  ))),
    fluidRow(column(12, plotOutput("speed_outlier_plot",                                                      dblclick = "speed_outlier_plot_dblclick",
                           brush = brushOpts(
                             id = "speed_outlier_plot_brush",
                             resetOnNew = TRUE)))),
    fluidRow(column(9, h4("Points without Valid Speed"))),
    hr(),
    fluidRow(column(12,
                    DT::dataTableOutput("points_speed_non_valid"))),
    fluidRow(column(9, h4("Points in Range")),
             column(3, offset = 0,
                    actionButton("remove_speed_selected",
                                 "Remove Selected",
                                 icon = icon("trash-o"),
                                 style = styles$page_action))),
    hr(),
    fluidRow(column(12,
                    DT::dataTableOutput("points_in_speed_range")))
    # fluidRow(column(3, offset = 9,
    #                 actionButton("remove_speed_selected",
    #                              "Remove Selected",
    #                              icon = icon("trash-o"),
    #                              style = styles$page_action)))
    ))
#     select_outliers_box <- box(title = "Select Outliers",
#                                  status = "primary", solidHeader = TRUE, width = 12,
#                      fluidRow(column(12,
#                                      DT::dataTableOutput("distance_NAs"))),
#                      fluidRow(column(12,
#                              DT::dataTableOutput("points_in_distance_range"))),
#                      fluidRow(column(3, offset = 9,
#                                      actionButton("remove_distance_selected",
#                                             "Remove Selected",
#                                             icon = icon("trash-o"),
#                                             style = styles$page_action)))
# )
# outlier_plot_box <- box(title = "Selected Outliers Plot",
#                         status = "primary", solidHeader = TRUE, width = 12,
#                         fluidRow(column(12, plotOutput("outlier_plot"))))
removed_outliers_box <- box(title = "Remove Outliers",
                           status = "primary", solidHeader = TRUE, width = 12,
                     # fluidRow(column(9, h4("Marked Outliers")),
                     #          column(3,
                     #                 actionButton("remove_outliers",
                     #                        "Remove",
                     #                        icon = icon("trash-o"),
                     #                        style = styles$page_action))),
                     # fluidRow(column(12,
                     #                 DT::dataTableOutput("marked_outliers"))),
                     # hr(),
                     fluidRow(column(9, h4("Removed outliers")),
                              column(3,
                                     actionButton("reset_outliers",
                                            "Reset",
                                            icon = icon("ban"),
                                            style = styles$page_action))),
                     fluidRow(column(12,
                                     DT::dataTableOutput("removed_outliers"))))
# p4. time subsetting ----
# histogram need to wrapped in column and fluidrow to avoid out of border, which disabled the brush
histogram_subsetting_box <- box(title = "Select Time Range",
                                status = "info", solidHeader = TRUE, width = 12,
                                height = styles$height_hist_subset_box,
          fluidRow(column(6, offset = 3,
                          sliderInput("time_color_bins", "Histogram Bins",
                                      min = 2, max = 20, value = 7, step = 1))),
          fluidRow(column(12, plotOutput("histogram_subsetting",
                                         height = styles$height_hist_subset_output,
                                         brush = brushOpts(
                                           id = "time_sub_his_brush",
                                           direction = "x",
                                           stroke = "purple",
                                           fill = "blue",
                                           resetOnNew = TRUE)))))
current_range_box <- box(title = "Current Time Range",
                         status = "primary", solidHeader = TRUE, width = 12,
                         fluidRow(column(10, DT::dataTableOutput("current_range")),
                                  column(2, br(), br(),
                                         actionButton("add_time",
                                            "Add", icon = icon("plus"),
                                            style = styles$page_action))))
selected_plot_box <- box(title = "Locations in Selected Time Range",
                         status = "primary", solidHeader = TRUE, width = 12,
                         # height = height_selected_loc_box,
                         fluidRow(column(5, offset = 4,
                                 sliderInput("point_size_time_loc", "Size of selected points in plot",
                                             min = 0.05, max = 1, value = 0.1, step = 0.05,
                                             width = "100%"))),
                         plotOutput("selected_loc",
                                    dblclick = "selected_loc_dblclick",
                                    brush = brushOpts(
                                      id = "selected_loc_brush",
                                      resetOnNew = TRUE
                                    )))
selected_ranges_box <- box(title = "Selected Time Ranges",
                           status = "primary", solidHeader = TRUE, width = 12,
                           column(2, offset = 10,
                                  actionButton("reset", "Reset",
                                               icon = icon("ban"),
                                               style = styles$page_action)),
                           DT::dataTableOutput('selected_ranges'))

# p5. variogram boxes ----
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
    tabItem(tabName = "import", fluidRow(upload_box, movebank_login_box),
            fluidRow(movebank_studies_box, movebank_study_detail_box,
                     movebank_study_preview_box)),
    tabItem(tabName = "plots",
            fluidRow(data_summary_box,
                     location_plot_box,
                     histogram_facet_box)
            # fluidRow(action_data_box),
            # fluidRow(location_plot_box),
            # fluidRow(histogram_facet_box)
    ),
    tabItem(tabName = "subset",
            fluidRow(histogram_subsetting_box,
                     current_range_box,
                     selected_plot_box,
                     selected_ranges_box)),
    tabItem(tabName = "filter",
            fluidRow(outlier_filter_box,
                     # select_outliers_box,
                     # outlier_plot_box,
                     removed_outliers_box)),
    tabItem(tabName = "visual",
            fluidRow(vario_plot_box_1, vario_plot_box_2),
            fluidRow(vario_plot_box_3)),
    tabItem(tabName = "model",
            fluidRow(model_summary_box),
            fluidRow(model_plot_box_1, model_plot_box_2)),
    tabItem(tabName = "homerange",
            fluidRow(range_summary_box),
            fluidRow(range_plot_box)),
    tabItem(tabName = "report", fluidPage(includeMarkdown("help/workflow1.md")))
  )
)
# assemble UI
ui <- dashboardPage(header, sidebar, body,skin = "green")
