header <- dashboardHeader(title = "Animal Movement")
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # match tabItem
    # menuItem("Introduction", tabName = "intro", icon = icon("info")),
    menuItem("Import Data", tabName = "import", icon = icon("upload"),
             selected = TRUE),
    menuItem("Visualization", tabName = "plots", icon = icon("line-chart")),
    menuItem("Filter Outliers", tabName = "filter", icon = icon("filter")),
    menuItem("Time Subsetting", tabName = "subset", icon = icon("pie-chart")),
    menuItem("Model Selection", tabName = "model", icon = icon("hourglass-start")),
    menuItem("Home Range", tabName = "homerange", icon = icon("map-o")),
    menuItem("Occurrence", tabName = "occurrence", icon = icon("map-marker")),
    menuItem("Map", tabName = "map", icon = icon("globe")),
    menuItem("Work Report", tabName = "report", icon = icon("file-text-o"))
  )
  # ,
  # uiOutput("outlier_msg", inline = TRUE)
  # h4(" message about outlier")

)
# p1. import ----
upload_box <- box(title = "Local Data Import",
                  # height = styles$height_data_import_box,
                  status = "info", solidHeader = TRUE, width = 6,
  fluidRow(column(8, radioButtons('load_option', NULL,
                                  c("Use Bufflo Data in ctmm" = 'ctmm',
                                    "Use Sample of Buffalo Data" = 'ctmm_sample',
                                    "Upload Movebank format file" = 'upload'),
                                  selected = "upload")
      #             ,
      # tags$style("input[type='radio']+span{font-weight: 600;font-size: small;}")
                  ),
          column(4, numericInput("sample_size", "Sample Size",
                                 value = 100, step = 50)),
          column(12, fileInput('tele_file', label = NULL)),
          column(12, fileInput("load_session", label = "Load Saved Session"
                               # ,
                               # placeholder = "Session zip"
                           # buttonLabel = "Load Session ..."
                           )),
          column(7, checkboxInput("record_on", "Record Actions", value = TRUE)),
          column(5, offset = 0, help_button("import"))
           )
    )
movebank_login_box <- box(title = "Movebank Login",
                          status = "warning", solidHeader = TRUE, width = 6,
                          # height = styles$height_movebank_login_box,
                          fluidRow(
                            column(12, br(), br()),
                            column(12,
                                  textInput("user", "User Name"), br(),
                                  passwordInput("pass", label = "Password")),
                            column(12, br(), br()),
                            column(5, actionButton("login", "Login",
                                          icon = icon("sign-in"),
                                          style = styles$page_action)),
                            column(5, offset = 2,
                                  help_button("login")),
                            column(12, br())
                            ))
movebank_studies_box <- box(title = "Movebank Studies", collapsible = TRUE,
                            status = "primary", solidHeader = TRUE, width = 12,
      fluidRow(column(9, verbatimTextOutput("all_studies_stat")),
               column(3, checkboxInput("data_manager",
                                       "Only show I'm data manager"))),
      fluidRow(column(12, DT::dataTableOutput('studies'))))
movebank_study_detail_box <- box(title = "Selected Study Detail", width = 12,
                                 collapsible = TRUE,
                                 status = "primary", solidHeader = TRUE,
     fluidRow(column(3,
                     actionButton("download_movebank",
                "Download",
                   icon = icon("cloud-download"),
                   style = styles$page_action)),
              column(4, offset = 1,
                     uiOutput("open_study")),
              column(3, offset = 1,
                     help_button("download_movebank")
                     )),
     hr(),
     fluidRow(column(12, DT::dataTableOutput("study_detail"))))
movebank_study_preview_box <- box(title = "Selected Study Data", width = 12,
                                  status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
      fluidRow(column(3, downloadButton("save_movebank", "Save",
                                        icon = icon("floppy-o"),
                                        style = styles$download_button)),
               column(3, offset = 6,
                      actionButton("import_movebank", "Import",
                                   icon = icon("arrow-right"),
                                   style = styles$page_switch))),
      hr(),
      fluidRow(column(12, verbatimTextOutput("study_data_response"))),
      fluidRow(column(12, DT::dataTableOutput('study_preview'))))
# p2. plots ----
data_summary_box <- box(title = "1. Individuals", status = "info",
                        solidHeader = TRUE, width = 12,
      fluidRow(column(3, offset = 0, checkboxInput("time_in_sec",
                                          ("Time in Seconds"),
                                          value = FALSE)),
               column(6, uiOutput("outlier_report")),
               column(3, offset = 0, help_button("visual"))),
      fluidRow(column(12, DT::dataTableOutput('individuals'))),
      br(),
      fluidRow(column(3, offset = 0, actionButton("delete_individuals",
                                          "Delete Selected",
                                          icon = icon("trash-o"),
                                          style = styles$page_action)),
              column(3, offset = 3, actionButton("select_all", "Select All",
                                          icon = icon("check-square-o"),
                                          style = styles$page_action)),
              column(3, offset = 0, actionButton("deselect_all",
                                          "Clear Selection",
                                          icon = icon("square-o"),
                                          style = styles$page_action))
               ))
# relying naming convention here. use plot id with postfix for event name.
location_plot_box <- tabBox(title = "Animal Locations",
                            id = "location_plot_tabs",
                            # height = styles$height_location_box,
                            width = 12,
  tabPanel("2. Overview",
   fluidRow(
     column(3, offset = 1, numericInput("canvas_height", "Canvas Height", 600,
                            min = 400, max = 1200, step = 200),
            checkboxInput("overlay_all",
                          "Others in background",
                          value = TRUE)),
     column(4, offset = 3,
            sliderInput("point_size_1", "Size of points in plot",
                        min = 0.05, max = 1, value = 0.1, step = 0.05,
                        width = "100%"))
     # ,
     #        column(5, offset = 0, br(),
     #               checkboxInput("overlay_all",
     #                             "Others in background",
     #                             value = TRUE))
     ),
   plotOutput("location_plot_gg",
              dblclick = "location_plot_gg_dblclick",
              brush = brushOpts(id = "location_plot_gg_brush",
                                resetOnNew = TRUE)
              ,
              width = "99%", height = "100%"
              )
                    ),
  tabPanel("3. Facet", plotOutput("location_plot_facet_fixed",
                                  width = "99%", height = "100%")),
  tabPanel("4. Individual",
           fluidRow(
            column(2, numericInput("plot4_col", "Columns", value = 2,
                                    min = 1, max = 8, step = 1)),
            column(5, offset = 0,
              sliderInput("include_level", "Zoom Into Portion of Plots",
                min = 0.9, max = 1, value = 1, step = 0.001,
                width = "100%")),
            column(4, offset = 1,
                   sliderInput("point_size_3", "Size of points in plot",
                               min = 0.05, max = 0.5, value = 0.1, step = 0.05,
                               width = "100%"))
            ),
           plotOutput("location_plot_individual",
                      width = "99%", height = "100%"))
  # tabPanel("4. Basic Plot", plotOutput("location_plot_basic"))
  )
histogram_facet_box <- box(title = "5. Sampling Time",
                           # height = styles$height_hist_box,
                           status = "primary", solidHeader = TRUE, width = 12,
                           plotOutput("histogram_facet",
                                      width = "99%", height = "100%"))
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
             column(2, offset = 1, br(), help_button("outlier_distance"))),
    fluidRow(column(12, plotOutput("distance_histogram",
                                   brush = brushOpts(
                                     id = "distance_his_brush",
                                     direction = "x",
                                     stroke = "purple",
                                     fill = "blue",
                                     resetOnNew = TRUE),
                                   height = styles$height_outlier_hist))),
    fluidRow(column(4, offset = 1, sliderInput("distance_point_size",
                                               "Point Size for Selected Range",
                           min = 0.1, max = 2, value = 1.5, step = 0.1))
             # column(4, offset = 0, sliderInput("distance_alpha",
             #                                   "Selected Point Alpha ",
             #               min = 0.1, max = 1, value = 1, step = 0.1))
             ),
    fluidRow(column(12, plotOutput("distance_outlier_plot",                                                      dblclick = "distance_outlier_plot_dblclick",
                                   brush = brushOpts(
                                     id = "distance_outlier_plot_brush",
                                     resetOnNew = TRUE
                                   )))),
    fluidRow(column(9, h4("Points in Selected Range on Histogram"))),
    fluidRow(column(9, h5("Select rows in table to highlight")),
             column(3, offset = 0,
                    actionButton("remove_distance_selected",
                                 "Remove Selected",
                                 icon = icon("trash-o"),
                                 style = styles$page_action))),
    hr(),
    fluidRow(column(12,
                    DT::dataTableOutput("points_in_distance_range")))),
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
                                resetOnNew = TRUE),
                              height = styles$height_outlier_hist))),
    fluidRow(column(4, offset = 1, sliderInput("speed_point_size",
                                               "Point Size for Selected Range",
                          min = 0.1, max = 2, value = 1.5, step = 0.1)),
             column(5, offset = 1,
                    textInput("device_error",
                              "Standardized Device Error(meter)", 0),
                    h5("Example: GPS: 10, VHF: 100, ARGOS: 1000"))
             ),
    fluidRow(column(12, plotOutput("speed_outlier_plot",                                                      dblclick = "speed_outlier_plot_dblclick",
                           brush = brushOpts(
                             id = "speed_outlier_plot_brush",
                             resetOnNew = TRUE)))),
    fluidRow(column(6, h4("Points in Selected Range on Histogram"))
             ),
    fluidRow(
      # column(3, h5("Select rows in table")),
    # fluidRow(column(3, h4("Points in Range")),
             column(9, checkboxGroupInput("selected_details",
                         label = NULL, inline = TRUE,
                         c("Draw Path Around Selected Rows" = "draw_speed_path",
                           "Label Row Number in Path" = "add_label"))),
             column(3, offset = 0,
                    actionButton("remove_speed_selected",
                                 "Remove Selected",
                                 icon = icon("trash-o"),
                                 style = styles$page_action))),
    hr(),
    fluidRow(column(12,
                    DT::dataTableOutput("points_in_speed_range")))))
all_removed_outliers_box <- box(title = "Removed Outliers",
                           status = "primary", solidHeader = TRUE, width = 12,
               fluidRow(
                        column(3, offset = 9,
                               actionButton("reset_outliers",
                                      "Reset All",
                                      icon = icon("ban"),
                                      style = styles$page_action))
                        # column(3, offset = 6,
                        #        actionButton("delete_outlier_rows",
                        #                     "Delete Selected",
                        #                     icon = icon("trash-o"),
                        #                     style = styles$page_action))
                        ),
               fluidRow(column(12,
                               DT::dataTableOutput("all_removed_outliers"))))
# p4. time subsetting ----
# histogram need to wrapped in column and fluidrow to avoid out of border, which disabled the brush
histogram_subsetting_box <- box(title = "Select Time Range", status = "info",
                                solidHeader = TRUE, width = 12,
                                # height = styles$height_hist_subset_box,
      fluidRow(column(6, offset = 0,
                      sliderInput("time_color_bins", "Histogram Bins",
                                  min = 2, max = 20, value = 7, step = 1)),
               column(2, offset = 4, br(), help_button("time_subsetting"))),
      fluidRow(column(12, plotOutput("histogram_subsetting",
                                     height = styles$height_hist_subset_output,
                                     brush = brushOpts(
                                       id = "time_sub_his_brush",
                                       direction = "x",
                                       stroke = "purple",
                                       fill = "blue",
                                       resetOnNew = TRUE)
                                     # width = "99%", height = "100%"
                                     )),
               column(9, offset = 0, dateRangeInput('date_range',
                                                    label = 'Set date range manually'
               )),
               column(2, offset = 1, br(), actionButton("set_date_range", "Set",
                                                        icon = icon("arrow-down"),
                                                        style = styles$page_action))))
current_range_box <- box(title = "Current Time Range",
                         status = "primary", solidHeader = TRUE, width = 12,
       fluidRow(
         column(10, DT::dataTableOutput("current_range")),
         column(2, br(), br(),
                 actionButton("add_time",
                    "Add", icon = icon("plus"),
                    style = styles$page_action))))
selected_plot_box <- box(title = "Locations in Selected Time Range",
                         status = "primary", solidHeader = TRUE, width = 12,
                         # height = height_selected_loc_box,
       fluidRow(column(5, offset = 4,
               sliderInput("point_size_time_loc",
                           "Size of selected points in plot",
                           min = 0.05, max = 1, value = 0.1, step = 0.05,
                           width = "100%"))),
       plotOutput("selected_loc",
                  dblclick = "selected_loc_dblclick",
                  brush = brushOpts(
                    id = "selected_loc_brush",
                    resetOnNew = TRUE)
                  # ,
                  # width = "99%", height = "100%"
                  ))
# this is called selected_ranges/time_ranges everywhere, difficult to change as too many places involved, also some implict names.
selected_ranges_box <- box(title = "Time Range List",
                           status = "primary", solidHeader = TRUE, width = 12,
          fluidRow(column(3, offset = 0,
                          actionButton("delete_time_sub_rows",
                                       "Delete Selected",
                                       icon = icon("trash-o"),
                                       style = styles$page_action)),
                   column(3, offset = 0,
                          actionButton("reset_time_sub", "Reset All",
                                       icon = icon("ban"),
                                       style = styles$page_action)),
                   column(3, offset = 3,
                          actionButton("generate_time_sub", "Combine to New",
                                       icon = icon("pie-chart"),
                                       style = styles$page_action))
                   ),
          fluidRow(column(12, DT::dataTableOutput('time_ranges'))))
# p5. vario control ----
# vario_control_box <- tabBox(title = "Plot Controls",
#                              id = "vario_control_tabs", width = 12,
#    # p5.c.a layout ----
#    tabPanel("Control",
vario_control_box <- box(title = "Plot Controls",
                           status = "info", solidHeader = TRUE, width = 12,
      fluidRow(
        tags$head(tags$script(HTML(JS.logify(3)))),
        tags$head(tags$script(HTML(JS.onload("zoom_lag_fraction")))),
        column(6, offset = 0, sliderInput("zoom_lag_fraction",
                                          "Fraction of Time-lag range",
                                          min = -3, max = 0, step = 0.01,
                                          value = log10(0.5))),
        column(2, offset = 0, radioButtons("vario_option",
                                           label = NULL,
                                           choices = c("Absolute" = "absolute",
                                                       "Relative" = "relative"),
                                           selected = "relative",
                                           inline = FALSE)),
        column(2, offset = 0, numericInput("vario_height",
                                           "Figure height",
                                           value = 250, min = 50, max = 800,
                                           step = 50)),
        column(2, offset = 0, numericInput("vario_columns",
                                           "Columns",
                                           value = 2, min = 1, max = 6,
                                           step = 1))
      )
      # )
# ,
    # # p5.c.b irregular ----
    # tabPanel("Irregular Data",
    #          fluidRow(
    #            column(3, actionButton("para_dt", "Set dt",
    #                                   icon = icon("bar-chart"),
    #                                   style = styles$page_action)),
    #            column(3, offset = 1, actionButton("para_res", "Set res",
    #                                   icon = icon("search-plus"),
    #                                   style = styles$page_action)),
    #            column(3, offset = 2, actionButton("para_error", "Set ERROR",
    #                                   icon = icon("exclamation-triangle"),
    #                                   style = styles$page_action)),
    #            column(12, DT::dataTableOutput("irregular_para_dt")),
    #            column(3, actionButton("para_pool", "Pool Variograms",
    #                                   icon = icon("pie-chart"),
    #                                   style = styles$page_action)),
    #            column(2, offset = 7, help_button("vario_irregular"))
    #            ))
)
# p5. variograms ----
variograms_box <- box(title = "Variograms", status = "primary",
    solidHeader = TRUE, width = 12,
# variograms_box <- tabBox(title = "Variograms",
#                             id = "vario_tabs", width = 12,
  # p5.v.a empirical
  # tabPanel("Empirical",
    fluidRow(
      # column(4, offset = 0, checkboxInput("guesstimate", "Guesstimate model")),
      column(4, radioButtons("vario_mode", NULL,
                             choiceNames = list(div(icon("battery-empty"),
                                                    ("Empirical")),
                                                div(icon("battery-half"),
                                                    ("Guesstimate")),
                                                div(icon("battery-full"),
                                                    ("Modeled"))),
                             choiceValues = c("empirical", "guesstimate",
                                              "modeled")
                             )),
      column(3, offset = 0, br(), uiOutput("fit_selector")),
      column(3, br(), actionButton("fit_models", "Fit Models",
                             icon = icon("hourglass-start"),
                             style = styles$page_action)),
      # column(3, actionButton("test_digest", "test")),
      column(2, offset = 0, br(), help_button("variogram")),
      column(12, plotOutput("vario_plot_zoom",
                         # less than 100%, otherwise out of boundary because we updated figure size by parameter
                         width = "99%", height = "98%")))
  # p5.v.b model
  # tabPanel("Model",
  #          fluidRow(column(12, plotOutput("vario_plot_model",
  #                                         # less than 100%, otherwise out of boundary
  #                                         width = "99%", height = "98%"))))
)
# p5. model selection ----
model_selection_box <- box(title = "Model Selection", status = "info",
                         solidHeader = TRUE, width = 12,
  fluidRow(
           column(3, actionButton("clear_models", "Clear Selection",
                                 icon = icon("square-o"),
                                 style = styles$page_action)),
           column(4, checkboxInput("hide_ci_model",
                                         "Hide Confidence Intervals")),
           column(2, offset = 3, help_button("model_selection")),
           column(12, br()),
           column(12, DT::dataTableOutput("model_fit_summary")))
           # column(12, verbatimTextOutput("model_fit_results")))
  )
# p6. home range ----
# under_construction_box <- box(title = "Coming soon", status = "primary",
#                               solidHeader = TRUE, width = 12
#                               )
range_plot_box <- box(title = "Home Range Estimation", status = "info",
                 solidHeader = TRUE, width = 12,
   fluidRow(
     column(6, offset = 1,
            textInput("hr_level_text",
                      "% Contour level of Home Range",
                      value = 95)),
     column(3, offset = 2, br(),
            downloadButton("export_hrange",
                           "Export Shapefiles",
                           icon = icon("save"),
                           style = styles$download_button)),
     column(12, plotOutput("range_plot",
                                  # less than 100%, otherwise out of boundary
                                  width = "99%", height = "98%"))))
range_summary_box <- box(title = "Home Range Summary", status = "primary",
                      solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(4, checkboxInput("hide_ci_hrange",
                                                "Hide Confidence Intervals")),
                        column(2, offset = 6, help_button("home_range")),
                        column(12, DT::dataTableOutput("range_summary"))
                        )
)
# p7. occurrence ----
occurrence_plot_box <- box(title = "Occurrence Distribution", status = "info",
                      solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6, offset = 1,
                               textInput("oc_level_text",
                    "% Contour level of the Occurrence Distribution",
                                         value = 95)),
                        column(2, offset = 3, br(), help_button("occurrence")),
                        column(12, plotOutput("occurrence_plot",
                                width = "99%", height = "98%"))))
# p8. map ----
map_control_box <- box(title = "Map Controls", status = "primary",
                           solidHeader = TRUE, width = 12,
  fluidRow(column(2, offset = 0,
                  numericInput("map_height", "Map Height", 600,
                               min = 400, max = 2000, step = 100)),
           column(5, offset = 1, br(), checkboxInput("apply_heat_to_point",
                                   "Apply Heatmap Range to Point Map",
                                   value = TRUE)),
           column(3, offset = 1, br(), help_button("map"))),
  fluidRow(
    column(3, actionButton("reset_map_view", "Reset Map View",
                           icon = icon("ban"),
                           style = styles$page_action)),
    column(3, offset = 6, downloadButton("download_map",
                                         "Download Map",
                                         style = styles$download_button))
  ))
map_box <- tabBox(title = "Maps", id = "map_tabs", width = 12,
  tabPanel("Point",
           # use uiOutput because the height is determined in leafletOutput, so we need to move it to server side.
           fluidRow(column(12, uiOutput("point_map_holder")))),
  tabPanel("Heatmap",
           fluidRow(column(12, uiOutput("heat_map_holder"))))
  # ,
  # tabPanel("Cluster",
  #          fluidRow(column(12, uiOutput("cluster_map_holder"))))
)
# map_box <- uiOutput("map_box")
# tabsetpanel
# map_box <- box(title = "Maps", status = "info",
#                solidHeader = TRUE, width = 12,
#                tabsetPanel(
#                  tabPanel("Point",
#                           # use uiOutput because the height is determined in leafletOutput, so we need to move it to server side.
#                           fluidRow(column(12, uiOutput("point_map_holder")))),
#                  tabPanel("Heatmap",
#                           fluidRow(column(12, uiOutput("heat_map_holder")))),
#                  tabPanel("Cluster",
#                           fluidRow(column(12, uiOutput("cluster_map_holder")))),
#                  id = "map_tabs"
#                )
#
# )

# p9. work report ----
report_box <- box(title = "Report", status = "info",
                          solidHeader = TRUE, width = 12,
  fluidRow(
    column(3,
           downloadButton("save_session",
                          "Save Session",
                          style = styles$download_button),
           br(), br(),
           uiOutput("view_report")
           ),
    column(4, offset = 5,
           downloadButton("download_report_zip",
                          "Download Report zip",
                          style = styles$download_button),
           br(), br(),
           help_button("report"))
  )
                          )
# body ----
body <- dashboardBody(
  includeCSS("www/styles.css"),
  # match menuItem
  tabItems(
    # tabItem(tabName = "intro", fluidPage(includeMarkdown("help/workflow1.md"))),
    tabItem(tabName = "import", fluidRow(upload_box, movebank_login_box),
            fluidRow(movebank_studies_box, movebank_study_detail_box,
                     movebank_study_preview_box)),
    tabItem(tabName = "plots",
            fluidRow(data_summary_box,
                     location_plot_box,
                     histogram_facet_box
                     # ,
                     # all_removed_outliers_box
                     )),
    tabItem(tabName = "subset",
            fluidRow(histogram_subsetting_box,
                     current_range_box,
                     selected_plot_box,
                     selected_ranges_box)),
    tabItem(tabName = "filter",
            fluidRow(outlier_filter_box,
                     all_removed_outliers_box)),
    tabItem(tabName = "model",
            fluidRow(vario_control_box, variograms_box, model_selection_box)),
    # tabItem(tabName = "model",
    #         fluidRow(model_summary_box)
    #         # ,
    #         # fluidRow(model_plot_box_1, model_plot_box_2)
    #         ),
    tabItem(tabName = "homerange",
            # under_construction_box
            fluidRow(range_plot_box, range_summary_box)
            ),
    tabItem(tabName = "occurrence",
            fluidRow(occurrence_plot_box)),
    tabItem(tabName = "map",
            fluidRow(map_control_box, map_box)),
    # tabItem(tabName = "report", fluidPage(includeMarkdown("help/workflow1.md")))
    tabItem(tabName = "report", fluidRow(report_box))
  )
)
# assemble UI
ui <- dashboardPage(header, sidebar, body,skin = "green")
