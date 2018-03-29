# help module ui part ----
help_button <- function(module_id) {
  prefix <- NS(module_id)
  actionButton(prefix("help"),
               "Help",
               icon = icon("question"),
               style = ctmmweb:::STYLES$help_button
  )
}
header <- dashboardHeader(title = "Animal Movement",
            dropdownMenu(type = "messages",
               # from for first line, message 2nd line smaller font
               messageItem(
                 from = "Project in Github",
                 message = "Documentation, Source, Citation",
                 icon = icon("github"),
                 href = "https://github.com/ctmm-initiative/ctmmweb"),
               messageItem(
                 from = "Installed On",
                 message = PKG_INSTALLATION_TIME,
                 icon = icon("calendar-o")),
               messageItem(
                 from = "Issues",
                 message = "Report Issues",
                 icon = icon("exclamation-circle"),
                 href = "https://github.com/ctmm-initiative/ctmmweb/issues"),
               badgeStatus = NULL,
               icon = icon("info-circle fa-lg"),
               headerText = "App Information"
               ))
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # match tabItem, page_title in server.R need to sync with this.
    menuItem("Import Data", tabName = "import",
                             icon = icon("upload"), selected = TRUE),
    menuItem("Visualization", tabName = "plots",
                             icon = icon("line-chart")),
    menuItem("Filter Outliers", tabName = "filter",
                             icon = icon("filter")),
    menuItem("Time Subsetting", tabName = "subset",
                             icon = icon("pie-chart")),
    menuItem("Model Selection", tabName = "model",
                             icon = icon("hourglass-start")),
    menuItem("Home Range", tabName = "homerange",
                             icon = icon("map-o")),
    menuItem("Overlap", tabName = "overlap",
                             icon = icon("clone")),
    menuItem("Occurrence", tabName = "occurrence",
                             icon = icon("map-marker")),
    menuItem("Map", tabName = "map", icon = icon("globe"))
    ,
    # menuItem("Work Report", tabName = "report",
    #                          icon = icon("file-text-o")),
    br(), br(),
    fluidRow(
      column(6, offset = 0,
                      downloadButton("save_data",
                                     "Save Progress",
                                     style =
  "color: #02c1ef;background-color: #232d33;border: transparent;margin-left: 4%;")
      )
    )
  )
  # ,
  # uiOutput("outlier_msg", inline = TRUE)
  # h4(" message about outlier")
)
# p1. import ----
app_options_box <- box(title = "App Options",
                                      status = "primary", solidHeader = TRUE,
                                      width = 12,
  fluidRow(
    column(4, checkboxInput("record_on",
                             div(icon("video-camera"),
                                 HTML('&nbsp;'),
                                 "Record Actions")
                             , value = TRUE)),
    column(3, offset = 1, checkboxInput("parallel",
                            div(icon("cogs"),
                                HTML('&nbsp;'),
                                "Parallel Mode"),
                            value = TRUE)),
    column(3, offset = 1, checkboxInput("capture_error",
                            div(icon("exclamation-triangle"),
                                HTML('&nbsp;'),
                                "Capture Errors"))),
    # column(4, checkboxGroupInput("error_log_parallel", label = NULL,
    #                              choiceNames = list(
    #                                div(icon("exclamation-triangle"),
    #                                    HTML('&nbsp;'),
    #                                    "Capture Error Messages"),
    #                                div(icon("cogs"),
    #                                    HTML('&nbsp;'),
    #                                    "Parallel Mode")),
    #                              choiceValues = list("capture_error",
    #                                                  "parallel"),
    #                              selected = "parallel")),
    column(3, uiOutput("view_report")),
    column(3, offset = 6, actionButton("show_error", "Error Messages",
                           icon = icon("exclamation-triangle"),
                           style = ctmmweb:::STYLES$page_action)),
    column(12, br()),
    column(3, offset = 0, help_button("report")),
    column(3, offset = 6, help_button("app_options"))

    # if (DEBUG_BUTTON) {
    #   # debug mode, to inject browser in running. not sure if it will
    #   column(3, actionButton("inject_debug", "Debug", icon = icon("bug")))
    # },
    # column(3, offset = if (DEBUG_BUTTON) 0 else 3, help_button("app_options"))
                                      ))
upload_box <- box(title = "Local Data Import",
                  # height = ctmmweb:::STYLES$height_data_import_box,
                  status = "info", solidHeader = TRUE, width = 6,
  fluidRow(column(8, radioButtons('load_option', NULL,
                                  c("Use Bufflo Data in ctmm" = 'ctmm',
                                    "Use Sample of Buffalo Data" = 'ctmm_sample',
                                    "Upload File" = 'upload'),
                                  selected = "upload")
                  ),
          column(4, numericInput("sample_size", "Sample Size",
                                 value = 100, step = 50)),
          column(12, fileInput('tele_file', label = "Movebank Format",
                               placeholder = "csv or zip")),
          column(12, fileInput("load_data", label = "Restore Progress",
                               placeholder = "Previously saved zip"
                           )),
          column(5, offset = 7, help_button("import"))
           )
    )
movebank_login_box <- box(title = "Movebank Login",
                          status = "warning", solidHeader = TRUE, width = 6,
                          # height = ctmmweb:::STYLES$height_movebank_login_box,
                          fluidRow(
                            column(12, br(), br()),
                            column(12,
                                  textInput("user", "User Name"), br(),
                                  passwordInput("pass", label = "Password")),
                            column(12, br(), br(), br()),
                            column(5, actionButton("login", "Login",
                                          icon = icon("sign-in"),
                                          style = ctmmweb:::STYLES$page_action)),
                            column(5, offset = 2,
                                  help_button("login"))
                            # ,
                            # column(12, br())
                            ))
movebank_studies_box <- box(title = "Movebank Studies",
                                            collapsible = TRUE,
                            status = "primary", solidHeader = TRUE, width = 12,
      fluidRow(column(9, verbatimTextOutput("all_studies_stat")),
               column(3, checkboxInput("data_manager",
                                       "Only Show I'm Data Manager"))),
      fluidRow(column(12, DT::dataTableOutput('studies'))))
movebank_study_detail_box <- box(title = "Selected Study Detail",
                                                 width = 12,
                                 collapsible = TRUE,
                                 status = "primary", solidHeader = TRUE,
     fluidRow(column(3, actionButton("download_movebank",
                                     "Download",
                                     icon = icon("cloud-download"),
                                     style = ctmmweb:::STYLES$page_action)),
              column(4, offset = 1, uiOutput("open_study")),
              column(3, offset = 1, help_button("download_movebank")
                     )),
     hr(),
     fluidRow(column(12, DT::dataTableOutput("study_detail"))))
movebank_study_preview_box <- box(title = "Selected Study Data",
                                                  width = 12,
                                  status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
      fluidRow(column(3, downloadButton("save_movebank", "Save",
                                        icon = icon("floppy-o"),
                                        style = ctmmweb:::STYLES$download_button)),
               column(3, offset = 6,
                      actionButton("import_movebank", "Import",
                                   icon = icon("arrow-right"),
                                   style = ctmmweb:::STYLES$page_switch))),
      hr(),
      fluidRow(column(12, verbatimTextOutput("study_data_response"))),
      fluidRow(column(12, DT::dataTableOutput('study_preview'))))
# p2. plots ----
data_summary_box <- box(title = "1. Individuals",
                                        status = "info",
                        solidHeader = TRUE, width = 12,
      fluidRow(
        column(3, offset = 0, actionButton("delete_individuals",
                                           "Delete Selected",
                                           icon = icon("trash-o"),
                                           style = ctmmweb:::STYLES$page_action)),
        column(6, offset = 0, uiOutput("outlier_report")),
        column(3, offset = 0, help_button("visual"))),
      br(),
      fluidRow(column(12, DT::dataTableOutput('individuals'))),
      br(),
      fluidRow(
        # column(3, offset = 0, checkboxInput("time_in_sec",
        #                                   ("Time in Seconds"),
        #                                   value = FALSE)),
        column(3, offset = 0, actionButton("select_all", "Select All",
                                           icon = icon("check-square-o"),
                                           style = ctmmweb:::STYLES$page_action)),
        column(3, offset = 6, actionButton("deselect_all",
                                           "Clear Selection",
                                           icon = icon("square-o"),
                                           style = ctmmweb:::STYLES$page_action))
      )
               )
# relying naming convention here. use plot id with postfix for event name.
location_plot_box <- tabBox(title = "Animal Locations",
                            id = "location_plot_tabs",
                            # height = ctmmweb:::STYLES$height_location_box,
                            width = 12,
  tabPanel("2. Overview",
   fluidRow(
     column(3, offset = 1, numericInput("canvas_height", "Canvas Height", 600,
                            min = 400, max = 1200, step = 200),
            checkboxInput("overlay_all",
                          "Others in Background",
                          value = TRUE)),
     column(4, offset = 3,
            sliderInput("point_size_1", "Size of Points in Plot",
                        min = 0.05, max = 1, value = 0.1, step = 0.05,
                        width = "100%"))
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
                   sliderInput("point_size_3", "Size of Points in Plot",
                               min = 0.05, max = 0.5, value = 0.1, step = 0.05,
                               width = "100%"))
            ),
           plotOutput("location_plot_individual",
                      width = "99%", height = "100%"))
  # tabPanel("4. Basic Plot", plotOutput("location_plot_basic"))
  )
histogram_facet_box <- box(title = "5. Sampling Time",
                           # height = ctmmweb:::STYLES$height_hist_box,
                           status = "primary", solidHeader = TRUE, width = 12,
                           plotOutput("histogram_facet",
                                      width = "99%", height = "100%"))
# p3. outlier ----
telemetry_error_box <- box(title = "Telemetry Errors",
           status = "primary", solidHeader = TRUE, width = 12,
           fluidRow(
             column(5, offset = 1,
                    textInput("device_error",
                              "Standardized Device Error(meter)",
                              value = "10"),
                    h5("Example: GPS: 10, VHF: 100")),
             column(2, offset = 4, br(), help_button("telemetry_errors"))
             # ,
             # column(3, offset = 3, br(),
             #        actionButton("standarize_error",
             #                     "Standarize Error",
             #                     icon = icon("trash-o"),
             #                     style = ctmmweb:::STYLES$page_action))
           )
)
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
                                   height = ctmmweb:::STYLES$height_outlier_hist))),
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
    fluidRow(column(9, h5("Select Rows in Table to Highlight")),
             column(3, offset = 0,
                    actionButton("remove_distance_selected",
                                 "Remove Selected",
                                 icon = icon("trash-o"),
                                 style = ctmmweb:::STYLES$page_action))),
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
             column(2, offset = 1, br(), help_button("outlier_speed"))),
    fluidRow(column(12, plotOutput("speed_histogram",
                              brush = brushOpts(
                                id = "speed_his_brush",
                                direction = "x",
                                stroke = "purple",
                                fill = "blue",
                                resetOnNew = TRUE),
                              height = ctmmweb:::STYLES$height_outlier_hist))),
    fluidRow(column(4, offset = 1, sliderInput("speed_point_size",
                                               "Point Size for Selected Range",
                          min = 0.1, max = 2, value = 1.5, step = 0.1))
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
                                 style = ctmmweb:::STYLES$page_action))),
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
                                      style = ctmmweb:::STYLES$page_action))
                        ),
               fluidRow(column(12,
                               DT::dataTableOutput("all_removed_outliers"))))
# p4. time subsetting ----
# histogram need to wrapped in column and fluidrow to avoid out of border, which disabled the brush
histogram_subsetting_box <- box(title = "Select Time Range",
                                                status = "info",
                                solidHeader = TRUE, width = 12,
                                # height = ctmmweb:::STYLES$height_hist_subset_box,
      fluidRow(column(6, offset = 0,
                      sliderInput("time_color_bins", "Histogram Bins",
                                  min = 2, max = 20, value = 7, step = 1)),
               column(2, offset = 4, br(), help_button("time_subsetting"))),
      fluidRow(column(12, plotOutput("histogram_subsetting",
                                     height =
                                       ctmmweb:::STYLES$height_hist_subset_output,
                                     brush = brushOpts(
                                       id = "time_sub_his_brush",
                                       direction = "x",
                                       stroke = "purple",
                                       fill = "blue",
                                       resetOnNew = TRUE)
                                     # width = "99%", height = "100%"
                                     )),
               column(9, offset = 0, dateRangeInput('date_range',
                           label = 'Set Date Range Manually'
               )),
               column(2, offset = 1, br(),
                      actionButton("set_date_range", "Set",
                                   icon = icon("arrow-down"),
                                   style = ctmmweb:::STYLES$page_action))))
current_range_box <- box(title = "Current Time Range",
                         status = "primary", solidHeader = TRUE, width = 12,
       fluidRow(
         column(10, DT::dataTableOutput("current_range")),
         column(2, br(), br(),
                 actionButton("add_time",
                    "Add", icon = icon("plus"),
                    style = ctmmweb:::STYLES$page_action))))
selected_plot_box <- box(title = "Locations in Selected Time Range",
                         status = "primary", solidHeader = TRUE, width = 12,
                         # height = height_selected_loc_box,
       fluidRow(column(5, offset = 4,
               sliderInput("point_size_time_loc",
                           "Size of Selected Points in Plot",
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
                                       style = ctmmweb:::STYLES$page_action)),
                   column(3, offset = 0,
                          actionButton("reset_time_sub", "Reset All",
                                       icon = icon("ban"),
                                       style = ctmmweb:::STYLES$page_action)),
                   column(3, offset = 3,
                          actionButton("generate_time_sub", "Generate Subset",
                                       icon = icon("pie-chart"),
                                       style = ctmmweb:::STYLES$page_action))
                   ),
          fluidRow(column(12, DT::dataTableOutput('time_ranges'))))
# p5. vario control ----
# vario_control_box <- tabBox(title = "Plot Controls",
#                              id = "vario_control_tabs", width = 12,
#    # p5.1 layout ----
#    tabPanel("Control",
vario_control_box <- box(title = "Plot Controls",
                           status = "info", solidHeader = TRUE, width = 12,
      fluidRow(
        tags$head(tags$script(HTML(ctmmweb::JS.logify(3)))),
        tags$head(tags$script(HTML(ctmmweb::JS.onload("zoom_lag_fraction")))),
        column(6, offset = 0, sliderInput("zoom_lag_fraction",
                                          "Fraction of Time-lag Range",
                                          min = -3, max = 0, step = 0.01,
                                          value = log10(0.5))),
        column(2, offset = 0, radioButtons("vario_option",
                                           label = NULL,
                                           choices = c("Absolute" = "absolute",
                                                       "Relative" = "relative"),
                                           selected = "relative",
                                           inline = FALSE)),
        column(2, offset = 0, numericInput("vario_height",
                                           "Figure Height",
                                           value = 250, min = 50, max = 800,
                                           step = 50)),
        column(2, offset = 0, numericInput("vario_columns",
                                           "Columns",
                                           value = 2, min = 1, max = 6,
                                           step = 1)),
        column(2, offset = 10, help_button("vario_control"))
      )
      # )
# ,
    # # p5.2 irregular ----
    # tabPanel("Irregular Data",
    #          fluidRow(
    #            column(3, actionButton("para_dt", "Set dt",
    #                                   icon = icon("bar-chart"),
    #                                   style = ctmmweb:::STYLES$page_action)),
    #            column(3, offset = 1, actionButton("para_res", "Set res",
    #                                   icon = icon("search-plus"),
    #                                   style = ctmmweb:::STYLES$page_action)),
    #            column(3, offset = 2, actionButton("para_error", "Set ERROR",
    #                                   icon = icon("exclamation-triangle"),
    #                                   style = ctmmweb:::STYLES$page_action)),
    #            column(12, DT::dataTableOutput("irregular_para_dt")),
    #            column(3, actionButton("para_pool", "Pool Variograms",
    #                                   icon = icon("pie-chart"),
    #                                   style = ctmmweb:::STYLES$page_action)),
    #            column(2, offset = 7, help_button("vario_irregular"))
    #            ))
)
# p5.3 variograms ----
variograms_box <- tabBox(title = "Variograms", id = "vario_tabs", width = 12,
     tabPanel(div(icon("battery-empty"), "1. Empirical"), value = "1",
              fluidRow(
                column(2, offset = 10, help_button("variograms")),
                column(12, br(), plotOutput("vario_plot_1",
                                             width = "99%", height = "98%"))
              )
     ),
     tabPanel(div(icon("battery-half"), "2. Guesstimate"),
              fluidRow(
                column(3, offset = 0, uiOutput("tune_selector")),
                column(12, plotOutput("vario_plot_2",
                                      width = "99%", height = "98%"))
              )
     ),
     tabPanel(div(icon("hourglass-start"), icon("battery-full"), "3. Modeled"),
      fluidRow(
        # column(3, offset = 0, actionButton("try_models", "Try Models",
        #                              icon = icon("hourglass-start"),
        #                              style = ctmmweb:::STYLES$page_action),
        #        br(), br()),
        column(2, offset = 10, help_button("model_selection")),
        column(12, plotOutput("vario_plot_3",
                              width = "99%", height = "98%")),
        # model selection table
        column(12, hr()),
        column(3, actionButton("select_1st_models", "Select Best Models",
                               icon = icon("square-o"),
                               style = ctmmweb:::STYLES$page_action)),
        column(4, offset = 1, checkboxInput("hide_ci_model",
                                "Hide Confidence Intervals")),
        column(3, offset = 1, actionButton("clear_models", "Clear Selection",
                               icon = icon("square-o"),
                               style = ctmmweb:::STYLES$page_action)),
        # column(2, offset = 3, help_button("model_selection")),
        column(12, br()),
        column(12, DT::dataTableOutput("tried_models_summary"))
      )
     ))
# p6. home range ----
range_plot_box <- box(title = "Home Range Estimation",
                                      status = "info",
                 solidHeader = TRUE, width = 12,
   fluidRow(
     # column(3, offset = 0, br(), checkboxInput("hrange_hide_contours",
     #                                     "Hide Contours",
     #                                     value = FALSE)),
     # we could put this into a function, but occurrence only use 2 of 3, and every one have different default values.
     column(4, checkboxGroupInput("hrange_option", label = NULL,
                  choiceNames = list(div(icon("circle-o"),
                                         HTML('&nbsp;'),
                                         "Home Range Contours"),
                                     div(icon("bullseye"),
                                         HTML('&nbsp;'),
                                         "Confidence Envelopes"),
                                     div(icon("map-marker fa-lg"),
                                         HTML('&nbsp;'),
                                         "Location Points")),
                  choiceValues = c("contour",
                                   "interval",
                                   "location"),
                  selected = c("contour",
                               "interval",
                               "location"))),
     column(5, offset = 0,
            textInput("hr_contour_text",
                      "Home Range Contours in %",
                      value = "95")),
     column(2, offset = 1, br(),
            actionButton("export_homerange_dialog", "Export",
                            icon = icon("save"),
                            style = ctmmweb:::STYLES$page_action)),
     # column(3, offset = 0, br(),
     #        downloadButton("export_raster",
     #                       "Export Raster",
     #                       icon = icon("save"),
     #                       style = ctmmweb:::STYLES$download_button)),
     # column(3, offset = 0, br(),
     #        downloadButton("export_hrange",
     #                       "Export Shapefiles",
     #                       icon = icon("save"),
     #                       style = ctmmweb:::STYLES$download_button)),
     column(12, plotOutput("range_plot",
                                  # less than 100%, otherwise out of boundary
                                  width = "99%", height = "98%"))))
range_summary_box <- box(title = "Home Range Summary",
                                         status = "primary",
                      solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(4, checkboxInput("hide_ci_hrange",
                                                "Hide Confidence Intervals")),
                        column(2, offset = 6, help_button("home_range")),
                        column(12, DT::dataTableOutput("range_summary"))
                        )
)
# p7. overlap ----
overlap_summary_box <- box(title = "Overlap of Home Ranges",
                                         status = "info",
                                         solidHeader = TRUE, width = 12,
         fluidRow(
           column(2, offset = 10, help_button("overlap")),
           br(), br(),
           column(12, DT::dataTableOutput("overlap_summary"))
         )
)
overlap_plot_box <- tabBox(title = "Plot", id = "overlap_tabs", width = 12,
          tabPanel("Overlap Values",
                   fluidRow(
                     column(3, offset = 1, numericInput("overlap_plot_height",
                                                        "Canvas Height",
                                                        value = 600,
                                                        min = 200, max = 1200,
                                                        step = 100)),
                     column(3, offset = 5, br(),
                            checkboxInput("show_overlap_label",
                                          "Label Values", value = TRUE)),
                     column(12,
                             plotOutput("overlap_plot_value_range",
                                        width = "99%", height = "100%")))),
          tabPanel("Home Range",
           fluidRow(
             # column(3, checkboxGroupInput("hrange_control", label = NULL,
             #                              choices = c("Hide Contours",
             #                                          "Confidence envelopes",
             #                                          "Location points"))),
             column(4, checkboxGroupInput("overlap_hrange_option", label = NULL,
                          choiceNames = list(div(icon("circle-o"),
                                                 HTML('&nbsp;'),
                                                 "Home Range Contours"),
                                             div(icon("bullseye"),
                                                 HTML('&nbsp;'),
                                                 "Confidence Envelopes"),
                                             div(icon("map-marker fa-lg"),
                                                 HTML('&nbsp;'),
                                                 "Location Points")),
                          choiceValues = c("contour",
                                           "interval",
                                           "location"),
                          selected = "contour")),
             column(4, offset = 0, textInput("overlap_hrange_contour_text",
                                             "Home Range Contours in %",
                                             value = "95")),
             column(2, offset = 0, numericInput("overlap_hrange_height",
                                                "Figure Height",
                                                value = 250,
                                                min = 50, max = 800,
                                                step = 50)),
             column(2, offset = 0, numericInput("overlap_hrange_columns",
                                                "Columns",
                                                value = 2, min = 1, max = 6,
                                                step = 1)),
             # column(3, offset = 0, checkboxInput("overlap_hide_contours",
             #                                     "Hide Contours",
             #                                     value = FALSE)),
             # column(3, offset = 0,
             #        checkboxInput("overlap_hrange_envelopes",
             #                      "Confidence envelopes",
             #                      value = FALSE)),
             # column(3, offset = 1, checkboxInput("overlap_location_point",
             #                                     "Location points",
             #                                     value = FALSE)),
             column(12, plotOutput("overlap_plot_hrange",
                        width = "99%", height = "100%")
                    )))
          # ,
          # tabPanel("Location",
          #  fluidRow(
          #    column(2, offset = 1, numericInput("overlap_loc_height",
          #                                       "Canvas height",
          #                                       value = 600,
          #                                       min = 200, max = 1200,
          #                                       step = 100)),
          #    column(2, offset = 6, numericInput("overlap_loc_columns",
          #                                       "Columns",
          #                                       value = 1, min = 1, max = 6,
          #                                       step = 1)),
          #    column(12,
          #           plotOutput("overlap_plot_location",
          #              dblclick = "overlap_plot_location_dblclick",
          #              brush = brushOpts(id = "overlap_plot_location_brush",
          #                                resetOnNew = TRUE),
          #              width = "99%", height = "100%")
          #           )))
)
# p8. occurrence ----
occurrence_plot_box <- box(title = "Occurrence Distribution",
                                           status = "info",
                      solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4, checkboxGroupInput("occur_option", label = NULL,
                           choiceNames = list(div(icon("circle-o"),
                                                  HTML('&nbsp;'),
                                                  "Occurrence Contours"),
                                              # div(icon("bullseye"),
                                              #     HTML('&nbsp;'),
                                              #     "Confidence envelopes"),
                                              div(icon("map-marker fa-lg"),
                                                  HTML('&nbsp;'),
                                                  "Location Points")),
                           choiceValues = c("contour",
                                            # "interval",
                                            "location"),
                           selected = "contour")),
                    column(5, offset = 0,
                           textInput("oc_contour_text",
                                     "Occurrence Distribution Contours in %",
                                     value = "95")),
                    # column(3, offset = 0, br(), checkboxInput("oc_hide_contours",
                    #                                           "Hide Contours",
                    #                                           value = FALSE)),
                    column(2, offset = 1, br(), help_button("occurrence")),
                    column(12, plotOutput("occurrence_plot",
                            width = "99%", height = "98%"))))
# p9. map ----
map_control_box <- box(title = "Map Controls",
                                       status = "primary",
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
                           style = ctmmweb:::STYLES$page_action)),
    column(3, offset = 6, downloadButton("download_map",
                                         "Download Map",
                                         style = ctmmweb:::STYLES$download_button))
  ))
map_box <- tabBox(title = "Maps", id = "map_tabs", width = 12,
  tabPanel("Point",
           # use uiOutput because the height is determined in leafletOutput, so we need to move it to server side.
           fluidRow(column(12, uiOutput("point_map_holder")))),
  tabPanel("Heatmap",
           fluidRow(column(12, uiOutput("heat_map_holder"))))
)
# p10. work report ---
# report_box <- box(title = "Report", status = "info",
#                           solidHeader = TRUE, width = 12,
#   fluidRow(
#     # column(3,
#     #        # downloadButton("save_data",
#     #        #                "Save Data",
#     #        #                style = ctmmweb:::STYLES$download_button),
#     #        br(), br(),
#     #        # uiOutput("view_report")
#     #        ),
#     # column(4, offset = 1, checkboxInput("save_tele",
#     #                                     "Save Telemetry Data")),
#     column(4, offset = 5,
#            downloadButton("download_report_zip",
#                           "Download Report as zip",
#                           style = ctmmweb:::STYLES$download_button),
#            br(), br(),
#            help_button("report"))
#   ))
# show debug information in app, because hosted app log often mess up
# debug_box <- box(title = "Debug", status = "primary",
#                                  solidHeader = TRUE, width = 12,
#    fluidRow(
#      column(12, verbatimTextOutput("session_info")),
#      column(12, verbatimTextOutput("occurrence_info"))
#    ))
# error_log_box <- uiOutput("error_log_box")
# body ----
body <- dashboardBody(
  includeCSS("www/styles.css"),
  # match menuItem
  tabItems(
    # tabItem(tabName = "intro", fluidPage(includeMarkdown("help/workflow1.md"))),
    tabItem(tabName = "import",
                            fluidRow(app_options_box,
                                     upload_box, movebank_login_box),
                            fluidRow(movebank_studies_box,
                                     movebank_study_detail_box,
                                     movebank_study_preview_box)),
    tabItem(tabName = "plots",
            fluidRow(data_summary_box,
                     location_plot_box,
                     histogram_facet_box
                     )),
    tabItem(tabName = "subset",
            fluidRow(histogram_subsetting_box,
                     current_range_box,
                     selected_plot_box,
                     selected_ranges_box)),
    tabItem(tabName = "filter",
            fluidRow(telemetry_error_box, outlier_filter_box,
                     all_removed_outliers_box)),
    tabItem(tabName = "model",
            fluidRow(vario_control_box, variograms_box
                     # , model_selection_box
                     )),
    tabItem(tabName = "homerange",
            fluidRow(range_plot_box, range_summary_box)),
    tabItem(tabName = "overlap",
            fluidRow(overlap_summary_box, overlap_plot_box)),
    tabItem(tabName = "occurrence",
            fluidRow(occurrence_plot_box)),
    tabItem(tabName = "map",
            fluidRow(map_control_box, map_box))
    # ,
    # tabItem(tabName = "report",
    #                         fluidRow(report_box))
  )
)
# assemble UI
ui <- dashboardPage(header, sidebar, body,skin = "green")
