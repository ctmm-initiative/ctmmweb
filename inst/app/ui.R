
# header ----
# https://www.r-bloggers.com/a-little-trick-for-debugging-shiny/ run `$('#browser').show();` in browser console to show button.
# disabled because we genearate side bar dynamically, and in there js will not work to hide the button. it's possible we separate the dynamic part and static part and still add it here but need too much efforts. [add button to header](https://stackoverflow.com/questions/46231234/login-button-in-shinydashboard-dashboardheader)
header <- dashboardHeader(title = "ctmmweb",
                          tags$li(class = "dropdown", actionButton("browser", "browser"),
                                  tags$script("$('#browser').hide();")),
                          dropdownMenuOutput("messageMenu"))
# sidebar ----
sidebar <- dashboardSidebar(
  # we need to use special function instead of uiOutput
  sidebarMenuOutput("side_menus")
)
# p0.a app options ----
app_options_box <- box(title = "App Options",
                                      status = "primary", solidHeader = TRUE,
                                      width = 12,
  fluidRow(
    column(4, checkboxInput("record_on",
                             div(icon("video-camera"),
                                 HTML('&nbsp;'),
                                 "Record Actions")
                             , value = TRUE)),
    column(4, offset = 0, checkboxInput("capture_error",
                                        div(icon("stethoscope"),
                                            HTML('&nbsp;'),
                                            "Collect Diagnostic Info"),
                                        value = FALSE)),
    column(3, offset = 1, checkboxInput("parallel",
                            div(icon("cogs"),
                                HTML('&nbsp;'),
                                "Parallel Mode"),
                            value = TRUE)),
    column(3, uiOutput("view_report")),
    column(2, offset = 7, ctmmweb:::help_button("app_options"))
                                      ))
# p0.b guide ----
guide_box <- box(title = "Analysis Guide",
                       status = "primary", solidHeader = TRUE,
                       width = 12,
                       fluidRow(
                         # column(9, checkboxGroupInput("workflow_modes", label = "Select goal(s) to see required steps highlighted",
                         #                         choices = names(ctmmweb:::side_bar_modes),
                         #                         inline = TRUE)),
                         # regular checkboxgroup doesn't align in wrapped 2nd row. if we have to align each checkbox, we can use independent checkbox with columns to fix the layout. but that need manual write each option, and collect all values manually in server end, not like now I just edit a list.
                         column(10, prettyCheckboxGroup(
                           inputId = "workflow_modes", label = "Select goal(s) to see required steps highlighted",
                           choices = names(ctmmweb:::side_bar_modes),
                           # icon = icon("check-square-o"),
                           status = "success", outline = FALSE, inline = TRUE
                         )),
                         column(2, offset = 0, ctmmweb:::help_button("guide"))
                       ))
# p0.c vignette ----
vigenette_box <- box(title = "Vignettes",
                     status = "primary", solidHeader = TRUE,
                     width = 12,
                     fluidRow(column(10,
                                     fluidPage(includeMarkdown("help/0_vignette.md"))),
                              column(2, offset = 0, ctmmweb:::help_button("vignettes")))

                       )
# p1.a upload ----
upload_box <- box(title = "Upload Data",
                  status = "info", solidHeader = TRUE, width = 12,
  fluidRow(
    column(3, h4(icon("upload"), "Upload")),
    column(2, offset = 7, ctmmweb:::help_button("upload_data"))),
  fluidRow(
          column(6, fileInput('tele_file', label =
                         shiny::a("Move Bank Format Data",
                                  target = "_blank",
                                  href = "https://www.movebank.org/node/13",
                                  style = "text-decoration: underline;"),
                                 # "Movebank Format",
                         multiple = TRUE,
                         buttonLabel = "Browse or Drop...",
                         placeholder = "(multiple) csv or zip")),
          column(6, fileInput("load_saved_data", label = "Restore Progress",
                               buttonLabel = "Browse or Drop...",
                               placeholder = "Previously saved zip"
                           ))
           )
    )
# p1.b ctmm internal data ----
ctmm_import_box <- box(title = "Import from ctmm package",
                  collapsible = TRUE,
                  # collapsed = TRUE,
                  status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(12, h4(icon("database"), "Dataset in ctmm package")),
                    column(4, checkboxInput("take_sample",
                                            div(icon("filter"),
                                                HTML('&nbsp;'),
                                                "Take a sample of"),
                                            value = FALSE)),
                    column(2, numericInput("sample_size", NULL,
                                           value = 100, step = 50)),
                    column(2, offset = 0, actionButton("load_ctmm_data", "Load",
                                                       icon = icon("bolt"),
                                                       style = ctmmweb:::STYLES$page_action)),
                    column(2, offset = 2, ctmmweb:::help_button("ctmm_import")),
                    column(12, DT::DTOutput("data_set_table"))
                  )
)
# p1.c movebank studies ----
movebank_studies_box <- box(title = "Import from Movebank", collapsible = TRUE,
                            status = "warning", solidHeader = TRUE, width = 12,
      fluidRow(
        column(4, textInput("user", label = NULL, placeholder = "User Name")),
        column(3, passwordInput("pass", label = NULL, placeholder = "Password")),
        column(2, offset = 1, actionButton("login", "Login",
                               icon = icon("sign-in"),
                               style = ctmmweb:::STYLES$page_action)),
        column(2, ctmmweb:::help_button("login"))
      ),
      fluidRow(column(9, verbatimTextOutput("all_studies_stat")),
               column(3, checkboxInput("data_manager",
                                       "Switch to Studies Managed by me"))),
      fluidRow(column(12, DT::DTOutput('studies'))))
movebank_study_detail_box <- uiOutput("movebank_study_detail_box")
# movebank_study_detail_box <- box(title = "Selected Study Detail",
#                                                  width = 12,
#                                  collapsible = TRUE,
#                                  status = "primary", solidHeader = TRUE,
#      fluidRow(column(3, actionButton("download_movebank",
#                                      "Download",
#                                      icon = icon("cloud-download"),
#                                      style = ctmmweb:::STYLES$page_action)),
#               column(4, offset = 1, uiOutput("open_study")),
#               column(3, offset = 1, help_button("download_movebank")
#                      )),
#      hr(),
#      fluidRow(column(12, DT::DTOutput("study_detail"))))
movebank_downloaded_data_preview_box <- uiOutput("movebank_downloaded_data_preview_box")
# movebank_study_preview_box <- box(title = "Selected Study Data",
#                                                   width = 12,
#                                   status = "primary", solidHeader = TRUE,
#                                   collapsible = TRUE,
#       fluidRow(column(3, downloadButton("save_movebank", "Save",
#                                         icon = icon("floppy-o"),
#                                         style = ctmmweb:::STYLES$download_button)),
#                column(3, offset = 6,
#                       actionButton("import_movebank", "Import",
#                                    icon = icon("arrow-right"),
#                                    style = ctmmweb:::STYLES$page_switch))),
#       hr(),
#       fluidRow(column(12, verbatimTextOutput("study_data_response"))),
#       fluidRow(column(12, DT::DTOutput('study_preview'))))
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
        column(3, offset = 0, ctmmweb:::help_button("visual"))),
      br(),
      fluidRow(column(12, DT::DTOutput('individuals'))),
      br(),
      fluidRow(
        column(3, offset = 0, actionButton("select_all", "Select All",
                                           icon = icon("check-square-o"),
                                           style = ctmmweb:::STYLES$page_action)),
        column(2, offset = 1, checkboxInput("keep_outliers", "Keep Outliers")),
        column(3, offset = 0, downloadButton("export_rows",
                                             "Export Current",
                                             style = ctmmweb:::STYLES$download_button)),
        column(3, offset = 0, actionButton("deselect_all",
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
     column(4, offset = 0,
            sliderInput("point_size_1", "Size of Points in Plot",
                        min = 0.05, max = 1, value = 0.1, step = 0.05,
                        width = "100%")),
     column(3, offset = 1, br(), br(), actionButton("crop_loc_subset",
                                        "Crop Subset",
                                        icon = icon("crop"),
                                        style = ctmmweb:::STYLES$page_action))
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
                      width = "99%", height = "100%")),
  tabPanel("5. Error",
           fluidRow(
             # column(8, h4("Device Errors")),
             # column(12, verbatimTextOutput("error_summary")),
             column(8, radioButtons("error_plot_mode",
                                     label = "Plot With Device Error",
                                     choices = c("Error Circle" = 1,
                                                 "Error Disc" = 2,
                                                 "Densities" = 3),
                                     selected = 2, inline = TRUE)),
             column(4, br(), ctmmweb:::help_button("device_error")),
             column(12, plotOutput("error_plot"))),
           fluidRow(
             column(12, hr(), h4("Calibrate Current Data Set")),
             column(9, h5("A. Load Calibration Data")),
             column(3, offset = 0, h5("B. Or input UERE")),
             column(9, fileInput("cali_file", label = NULL, width = "100%")),
             column(3, offset = 0, numericInput("uere_num_input", label = NULL,
                                                value = 0))
           ),
           fluidRow(
             column(9, h5("Calibration Data Information")),
             column(9, verbatimTextOutput("uere_print", placeholder = TRUE)),
             column(3, offset = 0, actionButton("apply_uere",
                                                "Apply To Current",
                                                icon = icon("wrench"),
                                                style = ctmmweb:::STYLES$page_action))
           )
           )
  )
histogram_facet_box <- box(title = "6. Sampling Time",
                           # height = ctmmweb:::STYLES$height_hist_box,
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
             column(2, offset = 1, br(), ctmmweb:::help_button("outlier_distance"))),
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
                    DT::DTOutput("points_in_distance_range")))),
  # p3.b speed ----
  tabPanel("Speed",
    fluidRow(column(4, offset = 1, sliderInput("speed_his_bins",
                                               "Histogram Bins",
                             min = 2, max = 20, value = 7, step = 1)),
             column(4, offset = 0, sliderInput("speed_his_y_limit",
                                  "Limit y axis",
                                  min = 10, max = 50, value = 20, step = 1)),
             column(2, offset = 1, br(), ctmmweb:::help_button("outlier_speed"))),
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
                    DT::DTOutput("points_in_speed_range")))))
all_removed_outliers_box <- box(title = "Removed Outliers",
                           status = "primary", solidHeader = TRUE, width = 12,
               fluidRow(
                        column(4, offset = 8,
                               actionButton("reset_outliers",
                                      "Restore to Original",
                                      icon = icon("ban"),
                                      style = ctmmweb:::STYLES$page_action))
                        ),
               fluidRow(column(12,
                               DT::DTOutput("all_removed_outliers"))))
# p4. time subsetting ----
# histogram need to wrapped in column and fluidrow to avoid out of border, which disabled the brush
histogram_subsetting_box <- box(title = "Select Time Range",
                                                status = "info",
                                solidHeader = TRUE, width = 12,
                                # height = ctmmweb:::STYLES$height_hist_subset_box,
      fluidRow(column(6, offset = 0,
                      sliderInput("time_color_bins", "Histogram Bins",
                                  min = 2, max = 20, value = 7, step = 1)),
               column(2, offset = 4, br(), ctmmweb:::help_button("time_subsetting"))),
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
         column(10, DT::DTOutput("current_range")),
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
                          actionButton("clear_all_time_sub", "Clear All",
                                       icon = icon("ban"),
                                       style = ctmmweb:::STYLES$page_action)),
                   column(3, offset = 3,
                          actionButton("generate_time_sub", "Generate Subset",
                                       icon = icon("pie-chart"),
                                       style = ctmmweb:::STYLES$page_action))
                   ),
          fluidRow(column(12, DT::DTOutput('time_ranges'))))
# p5.a vario control ----
vario_control_box <- tabBox(title = "Plot Controls",
                             id = "vario_control_tabs", width = 12,
   # p5.a.1 layout ----
   tabPanel("Control",
# vario_control_box <- box(title = "Plot Controls",
#                            status = "info", solidHeader = TRUE, width = 12,
      fluidRow(
        tags$head(tags$script(HTML(ctmmweb::JS.logify(3)))),
        tags$head(tags$script(HTML(ctmmweb::JS.onload("zoom_lag_fraction")))),
        column(5, offset = 0, sliderInput("zoom_lag_fraction",
                                          "Fraction of Time-lag Range",
                                          min = -3, max = 0, step = 0.01,
                                          value = log10(0.5))),
        column(2, offset = 0, br(), radioButtons("vario_option",
                                           label = NULL,
                                           choices = c("Absolute" = "absolute",
                                                       "Relative" = "relative"),
                                           selected = "relative",
                                           inline = FALSE)),
        column(3, offset = 0, br(), numericInput("vario_height",
                                           "Figure Height",
                                           value = 250, min = 50, max = 800,
                                           step = 50)),
        column(2, offset = 0, br(), numericInput("vario_columns",
                                           "Columns",
                                           value = 2, min = 1, max = 6,
                                           step = 1)),
        column(2, offset = 10, ctmmweb:::help_button("vario_control")))),
# # p5.a.2 multiple schedules ----
tabPanel("Schedule",
      fluidRow(
        column(6, h4(shiny::a("Multiple Sampling Schedules",
                               target = "_blank",
                               href = "https://ctmm-initiative.github.io/ctmm/articles/variogram.html#irregular-sampling-schedules",
                               style = "text-decoration: underline;"))),
        # optional kmeans detection --
        column(4, offset = 0, checkboxInput("enable_kmeans",
                                            div(style = "color:#f39c12;",
                                                "Auto detect with kmeans"),
                                value = FALSE, width = "100%")),
        column(2, ctmmweb:::help_button("vario_schedule"))),
      fluidRow(column(12, uiOutput("kmeans_extra_ui")),
               column(12, hr())),
      # adding intervals --
      fluidRow(
        # choices updated in server side
        column(5, selectInput("vario_intervals_ids", label = "Identities",
                              choices = NULL, multiple = TRUE)),
        column(3, textInput("vario_intervals", label = "Intervals",
                            placeholder = "comma separated")),
        column(2, selectInput("vario_intervals_unit", label = "Time Unit",
                              choices = c("second", "minute", "hour", "day"),
                              selected = "hour")),
        column(2, div(br(), style = "line-height: 180%;"),
               actionButton("add_vario_intervals", "Add",
                               icon = icon("angle-double-down"),
                               style = ctmmweb:::STYLES$page_action))),
      fluidRow(
        column(12, h4("Added Schedules")),
        column(12, DT::DTOutput("vario_intervals_table"), br()),
        column(3, offset = 0, actionButton("remove_row_vario_intervals",
                                           "Remove Selected",
                                           icon = icon("trash-o"),
                                           style = ctmmweb:::STYLES$page_action)),
        column(3, offset = 6, actionButton("reset_vario_intervals", "Reset All",
                               icon = icon("ban"),
                               style = ctmmweb:::STYLES$page_action))
      )),
# p5.a.3 pool variogram ----
tabPanel("Pool",
         fluidRow(
           column(12, h4(shiny::a("Pool Variograms",
                                  target = "_blank",
                                  href = "https://ctmm-initiative.github.io/ctmm/articles/variogram.html#pooling-variograms",
                                  style = "text-decoration: underline;"))),
           # choices updated in server side
           column(8, selectInput("pool_vario_ids", label = NULL,
                                 choices = NULL, multiple = TRUE, width = "100%")),
           column(2,
                  actionButton("reset_pool_vario", "Reset",
                               icon = icon("ban"),
                               style = ctmmweb:::STYLES$page_action)),
           column(2,
                  actionButton("apply_pool_vario", "Pool",
                               icon = icon("pie-chart"),
                               style = ctmmweb:::STYLES$page_action))
           )
        )
)
# p5.b variograms ----
ctmm_colors <- ctmmweb:::CTMM_colors
variograms_box <- tabBox(title = "Variograms", id = "vario_tabs", width = 12,
     tabPanel(div(icon("battery-half"), "1. Empirical"), value = "1",
              fluidRow(
                column(3, div(style = ctmmweb:::STYLES$align_up_group,
                              checkboxGroupInput("guess_curve_selector",
                                                 label = NULL, inline = FALSE,
                            choiceNames = list(div(style = paste0("color:", ctmm_colors[1]),
                                                   "Original Guesstimate"),
                                               div(style = paste0("color:", ctmm_colors[2]),
                                                   "Current Guesstimate")),
                            choiceValues = names(ctmm_colors)[1:2],
                            selected = names(ctmm_colors)[1:2])
                            ),
                       checkboxInput("guess_error_on", "Turn on error"),
                       ),
                column(3, offset = 0, ctmmweb:::tuneSelectorUI("guess")),
                column(4, selectInput("IC", "IC for Model Selection",
                                   choices = c("AICc", "AIC", "BIC", "LOOCV", "HSCV"))),
                column(2, offset = 0, br(),
                       # div(style = ctmmweb:::STYLES$align_up,
                       #                    checkboxInput("guess_error_on", "Turn on error")),
                       ctmmweb:::help_button("variograms"))),
              fluidRow(
                column(12, br(), plotOutput("vario_plot_empirical",
                                             width = "99%", height = "98%"))
              )
     ),
     tabPanel(div(icon("hourglass-start"), icon("battery-full"), "2. Modeled"),
      fluidRow(
        # p5.b.1 model summary ----
        # refit tool row
        column(3, offset = 0, actionButton("refit", "Refit Selected",
                                           icon = icon("undo"),
                                           style = ctmmweb:::STYLES$page_action)),
        # adjust radiobutton vertical alignment, only change this for now. if need to change for all radiobuttons, use styles.css
        column(3, div(style = ctmmweb:::STYLES$align_up,
                    checkboxInput("refit_tuned_only", label = "Refit Fine-tuned Only")
              )),
        column(3, offset = 3, actionButton("remove_bad_models",
                                           "Clean Up",
                               icon = icon("trash-o"),
                               style = ctmmweb:::STYLES$page_action)),
        column(12, DT::DTOutput("tried_models_summary")),
        # selection tool row
        column(12, br()),
        column(3, actionButton("select_1st_models", "Select Best",
                               icon = icon("check-square-o"),
                               style = ctmmweb:::STYLES$page_action)),
        column(4, offset = 0, div(style = ctmmweb:::STYLES$align_up,
                                  checkboxInput("hide_ci_model",
                                                "Hide Confidence Intervals"))),
        column(3, offset = 2, actionButton("clear_models", "Clear Selection",
                                           icon = icon("square-o"),
                                           style = ctmmweb:::STYLES$page_action)),
        # column(12, hr()),
        # p5.b.2 model variograms ----
        column(12, hr()),
        column(4, div(style = ctmmweb:::STYLES$align_up_group,
                      checkboxGroupInput("model_curve_selector",
             label = NULL, inline = FALSE,
             choiceNames = list(div(style = paste0("color:", ctmm_colors[3]),
                                    "Initial Parameter"),
                                div(style = paste0("color:", ctmm_colors[4]),
                                    "Original Model Result"),
                                div(style = paste0("color:", ctmm_colors[5]),
                                    "Current Model Result")),
             choiceValues = names(ctmm_colors)[3:5],
             selected = names(ctmm_colors)[3:5]))
        ),
        column(5, offset = 1, ctmmweb:::tuneSelectorUI("model")),
        column(2, offset = 0, ctmmweb:::help_button("model_selection")),
        column(12, plotOutput("vario_plot_modeled",
                              width = "99%", height = "98%"))
      )
     ))
# p6. home range ----
# it's worth putting home range option and estimate action into separate boxes, one is must, one is optional
range_action_box <- box(title = "Home Range Estimation",
                        status = "info",
                        solidHeader = TRUE, width = 12,
                        # fluidRow(
                        #   # column(8, radioButtons("hrange_grid_option", "Estimate Home Range",
                        #   #                        choices = c("In Same Grid (to compare overlap)" = "same_grid",
                        #   #                                    "Separately (save memory for spread out individuals)" = "separate"),
                        #   #                        inline = FALSE)),
                        #   column(2, offset = 2, actionButton("calc_hrange", "Estimate",
                        #                                      icon = icon("map-o"),
                        #                                      style = ctmmweb:::STYLES$page_action))
                        #   ),
                        fluidRow(
                          column(4, h5(icon("balance-scale"),
                                       shiny::a("Optimal Weighting",
                                                target = "_blank",
                                                href = "https://ctmm-initiative.github.io/ctmm/articles/akde.html",
                                                style = "text-decoration: underline;font-weight: 600;"))),
                          column(2, offset = 2, checkboxInput("hrange_weight_all", "Enable All"))),
                        fluidRow(
                          column(8, selectInput("hrange_weight", label = NULL,
                                                choices = NULL, multiple = TRUE)),
                          column(2, offset = 2, ctmmweb:::help_button("home_range"))
                        )
                        )
range_plot_box <- box(title = "Home Range Plots", status = "primary",
                 solidHeader = TRUE, width = 12,
   fluidRow(
     # column(12, h4("Options")),
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
     column(5, offset = 1,
            textInput("hr_contour_text",
                      "Home Range Contours in %",
                      value = "95")),
     column(2, offset = 0, br(),
            actionButton("export_homerange_dialog", "Export",
                            icon = icon("save"),
                            style = ctmmweb:::STYLES$page_action))),
   fluidRow(
     column(12, plotOutput("range_plot",
                                  # less than 100%, otherwise out of boundary
                                  width = "99%", height = "98%"))))
range_summary_box <- box(title = "Home Range Summary",
                                         status = "primary",
                      solidHeader = TRUE, width = 12,
                      fluidRow(
                        # column(2, offset = 10, help_button("home_range")),
                        # column(12, h5("Select rows, add to group")),
                        column(12, DT::DTOutput("range_summary")),
                        column(12, hr(), h3("Meta-analysis")),
                        column(12, h4("By default the Meta-analysis treat all home ranges in table as same population. To create sub-population for meta-analysis, select rows in home range summary table, input group name, click button to group them.")),
                        column(6,
                               fluidRow(
                                 column(6, textInput("range_summary_group_input", label = NULL)),
                                 column(6, offset = 0, actionButton("group_range_summary_rows", "Make Group",
                                                                    icon = icon("pie-chart"),
                                                                    style = ctmmweb:::STYLES$page_action))
                               ),
                               fluidRow(
                                 column(6, checkboxInput("range_summary_meta_mean",
                                                         "Plot population mean estimate", value = TRUE)),
                                 column(6, offset = 0, actionButton("clear_group_range_summary", "Clear Group",
                                                                    icon = icon("ban"),
                                                                    style = ctmmweb:::STYLES$page_action))
                               ),
                               fluidRow(
                                column(12, verbatimTextOutput("range_meta_print", placeholder = TRUE))
                               )
                              ),
                        column(6, plotOutput("range_meta_plot", width = "99%", height = "98%"))

                        )
)
# meta analysis in tabbed box
# range_meta_box <- tabBox(title = "Meta-analysis",
#                             # id = "range_meta_tabs",
#                             # height = ctmmweb:::STYLES$height_location_box,
#                             width = 12,
#                             tabPanel("On Population",
#                                      fluidRow()))
# it's better not to change/update range summary table, which came from upstream result.  if changing that for group, we need to make table as reactive value and modify from two side (upstream home range and grouping actions). rather we can just select rows but group into a result table in meta box, put all text and button in meta box which is more relevant. exclude/override duplicate groups.
# range_meta_box <- box(title = "Meta-analysis",
#                             # id = "range_meta_tabs",
#                             # height = ctmmweb:::STYLES$height_location_box,
#                       status = "primary",
#                       solidHeader = TRUE,
#                       width = 12,
#                       fluidRow(
#                                column(4, verbatimTextOutput("range_meta_print", placeholder = TRUE))
#                                # column(8, plotOutput("range_meta_plot")))
#                       )
#                       )

# p7. overlap ----
overlap_summary_box <- box(title = "Overlap of Home Ranges",
                                         status = "info",
                                         solidHeader = TRUE, width = 12,
         fluidRow(
           column(2, offset = 10, ctmmweb:::help_button("overlap")),
           br(), br(),
           column(12, DT::DTOutput("overlap_summary"))
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
          tabPanel("Pairwise Plots",
           fluidRow(
             column(4, checkboxGroupInput("overlap_hrange_option", label = NULL,
                          choiceNames = list(div(icon("circle-o"),
                                                 HTML('&nbsp;'),
                                                 "Home Range Contours"),
                                             div(icon("bullseye"),
                                                 HTML('&nbsp;'),
                                                 "Confidence Envelopes"),
                                             div(icon("map-marker fa-lg"),
                                                 HTML('&nbsp;'),
                                                 "Location Points"),
                                             div(icon("adjust"),
                                                 HTML('&nbsp;'),
                                                 "Two Colors Only")),
                          choiceValues = c("contour",
                                           "interval",
                                           "location",
                                           "two_colors"),
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
             column(12, plotOutput("overlap_plot_hrange",
                        width = "99%", height = "100%")
                    )))
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
                    column(4, offset = 0,
                           textInput("oc_contour_text",
                                     "Occurrence Contours in %",
                                     value = "95")),
                    column(2, offset = 0, br(),
                           actionButton("export_occurrence_dialog", "Export",
                                        icon = icon("save"),
                                        style = ctmmweb:::STYLES$page_action)),
                    column(2, offset = 0, br(), ctmmweb:::help_button("occurrence")),
                    column(12, plotOutput("occurrence_plot",
                            width = "99%", height = "98%"))))
# p9. estimate speed ----
# to differentiate from speed outlier
speed_control_box <- box(title = "Estimate Speed", status = "info",
                                        solidHeader = TRUE, width = 12,
   fluidRow(column(3, offset = 0,
                   numericInput("estimate_speed_level", "Confidence Level", 95,
                                min = 1, max = 100, step = 1)),
            # column(4, offset = 0,
            #        # if using group input, one value change trigger the whole input value, thus label change trigger speed calculations. use align_up to reduce gap between them.
            #        checkboxInput("estimate_speed_robust",
            #                      div(icon("anchor"),
            #                          HTML('&nbsp;'),
            #                          "Use robust statistics")),
            #        # div(style = ctmmweb:::STYLES$align_up_group,
            #            checkboxInput("show_estimate_plot_label",
            #                          div(icon("font"),
            #                              HTML('&nbsp;'),
            #                              "Label Values"), value = TRUE),
            #            # ),
            #        checkboxInput("show_estimate_ci",
            #                      div(icon("anchor"),
            #                          HTML('&nbsp;'),
            #                          "Show Confidence Intervals", value = TRUE))
            #        ),
            column(3, offset = 2, numericInput("estimate_plot_height",
                                               "Canvas Height",
                                               value = 400,
                                               min = 200, max = 1200,
                                               step = 100)),
            column(3, offset = 1, br(), ctmmweb:::help_button("estimate_speed")),
            column(5, offset = 0,
                   # if using group input, one value change trigger the whole input value, thus label change trigger speed calculations. use align_up to reduce gap between them.
                   checkboxInput("show_estimate_ci",
                                 div(icon("bullseye"),
                                     HTML('&nbsp;'),
                                     "Show Confidence Intervals"), value = TRUE)
                   ),
            column(4,
                   checkboxInput("estimate_speed_robust",
                                 div(icon("anchor"),
                                     HTML('&nbsp;'),
                                     "Use robust statistics"))
            ),
            column(3, offset = 0,
                   # div(style = ctmmweb:::STYLES$align_up_group,
                   checkboxInput("show_estimate_plot_label",
                                 div(icon("font"),
                                     HTML('&nbsp;'),
                                     "Label Values"), value = TRUE))
            )
)
speed_box <- tabBox(title = NULL,
                    id = "estimate_speed_tabs", width = 12,
  # p9.a speed ----
  tabPanel("Average Speed",
    fluidRow(column(12, DT::DTOutput("estimate_speed_table")),
             column(12, plotOutput("estimate_speed_plot",
                                 width = "99%", height = "100%")))),
  # p9.b distance ----
  tabPanel("Distance Traveled",
   fluidRow(column(12, DT::DTOutput("estimate_distance_table")),
            column(12, plotOutput("estimate_distance_plot",
                                  width = "99%", height = "100%"))))
  )
# p10. map ----
map_control_box <- box(title = "Map Controls", status = "primary",
                       solidHeader = TRUE, width = 12,
  fluidRow(column(2, offset = 0,
                  numericInput("map_height", "Map Height", 600,
                               min = 400, max = 2000, step = 100)),
           column(5, offset = 1, br(), checkboxInput("apply_heat_to_point",
                                   "Apply Heatmap Range to Point Map",
                                   value = TRUE)),
           column(3, offset = 1, br(), ctmmweb:::help_button("map"))),
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
# body ----
body <- dashboardBody(
  includeCSS("www/styles.css"),
  # tags$head(tags$script(HTML(
  #   'data-goatcounter="https://ctmmwebtest.goatcounter.com/count"
  #       async src="//gc.zgo.at/count.js"'
  # ))),
  # tags$head(includeHTML(("analytics.html"))),
  # match menuItem
  tabItems(
    tabItem(tabName = "intro", fluidRow(app_options_box,
                                        guide_box,
                                        vigenette_box)),
    tabItem(tabName = "import",
                            fluidRow(upload_box,
                                     ctmm_import_box),
                            fluidRow(movebank_studies_box,
                                     movebank_study_detail_box,
                                     movebank_downloaded_data_preview_box)),
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
            fluidRow(outlier_filter_box,
                     all_removed_outliers_box)),
    tabItem(tabName = "model",
            fluidRow(vario_control_box, variograms_box
                     # , model_selection_box
                     )),
    tabItem(tabName = "homerange",
            fluidRow(range_action_box, range_plot_box,
                     range_summary_box
                     # , range_meta_box
                     )),
    tabItem(tabName = "overlap",
            fluidRow(overlap_summary_box, overlap_plot_box)),
    tabItem(tabName = "occurrence",
            fluidRow(occurrence_plot_box)),
    tabItem(tabName = "speed",
            fluidRow(speed_control_box, speed_box)),
    tabItem(tabName = "map",
            fluidRow(map_control_box, map_box))
  )
  # ,
  # tags$script(HTML(
  #   'data-goatcounter="https://ctmmwebtest.goatcounter.com/count"
  #       async src="//gc.zgo.at/count.js"'
  # ))
)
# assemble UI
ui <- dashboardPage(header, sidebar, body,skin = "green")
