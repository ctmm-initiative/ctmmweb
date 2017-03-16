header <- dashboardHeader(title = "Animal Movement")
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # match tabItem
    menuItem("Data", tabName = "data", icon = icon("database"),
             menuSubItem("Import Data", tabName = "import", icon = icon("upload")),
             menuSubItem("Filter Outliers", tabName = "filter", icon = icon("filter"))),
    # menuItem("Import Data", tabName = "import", icon = icon("upload")),
    menuItem("Visualization", tabName = "plots", icon = icon("line-chart")),
    menuItem("Time Subsetting", tabName = "subset", icon = icon("pie-chart")),
    menuItem("Visual Diagnostics", tabName = "visual", icon = icon("stethoscope")),
    menuItem("Model Fitting", tabName = "model", icon = icon("hourglass-start")),
    menuItem("Home Range", tabName = "homerange", icon = icon("map-o")),
    menuItem("Work Report", tabName = "report", icon = icon("file-text-o")),
    menuItem("Help", tabName = "intro", icon = icon("question"))
  )
)
# p1. import boxes ----
upload_box <- box(title = "Local Data Import",
                  status = "info", solidHeader = TRUE,
                  width = 6, height = height_data_import_box,
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
                                                          style = page_action_style)),
                                   column(5, offset = 2,
                                          actionButton("login_help",
                                                       "Help",
                                                       icon = icon("question"),
                                                       style = help_button_style
                                          ))
                          )
)
movebank_studies_box <- box(title = "Movebank Studies",
                            status = "primary",
                            solidHeader = TRUE, width = 12,
                            fluidRow(column(9, verbatimTextOutput("all_studies_stat")),
                                     column(3, checkboxInput("data_manager", "Only show I'm data manager"))),
                            fluidRow(column(12, DT::dataTableOutput('studies')))

)
movebank_study_detail_box <- box(title = "Selected Study Detail",
                                 status = "primary",
                                 solidHeader = TRUE, width = 12,
                                 fluidRow(column(4, actionButton("download", "Download",
                                                                 icon = icon("cloud-download"),
                                                                 style = page_action_style)),
                                          column(4, uiOutput("open_study")),
                                          column(4, actionButton("download_help",
                                                                "Help",
                                                                icon = icon("question"),
                                                                style = help_button_style
                                                 ))),
                                 hr(),
                                 fluidRow(column(12, DT::dataTableOutput("study_detail"))))
movebank_study_preview_box <- box(title = "Selected Study Data",
                                  status = "primary",
                                  solidHeader = TRUE, width = 12,
                                  fluidRow(column(4, downloadButton("save", "Save",
                                                        icon = icon("floppy-o"),
                                                        style = page_action_style)),
                                           column(4, offset = 4, actionButton("import", "Import",
                                                                   icon = icon("arrow-right"),
                                                                   style = page_action_style))),
                                  hr(),
                                  fluidRow(column(12, verbatimTextOutput("study_data_response"))),
                                  fluidRow(column(12, DT::dataTableOutput('study_preview')))
)
# p2. plots boxes ----
data_summary_box <- box(title = "Data Summary", status = "info",
                        solidHeader = TRUE, width = 12,
                        fluidRow(column(12, DT::dataTableOutput('data_summary'))),
                        br(),
                        fluidRow(column(4, actionButton("batch",
                                                        "Batch process",
                                                        icon = icon("tasks"),
                                                        style = page_action_style)),
                                 column(4, radioButtons("time_unit",
                                                "Time range in:",
                                                c("Seconds" = "secs",
                                                  "Normal unit" = "normal"),
                                                selected = "normal",
                                                inline = TRUE)),
                                 column(4, actionButton("selected",
                                                                    "Analyze single",
                                                                    icon = icon("arrow-right"),
                                                                    style = page_action_style))

                        ))
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
                                        min = 0.85, max = 1, value = 1, step = 0.005,
                                        width = "100%"))),
                                     plotOutput("location_plot_individual")),
                            tabPanel("4. Basic Plot", plotOutput("location_plot_basic")))
histogram_facet_box <- box(title = "Sampling Time",
                           status = "primary", solidHeader = TRUE,
                           width = 12, height = height_hist_box,
                           plotOutput("histogram_facet"))
# p3. subset boxes ----
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
# p4. variogram boxes ----
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
