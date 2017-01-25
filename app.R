# lib ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, ctmm, ggplot2)
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
                  status = "primary", solidHeader = TRUE,
                  radioButtons('load_option', 'Load Data From',
                               c("Upload File" = 'upload',
                                 "Use ctmm Bufflo Data" = 'ctmm'), selected = "upload"
                  ),
                  fileInput('file1', label = "",
                            accept = c('text/csv',
                                       'text/comma-separated-values,text/plain',
                                       '.csv')))
data_summary_box <- box(title = "Data Summary", status = "info",
                        solidHeader = TRUE, 
                        verbatimTextOutput("data_summary"))
data_plot_box <- tabBox(title = "Data Plot",
                        id = "plottabs", height = "450px", width = 12,
                        tabPanel("Basic Plot", plotOutput("data_basic")),
                        tabPanel("ggplot2", plotOutput("data_gg")))
vario_plot_box_1 <- box(title = "Variogram with up to 50% lag", id = "vario_box_1",
                        status = "info", solidHeader = TRUE,
                        plotOutput("vario_plot_1"))
vario_plot_box_2 <- box(title = "Variogram with minimal lag", id = "vario_box_2",
                        status = "info", solidHeader = TRUE,
                        plotOutput("vario_plot_2"))
vario_plot_box_3 <- box(title = "Variogram with Zoom", id = "vario_box_3",
                        status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("vario_plot_3"))


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
    tabItem(tabName = "model"),
    tabItem(tabName = "homerange"),
    tabItem(tabName = "report")
  )
)
# assemble UI
ui <- dashboardPage(header, sidebar, body,
                    skin = "green")
# server ----
server <- function(input, output) { 
  
}

shinyApp(ui, server)
