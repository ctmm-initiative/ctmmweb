## app.R ##
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    # match tabItem
    menuItem("Intro", tabName = "intro", icon = icon("book")),
    menuItem("Upload", tabName = "upload", icon = icon("upload")),
    menuItem("Time-lag", tabName = "timelag", icon = icon("line-chart")),
    menuItem("Model", tabName = "model", icon = icon("hourglass-start")),
    menuItem("Home Range", tabName = "homerange", icon = icon("map-o"))
  )
)
body <- dashboardBody(
  # match menuItem
  tabItems(
    tabItem(tabName = "intro",
            fluidPage(includeMarkdown("workflow1.md"))
    ),

    tabItem(tabName = "upload",
            h2("Upload your data")
    )
  )
)

ui <- dashboardPage(dashboardHeader(title = "Animal Movement"),
                    sidebar, body,
                    skin = "green")

server <- function(input, output) { }

shinyApp(ui, server)
