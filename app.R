## app.R ##
library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Intro", tabName = "intro", icon = icon("book"), badgeLabel = "Read me first", badgeColor = "yellow"),
    menuItem("Upload", tabName = "upload", icon = icon("upload"))

  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "intro",
            h2("Introduction to the app")
    ),

    tabItem(tabName = "upload",
            h2("Upload your data")
    )
  )
)

ui <- dashboardPage(dashboardHeader(title = "ctmm Shiny app"),
                    sidebar, body,
                    skin = "green")

server <- function(input, output) { }

shinyApp(ui, server)
