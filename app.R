## app.R ##
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    # match tabItem
    menuItem("Intro", tabName = "intro", icon = icon("book"), 
             badgeLabel = "Read me first", badgeColor = "yellow"),
    menuItem("Upload", tabName = "upload", icon = icon("upload"))
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

ui <- dashboardPage(dashboardHeader(title = "ctmm Shiny app"),
                    sidebar, body,
                    skin = "green")

server <- function(input, output) { }

shinyApp(ui, server)
