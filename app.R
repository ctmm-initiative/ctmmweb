# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)
# use local = TRUE is equal to paste inline. default option will source in global environment
# libraries and gloabl options
source("global.R", local = TRUE)
# don't want to use ui.R name because that convention suggest to run from ui.R directly which will not work.
source("ui_part.R", local = TRUE)
source("server_part.R", local = TRUE)

shinyApp(ui, server)
