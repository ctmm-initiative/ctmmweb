# use local = TRUE is equal to paste inline. default option will source in global environment
source("global.R", local = TRUE)
source("helpers.R", local = TRUE)
# don't want to use ui.R name because that convention suggest to run from ui.R directly which will not work.
source("ui_part.R", local = TRUE)
source("server_part.R", local = TRUE)

shinyApp(ui, server)
