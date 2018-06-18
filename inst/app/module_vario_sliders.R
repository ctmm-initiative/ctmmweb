vario_sliders_Input <- function(id, dialog_title) {
  ns <- NS(id)
  modalDialog(title = dialog_title,
              fluidRow(column(4, uiOutput(ns("fit_sliders"))),
                       column(8, plotOutput(ns("fit_plot"))),
                       column(4, offset = 2, uiOutput(ns("fit_zoom")))),
              size = "l",
              footer = fluidRow(
                column(3, actionButton(ns("center_slider"),
                                       "Center current sliders",
                                       icon = icon("align-center"))),
                column(3, offset = 2,
                       modalButton("Cancel", icon = icon("ban"))),
                column(2, offset = 2,
                       actionButton(ns("tuned"), "Apply",
                                    icon = icon("check"),
                                    style = ctmmweb:::STYLES$page_action)))
  )
}


# init values of sliders ----
init_slider_values <- reactive({
  vario_list <- req(select_data_vario()$vario_list)
  ids <- names(vario_list)
  vario <- vario_list[ids == input$tune_selected][[1]]
  CTMM <- values$selected_data_guess_list[ids == input$tune_selected][[1]]
  fraction <- 10 ^ input$zoom_lag_fraction
  STUFF <- ctmm:::variogram.fit.backend(vario, CTMM = CTMM,
                                        fraction = fraction, b = 10)
  dt <- data.table(STUFF$DF)
  dt[, name := row.names(STUFF$DF)]
  # zoom slider used different base, and minus 1 from min,max.
  dt[name == "z", c("min", "max") := list(min - 1, max - 1)]
  # initial is taken from last page control directly
  dt[name == "z", initial := input$zoom_lag_fraction]
  # didn't use the step value in ctmm. use 0.001 instead, but need to adjust for error when data is calibrated
  dt[, step := 0.001]
  if ("MSE" %in% names(vario)) {
    dt[name == "error", step := 1]
  }
  slider_list <- lapply(1:nrow(dt), function(i) {
    sliderInput(
      inputId = paste0("vfit_", dt[i, name]),
      label = dt[i, label], min = round(dt[i, min], 3),
      max = round(dt[i, max], 3), value = round(dt[i, initial], 3),
      step = dt[i, step])
  })
  names(slider_list) <- dt$name

  # zoom is for view only, separate it from others
  return(list(vario = vario, STUFF = STUFF,
              control_dt = dt[name != "z"],
              control_sliders = slider_list[names(slider_list) != "z"],
              zoom_slider = slider_list[names(slider_list) == "z"]))
})
# init control sliders
output$fit_sliders <- renderUI({
  req(init_slider_values()$control_sliders)
})
output$fit_zoom <- renderUI({
  list(tags$head(tags$script(HTML(ctmmweb::JS.onload("vfit_z")))),
       req(init_slider_values()$zoom_slider))
})
observeEvent(input$center_slider, {
  adjust_slider <- function(name) {
    # Shiny will complain for named vector
    id <- paste0("vfit_", name)
    # error slider usually have initial value of 0, double that will get 0.
    if (input[[id]] != 0) {
      updateSliderInput(session, id,
                        max = round(input[[id]] * 2, 2))
    }
  }
  lapply(init_slider_values()$control_dt$name, adjust_slider)
})
# current CTMM according to sliders
slider_to_CTMM <- reactive({
  # there is a time when sliders are initialized but without value, then later storer call get NULL parameters
  req(!is.null(input$vfit_sigma))
  slider_values <- lapply(init_slider_values()$control_dt$name,
                          function(x) {
                            input[[paste0("vfit_", x)]]
                          })
  names(slider_values) <- init_slider_values()$control_dt$name
  CTMM <- do.call(init_slider_values()$STUFF$storer, slider_values)
})
# update plot by sliders ----
output$fit_plot <- renderPlot({
  req(slider_to_CTMM())  # otherwise error: replacement of length zero
  fraction <- 10 ^ input$vfit_z
  plot(init_slider_values()$vario, CTMM = slider_to_CTMM(),
       col.CTMM = "green", fraction = fraction)
})
observeEvent(input$tuned, {
  # LOG fine tune apply
  log_msg("Apply Fine-tuned Parameters")
  removeModal()
  ids <- sapply(select_data_vario()$vario_list,
                function(vario) vario@info$identity)
  values$selected_data_guess_list[ids == input$tune_selected][[1]] <-
    slider_to_CTMM()
})
