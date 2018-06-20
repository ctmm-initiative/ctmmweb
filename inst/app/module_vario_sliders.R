# abstract original code into module so it can be reused. need to support two curves.
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
# get slider info from vario, ctmm_obj. fraction is the internal value of zoom slider in vario control box. id_prefix need a _ in end. for our purpose, it's easier to further wrap input processing code here, only provide the original input value in parameter.
get_sliders_info <- function(vario_id, vario_list, ctmm_obj_list,
                            fraction_internal_value,
                            slider_id_prefix) {
  ids <- names(vario_list)
  vario <- vario_list[ids == vario_id][[1]]
  ctmm_obj <- ctmm_obj_list[ids == vario_id][[1]]
  fraction_face_value <- 10 ^ fraction_internal_value
  STUFF <- ctmm:::variogram.fit.backend(vario, CTMM = ctmm_obj,
                                        fraction = fraction_face_value, b = 10)
  dt <- data.table(STUFF$DF)
  dt[, name := row.names(STUFF$DF)]
  # zoom slider used different base, and minus 1 from min,max.
  dt[name == "z", c("min", "max") := list(min - 1, max - 1)]
  # initial is taken from last page control directly
  dt[name == "z", initial := fraction_internal_value]
  # didn't use the step value in ctmm. use 0.001 instead, but need to adjust for error when data is calibrated
  dt[, step := 0.001]
  if ("MSE" %in% names(vario)) {
    dt[name == "error", step := 1]
  }
  slider_list <- lapply(1:nrow(dt), function(i) {
    sliderInput(
      inputId = paste0(slider_id_prefix, dt[i, name]),
      label = dt[i, label], min = round(dt[i, min], 3),
      max = round(dt[i, max], 3), value = round(dt[i, initial], 3),
      step = dt[i, step])
  })
  names(slider_list) <- dt$name
  # need to separate the zoom slider and control slider
  return(list(vario = vario, STUFF = STUFF, control_dt = dt[name != "z"],
              control_sliders = slider_list[names(slider_list) != "z"],
              zoom_slider = slider_list[names(slider_list) == "z"]))
}
# use reactive to get current vario, ctmm_obj. this will be duplicated on 2 tabs, make the code as simple as possible
guess_slider_values <- reactive({
  get_sliders_info(input$tune_selected,
                   req(select_data_vario()$vario_list),
                   values$selected_data_guess_list,
                   input$zoom_lag_fraction,
                   "tune_guess_")
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
# the convert function is already wrapped, need to get slider names and input values properly, which can be dynamic.
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
