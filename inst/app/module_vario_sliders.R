# abstract original code into module so it can be reused. need to support two curves. Need to use camelcase naming convention
# previously slider id is built with vfitz_sigma, vfitz_z, vfitz_tau1 etc. now with module id, should use slider name directly.
varioSlidersInput <- function(id, dialog_title) {
  ns <- NS(id)
  modalDialog(title = dialog_title,
              fluidRow(column(4, uiOutput(ns("fit_sliders"))),
                       column(8, plotOutput(ns("fit_plot"))),
                       column(4, offset = 6, uiOutput(ns("fit_zoom")))),
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
# sliders id are created in get_sliders_info with module_id, so we don't need to get ns from session$ns, just make sure module_id is same with id parameter
varioSliders <- function(input, output, session, sliders_info, color_vec) {
  # init sliders ----
  output$fit_sliders <- renderUI({
    req(sliders_info()$control_sliders)
  })
  output$fit_zoom <- renderUI({
    # js function need to know the id of zoom slider, which is also used in plot code. logify is also needed.
    list(tags$head(tags$script(HTML(ctmmweb::JS.logify(3)))),
         tags$head(tags$script(HTML(ctmmweb::JS.onload(session$ns("z"))))),
         req(sliders_info()$zoom_slider))
  })
  # get_current_ctmm()----
  # convert current slider value to ctmm object. also returned by module server function. apply button will take that to update app global variable
  get_current_ctmm <- reactive({
    # there is a time when sliders are initialized but without value, then later storer call get NULL parameters
    # cat(input$sigma)
    req(!is.null(input$sigma))
    # slider names can be dynamic depend on data
    slider_values <- lapply(sliders_info()$control_dt$name,
                            function(x) {
                              input[[x]]
                            })
    names(slider_values) <- sliders_info()$control_dt$name
    CTMM <- do.call(sliders_info()$STUFF$storer, slider_values)
  })
  # plot by sliders ----
  output$fit_plot <- renderPlot({
    # req(slider_to_CTMM())  # otherwise error: replacement of length zero
    # within module we can access input without ns, it will be converted. ns is only used in creating ui items.
    # use list, draw existing curve and adjusted curve.
    plot(sliders_info()$vario, CTMM = list(sliders_info()$ctmm_obj,
                                           req(get_current_ctmm())),
         col.CTMM = color_vec, fraction = 10 ^ input$z)
  })
  # center sliders ----
  observeEvent(input$center_slider, {
    adjust_slider <- function(name) {
      # Shiny will complain for named vector
      # id <- paste0("vfit_", name)
      # error slider usually have initial value of 0, double that will get 0.
      if (input[[name]] != 0) {
        updateSliderInput(session, name,
                          max = round(input[[name]] * 2, 2))
      }
    }
    lapply(sliders_info()$control_dt$name, adjust_slider)
  })
  return(get_current_ctmm)

}

# get slider info from vario, ctmm_obj. fraction is the internal value of zoom slider in vario control box. module using id- as prefix. for our purpose, it's easier to further wrap input processing code here, only provide the original input value in parameter.
get_sliders_info <- function(vario, ctmm_obj,
                            fraction_internal_value,
                            module_id) {
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
      inputId = paste0(module_id, "-", dt[i, name]),
      label = dt[i, label], min = round(dt[i, min], 3),
      max = round(dt[i, max], 3), value = round(dt[i, initial], 3),
      step = dt[i, step])
  })
  names(slider_list) <- dt$name
  # need to separate the zoom slider and control slider
  return(list(vario = vario, ctmm_obj = ctmm_obj,  # for convinence
              STUFF = STUFF, control_dt = dt[name != "z"],
              control_sliders = slider_list[names(slider_list) != "z"],
              zoom_slider = slider_list[names(slider_list) == "z"]))
}
