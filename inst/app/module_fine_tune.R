# abstract original code into module so it can be reused. need to support two curves. Need to use camelcase naming convention. If we move this inside package, need to use full qualifer in a lot of places, also the changes need to be installed to be reflected, not suitable for current development phrase. will move to package in future (for now also leave the functions in global environment after app running. In future it can be a basis for a shiny gadget work similar to manipulate vario feature.
# also check server.R code comment for usage
# tuneSelector ----
## the UI and selector code for fine-tune. client code just place the UI with module id, call module server function with selections etc. The selection action in turn call varioSliders UI with selector module id as namespace.
#  nesting modules will be too complicated, so only call slider module ui inside this module, leave the slider server module call outside in user code.
# check 05_1_model_variogram.Rmd vario_slider_module for script
# selectInput UI
tuneSelectorUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("tune_selector"))
}
# selections is dynamic, need to be transfered as a reactive expression not resolved, so the server function can take the dynamic value. calling with the reactive value directly doesn't work because to update inside means the call need to be updated
# log_msg function is not available in this context, transfer it as parameter
# called slider module with tune as id. so slider module will have namespace from `selector id - tune`
tuneSelector <- function(input, output, session, placeholder, selections, log_msg) {
  ns <- session$ns
  output$tune_selector <- renderUI({
    # the first choice is empty, used as a placeholder. need to create that choice item from variable first
    init_choice <- list("")
    names(init_choice) <- placeholder
    selectInput(ns("tune_selected"), NULL,
                # c("Fine-tune" = "", selections()))
                c(init_choice, selections()))
  })
  observeEvent(input$tune_selected, {
    if (input$tune_selected != "") {
      # LOG fine tune start
      log_msg("Fine-tune ", input$tune_selected)
      showModal(varioSlidersInput(ns("tune"),
                                  paste0("Fine-tune ",
                                         input$tune_selected)))
    }
  })
}
# varioSliders ----
## sliders' UI code, but actual sliders need to dynamic generated in server code
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
## utility function to calculate slider parameters from data values(check its input and return value).
# will be called inside reactive expression, serve as dynamic data holder for all data in page. the reactive expression will be used as data parameter of module server function.
# initialize slider values from vario, ctmm_obj_current. ctmm_obj_ref is the reference curve of original value, just transfered in and out. if user modified it then modify again, will still show original ref curve, but using new current curve. this is only needed by plot, but we are putting all info needed in ui here. fraction is the internal value of zoom slider in vario control box. module using id- as prefix. for our purpose, it's easier to further wrap input processing code here, only provide the original input value in parameter.
get_tune_page_data <- function(vario, ctmm_obj_ref, ctmm_obj_current,
                               fraction_internal_value,
                               module_id) {
  fraction_face_value <- 10 ^ fraction_internal_value
  STUFF <- ctmm:::variogram.fit.backend(vario, CTMM = ctmm_obj_current,
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
  return(list(vario = vario, ctmm_obj_ref = ctmm_obj_ref,  # for convinence
              ctmm_obj_current = ctmm_obj_current,
              STUFF = STUFF, control_dt = dt[name != "z"],
              control_sliders = slider_list[names(slider_list) != "z"],
              zoom_slider = slider_list[names(slider_list) == "z"]))
}
## draw sliders with slider data, which came from reactive expression as parameter. the reactive expression page_data is created in server.R using function above (just a utility function, not reactive or module), then transferred as parameter.
# sliders id are created in get_sliders_info with module_id, so we don't need to get ns from session$ns, just make sure module_id is same with id parameter. page_data is reactive expression which called get_tune_page_data, need to be used with () inside the server function.
varioSliders <- function(input, output, session, page_data, color_vec, log_dt_md) {
  # init sliders ----
  output$fit_sliders <- renderUI({
    # LOG slider values
    ## it's convenient for us that this just print in fine-tune begin and end. page_data is a reactive expression calling on ctmm_obj_current, which is part of guess list. apply action changed guess list value, triggered change in page_data reactive, triggered change here, even after pop up page is removed. the order of log is a little bit not perfect, alternative is print outside this function using isolate, then print again in apply action, kind of duplicate.
    log_dt_md(page_data()$control_dt[, .(label, value = initial)],
              "Parameter Values")
    req(page_data()$control_sliders)
  })
  output$fit_zoom <- renderUI({
    # js function need to know the id of zoom slider, which is also used in plot code. logify is also needed.
    list(tags$head(tags$script(HTML(ctmmweb::JS.logify(3)))),
         tags$head(tags$script(HTML(ctmmweb::JS.onload(session$ns("z"))))),
         req(page_data()$zoom_slider))
  })
  # get_current_ctmm()----
  # convert current slider value to ctmm object. also returned by module server function. apply button will take that to update app global variable
  get_current_ctmm <- reactive({
    # there is a time when sliders are initialized but without value, then later storer call get NULL parameters
    # cat(input$sigma)
    req(!is.null(input$sigma))
    # slider names can be dynamic depend on data
    slider_values <- lapply(page_data()$control_dt$name,
                            function(x) {
                              input[[x]]
                            })
    names(slider_values) <- page_data()$control_dt$name
    CTMM <- do.call(page_data()$STUFF$storer, slider_values)
  })
  # plot by sliders ----
  output$fit_plot <- renderPlot({
    # within module we can access input without ns, it will be converted. ns is only used in creating ui items.
    # use list, draw existing curve and adjusted curve.
    plot(page_data()$vario, CTMM = list(page_data()$ctmm_obj_ref,
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
    lapply(page_data()$control_dt$name, adjust_slider)
  })
  # the reactive expression will be returned, to be called with () later
  return(get_current_ctmm)
}
