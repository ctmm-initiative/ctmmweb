# increase the uploading file size limit to 2000M, now our upload is not just about movebank file, it also include the saved data.
options(shiny.maxRequestSize = 2000*1024^2)
# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)
# enable more debugging and messages
VERIFY_DATA_SYNC <- FALSE

server <- function(input, output, session) {
  values <- reactiveValues()
  # log/error options ----
  # log functions will use these options, so need to prepare them first
  # test checkbox status passively, one time read when called. wrap it into a function because if we decided to switch between independent checkbox or checkboxgroup, the changes only happens here, not every calling place.
  option_selected <- function(option) {
    isolate(input[[option]])
  }
  # inject a browser when debug button is clicked.
  # if (DEBUG_BUTTON) {
  #   observeEvent(input$inject_debug, browser())
  # }
  # global LOG variables ----
  # one time check if app is running in hosted mode
  APP_local <- (isolate(session$clientData$url_hostname) == "127.0.0.1")
  # to copy from 07_report_save_load.Rmd --
  LOG_console <- TRUE
  LOG_color_mappings <- list(time_stamp = crayon::cyan,
                             msg = crayon::green,
                             detail = crayon::blue)
  # global variable that hold the markdown strings.
  LOG_rmd_vec <- vector(mode = "character")
  # session temp folder
  session_tmpdir <- file.path(tempdir(), session$token)
  # log functions ----
  # add extra lines in markdown format without timestamp, this will not appear in console. vec can be a vector. all appends to global variable happen here, easier to manage.
  log_add_rmd <- function(vec, on = option_selected("record_on")) {
    if (!on) return()
    # used as function, will search variable in parent first, only go to global when not found. so need to make sure parent function don't have this
    LOG_rmd_vec <<- c(LOG_rmd_vec, vec)
  }
  # always do even switch is off. each session need to have individual folder.
  # use token as folder name, still create the timestamp folder as subfolder, so that the zip will have the timestamped folder
  # LOG_folder is session specific global variable inside server function, many functions need this as global variable and need to defined inside server instead of helpers.R
  create_log_folder <- function() {
    ctmmweb:::create_folder(file.path(session_tmpdir,
                            stringr::str_c("Report_",
                                           ctmmweb:::current_timestamp())))
  }
  # rely on several global variables. have side effect on console msg, and write string to global vector.
  # usually console content is same with markdown, except the data frame table need to be plain in console, table in markdown. detail will be in 2nd line with code format
  log_msg_console <- function(msg, detail = "") {
    time_stamp <- stringr::str_c("[", Sys.time(), "]")
    if (detail != "") {
      detail <- stringr::str_c("\n\t", detail)
    }
    if (LOG_console) {
      cat(LOG_color_mappings$time_stamp(time_stamp),
          LOG_color_mappings$msg(msg),
          LOG_color_mappings$detail(detail), "\n")
    }
    return(time_stamp)
  }
  # setting default value to use app control, only need to override it in internal usage
  log_msg <- function(msg, detail = "", on = option_selected("record_on")) {
    if (!on) return()
    time_stamp <- log_msg_console(msg, detail)
    # need extra new line for markdown
    log_add_rmd(stringr::str_c("`", time_stamp, "` ", msg, "\n\n\t", detail))
  }
  # common process for saving a plot
  log_prepare_plot <- function(f_name, f_ext = ".png") {
    pic_name <- stringr::str_c(f_name, "_",
                               ctmmweb:::current_timestamp(),
                               f_ext)
    log_msg("saving plot as", pic_name)
    log_add_rmd(stringr::str_c("![](", pic_name, ")"))
    return(file.path(LOG_folder, pic_name))
  }
  log_save_ggplot <- function(g, f_name, on = option_selected("record_on")) {
    if (!on) return(g)
    # need to save current device and restore it. otherwise plotting in R console will cause app draw plot to RStudio plot window. https://stackoverflow.com/questions/47699956/ggplot-in-shiny-app-go-to-rstudio-plot-window/.
    cur_dev <- dev.cur()
    print(system.time(ggplot2::ggsave(filename = log_prepare_plot(f_name),
                                      plot = g)))
    dev.set(cur_dev)
    return(g)
  }
  # only used for variogram, with specific format and parameters, some came from input. we don't need to return something in end of renderPlot for basic plot, since plot seemed to be side effect. (ggplot need the object to be overriden so interactive plot can have proper scale, see ?renderPlot). It also don't have the ggsave changing current device problem
  log_save_vario <- function(f_name, rows, cols,
                             on = option_selected("record_on")) {
    if (!on) return()
    grDevices::dev.print(png, file = log_prepare_plot(f_name),
                         units = "in", res = 220,
                         width = cols * 4, height = rows * 3)
  }
  # pdf is better for home range, occurrence
  log_save_UD <- function(f_name, on = option_selected("record_on")) {
    if (!on) return()
    grDevices::dev.copy2pdf(file = log_prepare_plot(f_name, f_ext = ".pdf"))
  }
  # save dt into markdown table or csv. note the msg could be in different format
  log_dt_md <- function(dt, msg, on = option_selected("record_on")) {
    if (!on) return()
    # need the extra \t because log_msg put \t before first line of detail
    time_stamp <- log_msg_console(msg,
                                  stringr::str_c(utils::capture.output(dt),
                                                 collapse = "\n\t"))
    log_add_rmd(c(stringr::str_c("`", time_stamp, "` ", msg, "\n"),
                  knitr::kable(dt, format = "markdown")))
  }
  # save dt in csv, need different msg format and a file name, so in independent function. f_name is used for part of csv file name, full name will be detail part of message
  log_dt_csv <- function(dt, msg, f_name, on = option_selected("record_on")) {
    if (!on) return()
    csv_name <- stringr::str_c(f_name, "_",
                               ctmmweb:::current_timestamp(),
                               ".csv")
    fwrite(dt, file = file.path(LOG_folder, csv_name))
    log_msg(msg, detail = csv_name)
    log_add_rmd(stringr::str_c("[", csv_name, "](", csv_name, ")"))
  }
  # copy end --
  # LOG app start
  LOG_folder <- create_log_folder()
  # initialize RMarkdown ----
  # note rstudio will format after paste, need to keep indent right.
  rmd_header <-
'---
title: "Work Report of ctmm web-app"
output:
  html_document:
    theme: yeti
    toc: yes
    toc_float: yes
---

'
  log_add_rmd(rmd_header)
  # page observer ----
  # add subtitle in log for every page. need to sync with ui.R for this.
  page_title <- list(import = "Import Data",
                     plots = "Visualization",
                     filter = "Filter Outliers",
                     subset = "Time Subsetting",
                     model = "Model Selection",
                     homerange = "Home Range",
                     overlap = "Overlap",
                     occurrence = "Occurrence",
                     map = "Map",
                     report = "Work Report")
  log_page <- function(title, on = option_selected("record_on")) {
    if (!on) return()
    log_msg_console(stringr::str_c("## ", title))
    log_add_rmd(stringr::str_c("\n## ", title, "\n"))
  }
  # log each page, also notify the requirement of time subsetting. we want to show this everytime switched to this page. if put inside color_bin_animal it will only show once if switched back and forth.
  observeEvent(input$tabs, {
    req(values$data)
    # since we req data, so it will not record pages without data. This is good.
    log_page(page_title[[input$tabs]])
    # time subset page need single animal be selected
    if (input$tabs == "subset") {
      if (length(input$individuals_rows_selected) != 1) {
        shinydashboard::updateTabItems(session, "tabs", "plots")
        showNotification(
          "Please select single individual first before time subsetting",
          type = "error", duration = 6)
      }
    }
    # # overlap page will jump to home range if it's not calculated yt
    # if (input$tabs == "overlap") {
    #   if (!ctmmweb:::reactive_validated(select_models_hranges())) {
    #     shinydashboard::updateTabItems(session, "tabs", "homerange")
    #     showNotification(
    #       "Please check home range first",
    #       type = "warning", duration = 5)
    #   }
    # }
  })
  # call outside of reactive context need isolate, they are also one time call only run when app started.
  # app log start ----
  # record pkg build date for easier issue report. it will also appear in work report. hosted app user can click the info button.
  log_msg("App started", paste0("Installed On: ", PKG_INSTALLATION_TIME))
  # first page need to be added manually since no page switching event fired
  log_page(page_title$import)
  # log app options ----
  # just log option changes, the value is taken directly when needed.
  observeEvent(input$record_on, {
    # this call doesn't use the default switch to turn off itself
    log_msg(stringr::str_c("Recording is ",
                           if (input$record_on) "On" else "Off"), on = TRUE)
  })
  # help module server ----
  # help function now have proper folder
  click_help <- function(input, output, session, title, size, file){
    observeEvent(input$help, {
      showModal(modalDialog(
        title = title, size = size,
        # APP_wd could be package app folder or just app folder depend on loading method
        fluidPage(includeMarkdown(file.path(APP_wd, file))),
        easyClose = TRUE, fade = FALSE
      ))
    })
  }
  # the app option help is registered after help function is ready
  callModule(click_help, "app_options", title = "App Options",
             size = "l", file = "help/1_app_options.md")
  # capture error ----
  # CAPTURE_error_msg <- !APP_local  # deployed version
  # CAPTURE_error_msg <- APP_local  # for local testing
  # clean up. needed in app exit and checking option off.
  clean_up_error_capture <- function(error_con) {
    # need to restore sink first, otherwise connection cannot be closed. if don't restore, other message got lost too.
    sink(type = "message")
    flush(error_con)
    close(error_con)
  }
  # checking on/off option should either prepare the error file or clean it up
  observeEvent(input$capture_error, {
    if (input$capture_error) {
      # each session have one error log file. different client will have different file in same server
      values$error_file <- tempfile()
      # the capturing code is not inside observer anymore, but it need to be inside a reactive context (there is no warning?), put it here
      values$error_file_con <- file(values$error_file, open = "a")
      sink(values$error_file_con, type = "message")
      log_msg("Error messages captured in App")
    } else {
      clean_up_error_capture(values$error_file_con)
      log_msg("Error message directed to R Console")
    }
  })
  # on.exit need to be inside server function so outside of renderUI
  onStop(function() {
    # if option is off, clean up is done already
    if (isolate(input$capture_error)) {
      clean_up_error_capture(isolate(values$error_file_con))
    }
  })
  # the button itself need to depend on option, cannot be inside the if call which doesn't remove in else branch
  # add side bar button
  output$error_popup <- renderUI(
    if (input$capture_error) {
      actionButton("show_error", "Error Message",
                   icon = icon("exclamation-triangle"),
                   style = "color: #ffec3b;background-color: #232d33;border: transparent;margin-left: 4%;")
    }
  )
  # show error msg ----
  observeEvent(input$show_error, {
    showModal(modalDialog(title = "Error Messages",
                fluidRow(
                  column(12, pre(includeText(req(values$error_file)))),
                  column(12, h4("App Installed On")),
                  column(12, verbatimTextOutput("app_info")),
                  column(12, h4("Session information")),
                  column(12, verbatimTextOutput("session_info"))),
                size = "l", easyClose = TRUE, fade = FALSE))
    output$app_info <- renderPrint(cat(PKG_INSTALLATION_TIME, "\n"))
    output$session_info <- renderPrint(sessionInfo())
  })
  # just log option changes, the value is taken directly when needed.
  observeEvent(input$parallel, {
    if (input$parallel) {
      log_msg("Parallel mode enabled")
    } else {
      # try(log("a"))  # for testing error log
      log_msg("Parallel mode disabled")
    }
  })
  # cache setup ----
  create_cache <- function() {
    ctmmweb:::create_folder(file.path(session_tmpdir, "cache"))
  }
  reset_cache <- function(cache_path) {
    cache_files <- list.files(cache_path, full.names = TRUE)
    file.remove(cache_files)
  }
  cache_path <- create_cache()
  par_try_tele_guess_mem <- memoise::memoise(
    ctmmweb:::par_try_tele_guess,
    cache = memoise::cache_filesystem(cache_path))
  akde_mem <- memoise::memoise(
    ctmm::akde,
    cache = memoise::cache_filesystem(cache_path))
  par_occur_mem <- memoise::memoise(
    ctmmweb::par_occur,
    cache = memoise::cache_filesystem(cache_path))
  # p1. import ----
  # run this after every modification on data and list separately. i.e. values$data$tele_list changes, or data not coming from combine. this should got run automatically? no if not referenced. need reactive expression to refer values$.
  # this is a side effect reactive expression that depend on a switch.
  verify_global_data <- reactive({
    if (VERIFY_DATA_SYNC) {
      ctmmweb:::match_tele_merged(values$data$tele_list, values$data$merged)
    }
  })
  # values$ ----
  # data hold various aspects of core data, 4 items need to be synced
  values$data <- NULL
  # important reactive value and expressions need special comments, use <--. the design need to well thought
  # input_tele_list: telemetry obj list from as.telemetry on input data: movebank download, local upload, package data. all reference of this value should wrap req around it. Once it's used, no need to keep the copy. thus add it with the new time subset. We don't need to keep the dt version because we can often just use existing dt and other info. do need to verify tele and dt is synced.
  # tele_list, merged: the telemetry version and merged data.table version of updated data reflected changes on outlier removal and time subsetting.
  # merged hold $data_dt and $info. we used to call $dt but it was renamed because of exported function may have naming conflict with dt. data is a more generic name with more items, data_dt is the main dt.
  # all_removed_outliers: records of all removed outliers. original - all removed = current. the table have id column so this can work across different individuals.
  # the time subset only live in time subsetting process, the result of the process update tele_list and merged.
  # the extra column of outliers only live in outlier page. the result of the process update whole data. note may need to use column subset when operating between dt with or without extra columns.
  # for any data source changes, need to update these 4 items together.
  # selected_model_try_res is updated in model fitting stage, need to be cleared when input change too.
  # build id_pal from info. this is needed in importing tele data, and restoring session data.
  build_id_pal <- function(info) {
    leaflet::colorFactor(
      scales::hue_pal()(nrow(info)), info$identity, ordered = TRUE
    )
  }
  update_input_data <- function(tele_list) {
    values$data$input_tele_list <- tele_list
    values$data$tele_list <- tele_list
    values$data$merged <- ctmmweb:::combine_tele_list(tele_list)
    values$data$all_removed_outliers <- NULL
    # values$selected_data_model_try_res <- NULL
    # this need to be built with full data, put as a part of values$data so it can be saved in session saving. if outside data, old data's value could be left to new data when updated in different route.
    # however saveRDS save this to a 19M rds. have to put it outside of data, rebuild it when loading session. (update input will update it here)
    values$id_pal <- build_id_pal(values$data$merged$info)
    shinydashboard::updateTabItems(session, "tabs", "plots")
    # LOG input data updated
    log_msg("Input data updated")
  }
  # 1.1 csv to telemetry ----
  # call this function for side effect, set values$data
  # as.telemetry work on both file path and data.frame, so it works on both of uploaded file and downloaded movebank data frame.
  data_import <- function(data_path) {
    # sometimes there is error: Error in <Anonymous>: unable to find an inherited method for function ‘span’ for signature ‘"shiny.tag"’. added tags$, not sure if it will fix it.
    note_import <- showNotification(
      shiny::span(icon("spinner fa-spin"), "Importing data..."),
      type = "message", duration = NULL)
    on.exit(removeNotification(note_import))
    # warning need to be recorded and notify at last (not in every warning, ony notify once), error need to notify and stop
    # every warning will trigger handler, need to only notify once.
    warning_generated <- FALSE
    # after return, move to next handler
    wHandler <- function(w) {
      warning_generated <<- TRUE
    }
    eHandler <- function(e) {
      showNotification("Error in import, check data again",
                       duration = 7, type = "error")
    }
    tele_list <- tryCatch(
      withCallingHandlers(
        ctmmweb:::wrap_single_telemetry(ctmm::as.telemetry(data_path)),
        warning = wHandler
        ),
      error = eHandler)
    if (warning_generated) {
      log_msg("Warning generated in import")
      if (input$capture_error) {
        showModal(modalDialog(title = "Import Warning",
                    fluidRow(
                      column(12, pre(includeText(req(values$error_file))))),
                    size = "l", easyClose = TRUE, fade = FALSE))
      } else {
        showNotification("Warning in import, check R console",
                         duration = 5, type = "warning")
        # showModal(modalDialog(title = "Import Warning",
        #                       fluidRow(
        #                         column(12, verbatimTextOutput("warnings"))),
        #                       size = "l", easyClose = TRUE, fade = FALSE))
        # output$warnings <- renderPrint(warnings())
      }
    }
    # wrap it so even single individual will return a list with one item
    # tele_list <- tryCatch(
    #   ctmmweb:::wrap_single_telemetry(ctmm::as.telemetry(data_path)),
    #   error = function(e) {
    #     showNotification("Import error, check data again",
    #                      duration = 4, type = "error")
    #     })
    # only proceed if no error
    test_class <- lapply(tele_list, function(x) {"telemetry" %in% class(x)})
    req(all(unlist(test_class)))
    # sort list by identity. only sort list, not info table. that's why we need to sort it again after time subsetting.
    tele_list <- ctmmweb:::sort_tele_list(tele_list)
    update_input_data(tele_list)
  }
  # clicking browse button without changing radio button should also update, this is why we make the function to include all behavior after file upload.
  # using parameter because launching app with path also use this function
  file_uploaded <- function(data_path){
    data_import(data_path)
    updateRadioButtons(session, "load_option", selected = "upload")
    shinydashboard::updateTabItems(session, "tabs", "plots")
  }
  # app launch mode ----
  # app can be launched from rstudio on server.R directly(i.e. runshinydir for app folder, used to be the run.R method), or from package function app(). Need to detect launch mode first, then detect app() parameters if in app mode. By checking environment strictly, same name object in global env should not interfer with app.
  # if app started from starting server.R, current env 2 level parent is global, because 1 level parent is server function env. this is using parent.env which operating on env. parent.frame operating on function call stack, which could be very deep, sys.nframe() reported 37 in browser call, sys.calls give details, the complex shiny maintaince stack.
  # run() function env if called from ctmmweb::app(), one level down from global if run server.R in Rstudio
  calling_env <- parent.env(environment())
  # app() mode: if not from global
  if (!identical(parent.env(calling_env), globalenv())) {
    # cat("running in app() mode\n")
    #set app directory to installed package app folder (from app()), which is needed by loading help documentations
    APP_wd <- get("app_DIR", envir = calling_env)
    # further check if data parameter is avaialbe
    if (exists("shiny_app_data", where = calling_env)) {
      app_input_data <- get("shiny_app_data", envir = calling_env)
      if (is.character(app_input_data)) {
        # LOG file loaded from app()
        log_msg("Importing file from app(shiny_app_data)", app_input_data)
        # accessed reactive values so need to isolate
        isolate(file_uploaded(app_input_data))
      } else if (("telemetry" %in% class(app_input_data)) ||
                 (is.list(shiny_app_data) &&
                  "telemetry" %in% class(shiny_app_data[[1]]))
      ) {
        # LOG data loaded from app()
        log_msg("Loading telemetry data from app(shiny_app_data)")
        isolate(update_input_data(app_input_data))
      }
    }
  } else {
    # if did launched from server.R, it should be current directory which is set to server.R directory by runshinydir
    # cat("running in runShinydir mode\n")
    APP_wd <- "."
  }
  # upload dialog
  observeEvent(input$tele_file, {
    req(input$tele_file)
    # LOG file upload.
    log_msg("Importing file", input$tele_file$name)
    file_uploaded(input$tele_file$datapath)
  })
  # abstract because need to do this in 2 places
  set_sample_data <- function() {
    data("buffalo", package = "ctmm", envir = environment())
    sample_data <- ctmmweb:::pick_tele_list(buffalo, input$sample_size)
    # LOG use sample
    log_msg("Using data", "buffalo sample from ctmm")
    update_input_data(sample_data)
  }
  # observe radio button changes
  observeEvent(input$load_option, {
    switch(input$load_option,
           ctmm = {
             data("buffalo", package = "ctmm", envir = environment())
             # LOG use buffalo
             log_msg("Using data", "buffalo from ctmm")
             update_input_data(buffalo)
           },
           ctmm_sample = {
             set_sample_data()
           },
           upload = {
             # the radiobutton itself doesn't upload, just reuse previously uploaded file if switched back.
             # need to check NULL input from source, stop error in downstream
             req(input$tele_file)
             # LOG file upload.
             log_msg("Importing file", input$tele_file$name)
             file_uploaded(input$tele_file$datapath)
           })
  })
  # also update the app when sample size changed and is already in sample mode
  observeEvent(input$sample_size, {
    if (input$load_option == "ctmm_sample") {
      set_sample_data()
    }
  })
  callModule(click_help, "import", title = "Data Import Options", size = "l",
             file = "help/1_import_options.md")
  # 1.2 movebank login ----
  # look up user R environment for movebank login
  mb_env <- Sys.getenv(c("movebank_user", "movebank_pass"))
  if (identical(stringr::str_sort(names(mb_env)), c("movebank_pass", "movebank_user")) &&
      all(nchar(mb_env) != 0)) {
    mb_user_env <- unname(mb_env["movebank_user"])
    mb_pass_env <- unname(mb_env["movebank_pass"])
    # the textinput value are always sync to date, so we can just use textinput everywhere which is reactive
    updateTextInput(session, "user", value = mb_user_env)
    updateTextInput(session, "pass", value = mb_pass_env)
    showNotification("Movebank login info found", duration = 1,
                     type = "message")
  }
  callModule(click_help, "login", title = "Movebank Login", size = "l",
             file = "help/1_movebank_login.md")
  # 1.3 movebank studies ----
  # 1.3, 1.4, 1.5 are linked. Each content for rendering should be reactive but passive updated by observeEvent. Each action should check whether all other content need to be updated. with reactive we only need to update the variable, not really update rendering manually.
  # all studies box
  # $all_studies_stat ----
  values$all_studies_stat <- NULL
  output$all_studies_stat <- renderText(req(values$all_studies_stat))
  # values$studies hold complete data, only render part of it according to reactive input
  # $studies ----
  values$studies <- NULL
  # only show selected cols because we don't want to show owner col. want to keep it insivibly so we can switch it on and off.
  output$studies <- DT::renderDataTable(
    DT::datatable({
      req(values$studies)
      selected_studies_cols <- c("id", "name", "deployments",
                                 "events", "individuals")
      values$studies[owner == input$data_manager, selected_studies_cols,
                     with = FALSE]
      },
      rownames = FALSE,
      options = list(pageLength = 5),
      selection = 'single'
  ))
  # selected data box
  # $study_detail ----
  values$study_detail <- NULL
  output$study_detail <- DT::renderDataTable(
    DT::datatable(req(values$study_detail),
              rownames = FALSE,
              options = list(pageLength = 5),
              selection = 'none'))
  # data preview box
  # $study_data_response ----
  values$study_data_response <- NULL
  output$study_data_response <- renderText(req(values$study_data_response))
  # $study_preview ----
  values$study_preview <- NULL
  output$study_preview <- DT::renderDataTable(
    DT::datatable(req(values$study_preview), options = list(dom = 't')))
  # $move_bank_dt ----
  values$move_bank_dt <- NULL  # the downloaded whole data table, not rendered anywhere
  # the whole data preview box should be cleared with all actions other than download, otherwise it could be confusing when there is a previous download and user made other actions
  clear_mb_download <- function(res_msg = NULL){
    values$study_data_response <- res_msg
    values$study_preview <- NULL
    values$move_bank_dt <- NULL
  }
  observeEvent(input$login, {
    note_studies <- showNotification(
      shiny::span(icon("spinner fa-spin"), "Downloading studies..."),
      type = "message", duration = NULL)
    # always take current form value
    res <- ctmmweb:::get_all_studies(input$user, input$pass)  # may generate error notification if failed
    removeNotification(note_studies)
    # if failed, should clear previous studies table to avoid click on rows, which will update study details while the response is the error message text, not csv. then fread will have error to crash app
    if (res$status != "Success") {
      # `request` in helper will generate error notification and console msg
      # every action should compare to this list, verify what changes should be done to each value
      values$all_studies_stat <- ""
      values$studies <- NULL
      values$study_detail <- NULL
      clear_mb_download()
      # LOG movebank login
      log_msg("Movebank login failed")
    } else {
      studies_cols <- c("id", "name", "study_objective",
                           "number_of_deployments", "number_of_events",
                           "number_of_individuals",
                           "i_am_owner", "i_can_see_data", "license_terms")
      all_studies <- try(fread(res$res_cont, select = studies_cols))
      # using ifelse because we need vectorized conversion here.
      all_studies[, i_can_see_data :=
                    ifelse(i_can_see_data == "true", TRUE, FALSE)]
      all_studies[, i_am_owner := ifelse(i_am_owner == "true", TRUE, FALSE)]
      valid_studies <- all_studies[(i_can_see_data)]
      new_names <- sub(".*_", "", studies_cols)
      setnames(valid_studies, studies_cols, new_names)
      setkey(valid_studies, name)
      values$studies <- valid_studies
      values$all_studies_stat <- paste0("Total Studies ", all_studies[, .N],
          "; You can see data of ", values$studies[, .N],
          ";\nYou are data manager of ", values$studies[(owner), .N])
      values$study_detail <- NULL
      clear_mb_download()
      # LOG movebank login
      log_msg("Logged in Movebank as", input$user)
    }
  })
  # 1.4 selected details ----
  # save file name need study name, so need to duplicate code here.
  mb_id <- reactive({
    req(input$studies_rows_selected)
    values$studies[owner == input$data_manager][input$studies_rows_selected, id]
  })
  # deselect row should clear detail table, so added ignoreNULL
  observeEvent(input$studies_rows_selected, ignoreNULL = FALSE, {
    if (length(input$studies_rows_selected) == 0) {
      values$study_detail <- NULL
      clear_mb_download()
    } else {
      # note the data manager part, make sure the table is same with view in studies table. also need to use same expression in download part.
      # mb_id <- values$studies[owner == input$data_manager][input$studies_rows_selected, id]
      # link to movebank
      output$open_study <- renderUI({
        req(input$studies_rows_selected)
        shiny::a(tags$button(icon("external-link"), "Open in Movebank",
                             class = "btn btn-default action-button",
                             style = ctmmweb:::STYLES$external_link),
                 target = "_blank", href =
  paste0("https://www.movebank.org/movebank/#page=studies,path=study", mb_id()))
      })
      res <- ctmmweb:::get_study_detail(mb_id(), input$user, input$pass)
      # It's easier to specify cols here to drop some cols and reorder cols at the same time
      detail_cols <- c("id", "name", "study_objective", "study_type", "license_terms", "principal_investigator_name", "principal_investigator_address", "principal_investigator_email", "timestamp_start", "timestamp_end", "bounding_box", "location_description", "main_location_lat", "main_location_long", "number_of_tags", "acknowledgements", "citation", "comments", "grants_used", "there_are_data_which_i_cannot_see")
      detail_dt <- try(fread(res$res_cont, select = detail_cols))
      req("data.table" %in% class(detail_dt))
      # need to check content in case something wrong and code below generate error on empty table
      # never had error here because the mb_id came from table itself. so no extra clear up boxes
      if (detail_dt[, .N] == 0) {
        showNotification("No study information downloaded",
                         duration = 2, type = "error")
      } else{
        # exclude empty columns (value of NA)
        valid_cols <- names(detail_dt)[colSums(!is.na(detail_dt)) != 0]
        #  show table as rows. will have some warning of coercing different column types, ignored.
        detail_rows <- suppressWarnings(melt(detail_dt, id.vars = "id",
                                             na.rm = TRUE))
        detail_rows[, id := NULL]
        values$study_detail <- detail_rows
        # any selection in studies table should clear downloaded data table
        clear_mb_download()
      }
    }
  })
  # 1.4 download data ----
  observeEvent(input$download_movebank, {
    req(input$studies_rows_selected)
    # need to ensure here match the selected study mb_id. not too optimal, but may not worth a reactive expression too.
    # mb_id <- values$studies[owner == input$data_manager][
    #   input$studies_rows_selected, id]
    note_data_download <- showNotification(
      shiny::span(icon("spinner fa-spin"), "Downloading data..."),
      type = "message", duration = NULL)
    # always take current form value
    res <- ctmmweb:::get_study_data(mb_id(), input$user, input$pass)
    removeNotification(note_data_download)
    # need to check response content to determine result type. the status is always success
    # read first rows to determine if download is successful. fread will guess sep so still can read html for certain degree, specify `,` will prevent this
    # sometimes the result is one line "<p>No data are available for download.</p>". fread and read.csv will take one line string as file name thus cannot find the input file, generate some warnings. To use string as input need at least one "\n". Adding "\n" will solve this error but get valid dt with 0 row, also we cannot use the nrows parameters. We don't need to print error to console which can be confusing to user, and we have message in app. We can always turn this on in debugging.
    movebank_dt_preview <- try(fread(res$res_cont, sep = ",", nrows = 5),
                               silent = TRUE)
    # the fread in ctmm can use == directly because it was reading in df only, only one class attributes. Here we need to use %in% instead
    if (!("data.table" %in% class(movebank_dt_preview))) {
      showNotification(
        h4("No data available or you need to agree to license term first. See details in Selected Study Data box."),
        type = "warning", duration = 5)
      msg <- ctmmweb:::html_to_text(res$res_cont)
      clear_mb_download(paste0(msg, collapse = "\n"))
      # LOG download movebank data failed
      log_msg("Movebank data download failed", mb_id())
    } else {
      showNotification("Data downloaded", type = "message", duration = 2)
      note_parse <- showNotification(
        shiny::span(icon("spinner fa-spin"), "Parsing csv..."),
        type = "message", duration = NULL)
      move_bank_dt <- try(fread(res$res_cont, sep = ","))
      removeNotification(note_parse)
      row_count <- formatC(move_bank_dt[, .N], format = "d", big.mark = ",")
      # individual_count <- length(unique(move_bank_dt[, individual_id]))
      individual_count <- nrow(unique(move_bank_dt, by = "individual_id"))
      values$study_data_response <- paste0(
          "Data downloaded with ", row_count, " rows, ",
          individual_count, " individuals. ", "Preview:")
      values$study_preview <- movebank_dt_preview
      values$move_bank_dt <- move_bank_dt
      # LOG download movebank data
      log_msg("Movebank data downloaded", mb_id())
      # some detail table may have invalid characters that crash kable. disable this now.
      # log_dt_md(values$study_detail, "Downloaded study details",
      #           on = option_selected("record_on"))
    }
  })
  callModule(click_help, "download_movebank", title = "Download Movebank data",
             size = "l", file = "help/1_movebank_download.md")
  # 1.5 save, import data ----
  output$save_movebank <- downloadHandler(
    filename = function() {
        # mb_id <- values$studies[input$studies_rows_selected, id]
        # avoid special characters that invalid for file name
        study_name <- gsub('[^\\w]', ' ',
                           values$studies[owner == input$data_manager][
                             input$studies_rows_selected, name],
                           perl = TRUE)
        paste0("Movebank ", mb_id(), " - ", study_name, ".csv")
        },
    content = function(file) {
      req(values$move_bank_dt[, .N] > 0)
      fwrite(values$move_bank_dt, file)
      # LOG save movebank data. we don't know what's the final file name. file is temp file path
      log_msg("Movebank data saved", mb_id())
    }
  )
  observeEvent(input$import_movebank, {
    req(values$move_bank_dt[, .N] > 0)
    data_import(values$move_bank_dt)
    # LOG import movebank data
    log_msg("Movebank data imported", mb_id())
    shinydashboard::updateTabItems(session, "tabs", "plots")
  })
  # p2. plots ----
  # input (upload, movebank, buffalo) -> current -> chose animal in table
  # current: merge telemetry to df, remove outliers if in quene, return df, info table, removed outliers full data
  # 2.1 data summary ----
  output$outlier_report <- renderUI({
    if (!is.null(values$data$all_removed_outliers)) {
      h4(style = "color: #F44336;font-weight: bold;text-decoration: underline;",
         paste0(nrow(values$data$all_removed_outliers),
             " outliers removed from original"))
    }
  })
  output$individuals <- DT::renderDataTable({
    req(values$data)
    # if (input$time_in_sec) {
    #   info_p <- values$data$merged$info[,
    #               .(identity, start, end, interval_s, duration_s, points)]
    # } else {
    #   info_p <- values$data$merged$info[,
    #               .(identity, start, end, interval, duration, points)]
    # }
    info_p <- values$data$merged$info
    DT::datatable(info_p, options = list(pageLength = 6,
                                     lengthMenu = c(2, 4, 6, 8, 10, 20))) %>%
      DT::formatStyle('identity', target = 'row',
                  color = DT::styleEqual(info_p$identity,
                                     scales::hue_pal()(nrow(info_p)))
      )
  })
  # delete selected individuals ----
  # update tele_list, merged data and info, all removed outliers
  observeEvent(input$delete_individuals, {
    req(values$data)
    req(input$individuals_rows_current)
    id_vec <- values$data$merged$info[, identity]
    if (length(input$individuals_rows_selected) > 0) {
      chosen_row_nos <- input$individuals_rows_selected
      chosen_ids <- id_vec[chosen_row_nos]
      # if all are deleted, will have error in plots. this is different from the req check, just diable this behavior
      if (identical(chosen_ids, id_vec)) {
        showNotification("Cannot proceed because all data will be deleted",
                         duration = 3, type = "error")
        return()
      }
      # begin removal. freeze plots to avoid it update before data finish sync. we cannot freeze output, but the key factor is the selected rows which will reset after table updated, which could be slower than data updates. so we freeze the row selection which will also freeze select_data. This row selection is often slower than data update, same in remove outlier
      # tried freeze overlap table rows here to solve the double update problem of home range plot, not working.
      # freezeReactiveValue(input, "individuals_rows_selected")
      if (!is.null(values$data$all_removed_outliers)) {
        values$data$all_removed_outliers <- values$data$all_removed_outliers[
          !(identity %in% chosen_ids)
          ]
      }
      values$data$merged$data_dt <- values$data$merged$data_dt[
        !(identity %in% chosen_ids)
      ]
      remaining_indice <- !(values$data$merged$info$identity %in% chosen_ids)
      values$data$merged$info <- values$data$merged$info[remaining_indice]
      values$data$tele_list <- values$data$tele_list[remaining_indice]
      verify_global_data()
      # LOG delete inidividuals
      log_msg("Individuals deleted from data ",
              stringr::str_c(chosen_ids, collapse = ", "))
    }
  })
  proxy_individuals <- DT::dataTableProxy("individuals")
  observeEvent(input$select_all, {
    # this always select all rows
    # DT::selectRows(proxy_individuals, 1:nrow(values$data$merged$info))
    # this select all rows after filtering. user may want to filter, select, clear filter to compare what are not selected
    DT::selectRows(proxy_individuals, input$individuals_rows_all)
  })
  observeEvent(input$deselect_all, {
    # use list() instead of NULL to avoid R 3.4 warning on I(NULL). After DT fixed this warning we can change back to NULL
    DT::selectRows(proxy_individuals, list())
  })
  callModule(click_help, "visual", title = "Visualization",
             size = "l", file = "help/2_visualization.md")
  # select_data() ----
  # selected rows or current page, all pages start from this current subset
  # with lots of animals, the color gradient could be subtle or have duplicates
  select_data <- reactive({
    # need to wait the individual summary table initialization finish. otherwise the varible will be NULl and data will be an empty data.table but not NULL, sampling time histogram will have empty data input.
    req(values$data)
    # switching data summary units cause redraw of plot, because the table redraw changed the rows_current value to null then new, triggered this change.
    req(input$individuals_rows_current)
    id_vec <- values$data$merged$info[, identity]
    # table can be sorted, but always return row number in column 1
    if (length(input$individuals_rows_selected) == 0) {
      # select all in current page when there is no selection
      chosen_row_nos <- input$individuals_rows_current
    } else {
      chosen_row_nos <- input$individuals_rows_selected
    }
    chosen_ids <- id_vec[chosen_row_nos]
    # %in% didn't keep order. since our table update in sort change the data and redraw anyway, let's keep the order. the other similar usage is in removing outliers. should not have problem with new orders.
    # animals_dt <- values$data$merged$data_dt[identity %in% chosen_ids]
    animals_dt <- values$data$merged$data_dt[.(chosen_ids), on = "id"]
    # also need to change the order of levels of dt, so that ggplot will plot them in same order. all these are based on selected subset, should not modify original data
    animals_dt$id <- factor(animals_dt$id, levels = chosen_ids)
    # subset_indice <- values$data$merged$info$identity %in% chosen_ids
    # info only has identity, no id column
    info <- values$data$merged$info[.(chosen_ids), on = "identity"]
    # need to clear model fit result, change to original mode instead of modeled mode
    # values$selected_data_model_try_res <- NULL
    updateRadioButtons(session, "vario_mode", selected = "empirical")
    # LOG current selected individuals
    log_dt_md(info,
              "Current selected individuals")
    # didn't verify data here since it's too obvious and used too frequently. if need verfication, need call function on subset.
    # switch model selection tab to 1st as modeling need 1st tab data updated
    updateTabsetPanel(session, "vario_tabs", selected = "1")
    return(list(data_dt = animals_dt,
                info = info,
                chosen_row_nos = chosen_row_nos,
                tele_list = values$data$tele_list[chosen_ids]
                ))
  })
  # 2.2 overview plot ----
  # to add zoom in for a non-arranged plot, seem more in add_zoom.R and google group discussion
  # 1. add event id in ui, always use same naming pattern with plotid.
  # 2. call function to create reactive value of range
  # 3. use range in plot xlim/ylim
  add_zoom <- function(plot_id) {
    ranges <- reactiveValues(x = NULL, y = NULL)
    observeEvent(input[[paste0(plot_id, "_dblclick")]], {
      brush <- input[[paste0(plot_id, "_brush")]]
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    ranges
  }
  location_plot_gg_range <- add_zoom("location_plot_gg")
  output$location_plot_gg <- renderPlot({
    animals_dt <- req(select_data()$data_dt)
    # use dt parameter to determine whether to overlay
    if (input$overlay_all) {
      dt <- values$data$merged$data_dt
    } else {
      dt <- NULL
    }
    g <- ctmmweb::plot_loc(animals_dt, dt, input$point_size_1) +
      ggplot2::coord_fixed(xlim = location_plot_gg_range$x,
                           ylim = location_plot_gg_range$y)
    # g <- ggplot2::ggplot() +
    #   {if (input$overlay_all) {
    #     ggplot2::geom_point(data = values$data$merged$data_dt, ggplot2::aes(x, y),
    #                         size = input$point_size_1, alpha = 0.6,
    #                         colour = "gray")
    #   }} +
    #   ggplot2::geom_point(data = animals_dt, ggplot2::aes(x, y, colour = id),
    #                       size = input$point_size_1, alpha = 0.7) +
    #   ggplot2::coord_fixed(xlim = location_plot_gg_range$x,
    #                        ylim = location_plot_gg_range$y) +
    #   ctmmweb:::factor_color(animals_dt$id) +  # the color is right because id is factor, its levels included all values from full dataset ids.
    #   ggplot2::scale_x_continuous(labels =
    #                                 ctmmweb:::format_distance_f(animals_dt$x)) +
    #   ggplot2::scale_y_continuous(labels =
    #                                 ctmmweb:::format_distance_f(animals_dt$y)) +
    #   ggplot2::theme(legend.position = "top",
    #                  legend.direction = "horizontal") +
    #   ctmmweb:::BIGGER_THEME + ctmmweb:::BIGGER_KEY
    # LOG save pic
    log_save_ggplot(g, "plot_2_overview")
  }, height = function() { input$canvas_height }, width = "auto"
  )
  # 2.3 facet ----
  output$location_plot_facet_fixed <- renderPlot({
    # by convention animals_dt mean the data frame, sometimes still need some other items from list, use full expression
    animals_dt <- req(select_data()$data_dt)
    g <- ctmmweb::plot_loc_facet(animals_dt)
    # g <- ggplot2::ggplot(data = animals_dt, ggplot2::aes(x, y)) +
    #   ggplot2::geom_point(size = 0.1, ggplot2::aes(colour = id)) +
    #   ggplot2::scale_x_continuous(labels =
    #                                 ctmmweb:::format_distance_f(animals_dt$x)) +
    #   ggplot2::scale_y_continuous(labels =
    #                                 ctmmweb:::format_distance_f(animals_dt$y)) +
    #   ctmmweb:::factor_color(animals_dt$id) +
    #   ggplot2::facet_grid(id ~ .) +
    #   ggplot2::coord_fixed() +
    #   ggplot2::theme(strip.text.y = ggplot2::element_text(size = 12)) +
    #   ctmmweb:::BIGGER_THEME + ctmmweb:::BIGGER_KEY
    # LOG save pic
    log_save_ggplot(g, "plot_3_facet")
  }, height = function() { input$canvas_height }, width = "auto")
  # 2.4 individual plot ----
  output$location_plot_individual <- renderPlot({
    animals_dt <- req(select_data()$data_dt)
    new_ranges <- ctmmweb:::get_ranges_quantile_dt(animals_dt,
                                                   input$include_level)
    id_vector <- select_data()$info$identity
    g_list <- vector("list", length = length(id_vector))
    for (i in seq_along(id_vector)) {
      data_i <- animals_dt[identity == id_vector[i]]
      new_ranges_i <- new_ranges[identity == id_vector[i]]
      g_list[[i]] <- ggplot2::ggplot(data = data_i,
                                     ggplot2::aes(x, y, color = id)) +
        ggplot2::geom_point(size = input$point_size_3, alpha = 0.7) +
        ctmmweb:::factor_color(data_i$id) +
        ggplot2::scale_x_continuous(labels =
                                      ctmmweb:::format_distance_f(data_i$x)) +
        ggplot2::scale_y_continuous(labels =
                                      ctmmweb:::format_distance_f(data_i$y)) +
        ggplot2::labs(title = id_vector[i]) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
              legend.position = "none") +
        ggplot2::coord_fixed(xlim = c(new_ranges_i$x_start, new_ranges_i$x_end),
                             ylim = c(new_ranges_i$y_start, new_ranges_i$y_end),
                             expand = FALSE)
      # no bigger theme and key here since no key involved. bigger theme could mess up the axis labels too.
    }
    fig_count <- length(id_vector)
    # if the figure count is smaller than col count, like 1 for 2 columns, matrix become empty and cause problem. if the figure count is not multiply of cols, it also messed up. Just use ncol solve all the problems.
    # gr <- grid.arrange(grobs = g_list, layout_matrix =
    #                      matrix(1:fig_count,
    #                             nrow = fig_count / input$plot4_col,
    #                             ncol = input$plot4_col, byrow = TRUE))
    gr <- gridExtra::grid.arrange(grobs = g_list, ncol = input$plot4_col)
    # LOG save pic
    log_save_ggplot(gr, "plot_4_individual")
  }, height = function() { input$canvas_height }, width = "auto")
  # 2.5 histogram facet ----
  output$histogram_facet <- renderPlot({
    animals_dt <- req(select_data()$data_dt)
    g <- ctmmweb::plot_time(animals_dt)
    # g <- ggplot2::ggplot(data = animals_dt,
    #                      ggplot2::aes(x = timestamp, fill = id)) +
    #   ggplot2::geom_histogram(bins = 60) +
    #   ctmmweb:::factor_fill(animals_dt$id) +
    #   ggplot2::facet_grid(id ~ .) +
    #   ggplot2::theme(strip.text.y = ggplot2::element_text(size = 12)) +
    #   ctmmweb:::BIGGER_THEME + ctmmweb:::BIGGER_KEY
    # LOG save pic
    log_save_ggplot(g, "plot_5_histogram")
  }, height = ctmmweb:::STYLES$height_hist, width = "auto")
  # p3. outlier ----
  callModule(click_help, "telemetry_errors", title = "Telemetry Errors",
             size = "l", file = "help/3_telemetry_errors.md")
  callModule(click_help, "outlier_distance",
             title = "Outliers in Distance to Median Center",
             size = "l", file = "help/3_outlier_distance.md")
  callModule(click_help, "outlier_speed", title = "Outliers in Speed",
             size = "l", file = "help/3_outlier_speed.md")
  # calc_outlier() ----
  # take current subset, add distance and speed columns. everything in this page start from this data. The outlier removal need to apply to whole data then trickle down here
  calc_outlier <- reactive({
    # exclude non-numeric input
    req(!is.na(as.numeric(input$device_error)))
    outlier_page_data <- req(select_data())  # data, info, tele_list
    animals_dt <- outlier_page_data$data_dt
    # need telemetry list for error info
    # animals_dt <- ctmmweb::calc_distance(animals_dt,
    #                                      outlier_page_data$tele_list,
    #                                      as.numeric(input$device_error))
    # animals_dt <- ctmmweb::calc_speed(animals_dt,
    #                                   outlier_page_data$tele_list,
    #                                   as.numeric(input$device_error))
    animals_dt <- animals_dt %>%
      ctmmweb::assign_distance(outlier_page_data$tele_list,
                               as.numeric(input$device_error)) %>%
      ctmmweb::assign_speed(outlier_page_data$tele_list,
                            as.numeric(input$device_error))
    outlier_page_data$data_dt <- animals_dt
    return(outlier_page_data)
  })
  # p3.a.1 distance histogram ----
  # note this also add bin factor column
  bin_by_distance <- reactive({
    # animals_dt <- req(select_data()$data_dt)
    animals_dt <- req(calc_outlier()$data_dt)
    return(ctmmweb:::color_break(input$distance_his_bins, animals_dt,
                                 "distance_center",
                                 ctmmweb:::format_distance_f))
  })
  output$distance_histogram <- renderPlot({
    # need to get data from reactive, update by bin count
    distance_binned <- req(bin_by_distance())
    animals_dt <- distance_binned$animals_dt
    # use this to check if distance and speed data is synced
    # cat("dataset in distance page\n")
    # print(animals_dt[, .N, by = id])
    g <- ggplot2::ggplot(animals_dt, ggplot2::aes(x = distance_center)) +
      ggplot2::geom_histogram(breaks = distance_binned$color_bin_breaks,
                     # fill = hue_pal()(input$distance_his_bins),
                     ggplot2::aes(fill = distance_center_color_factor,
                       alpha = distance_center_color_factor)) +
      # need to exclude 0 count groups
      ggplot2::geom_text(stat = 'bin',
                         ggplot2::aes(label =
                                        ifelse(..count.. != 0, ..count.., "")),
                         vjust = -1,
                         breaks = distance_binned$color_bin_breaks) +
      ctmmweb:::factor_fill(animals_dt$distance_center_color_factor) +
      ctmmweb:::factor_alpha(animals_dt$distance_center_color_factor) +
      ggplot2::scale_x_continuous(breaks = distance_binned$non_empty_breaks,
                                  labels = distance_binned$vec_formatter) +
      # all counts above 20 are not shown, so it's easier to see the few outliers.
      ggplot2::coord_cartesian(ylim = c(0, input$distance_his_y_limit)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     legend.position = "none")
    # LOG save pic
    log_save_ggplot(g, "plot_distance_outlier_histogram")
  })
  # need the whole range to get proper unit selection
  format_outliers <- function(animal_selected_data, animals_dt) {
    unit_distance <- ctmmweb:::pick_unit_distance(animals_dt$distance_center)
    unit_speed <- ctmmweb:::pick_unit_speed(animals_dt$speed)
    # get this first otherwise the colname is changed
    dt <- animal_selected_data[, .(id, row_no,
       timestamp = ctmmweb:::format_datetime(timestamp),
       distance_center = distance_center,
       # distance_center = format(distance_center / unit_distance$scale,
       #                          digits = 3),
       # distance_unit = unit_distance$name,
       # speed = format(speed / unit_speed$scale, digits = 3),
       speed = speed
       # speed_unit = unit_speed$name
       )]
    name_unit_list <- list("distance_center" = ctmmweb:::pick_unit_distance,
                           "speed" = ctmmweb:::pick_unit_speed)
    ctmmweb:::format_dt_unit(dt, name_unit_list)
  }
  # brush selection function
  select_range <- function(his_type){
    return(reactive({
      # everything in outlier page should take animal_dt from binned version
      # the current data have distance/speed column, the binned version just create the color factors. in theory we could use the original data but we may need the color factor sometimes.
      switch(his_type,
             distance = {
               col_name = quote(distance_center)
               format_f <- ctmmweb:::format_distance_f
               # unit_name <- " m"
               animals_dt <- req(bin_by_distance()$animals_dt)
             },
             speed = {
               col_name = quote(speed)
               format_f <- ctmmweb:::format_speed_f
               # unit_name <- " m/s"
               animals_dt <- req(bin_by_speed()$animals_dt)
             })
      brush <- input[[paste0(his_type, "_his_brush")]]
      # col_name <- switch(his_type,
      #                    distance = quote(distance_center),
      #                    speed = quote(speed))
      if (is.null(brush)) {
        select_start <- 0
        select_end <- max(animals_dt[, eval(col_name)])
      } else {
        select_start <- brush$xmin
        select_end <- brush$xmax
      }
      animal_selected_data <- animals_dt[(eval(col_name) >= select_start) &
                                           (eval(col_name) <= select_end)]
      # if no point in range, setnames will complain
      if (nrow(animal_selected_data) == 0) {
        animal_selected_formatted <- NULL
      } else {# show both distance and speed in 3 tables
        animal_selected_formatted <- format_outliers(animal_selected_data,
                                                     animals_dt)
      }
      # LOG selection range, selected points count
      format_f_value <- format_f(c(select_start, select_end))
      # format_raw <- function(value, unit_name) {
      #   stringr::str_c(format(value, digits = 3), unit_name)
      # }
      # dt <- data.table(Unit = c("Formated", "SI"),
      #   Start = c(format_f_value(select_start),
      #             format_raw(select_start, unit_name)),
      #   End = c(format_f_value(select_end),
      #           format_raw(select_end, unit_name)))
      # log_dt_md(dt, "Range Selected")
      log_msg("Range Selected", paste0(format_f_value(select_start),
                                       " ~ ", format_f_value(select_end),
                                       ", ", nrow(animal_selected_data),
                                       " points"))
      # log_msg("Points in Selected Range", nrow(animal_selected_data))
      list(select_start = select_start, select_end = select_end,
           animal_selected_data = animal_selected_data,
           animal_selected_formatted = animal_selected_formatted)
    }))
  }
  select_distance_range <- select_range("distance")
  # distance outlier plot ----
  distance_outlier_plot_range <- add_zoom("distance_outlier_plot")
  output$distance_outlier_plot <- renderPlot({
    animals_dt <- req(bin_by_distance()$animals_dt)
    animal_selected_data <- select_distance_range()$animal_selected_data
    # browser()
    g <- ggplot2::ggplot(animals_dt, ggplot2::aes(x, y)) +
      ggplot2::geom_point(size = 0.05, alpha = 0.6, colour = "gray") +
      ggplot2::geom_point(data = animal_selected_data,
                 size = ifelse(is.null(input$distance_his_brush),
                               0.2,
                               input$distance_point_size),
                 # alpha = ifelse(is.null(input$distance_his_brush),
                 #                0.6,
                 #                input$distance_alpha),
                 ggplot2::aes(colour = distance_center_color_factor,
                     alpha = distance_center_color_factor)) +
      {if (!is.null(input$points_in_distance_range_rows_selected)) {
        points_selected <- select_distance_range()$animal_selected_data[
          input$points_in_distance_range_rows_selected]
        ggplot2::geom_point(data = points_selected, size = 3.5, alpha = 1,
                            color = "blue", shape = 22)
      }} +
      ggplot2::geom_point(data =
                            unique(animals_dt[, .(id, median_x, median_y)]),
                          ggplot2::aes(x = median_x, y = median_y, shape = id),
                          color = "blue", size = 0.8) +
      ctmmweb:::factor_color(animal_selected_data$distance_center_color_factor) +
      # scale_alpha_discrete(breaks = bin_by_distance()$color_bin_breaks) +
      ctmmweb:::factor_alpha(animal_selected_data$distance_center_color_factor) +
      ggplot2::scale_x_continuous(labels =
                                    ctmmweb:::format_distance_f(animals_dt$x)) +
      ggplot2::scale_y_continuous(labels =
                                    ctmmweb:::format_distance_f(animals_dt$y)) +
      ggplot2::coord_fixed(xlim = distance_outlier_plot_range$x,
                           ylim = distance_outlier_plot_range$y) +
      ggplot2::theme(legend.position = "top",
            legend.direction = "horizontal") + ctmmweb:::BIGGER_KEY
    # LOG save pic
    log_save_ggplot(g, "plot_distance_outlier_plot")
  })
  # points in selected distance range
  output$points_in_distance_range <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data
    req(input$distance_his_brush)
    # cols <- c("row_no", "timestamp", "id", "distance_center")
    # datatable(select_distance_range()$animal_selected_data[, cols, with = FALSE],
    DT::datatable(select_distance_range()$animal_selected_formatted,
              options = list(pageLength = 6,
                             lengthMenu = c(6, 10, 20),
                             scrollX = TRUE,
                             searching = FALSE),
              rownames = FALSE)
  })
  # remove distance outliers ----
  # use side effect, update values$data, not chose animal. assuming row_name is always unique. if all_removed_outliers came from whole data, there is no outlier columns, which are needed in removed outlier table. if carry the extra columns, need extra process in subset and merge back. now carry extra columns in all_removed_points, but build dt by subset with row_name only, so no extra column transferred.
  remove_outliers <- function(points_to_remove) {
    # update the all outlier table, always start from original - all outliers.
    # removed_points <- values$data$merged$data_dt[
    #   row_name %in% row_names_to_remove]
    # distance and speed color_break will add each own factor column, so two tab have different columns. we only need the extra columns minus these factor column in summary table
    points_to_remove <- points_to_remove[, timestamp:speed]
    values$data$all_removed_outliers <- rbindlist(list(
      values$data$all_removed_outliers, points_to_remove))
    animals_dt <- values$data$merged$data_dt[
      !(row_name %in% values$data$all_removed_outliers[, row_name])]
    # update tele obj. more general apporach is update them according to data frame changes.
    changed <- unique(points_to_remove$identity)
    tele_list <- values$data$tele_list
    tele_list[changed] <- lapply(tele_list[changed], function(x) {
      x[!(row.names(x) %in% points_to_remove[, row_name]),]
    })
    tele_list <- tele_list[lapply(tele_list, nrow) != 0]
    info <- ctmmweb:::info_tele_list(tele_list)
    # distance/speed calculation need to be updated. row_no not updated.
    # animals_dt <- ctmmweb::calculate_distance(animals_dt)
    # animals_dt <- ctmmweb::calculate_speed(animals_dt)
    values$data$tele_list <- tele_list
    values$data$merged <- NULL
    values$data$merged <- list(data_dt = animals_dt, info = info)
    verify_global_data()
  }
  proxy_points_in_distance_range <- DT::dataTableProxy(
    "points_in_distance_range", deferUntilFlush = FALSE)
  # actually just put row_name vec into reactive value. current_animal will update. note the reset can only reset all, not previous state, let current take from input again. let reset change a reactive value switch too, not updating current directly.
  # need to use row_name because once data updated, row_no may change.
  observeEvent(input$remove_distance_selected, {
    req(length(input$points_in_distance_range_rows_selected) > 0)
    # row_names_to_remove <- select_distance_range()$animal_selected_data[
    #   input$points_in_distance_range_rows_selected, row_name]
    points_to_remove <- select_distance_range()$animal_selected_data[
      input$points_in_distance_range_rows_selected]
    points_to_remove_formated <-
      select_distance_range()$animal_selected_formatted[
        input$points_in_distance_range_rows_selected]
    freezeReactiveValue(input, "points_in_distance_range_rows_selected")
    DT::selectRows(proxy_points_in_distance_range, list())
    freezeReactiveValue(input, "distance_his_brush")
    session$resetBrush("distance_his_brush")
    # LOG points to remove
    log_dt_md(points_to_remove_formated, "Points to be Removed by Distance")
    remove_outliers(points_to_remove)
  })
  # p3.b.1 speed histogram ----
  # bin_by_speed() ----
  bin_by_speed <- reactive({
    # animals_dt <- req(select_data()$data_dt)
    animals_dt <- req(calc_outlier()$data_dt)
    # too large UERE value will result calculated speed in 0
    zero_speeds <- all(range(animals_dt$speed) == c(0,0))
    if (zero_speeds) {
      showNotification("Calculated Speed = 0, is device error too big?",
                       type = "error")
    }
    req(!zero_speeds)
    return(ctmmweb:::color_break(input$speed_his_bins, animals_dt,
                       "speed", ctmmweb:::format_speed_f))
  })
  output$speed_histogram <- renderPlot({
    speed_binned <- req(bin_by_speed())
    animals_dt <- speed_binned$animals_dt
    # cat("dataset in speed page\n")
    # print(animals_dt[, .N, by = id])
    g <- ggplot2::ggplot(animals_dt, ggplot2::aes(x = speed)) +
      ggplot2::geom_histogram(breaks = speed_binned$color_bin_breaks,
                              ggplot2::aes(fill = speed_color_factor,
                         alpha = speed_color_factor)) +
      # need to exclude 0 count groups
      ggplot2::geom_text(stat = 'bin', ggplot2::aes(label = ifelse(..count.. != 0, ..count.., "")),
                vjust = -1, breaks = speed_binned$color_bin_breaks) +
      ctmmweb:::factor_fill(animals_dt$speed_color_factor) +
      ctmmweb:::factor_alpha(animals_dt$speed_color_factor) +
      ggplot2::scale_x_continuous(breaks = speed_binned$non_empty_breaks,
                         labels = speed_binned$vec_formatter) +
      ggplot2::coord_cartesian(ylim = c(0, input$speed_his_y_limit)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "none")
    # LOG save pic
    log_save_ggplot(g, "plot_speed_outlier_histogram")
  })
  # outputOptions(output, "speed_histogram", priority = 10)
  select_speed_range <- select_range("speed")
  # speed outlier plot ----
  speed_outlier_plot_range <- add_zoom("speed_outlier_plot")
  output$speed_outlier_plot <- renderPlot({
    animals_dt <- req(bin_by_speed()$animals_dt)
    # cat("dataset in speed scatter plot\n")
    # print(animals_dt[, .N, by = id])
    animal_selected_data <- select_speed_range()$animal_selected_data
    g <- ggplot2::ggplot(animals_dt, ggplot2::aes(x, y)) +
      ggplot2::geom_point(size = 0.05, alpha = 0.6, colour = "gray") +
      ggplot2::geom_point(data = animal_selected_data,
                 size = ifelse(is.null(input$speed_his_brush),
                               0.2,
                               input$speed_point_size),
                 # alpha = ifelse(is.null(input$speed_his_brush),
                 #                0.6,
                 #                input$speed_alpha),
                 ggplot2::aes(colour = speed_color_factor,
                     alpha = speed_color_factor)) +
      ctmmweb:::factor_color(animal_selected_data$speed_color_factor) +
      # scale_alpha_discrete(breaks = bin_by_speed()$color_bin_breaks) +
      ctmmweb:::factor_alpha(animal_selected_data$speed_color_factor) +
      ggplot2::scale_x_continuous(labels = ctmmweb:::format_distance_f(animals_dt$x)) +
      ggplot2::scale_y_continuous(labels = ctmmweb:::format_distance_f(animals_dt$y)) +
      ggplot2::coord_fixed(xlim = speed_outlier_plot_range$x,
                  ylim = speed_outlier_plot_range$y) +
      ggplot2::theme(legend.position = "top",
            legend.direction = "horizontal") + ctmmweb:::BIGGER_KEY
    # if selected some points in table of data in range. when some points are removed, data updated but this table is still there, not updated yet, so there are row selection values. Further, the plot is not updated so brush value is still there, select_speed_range() will get selected data with brush value, but last brush value is the higher range now have no match in data after outlier removal.
    # with 2nd points clicked in 2 points list, removing it cause the selected data update to one point, but the selection row is still 2nd. wrong execution order. reactive need reactive, not if check or normal branch.
    if (!is.null(input$points_in_speed_range_rows_selected)) {
      selected_points <- select_speed_range()$animal_selected_data[
        input$points_in_speed_range_rows_selected]
      # cat("selected row in table\n")
      # print(input$points_in_speed_range_rows_selected)
      # cat("selected points in table\n")
      # print(selected_points)
      # draw rectangle around selected points
      # browser()
      g <- g +
        ggplot2::geom_point(data = selected_points, size = 3.5, alpha = 1,
                                   color = "blue", shape = 22)
      # calculate path if needed
      if ("draw_speed_path" %in% input$selected_details) {
        neighbor_size <- 4
        animals_dt[, in_neighbor := NA]
        for (r_no in selected_points$row_no) {
          animals_dt[(identity == animals_dt[row_no == r_no, identity]) &
                       (abs(row_no - r_no) <= neighbor_size),
                     in_neighbor := TRUE]
        }
        # animals_dt[, c("xend", "yend") := NULL]
        animals_dt[(in_neighbor), `:=`(xend = x + inc_x, yend = y + inc_y)]
        g <- g +
          ggplot2::geom_point(data = animals_dt[(in_neighbor)],
                     color = "blue",
                     alpha = 0.8, size = 1, shape = 21) +
          # remove the warning of NA otherwise each zoom will have one warning.
          ggplot2::geom_segment(data = animals_dt[(in_neighbor)],
                       color = "blue", alpha = 0.3, na.rm = TRUE,
                       ggplot2::aes(xend = xend, yend = yend),
                       arrow = grid::arrow(length = grid::unit(2,"mm")))
        # add label in path if needed, this only work when path is selected.
        if ("add_label" %in% input$selected_details) {
          g <- g +
            ggplot2::geom_text(data = animals_dt[(in_neighbor)],
                               ggplot2::aes(label = row_no), alpha = 0.6, hjust = -0.1)
        }
      }
    }
    # LOG save pic
    log_save_ggplot(g, "plot_speed_outlier_plot")
  })
  # outputOptions(output, "speed_outlier_plot", priority = 1)
  # points without valid speed values
  # output$points_speed_non_valid <- DT::renderDataTable({
  #   # only render table when there is a selection. otherwise it will be all data.
  #   animals_dt <- req(values$data$merged$data_dt)
  #   cols <- c("row_no", "timestamp", "id", "speed")
  #   datatable(animals_dt[is.na(speed), cols, with = FALSE],
  #             options = list(pageLength = 6,
  #                            lengthMenu = c(6, 10, 20),
  #                            searching = FALSE),
  #             rownames = FALSE)
  # })
  # points in selected speed range
  output$points_in_speed_range <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data.
    req(input$speed_his_brush)
    DT::datatable(select_speed_range()$animal_selected_formatted,
              options = list(pageLength = 6,
                             lengthMenu = c(6, 10, 20),
                             scrollX = TRUE,
                             searching = FALSE),
              rownames = FALSE)
  })
  # give it high priority so it will update in before the plot updates
  # outputOptions(output, "points_in_speed_range", priority = 10)
  # remove speed outliers ----
  proxy_points_in_speed_range <- DT::dataTableProxy("points_in_speed_range",
                                                deferUntilFlush = FALSE)
  observeEvent(input$remove_speed_selected, {
    req(length(input$points_in_speed_range_rows_selected) > 0)
    # row_names_to_remove <- select_speed_range()$animal_selected_data[
    #   input$points_in_speed_range_rows_selected, row_name]
    points_to_remove <- select_speed_range()$animal_selected_data[
      input$points_in_speed_range_rows_selected]
    points_to_remove_formated <-
      select_speed_range()$animal_selected_formatted[
        input$points_in_speed_range_rows_selected]
    # to ensure proper order of execution, need to clear the points in range table row selection, and the brush value of histogram, otherwise some reactive expressions will take the leftover value of them when plot are not yet updated fully.
    # freeze it so all expression accessing it will be put on hold until update finish, because the reset here just send message to client, didn't update immediately
    freezeReactiveValue(input, "points_in_speed_range_rows_selected")
    DT::selectRows(proxy_points_in_speed_range, list())
    freezeReactiveValue(input, "speed_his_brush")
    session$resetBrush("speed_his_brush")
    # LOG points to remove
    log_dt_md(points_to_remove_formated, "Points to be Removed by Speed")
    remove_outliers(points_to_remove)
  })
  # all removed outliers ----
  output$all_removed_outliers <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data.
    req(values$data$all_removed_outliers)
    # animals_dt <- req(select_data()$data_dt)
    animals_dt <- req(calc_outlier()$data_dt)
    dt <- format_outliers(values$data$all_removed_outliers, animals_dt)
    log_dt_md(dt, "All Removed Outliers")
    DT::datatable(dt,
              options = list(pageLength = 6,
                             lengthMenu = c(6, 10, 20),
                             searching = FALSE),
              rownames = FALSE)
  })
  # tried to add delete rows like the time range table, but that need to update a lot of values in proper order, the reset is easy because it just use original input. Not really need this complex operations.
  # reset outlier removal ----
  # method 1. merge data back, just reverse the remove outlier. that require add rows to tele which is not possible now? need that tele update function later. if this is doable, pros: merge dt is faster than combine; time-subset don't need to update input tele, only need to maintain current tele/dt.
  # method 2. merge input. but time subset added new data. if we update input_tele with time subset, need to use the original input tele + new time subset, not the current tele which could have outlier removed. by merging tele we didn't keep two versions. but this could be expensive in merging.
  observeEvent(input$reset_outliers, {
    values$data$tele_list <- values$data$input_tele_list
    values$data$merged <- ctmmweb:::combine_tele_list(values$data$tele_list)
    values$data$all_removed_outliers <- NULL
    # LOG reset removal
    log_msg("All Removed Outliers Restored")
  })
  # p4. time subset ----
  callModule(click_help, "time_subsetting", title = "Subset data by time",
             size = "l", file = "help/4_time_subsetting.md")
  # color_bin_animal() ----
  values$selected_time_range <- NULL
  # when putting brush in same reactive value, every brush selection updated the whole value which update the histogram then reset brush.
  color_bin_animal <- reactive({
    # ensure time range table are cleared even there is no suitable single individual
    values$time_ranges <- NULL
    req(values$data)
    req(length(input$individuals_rows_selected) == 1)
    selected_id <- select_data()$info$identity
    data_i_dt <- select_data()$data_dt
    data_i_dt[, color_bin_start :=
             ctmmweb:::cut_date_time(timestamp, input$time_color_bins)]  # a factor
    color_bin_start_vec_time <- lubridate::ymd_hms(levels(data_i_dt$color_bin_start))
    color_bin_breaks <- c(color_bin_start_vec_time,
                                     data_i_dt[t == max(t), timestamp])
    # initital selection is full range
    # the manual set of date range triggered this whole expression to calculate again, and reset it to full range.
    isolate({
      values$selected_time_range <- list(
        select_start = data_i_dt[t == min(t), timestamp],
        select_end = data_i_dt[t == max(t), timestamp])
      updateDateRangeInput(session, "date_range",
                           start = values$selected_time_range$select_start,
                           end = values$selected_time_range$select_end)
    })
    # using id internally to make code shorter, in data frame id is factor
    return(list(identity = selected_id, data_dt = data_i_dt,
                # single tele object, not list, other places use tele_list
                tele = select_data()$tele_list[[1]],
                color_bin_start_vec_time = color_bin_start_vec_time,
                # vec for interval, findInterval. breaks for hist
                color_bin_breaks = color_bin_breaks))
  })
  # 4.1 histogram subsetting ----
  # histogram cut by color bins. default with less groups since color difference is limited.
  output$histogram_subsetting <- renderPlot({
    animal_binned <- color_bin_animal()
    g <- ggplot2::ggplot(data = animal_binned$data_dt, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_histogram(breaks = as.numeric(animal_binned$color_bin_breaks),
                     fill = scales::hue_pal()(input$time_color_bins)) +
      ggplot2::scale_x_datetime(breaks = animal_binned$color_bin_breaks,
                       labels = scales::date_format("%Y-%m-%d %H:%M:%S")) +
      ggplot2::ggtitle(animal_binned$data_dt[1, identity]) + ctmmweb:::CENTER_TITLE +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    # LOG save pic
    log_save_ggplot(g, "plot_time_subsetting_histogram")
  })
  # select time range ----
  # brush selection and matching color bins
  observeEvent(input$time_sub_his_brush, {
    values$selected_time_range <- list(
      select_start = lubridate::as_datetime(input$time_sub_his_brush$xmin),
      select_end = lubridate::as_datetime(input$time_sub_his_brush$xmax))
  })
  observeEvent(input$set_date_range, {
    start <- lubridate::as_datetime(input$date_range[1])
    end <- lubridate::as_datetime(input$date_range[2])
    if (end - start < 0) {
      showNotification("Start date is later than end date",
                       duration = 3, type = "error")
    } else {
      values$selected_time_range <- list(
        select_start = start,
        select_end = end)
    }
  })
  # 4.2 current range ----
  # format a time range table. need to deal with NULL input since in the initialization and after all rows deleted, this is still called.
  format_time_range <- function(time_range_df) {
    if (is.null(time_range_df) || nrow(time_range_df) == 0) {
      return(NULL)
    } else {
      time_range_dt <- data.table(time_range_df)
      time_range_dt[, `:=`(start = ctmmweb:::format_datetime(select_start),
                           end = ctmmweb:::format_datetime(select_end),
                           length = ctmmweb:::format_diff_time(select_end - select_start))]
      return(time_range_dt[, .(start, end, length)])
    }
  }
  output$current_range <- DT::renderDataTable({
    req(!is.null(values$selected_time_range))
    dt <- format_time_range(as.data.frame(values$selected_time_range))
    # LOG selection
    log_dt_md(dt, "Current Selected Time Range")
    DT::datatable(dt, options =
                list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
      DT::formatStyle(1, target = 'row', color = "#00c0ef")
  })
  # 4.3 selected locations ----
  selected_loc_ranges <- add_zoom("selected_loc")
  output$selected_loc <- renderPlot({
    animal_binned <- color_bin_animal()
    time_range <- values$selected_time_range
    animal_selected_data <- animal_binned$data_dt[
      (timestamp >= time_range$select_start) &
        (timestamp <= time_range$select_end)]
    g <- ggplot2::ggplot(data = animal_binned$data_dt, ggplot2::aes(x, y)) +
      ggplot2::geom_point(size = 0.01, alpha = 0.5, colour = "gray") +
      ggplot2::geom_point(size = input$point_size_time_loc, alpha = 0.9,
                 data = animal_selected_data,
                 ggplot2::aes(colour = color_bin_start)) +
      ctmmweb:::factor_color(animal_selected_data$color_bin_start) +
      ggplot2::scale_x_continuous(labels = ctmmweb:::format_distance_f(animal_binned$data_dt$x)) +
      ggplot2::scale_y_continuous(labels = ctmmweb:::format_distance_f(animal_binned$data_dt$y)) +
      ggplot2::coord_fixed(xlim = selected_loc_ranges$x, ylim = selected_loc_ranges$y) +
      ggplot2::theme(legend.position = "top",
            legend.direction = "horizontal") + ctmmweb:::BIGGER_KEY
    # LOG save pic
    log_save_ggplot(g, "plot_time_subsetting_plot")
  })
  # 4.4 time range table ----
  # time_subsets hold a table of time ranges for current individual, this should only live in one time subsetting process(clear in beginning, in color_bin_animal. clear after finish, when subset is generated), which is always on single individual. If user moved around pages without changing individual, the states are kept. Once generated, the new subset instance data and tele obj are inserted to values$current and kept there, which hold for all input session.
  observeEvent(input$add_time, {
    l <- list(values$time_ranges, as.data.frame(values$selected_time_range))
    values$time_ranges <- rbindlist(l)
    # LOG add
    log_dt_md(format_time_range(as.data.frame(values$selected_time_range)),
              "Time Range Added to List")
  })
  observeEvent(input$delete_time_sub_rows, {
    # with empty table the previous selected value is still there, need to check table too
    if (!is.null(input$time_ranges_rows_selected) &&
        (nrow(values$time_ranges) > 0)) {
      # LOG delete
      log_dt_md(values$time_ranges[as.numeric(input$time_ranges_rows_selected)],
        "Time Range Deleted")
      values$time_ranges <- values$time_ranges[
        -as.numeric(input$time_ranges_rows_selected)
      ]
      # LOG clear if empty
      if (nrow(values$time_ranges) == 0) {
        log_msg("Time Range List Cleared")
      }
    }
  })
  observeEvent(input$reset_time_sub, {
    values$time_ranges <- NULL
    # LOG clear
    log_msg("Time Range List Cleared")
  })
  # generate time subset ----
  # need a explicit button because once applied, the data will change and the plot and histogram will change too. the result applied to values$data, not current select_data(). also clear time_ranges, move to the visualization page.
  # update input_tele for reset_remove_outlier, but need to use the input_tele + new timesub, not current tele + new timesub to include the possible outliers. so we cannot use already updated current tele.
  observeEvent(input$generate_time_sub, {
    req(values$time_ranges)
    animal_binned <- color_bin_animal()
    # skip the new added column color_bin_start. the name of last column may change depend on other changes in data structure
    dt <- animal_binned$data_dt[, timestamp:row_no]
    res <- vector("list", length = nrow(values$time_ranges))
    for (i in 1:nrow(values$time_ranges)) {
      res[[i]] <- dt[(timestamp >= values$time_ranges[i, select_start]) &
                       (timestamp <= values$time_ranges[i, select_end])]
    }
    # note all ranges are combined. this is intended for a subset of non-overlapping sections. If need multiple subset, just generate several times.
    new_dt <- unique(rbindlist(res))
    setkey(new_dt, row_no)
    # new name
    matches <- stringr::str_match(values$data$merged$info$identity,
                         paste0(animal_binned$identity, "_subset_(\\d+)$"))
    matches[is.na(matches)] <- 0
    last_index <- max(as.numeric(matches[,2]))
    new_suffix <- paste0("_subset_", last_index + 1)
    new_id <- paste0(animal_binned$identity, new_suffix)
    new_dt[, identity := new_id]
    new_tele <- animal_binned$tele  # single tele obj from color_bin_animal
    # subset tele by row_name before it changes
    # new_tele <- new_tele[(row.names(new_tele) %in% new_dt[, row_name]),]
    new_tele <- new_tele[new_dt$row_name,]
    new_tele@info$identity <- new_id
    # update other columns
    new_dt[, row_name := paste0(row_name, new_suffix)]
    # update the row name in tele data frame by new row_name column
    row.names(new_tele) <- new_dt$row_name
    # update data
    all_dt <- values$data$merged$data_dt
    all_dt <- rbindlist(list(all_dt, new_dt))
    # ggplot sort id by name, to keep it consistent we also sort the info table. for data.table there is no need to change order (?), this can keep row_no mostly same
    all_dt[, id := factor(identity)]
    all_dt[, row_no := .I]
    values$data$merged$data_dt <- all_dt
    # need to wrap single obj otherwise it was flattened by c
    values$data$tele_list <- c(values$data$tele_list,
                               ctmmweb:::wrap_single_telemetry(new_tele))
    # also update input tele from original input + new tele
    values$data$input_tele_list <- c(values$data$input_tele_list,
                                     ctmmweb:::wrap_single_telemetry(new_tele))
    # sort info list so the info table will have right order. we can also sort the info table, but we used the row index of table for selecting indidivuals(sometimes I used identity, sometimes maybe use id), it's better to keep the view sync with the data
    # sorted_names <- sort(names(values$data$tele_list))
    values$data$tele_list <- ctmmweb:::sort_tele_list(values$data$tele_list)
    values$data$input_tele_list <- ctmmweb:::sort_tele_list(values$data$input_tele_list)
    values$data$merged$info <- ctmmweb:::info_tele_list(values$data$tele_list)
    values$time_ranges <- NULL
    verify_global_data()
    # LOG subset added
    log_msg("New Time Range Subset Added", new_id)
    shinydashboard::updateTabItems(session, "tabs", "plots")
    msg <- paste0(new_id, " added to data")
    showNotification(msg, duration = 2, type = "message")
  })
  output$time_ranges <- DT::renderDataTable({
    # it could be NULL from clear, or empty data.table from delete
    req(values$time_ranges)
    req(nrow(values$time_ranges) > 0)
    dt <- format_time_range(values$time_ranges)
    # LOG time range list
    log_dt_md(dt, "Time Range List")
    DT::datatable(dt, options =
                list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  # p5. variogram ----
  callModule(click_help, "vario_control", title = "Plot Controls",
             size = "l", file = "help/5_a_vario_control.md")
  callModule(click_help, "variograms", title = "Variograms",
             size = "l", file = "help/5_b_variograms.md")
  # callModule(click_help, "vario_irregular", title = "Irregular Data",
  #            size = "l", file = "help/5_b_irregular_data.md")
  # values$selected_data_guess_list guessed parameters for current data, also can be manual adjusted from fine tune.
  values$selected_data_guess_list <- NULL
  # calculate group plot row count and total canvas height from group list length and UI. this is needed in vario plot, overlap home range plot. vario mode and model mode need different value because model mode can coexist (home range/occur rely on it)
  layout_group <- function(group_list, figure_height, column) {
    fig_count <- length(group_list)
    row_count <- ceiling(fig_count / column)
    height <- figure_height * row_count
    return(list(row_count = row_count, height = height))
  }
  # select_data_vario() ----
  # variogram list and layout for current data in vario 1 and 2, based on select_data in visualization page. modeled mode have multiple models for every animal, need to have additional selection on models and new set of input, layout. The non-model mode and model mode are separate and need to independent from each other, both available no matter what mode is selected in UI, because home range/occurrence need model layout, fine-tune etc need vario info to avoid recalculation
  select_data_vario <- reactive({
    tele_list <- select_data()$tele_list
    # guess value need to be reactive so it can be modified in manual fit.
    values$selected_data_guess_list <- lapply(tele_list,
                    function(tele) ctmm::ctmm.guess(tele, interactive = FALSE))
    vario_list <- lapply(tele_list, ctmm::variogram)
    names(vario_list) <- names(tele_list)  # needed for figure title
    vario_layout <- layout_group(vario_list,
                                     input$vario_height, input$vario_columns)
    return(list(vario_list = vario_list, vario_layout = vario_layout))
  })
  # current_vario_model_list() ----
  # if put inside vario_parameters, fine tune didn't trigger changes correctly. Putting an expression used reactive values in function parameter may not work, that's why we need a data.table object call in DT. maybe because guess_list is defined inside that reactive, new changes didn't override the initialization
  # current_vario_model_list <- reactive({
  #   switch(input$vario_mode,
  #          empirical = NULL,
  #          guesstimate = values$selected_data_guess_list,
  #          modeled = select_models()$model_list
  #   )
  # })
  # current_vario() ----
  # switch vario_list, layout according to option. vario plot need height, log_save_vario need row count as figure height in inches. in modeled mode, there could be different vario count so layout is different
  # current_vario <- reactive({
  #   current <- list()
  #   if (input$vario_mode != "modeled") {
  #     current <- select_data_vario()
  #   } else {
  #     # model mode take from model selection
  #     current$vario_list <- select_models()$vario_list
  #     current$vario_layout <- select_models()$vario_layout
  #   }
  #   return(current)
  # })
  # variogram 1:empri ----
  # no model, model_color parameter
  output$vario_plot_1 <- renderPlot({
    # actual fraction value from slider is not in log, need to convert
    ctmmweb::plot_vario(select_data_vario()$vario_list,
                        fraction = 10 ^ input$zoom_lag_fraction,
                        relative_zoom = (input$vario_option == "relative"),
                        cex = 0.72,
                        columns = input$vario_columns)
    # LOG save pic
    log_save_vario("vario", select_data_vario()$vario_layout$row_count,
                   input$vario_columns)
  }, height = function() { # always use current selected layout
    select_data_vario()$vario_layout$height
    }
  )
  # variogram 2:guess ----
  output$vario_plot_2 <- renderPlot({
    # actual fraction value from slider is not in log, need to convert
    ctmmweb::plot_vario(select_data_vario()$vario_list,
                        values$selected_data_guess_list,
                        fraction = 10 ^ input$zoom_lag_fraction,
                        relative_zoom = (input$vario_option == "relative"),
                        model_color = "green", cex = 0.72,
                        columns = input$vario_columns)
    # LOG save pic
    log_save_vario("vario", select_data_vario()$vario_layout$row_count,
                   input$vario_columns)
  }, height = function() { # always use current selected layout
    select_data_vario()$vario_layout$height
  }
  )
  # variogram 3:modeled ----
  # all based on model selection table rows, by select_models(), only update after table generated and there is row selection updates. select_models() find model and variogram based on row selection, but if row selection didn't change, the reactive is not triggered so no modeled variogram drawn.
  output$vario_plot_3 <- renderPlot({
    # actual fraction value from slider is not in log, need to convert
    ctmmweb::plot_vario(select_models()$vario_list,
                        select_models()$model_list,
                        fraction = 10 ^ input$zoom_lag_fraction,
                        relative_zoom = (input$vario_option == "relative"),
                        model_color = "purple", cex = 0.72,
                        columns = input$vario_columns)
    # LOG save pic
    log_save_vario("vario", select_models()$vario_layout$row_count,
                   input$vario_columns)
  }, height = function() { # always use current selected layout
    select_models()$vario_layout$height
  }
  )
  # select individual plot to fine tune
  output$tune_selector <- renderUI({
    tele_list <- req(select_data()$tele_list)
    identities <- sapply(tele_list, function(x) x@info$identity)
    selectInput("tune_selected", NULL,
                c("Fine-tune" = "", identities))
  })
  # fine tune fit start ----
  observeEvent(input$tune_selected, {
    if (input$tune_selected != "") {
      # LOG fine tune start
      log_msg("Fine-tune Parameters for", input$tune_selected)
      showModal(modalDialog(title = paste0("Fine-tune parameters for ",
                                           input$tune_selected),
                      fluidRow(column(4, uiOutput("fit_sliders")),
                               column(8, plotOutput("fit_plot")),
                               column(4, offset = 2, uiOutput("fit_zoom"))),
                      size = "l",
                      footer = fluidRow(
        column(3, actionButton("center_slider", "Center current sliders",
                               icon = icon("align-center"))),
        column(3, offset = 2,
               modalButton("Cancel", icon = icon("ban"))),
        column(2, offset = 2,
               actionButton("tuned", "Apply",
                            icon = icon("check"),
                            style = ctmmweb:::STYLES$page_action)))
                            ))
    }
  })
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
    # didn't use the step value
    slider_list <- lapply(1:nrow(dt), function(i) {
      sliderInput(
        inputId = paste0("vfit_", dt[i, name]),
        label = dt[i, label], min = round(dt[i, min], 3),
        max = round(dt[i, max], 3), value = round(dt[i, initial], 3),
        step = 0.001)
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
  # fine tune fit end ----
  # p5. model selection ----
  callModule(click_help, "model_selection", title = "Model Selection",
             size = "l", file = "help/5_c_model_selection.md")
  # $selected_model_try_res ---
  # use value instead of reactive expression, because we used a button so need to use observeEvent, cannot start fit automatically by reactive expression.
  # this is the try model (model selection in ctmm context, but we have a select model process, so use different names now) results for current animal subset. home range and occurence are based on further selected models
  # values$selected_data_model_try_res <- NULL  # need to clear this at input change too
  # try models() ----
  try_models <- reactive({
    # need 1st tab ready. write separately, don't want to check length on req
    req(values$selected_data_guess_list)
    # not the best measure to detect data inconsistency but the simplest. rely on select_data to switch tab, make sure go through 1st tab first.
    req(length(select_data()$tele_list) ==
          length(values$selected_data_guess_list))
    # if data changed, using old 1st tab data is also not right. the first error is two list length don't match. use if instead of simple req to give message
    # if (length(select_data()$tele_list) !=
    #     length(values$selected_data_guess_list)) {
    #   updateTabsetPanel(session, "vario_tabs", selected = "1")
    #   req(FALSE)
    # } else {
    tele_guess_list <- ctmmweb::align_list(select_data()$tele_list,
                                           values$selected_data_guess_list)
    # LOG try models
    log_msg("Trying different models...")
    withProgress(print(system.time(
      res <-
        par_try_tele_guess_mem(tele_guess_list,
                               parallel = option_selected("parallel")))),
      message = "Trying different models to find the best ...")
    # always save names in list
    names(res) <- names(select_data()$tele_list)
    # we are selecting rows on a table just generated.
    # this line caused update loop, the summary_models changed
    # DT::selectRows(proxy_model_dt, summary_models()$first_models)
    return(res)
    # }
  })
  # observeEvent(input$try_models, {
  #   # it's common to use existing table row selection in some reactives, until the correct selection updated and reactive evaluate again. With previous fitted models and selection rows, next fit on different animal will first try to plot with existing selection number. Freeze it so we can update the correct selection first. freeze halt the chain (like req), then thaw after other finished.
  #   # freeze didn't solve the problem when fit models and have table generated, row selected. disable paralle, fit again, table didn't update, no row selection event, no selected models update. this can be solved by selecting some row in table.
  #   # freezeReactiveValue(input, "tried_models_summary_rows_selected")
  #   # instead, we clear the table selection, which should solve both needs. the clear is not executed until fitting finished, but it's queued before actual selecting, so table did update.
  #   DT::selectRows(proxy_model_dt, list())
  #   # guess_list is updated inside select_data_vario_list, but select_data_vario_list is not referenced here, if still in model mode, it was not referenced in UI too, so it didn't get updated.
  #   tele_guess_list <- ctmmweb::align_list(select_data()$tele_list,
  #                                 values$selected_data_guess_list)
  #   # LOG try models
  #   log_msg("Trying different models...")
  #   withProgress(print(system.time(
  #     values$selected_data_model_try_res <-
  #       par_try_tele_guess_mem(tele_guess_list,
  #                              parallel = option_selected("parallel")))),
  #     message = "Trying different models to find the best ...")
  #   # always save names in list
  #   names(values$selected_data_model_try_res) <- names(select_data()$tele_list)
  #   # sometimes nothing is shown in 3 modes. could be data lock in complex reactive relationship. try to freeze the radio button to make sure it update in last
  #   # freezeReactiveValue(input, "vario_mode")
  #   # updateRadioButtons(session, "vario_mode", selected = "modeled")
  #   # we are selecting rows on a table just generated.
  #   DT::selectRows(proxy_model_dt, summary_models()$first_models)
  # })
  # summary_models() ----
  # summary table and model dt with model as list column
  summary_models <- reactive({
    # the dt with model in list column
    models_dt <- ctmmweb:::model_try_res_to_model_list_dt(
      # req(values$selected_data_model_try_res)
      try_models()
      )
    # the model summary table
    formated_summary_dt <-
      ctmmweb:::model_list_dt_to_formated_model_summary_dt(models_dt)
    if (input$hide_ci_model) {
      formated_summary_dt <- formated_summary_dt[
        !stringr::str_detect(estimate, "CI")]
    }
    # need a full model table with identity(for base color), full name to create model color, basically a full version of selected model table. the color pallete and mapping function must be based on full table, not current selected subset.
    model_names_dt <- unique(formated_summary_dt[,
                            .(identity, model_type, model_name)])
    # prepare model color, identity color function
    model_names_dt[, base_color := values$id_pal(identity)]
    model_names_dt[, variation_number := seq_len(.N), by = identity]
    model_names_dt[, model_color :=
                     ctmmweb:::vary_color(base_color, .N)[variation_number],
                   by = identity]
    # need ordered = TRUE for character vector not being factor yet.
    hr_pal <- leaflet::colorFactor(model_names_dt$model_color,
                          model_names_dt$model_name, ordered = TRUE)
    # calculate the first model row number depend on table mode (hide/show CI)
    # we don't want the row number to show in the final table
    dt <- copy(formated_summary_dt)
    dt[, row_no := .I]
    model_position <- if (input$hide_ci_model) 1 else 2
    first_models <- dt[, row_no[model_position], by = identity]$V1
    return(list(models_dt = models_dt, # with CTMM model in column
                summary_dt = formated_summary_dt,
                model_names_dt = model_names_dt, # full name, color
                hr_pal = hr_pal,
                first_models = first_models))
  })
  # model summary ----
  # format model summary table as DT, also used in home range page
  render_model_summary_DT <- function(dt, model_types, info_p) {
    DT::datatable(dt,options = list(scrollX = TRUE,
                                    pageLength = 18,
                                    lengthMenu = c(18, 36, 72)),
                  rownames = FALSE) %>%
      # majority cells in color by model type
      DT::formatStyle('model_type', target = 'row',
                      color = DT::styleEqual(
                        model_types, scales::hue_pal()(length(model_types)))
      ) %>%
      # override the id col color
      DT::formatStyle('identity', target = 'cell',
                      color = DT::styleEqual(info_p$identity,
                                             scales::hue_pal()(nrow(info_p)))
      ) %>%
      # override the low/high cols with background
      DT::formatStyle('estimate', target = 'row',
                      backgroundColor = DT::styleEqual(
                        c("CI low", "ML" , "CI high"),
                        c("#FFFFFF", "#F7F7F7", "#F2F2F2"))
      )
  }
  output$tried_models_summary <- DT::renderDataTable({
    # should not need to use req on reactive expression if that expression have req inside.
    dt <- copy(summary_models()$summary_dt)
    # delete extra col here so it will not be shown, need to copy first otherwise it get modified.
    dt[, model_no := NULL]
    dt[, model_name := NULL]
    # LOG tried models
    log_dt_md(dt, "Tried Models")
    # need the full info table to keep the color mapping when only a subset is selected
    info_p <- values$data$merged$info
    # CI_colors <- color_CI(values$data$merged$info$identity)
    # base::sort have different result in linux, hosted server.
    model_types <- stringr::str_sort(unique(dt$model_type))
    DT_table <- render_model_summary_DT(dt, model_types, info_p)
    # select models otherwise no variogram plot
    DT::selectRows(proxy_model_dt, summary_models()$first_models)
    return(DT_table)
  })
  proxy_model_dt <- DT::dataTableProxy("tried_models_summary")
  observeEvent(input$select_1st_models, {
    DT::selectRows(proxy_model_dt, summary_models()$first_models)
  })
  observeEvent(input$clear_models, {
    # use list() instead of NULL to avoid R 3.4 warning on I(NULL). After DT fixed this warning we can change back to NULL
    DT::selectRows(proxy_model_dt, list())
  })
  # select_models() ----
  # this is the manual selecting rows in model summary table. previously first models are selected in try models action.
  # previously we use first model if no selection. now we select them automatically so the intent is more clear, and it's easier to modify selection based on this. this is triggered by row selection changes. need to force row selection change or clear it first, or freeze it when need to update this reactive, which is needed for drawing modeled variograms.
  select_models <- reactive({
    # change signal variable so that overlap table rows should not be used now. this is similar to the clear row selection action in try models
    # overlap_table_ready <- FALSE
    # DT::selectRows(proxy_overlap_dt, list())
    # req(!is.null(values$selected_data_model_try_res))
    req(length(input$tried_models_summary_rows_selected) > 0)
    # sort the rows selected so same individual models are together
    rows_selected_sorted <- sort(input$tried_models_summary_rows_selected)
    # previous model selection value may still exist
    model_summary_dt <- summary_models()$summary_dt
    selected_names_dt <- unique(model_summary_dt[rows_selected_sorted,
                                           .(identity, model_type, model_name)])
    # we want to remove the model part from displayed name if there is no multiple models from same animal. model_name is a unique full name, better keep it as it's used in color mapping, while the displayed name can change depend on selection -- once selected multiple models with same animal, displayed name will change.
    # display_name is a dynamic column depend on selection so it's created here. use simple animal name when no duplicate, full model name when multiple models from same animal are selected. Although created here, it's not shown in model summary table, but can be used in plot title, overlap tables. the condition is negative here but it matches the verb: !=0 means duplicate exist.
    # home range table, plot, overlap page take display_name. the model page still use modal name even no duplication, because the model table exists.
    if (anyDuplicated(selected_names_dt, by = "identity") != 0) {
      selected_names_dt[, display_name := model_name]
    } else {
      selected_names_dt[, display_name := identity]
    }
    # get color
    selected_names_dt <- merge(summary_models()$model_names_dt,
                               selected_names_dt,
                               by = c("identity", "model_type", "model_name"))
    # overlap table, overlap home range plot need colors. it cannot be based on identity only because multiple models of same identity can be selected. so it will be model_color, just like maps. apply them to home range, occurenc too.
    # These information came from model_summary (display name depend on row selection, in select_models)
    # color overlap table need a function map from v1 v2 value to color. all v1 v2 value came from display name, so we just add a color column.
    # DT color utility function require a v1 v2 name levels and color vector in same order, just display_name column and color column
    # home range plot need a color vector in same order of each pair, actually a function that map display name to color.
    # home range/occurrence plot need color vector in same order
    # we create a named vector [display_name = color] for indexing, and create a function from that vector in home range plot. compare to creating the mapping function here, the indexing vector is created in one time merging instead of each checking need a merge, and the transfered parameter is a static vector instead of a function with enclosed variables.
    # all needs can be satisfied with this named vector.
    display_color <- selected_names_dt$model_color
    names(display_color) <- selected_names_dt$display_name
    # selections can be any order, need to avoid sort to keep the proper model order
    selected_models_dt <- merge(selected_names_dt, summary_models()$models_dt,
                                by = c("identity", "model_type"), sort = FALSE)
    # the row click may be any order or have duplicate individuals, need to index by name instead of index
    selected_tele_list <- select_data()$tele_list[selected_names_dt$identity]
    # data.table of further selection of models on row selection select_data()
    selected_data_dt <- select_data()$data_dt[
      identity %in% selected_names_dt$identity]
    selected_model_list <- selected_models_dt$model
    # the modeled variogram plot title come from here. For now it's model_name.
    names(selected_model_list) <- selected_names_dt$model_name
    selected_vario_list <- select_data_vario()$vario_list[
      selected_names_dt$identity]
    # selected_names_dt[, model_name := stringr::str_c(identity, " - ", model_type)]
    # vario layout for selected models
    selected_vario_layout <- layout_group(selected_vario_list,
                                     input$vario_height, input$vario_columns)
    # LOG selected models
    log_dt_md(selected_names_dt[, .(identity, model_type)], "Selected Models")
    # must make sure all items in same order
    return(list(names_dt = selected_names_dt,
                display_color = display_color,
                tele_list = selected_tele_list,
                data_dt = selected_data_dt,
                model_list = selected_model_list,
                vario_list = selected_vario_list,
                vario_layout = selected_vario_layout
                ))
  })
  # p6. home range ----
  callModule(click_help, "home_range", title = "Home Range",
             size = "l", file = "help/6_home_range.md")
  # select_models_hranges() ----
  select_models_hranges <- reactive({
    req(select_models())
    # LOG home range calculation
    log_msg("Calculating Home Range ...")
    withProgress(print(system.time(
      res <- akde_mem(select_models()$tele_list,
                      CTMM = select_models()$model_list))),
      message = "Calculating Home Range ...")
    # add name so plot can take figure title from it
    # used to be model name, changed to display name. both the plot title and overlap result matrix names come from this.
    names(res) <- select_models()$names_dt$display_name
    return(res)
  })
  # home range levels ----
  # function on input didn't update, need a reactive expression? also cannot create a function to generate reactive expression, didn't update. don't really need a function but it was referenced 3 times so this is easier to use. compare to occur which only was used once so no need for function
  get_hr_levels <- reactive({ctmmweb:::parse_levels.UD(input$hr_contour_text)})
  # home range summary ----
  output$range_summary <- DT::renderDataTable({
    # hrange_summary_dt <- model_list_dt_to_model_summary_dt(
    #   build_hrange_list_dt(select_models()$names_dt, select_models_hranges()),
    #   hrange = TRUE)
    # dt <- format_hrange_summary_dt(hrange_summary_dt)
    hrange_list_dt <- ctmmweb:::build_hrange_list_dt(select_models()$names_dt,
                                           select_models_hranges())
    dt <- ctmmweb:::hrange_list_dt_to_formated_range_summary_dt(hrange_list_dt,
                                                                get_hr_levels())
    # remove extra columns to save space
    dt[, model_no := NULL]
    dt[, model_name := NULL]
    # LOG home range summary
    log_dt_md(dt, "Home Range Summary")
    if (input$hide_ci_hrange) {
      dt <- dt[!stringr::str_detect(estimate, "CI")]
      # dt[, estimate := NULL]
    }
    info_p <- values$data$merged$info
    # still use the full model type table color mapping to make it consistent.
    model_types <- stringr::str_sort(unique(
      summary_models()$summary_dt$model_type))
    render_model_summary_DT(dt, model_types, info_p)
  })

  # home range plot ----
  output$range_plot <- renderPlot({
    # browser()
    # selected_tele_list <- select_models()$tele_list
    ctmmweb::plot_ud(select_models_hranges(),
                     level_vec = get_hr_levels(),
                     color_vec = select_models()$display_color,
                     option = input$hrange_option,
                     columns = input$vario_columns, cex = 0.72,
                     tele_list = select_models()$tele_list)
    # selected_tele_list <- select_models()$tele_list
    # def.par <- graphics::par(no.readonly = TRUE)
    # graphics::par(mfrow = c(select_models()$vario_layout$row_count,
    #                         input$vario_columns),
    #     mar = c(5, 5, 4, 1), ps = 18, cex = 0.72, cex.main = 0.9)
    # lapply(seq_along(selected_tele_list), function(i) {
    #   plot(selected_tele_list[[i]], UD = select_models_hranges()[[i]],
    #        level.UD = get_hr_levels())
    #   graphics::title(select_models()$names_dt$model_name[i])
    #   # title(sub = "Error on", cex.sub = 0.85, col.sub = "red")
    # })
    # LOG save pic
    log_save_vario("home_range", select_models()$vario_layout$row_count,
                   input$vario_columns)
    log_save_UD("home_range")
    # always use model mode vario layout, different from vario plot which have 3 modes.
  }, height = function() { select_models()$vario_layout$height })
  # the actual export functions. multiple variables in environment are used. put them into functions so we can reorganize raster/shapefile in same dialog easier.
  # export raster ----
  # file_extension doesn't include . so we can use it also in folder name.
  # this need to be a function so that we can use different file extension with raster, and the switch call is much simpler. to combine into shapefile function need a lot parameters. could refactor if have more usage.
  export_rasterfiles <- function(file, file_extension) {
    save_rasterfiles <- function(hrange_list) {
      write_f <- function(folder_path) {
        # hrange_list came from select_models(), so the order should be synced
        for (i in seq_along(hrange_list)) {
          ctmm::writeRaster(hrange_list[[i]], folder = folder_path,
                            file = file.path(folder_path,
          # every component in file.path is a level in folder, file name need to concatenated first.
              paste0(select_models()$names_dt$model_name[i],
                     ".", file_extension)
                                    ))
        }
      }
      return(write_f)
    }
    ctmmweb:::build_zip(file, save_rasterfiles(select_models_hranges()),
                        session_tmpdir, paste0("Home_Range_",
                                               file_extension, "_"))
    # LOG build raster
    log_msg(paste0(file_extension, " files built and downloaded"))
  }
  # export shapefiles ----
  export_shapefiles <- function(file) {
    # closure: create a function that take reactive parameters, return a function waiting for folder path. use it as parameter for build zip function, which provide folder path as parameter
    # functional::Curry is misnomer, and it's extra dependency. this function take some data parameters, return a function that only need target path part. that function was called by the write file function in downloadhandler, when part of the target path was provided.
    save_shapefiles <- function(hrange_list, ud_levels) {
      write_f <- function(folder_path) {
        # hrange_list came from select_models(), so the order should be synced
        for (i in seq_along(hrange_list)) {
          ctmm::writeShapefile(hrange_list[[i]], level.UD = ud_levels,
                               folder = folder_path,
                               file = select_models()$names_dt$model_name[i])
        }
      }
      return(write_f)
    }
    ctmmweb:::build_zip(file, save_shapefiles(select_models_hranges(),
                                              get_hr_levels()),
                        session_tmpdir, "Home_Range_shapefile_")
    # LOG build shapefiles
    log_msg("Shapefiles built and downloaded")
  }
  # export dialog ----
  observeEvent(input$export_homerange_dialog, {
    showModal(modalDialog(title = "Export All Home Ranges to Zip",
      fluidRow(
        column(12, radioButtons("homerange_export_format", "Format",
                    choiceNames = list(
    div("Esri shapefile", pre("polygons of the low, ML, and high home-range area estimates.")),
    # "Esri shapefile: polygons corresponding to the low, ML, and high home-range area estimates.",
    div("raster package native format .grd", pre("pixel values corresponding to the density function.")),
    # "Native raster package format .grd: pixel values corresponding to the density function.",
    div("GeoTiff .tif", pre("pixel values corresponding to the density function."))
    # "GeoTiff .tif: pixel values corresponding to the density function."
                                    ),
                    # make sure file type name is consistent with extension. we used file type in zip file name, and extension in folder inside zip.
                    choiceValues = list("shapefile", "grd", "tif"),
                    width = "100%"
                               )
               ),
        column(12, h4("See more details about file format in ",
                      tags$a(href = "https://ctmm-initiative.github.io/ctmm/reference/export.html", "ctmm::export"), ", ",
                      tags$a(href = "https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/writeRaster", "raster::writeRaster")
                      ))
      ),
      size = "m",
      footer = fluidRow(
        column(3, offset = 0,
               modalButton("Cancel", icon = icon("ban"))),
        column(3, offset = 6,
               downloadButton("download_homerange",
                              "Save",
                              icon = icon("save"),
                              style = ctmmweb:::STYLES$download_button))
      )
    ))
  })
  # export home ranges ----
  output$download_homerange <- downloadHandler(
    filename = function() {
      # up to min so it should be consistent with the folder name inside zip
      current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M")
      paste0("Home_Range_", input$homerange_export_format, "_", current_time, ".zip")
    },
    content = function(file) {
      switch(input$homerange_export_format,
             shapefile = export_shapefiles(file),
             grd = export_rasterfiles(file, "grd"),
             tif = export_rasterfiles(file, "tif"))
    }
  )
  # # raster download --
  # output$export_raster <- downloadHandler(
  #   filename = function() {
  #     # up to min so it should be consistent with the folder name inside zip
  #     current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  #     paste0("Home_Range_Raster_", current_time, ".zip")
  #   },
  #   content = function(file) {
  #     export_rasterfiles(file)
  #   }
  # )
  # # shapefiles download --
  # output$export_hrange <- downloadHandler(
  #   filename = function() {
  #     # up to min so it should be consistent with the folder name inside zip
  #     current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  #     paste0("Home_Range_Shape_", current_time, ".zip")
  #   },
  #   content = function(file) {
  #     export_shapefiles(file)
  #     # # closure: create a function that take reactive parameters, return a function waiting for folder path. use it as parameter for build zip function, which provide folder path as parameter
  #     # # functional::Curry is misnomer, and it's extra dependency.
  #     # save_shapefiles <- function(hrange_list, ud_levels) {
  #     #   write_f <- function(folder_path) {
  #     #     # hrange_list came from select_models(), so the order should be synced
  #     #     for (i in seq_along(hrange_list)) {
  #     #       ctmm::writeShapefile(hrange_list[[i]], level.UD = ud_levels,
  #     #                      folder = folder_path,
  #     #                      file = select_models()$names_dt$model_name[i])
  #     #     }
  #     #   }
  #     #   return(write_f)
  #     # }
  #     # ctmmweb:::build_shapefile_zip(file,
  #     #                               save_shapefiles(select_models_hranges(),
  #     #                                               get_hr_levels()),
  #     #                               session_tmpdir)
  #     # # LOG build shapefiles
  #     # log_msg("Shapefiles built and downloaded")
  #   }
  # )
  # p7. overlap ----
  callModule(click_help, "overlap", title = "Overlap",
             size = "l", file = "help/7_overlap.md")
  # select_models_overlap() ----
  select_models_overlap <- reactive({
    # home range overlap
    overlap_hrange <- ctmm::overlap(select_models_hranges(),
                                    CTMM = select_models()$model_list)
    # data.table of overlap matrix. round 4 digits because value is 0 ~ 1
    overlap_hrange %>%
      ctmmweb::overlap_matrix_to_dt() %>%
      ctmmweb:::overlap_2d_to_1d()
    # overlap_matrix_dt <- ctmmweb::overlap_matrix_to_dt(
    #   overlap_hrange, clear_half = TRUE)
    # overlap_dt <- ctmmweb:::overlap_2d_to_1d(overlap_matrix_dt)
    # return(overlap_dt)
  })
  # overlap table ----
  output$overlap_summary <- DT::renderDataTable({
    dt <- copy(select_models_overlap())
    # to color the v1 v2 columns, need a function that map from v1 v2 values (could be identity or full model name) to color, when using DT utility function this means a levels (all possible names) vector and color vector in same order.
    display_color <- select_models()$display_color
    # don't need the combination column. we can hide columns in DT but it's quite complex with 1 index trap
    dt[, Combination := NULL]
    # LOG overlap summary
    log_dt_md(dt, "Overlap Summary")
    # when data updated, prevent location plot to use previous row selection value on new data (because row selection update slowest after table and plot). this will ensure row selection only get flushed in the end.
    # when not freezed, switch back and forth will cause plot update twice and save twice, first time old plot, then updated plot, or two same plot. see DT_row_update_problem.Rmd for minimal example, also see console output with shiny trace on commit 3.8 2pm.
    # however, sometimes this freeze will cause the range plot pause after switching back. that doesn't happen with trace On. could be some update order problem. sometimes this worked? add clear table for additional protection
    freezeReactiveValue(input, "overlap_summary_rows_selected")
    # reset signal variable after DT rendering finish
    # on.exit(overlap_table_ready <- TRUE)
    # COPY start note code changed in color part --
    DT::datatable(dt,
                  # class = 'table-bordered',
                  options = list(pageLength = 18,
                                 lengthMenu = c(6, 12, 18, 36),
                                 order = list(list(4, 'desc'))),
                  rownames = TRUE) %>%
      # override the low/high cols with background
      DT::formatStyle(c("CI low", "CI high"),
                      color = scales::hue_pal()(1)) %>%
      DT::formatStyle("ML", color = "blue") %>%
      # override the id col color
      DT::formatStyle(c("v1", "v2"), target = 'cell',
                      color = DT::styleEqual(names(display_color),
                                             display_color)
      )
    # COPY end --
  })
  # try to make DT render fastest so that other plot update later with proper row information. didn't work. it's only for output, not reactive update order.
  # outputOptions(output, "overlap_summary", priority = 99)
  # overlap value range ----
  output$overlap_plot_value_range <- renderPlot({
    overlap_dt <- select_models_overlap()
    # need to wait until table is finished, use current page.
    current_order <- overlap_dt[rev(req(input$overlap_summary_rows_current)),
                                Combination]
    # tried to move the dynamic column part to reactive expression, which would cause the table refresh twice in start (update after table is built), and row selection caused data change, table refresh and lost row selection.
    # want to show all values if just selected rows, but update with filter. rows_all update with filter, plot use limits to filter them. selected rows only update a column and change color. this is different from the other 2 tab.
    # COPY start --
    overlap_dt[, selected := FALSE]
    overlap_dt[input$overlap_summary_rows_selected, selected := TRUE]
    g <- ggplot2::ggplot(overlap_dt, ggplot2::aes(x = ML, y = Combination,
                                                  color = selected)) +
      # make plot sync with table sort and filtering
      ggplot2::scale_y_discrete(limits = current_order) +
      # na.rm in point, text, errorbar otherwise will warning in filtering
      ggplot2::geom_point(size = 2, na.rm = TRUE, color = "blue") +
      {if (input$show_overlap_label) {
        ggplot2::geom_text(ggplot2::aes(label = ML), hjust = 0, vjust = -0.5,
                           show.legend = FALSE, na.rm = TRUE)}} +
      ggplot2::geom_errorbarh(ggplot2::aes(xmax = `CI high`, xmin = `CI low`),
                              size = 0.45, height = 0.35, na.rm = TRUE) +
      ggplot2::xlab("Overlap") + ctmmweb:::BIGGER_THEME
    # COPY end --
    # LOG save pic
    log_save_ggplot(g, "overlap_plot_value_range")
  }, height = function() { input$overlap_plot_height }, width = "auto"
  )
  # choose_overlap_pairs() ----
  # plot height cannot use value calculated inside plot function. the height depend on selection count, so move the selection logic into reactive
  choose_overlap_pairs <- reactive({
    # rows_current inside ifelse, may not trigger changes. put one outside. esp test with more rows in 2nd try, more pages lead to slower DT and more prone to update problem.
    req(input$overlap_summary_rows_current)
    # chose all pairs in current page with overlap > 0 if no rows selected. otherwise selected rows with same order. the value ggplot use `selected` column in dt
    # go through rows_current to match order and filter, also in current page in both case, since order and filter can apply both when rows are selected or not
    # when nothing selected. don't use length == 0 because this is more specific
    if (is.null(input$overlap_summary_rows_selected)) {
      chosen_rows <- select_models_overlap()[
        req(input$overlap_summary_rows_current)][ML != 0, .(v1, v2)]
    } else {
      # req both value to prevent the status when table is not ready
      selected_rows_in_current_order <-
        intersect(req(input$overlap_summary_rows_current),
                  req(input$overlap_summary_rows_selected))
      chosen_rows <- select_models_overlap()[
        selected_rows_in_current_order, .(v1, v2)]
    }
    chosen_hranges_list <- lapply(1:nrow(chosen_rows), function(i) {
      # req: temporary hack to prevent empty data selected, when new smaller data used with old big row numbers, certain row vector become NA,NA. there still could be wrong data selected (not intended mismatch), but at least no error in console. There is no better solution now since with freeze sometimes the plot doesn't update after rows update finished.
      select_models_hranges()[req(unlist(chosen_rows[i]))]
    })
    chosen_tele_list_list <- lapply(1:nrow(chosen_rows), function(i) {
      select_models()$tele_list[req(unlist(chosen_rows[i]))]
    })
    # home range plot need a color vector in same order of each pair, actually a function that map display name to color.
    if ("two_colors" %in% input$overlap_hrange_option) {
      chosen_colors_list <- lapply(1:nrow(chosen_rows), function(i){
        c("#FF7970", "#619CFF")  # the cilla/queen combination, orange/blue
      })
    } else {
      chosen_colors_list <- lapply(1:nrow(chosen_rows), function(i) {
        sapply(chosen_rows[i], function(display_name) {
            select_models()$display_color[display_name]
        })})
    }
    overlap_hrange_layout <- layout_group(chosen_hranges_list,
                                          input$overlap_hrange_height,
                                          input$overlap_hrange_columns)
    return(list(chosen_hranges_list = chosen_hranges_list,
                chosen_tele_list_list = chosen_tele_list_list,
                chosen_colors_list = chosen_colors_list,
                overlap_hrange_layout = overlap_hrange_layout))
  })
  # overlap home range ----
  output$overlap_plot_hrange <- renderPlot({
    ctmmweb:::plot_hr_group_list(
      choose_overlap_pairs()$chosen_hranges_list,
      choose_overlap_pairs()$chosen_tele_list_list,
      choose_overlap_pairs()$chosen_colors_list,
      level.UD = ctmmweb:::parse_levels.UD(
        input$overlap_hrange_contour_text),
      option = input$overlap_hrange_option,
      columns = input$overlap_hrange_columns)
    # LOG save plot
    # row_count <- ceiling(nrow(chosen_rows) / input$overlap_hrange_columns)
    log_save_vario("Overlap of Home Range",
                   choose_overlap_pairs()$overlap_hrange_layout$row_count,
                   input$overlap_hrange_columns)
    log_save_UD("Overlap of Home Range")
  }, height = function() {choose_overlap_pairs()$overlap_hrange_layout$height},
     width = "auto")
  # tried with plot/DT priority to make it update after DT. didn't work, maybe it's only render order, but data change already, still will render with updated data.
  # outputOptions(output, "overlap_plot_hrange", priority = 1)
  # # ovrelap locations (disabled) ----
  # # plot using row selection value of table, when data updated, both table and plot start to update but plot can use exsiting old row selection value which is either wrong or doesn't exist in new table. freeze row selection value in table code to make sure the access here is only thawed after all other reactive finish
  # overlap_plot_location_range <- add_zoom("overlap_plot_location")
  # output$overlap_plot_location <- renderPlot({
  #   animals_dt <- req(select_models()$data_dt)
  #   # show overview when no rows selected
  #   if (length(input$overlap_summary_rows_selected) == 0) {
  #     # no global data overlay in background
  #     g <- ctmmweb::plot_loc(animals_dt, loc_data = NULL, input$point_size_1) +
  #       ggplot2::coord_fixed(xlim = overlap_plot_location_range$x,
  #                            ylim = overlap_plot_location_range$y)
  #   } else {# show grouped plot of pairs when rows selected
  #     # because data.table modify by reference, the plot code actually added selected column already, but we use the selection number directly and not relying on this.
  #     selected_pairs_current_order <- select_models_overlap()$dt[
  #       select_rows_in_current_order(), .(v1, v2)]
  #     # usling lapply like a loop, so we don't need to initialize the list
  #     g_list <- lapply(1:nrow(selected_pairs_current_order), function(i) {
  #       # warning of drawing plot on empty data, not error
  #       suppressWarnings(
  #         ctmmweb::plot_loc(select_models()$data_dt[
  #           identity %in% selected_pairs_current_order[i]])
  #       )
  #     })
  #     g <- gridExtra::grid.arrange(grobs = g_list,
  #                                  ncol = input$overlap_loc_columns)
  #   }
  #   # LOG save plot
  #   log_save_ggplot(g, "overlap_plot_location")
  # },
  # # changing canvas and column sometimes doesn't cause update, switching tabs will update. try this parameter, seemed better.
  # execOnResize = TRUE,
  # height = function() { input$overlap_loc_height }, width = "auto")
  # # tried to use priority to make sure location plot update after table update, didn't work, probably because the problem is row selection reset happened slower
  # # outputOptions(output, "overlap_plot_location", priority = 1)
  # p8. occurrence ----
  callModule(click_help, "occurrence", title = "Occurrence Distribution",
             size = "l", file = "help/8_occurrence.md")
  # select_models_occurrences() ----
  select_models_occurrences <- reactive({
    # LOG Occurrence calculation
    log_msg("Calculating Occurrence ...")
    withProgress(print(system.time(
      res <- par_occur_mem(select_models()$tele_list,
                           select_models()$model_list,
                           parallel = option_selected("parallel")))),
                 message = "Calculating Occurrence ...")
    # if (option_selected("log_error")) {
    #   output$occurrence_info <- renderPrint(str(res))
    # }
    # add name so plot can take figure title from it
    # # used to be model name, changed to display name. both the plot title and overlap result matrix names come from this.
    names(res) <- select_models()$names_dt$display_name
    res
  })
  # function on input didn't update, need a reactive expression?
  # occur levels ----
  # get_oc_levels <- reactive({ctmmweb:::parse_levels.UD(input$oc_level_text)})
  output$occurrence_plot <- renderPlot({
    ctmmweb::plot_ud(select_models_occurrences(),
                     level_vec = ctmmweb:::parse_levels.UD(
                       input$oc_contour_text),
                     color_vec = select_models()$display_color,
                     option = input$occur_option,
                     cex = 0.72, columns = input$vario_columns,
                     tele_list = select_models()$tele_list)
    # LOG save pic
    log_save_vario("occurrence", select_models()$vario_layout$row_count,
                   input$vario_columns)
    log_save_UD("occurrence")
    # graphics::par(def.par)
  }, height = function() { select_models()$vario_layout$height })
  # p9. map ----
  callModule(click_help, "map", title = "Map",
             size = "l", file = "help/9_map.md")
  MAP_NAME_BY_TAB <- list(Point = "point_map", Heatmap = "heat_map")
  CURRENT_map_path <- list(Point = NULL, Heatmap = NULL)
  # save map to html, record html path in CURRENT_map_path. this is used in log save, and download map button.
  save_map <- function(leaf, map_type) {
    map_file_name <- stringr::str_c(map_type, "_", ctmmweb:::current_timestamp(), ".html")
    # LOG saving map
    log_msg(stringr::str_c("Saving map: ", map_type))
    map_path <- file.path(LOG_folder, map_file_name)
    # the library folder is still saved even with selfcontained = TRUE. This didn't happen in vignettes script. the source code said pandoc is needed for selfcontained option, but there is no error message.
    htmlwidgets::saveWidget(leaf, file = map_path, selfcontained = TRUE)
    # add link in rmd, difficult to embed map itself.
    log_add_rmd(stringr::str_c("\n[", map_type, "](", map_file_name, ")\n"))
    # record the latest file path
    CURRENT_map_path[[map_type]] <<- map_path
  }
  # shared basemap
  # tiles_info <- list(here = c("HERE.terrainDay", "HERE.satelliteDay",
  #                             "HERE.hybridDay"),
  #                    open = c("OpenTopoMap",
  #                             "Esri.WorldTopoMap", "Esri.WorldImagery"),
  #                    here_app_id = 'ehftALetcOLjvopsXsZP',
  #                    here_app_code = 'a5oE5ewb0eH9ojahDBLUzQ'
  # )
  # used for both point and heat map
  basemap <- ctmmweb::base_map()
  # use dynamic UI so we can adjust map height
  output$point_map_holder <- renderUI(
    leaflet::leafletOutput("point_map",
                  height = input$map_height)
  )
  # get_point_map() ----
  get_point_map <- reactive({
    dt <- select_data()$data_dt
    info <- select_data()$info
    # the color pallete need to be built upon full data set, not current subset
    # we cannot put id_pal in same place with hr_pal because user may check map without fitting models, when summary_models doesn't exist.
    withProgress(leaf <- basemap %>%
                   ctmmweb:::add_points(dt, info$identity, values$id_pal),
                 message = "Building maps...")
    # there could be mismatch between individuals and available home ranges. it's difficult to test reactive value exist(which is an error when not validated), so we test select_models instead. brewer pallete have upper/lower limit on color number, use hue_pal with different parameters.
    if (ctmmweb:::reactive_validated(select_models_hranges())) {
      # color pallete need to be on full model name list, but we don't want to change the model summary table since it doesn't need to be displayed in app.
      # hr_pal <- model_pal(summary_models()$model_names_dt, id_pal)
      # the pallete function always came from full data
      hr_pal <- summary_models()$hr_pal
      # so we need to use full model_name as domains
      selected_model_names <- select_models()$names_dt$model_name
      # though the layer name can be different. they are all just vectors in certain order, the home range/model_name/mapped color/display name all in same order.
      # use display name as layer name, but need to add post fix in simple format, when identity is not duplicated and used as display name directly
      if (anyDuplicated(select_models()$names_dt, by = "identity") == 0) {
       hrange_layer_names <- stringr::str_c(select_models()$names_dt$identity,
                                            " - Home Range")
      } else {
        hrange_layer_names <- selected_model_names
      }
      leaf <- leaf %>%
        ctmmweb:::add_home_range_list(select_models_hranges(), get_hr_levels(),
                            hr_pal(selected_model_names)) %>%
        ctmmweb::add_control(c(info$identity, hrange_layer_names))
    } else {
      leaf <- leaf %>%
        ctmmweb::add_control(info$identity)
    }
    return(leaf)
  })
  # reactive get map, render function save map and count time
  output$point_map <- leaflet::renderLeaflet({
    leaf <- get_point_map()
    print(system.time(
      save_map(leaf, "Point")
    ))
    return(leaf)
  })
  output$heat_map_holder <- renderUI(
    leaflet::leafletOutput("heat_map",
                  height = input$map_height)
  )
  # get_heat_map() ----
  # need reactive here because save map button need to access it outside render function
  get_heat_map <- reactive({
    # we didn't use the package function here because we can reuse basemap
    basemap %>% ctmmweb:::add_heat(select_data()$data_dt)
  })
  output$heat_map <- leaflet::renderLeaflet({
    leaf <- get_heat_map()
    print(system.time(
      save_map(leaf, "Heatmap")
    ))
    return(leaf)
  })
  # output$cluster_map_holder <- renderUI(
  #   leaflet::leafletOutput("cluster_map",
  #                 height = input$map_height)
  # )
  # need a history list of tabs, from tab switching and page switching
  # values$map_tab_history <- NULL
  # first map page view ----
  # check data size, switch to heatmap if too big. this only happen when moving into map page. Later there is no limit
  observeEvent(input$tabs, {
    if (input$tabs == "map") {
      # buffalo 17k, gulls 32k start to be slow. set threshold to 25k.
      # cat(nrow(select_data()$data_dt), "\n")
      if (nrow(select_data()$data_dt) > 25000) {
        # need the tab title to identify tab
        updateTabsetPanel(session, "map_tabs", selected = "Heatmap")
      }
      # values$map_tab_history <- list(previous = NULL,
      #                                current = input$map_tabs)
      # print(values$map_tab_history)
      # the point map bounds may still hold previous values and cause heatmap to update bounds? 1. small map with both tab updated, keep in heatmap tab 2. change to bigger data, switch to map page.
      # cat("pointmap: ", unlist(input$point_map_bounds), "\n")
    }
  })
  # map tab switching ----
  # for debug: print current values when clicked on map
  # observeEvent(input$heat_map_click, {
  #   cat("heatmap\n")
  #   print(input$heat_map_zoom)
  #   print(unlist(input$heat_map_bounds))
  # })
  # observeEvent(input$point_map_click, {
  #   cat("points\n")
  #   print(input$point_map_zoom)
  #   print(unlist(input$point_map_bounds))
  # })
  # ~set new tab map bounds/zoom to value of previous tab~ just apply heatmap bounds to point if enabled
  observeEvent(input$map_tabs, {
    # values$map_tab_history$previous <- values$map_tab_history$current
    # values$map_tab_history$current <- input$map_tabs
    # # print(values$map_tab_history)
    # cat(input$map_tabs, "\n")
    # the map bounds may not be updated yet in map initialization. only access the previous map bounds after switching, that should be up to date.
    # cat("heatmap: ", unlist(input$heat_map_bounds), "\n")
    # cat("pointmap: ", unlist(input$point_map_bounds), "\n")
    if (input$apply_heat_to_point && (input$map_tabs == "Point")) {
      # browser()
      # center_lng <- mean(input$heat_map_bounds$east, input$heat_map_bounds$west)
      # center_lat <- mean(input$heat_map_bounds$north,
      #                    input$heat_map_bounds$south)
      leaflet::leafletProxy("point_map", session) %>%
        # fitBounds(input$heat_map_bounds$east, input$heat_map_bounds$north,
        #           input$heat_map_bounds$west, input$heat_map_bounds$south)
        # setView(NULL, NULL, zoom = input$heat_map_zoom) %>%
        ctmmweb:::apply_bounds(input$heat_map_bounds)
        # fitBounds(input$heat_map_bounds$east, input$heat_map_bounds$north,
        #           input$heat_map_bounds$west, input$heat_map_bounds$south)
    }
  })
  # reset map view ----
  observeEvent(input$reset_map_view, {
    # fitBounds will have some allowance so no need to add padding here.
    bounds <- ctmmweb:::get_bounds(select_data()$data_dt)
    leaflet::leafletProxy(MAP_NAME_BY_TAB[[input$map_tabs]], session) %>%
      leaflet::fitBounds(bounds$lng1, bounds$lat1, bounds$lng2, bounds$lat2)
  })
  # download map ----
  output$download_map <- downloadHandler(
    filename = function() {
      paste0(input$map_tabs, "_", ctmmweb:::current_timestamp(), ".html")
    },
    content = function(file) {
      # LOG download map
      log_msg("Downloading map")
      # to save map with current view, update the map object with current bounds. the proxy only updated the in memory structure, not the map objec itself. The previously saved map by log don't have current bounds info, also that could be turned off.
      if (input$map_tabs == "Point") {
        leaf <- get_point_map() %>%
          ctmmweb:::apply_bounds(input$point_map_bounds)
        save_map(leaf, "Point")
      } else {
        leaf <- get_heat_map() %>%
          ctmmweb:::apply_bounds(input$heat_map_bounds)
        save_map(leaf, "Heatmap")
      }
      # leaf <- get_heat_map() %>%
      #   fitBounds(input$heat_map_bounds$east, input$heat_map_bounds$north,
      #             input$heat_map_bounds$west, input$heat_map_bounds$south)
      file.copy(CURRENT_map_path[[input$map_tabs]], file)
    }
  )
  # p10. report ----
  # save data ----
  output$save_data <- downloadHandler(
    filename = function() {
      paste0("Saved_", ctmmweb:::current_timestamp(), ".zip")
    },
    content = function(file) {
      # we are checking input data instead of select_data, which is the real condition that can cause error, because it's easier to check and should be in same status
      if (is.null(values$data$input_tele_list)) {
        showNotification("No data to save", duration = 7,
                         type = "error")
      } else {
        # LOG save data
        log_msg("Saving Data")
        # pack and save cache
        cache_zip_path <- ctmmweb::zip_folder(cache_path, "cache.zip")
        # data in .rds format, pack multiple variables into list first.
        # saved <- list(data = values$data
        #               # chosen_row_nos = select_data()$chosen_row_nos,
        #               # selected_data_model_try_res =
        #               #   values$selected_data_model_try_res,
        #               # selected_data_guess_list =
        #               #   values$selected_data_guess_list
        #               # didn't sort this, need to sort it when restoring
        #               # tried_models_summary_rows_selected =
        #               #   input$tried_models_summary_rows_selected
        #               )
        saved_rds_path <- file.path(session_tmpdir, "data.rds")
        saveRDS(values$data, file = saved_rds_path)
        # LOG save current telemetry data as csv so it can be imported easier. Only do this in generated report, not in the process to avoid too frequent saves.
        log_dt_md(values$data$merged$info,
                  "Current Telemetry Data")
        fwrite(values$data$merged$data_dt,
               file = file.path(session_tmpdir, "combined_data_table.csv"))
        # save error msg if captured
        if (input$capture_error) {
          flush(values$error_file_con)
          file.copy(values$error_file, file.path(session_tmpdir,
                                                 "error_log.txt"))
        }
        # also save report for reference
        generate_report(preview = FALSE)
        # move to same directory for easier packing. use rename to reduce effort
        # file.copy(values$html_path, file.path(session_tmpdir, "report.html"),
        #           overwrite = TRUE)
        file.rename(values$html_path, file.path(session_tmpdir, "report.html"))
        # the whole LOG folder with plot png/pdf in separate files. zip folder put zip to one level up the target folder, which is session_tmpdir. because the generated report was moved (not copied) to upper level, only other files are put in this zip.
        ctmmweb::zip_folder(LOG_folder, "plot.zip")
        # pack to saved.zip, this is a temp name anyway. being sepecific should be better than zip everything.
        saved_zip_path <- ctmmweb:::zip_relative_files(
          session_tmpdir, c("cache.zip", "data.rds", "report.html",
                            "error_log.txt",
                            "combined_data_table.csv", "plot.zip"),
          "saved.zip")
        file.copy(saved_zip_path, file)
      }
    }
  )
  # load data ----
  observeEvent(input$load_data, {
    # LOG load data
    log_msg("Loading previously saved data", input$load_data$name)
    # saved.zip -> cache.zip, data.rds, report.html, combined_data_table.csv
    utils::unzip(input$load_data$datapath, exdir = session_tmpdir)
    if (APP_local) {
      utils::browseURL(file.path(session_tmpdir, "report.html"))
    }
    # first clear current cache.
    reset_cache(cache_path)
    # using hard coded file name, need to search all usage when changed. cache.zip have cache folder inside it, so need to extract one level up
    utils::unzip(file.path(session_tmpdir, "cache.zip"), exdir = session_tmpdir)
    loaded_data <- readRDS(file.path(session_tmpdir, "data.rds"))
    # restore variables, also need to update id_pal, which are outside of data thus not restored, but it need to be built.
    values$data <- loaded_data
    values$id_pal <- build_id_pal(values$data$merged$info)
    shinydashboard::updateTabItems(session, "tabs", "plots")
  })
  # view_report ----
  generate_report <- function(preview) {
    # LOG report generated, need to be placed before the markdown rendering, otherwise will not be included.
    log_msg("Work Report Generated")
    # write markdown file
    markdown_path <- file.path(LOG_folder, "report.rmd")
    writeLines(LOG_rmd_vec, con = markdown_path)
    # render markdown to html
    html_path <- file.path(LOG_folder, "report.html")
    rmarkdown::render(markdown_path, output_file = html_path, quiet = TRUE)
    # file.copy(html_path, "www/report.html", overwrite = TRUE)
    # non-encoded file path cannot have white space for browserURL
    if (preview) utils::browseURL(html_path)
    values$html_path <- html_path
  }
  # preview in local mode, download in host mode
  output$view_report <- renderUI(
    if (APP_local) {
      actionButton("preview_report", "Preview Report",
                   icon = icon("file-text-o"),
                   style = ctmmweb:::STYLES$page_action)
    } else {
      downloadButton("download_report", "Download Report",
                     style = ctmmweb:::STYLES$download_button)
    }
  )
  observeEvent(input$preview_report, {
    generate_report(preview = TRUE)
  })
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Report_", ctmmweb:::current_timestamp(), ".html")
    },
    content = function(file) {
      # LOG download report
      log_msg("Downloading work report")
      generate_report(preview = FALSE)
      file.copy(values$html_path, file)
    }
  )
  # output$download_report_zip <- downloadHandler(
  #   filename = function() {
  #     paste0("Report_", ctmmweb:::current_timestamp(), ".zip")
  #   },
  #   content = function(file) {
  #     # LOG download report zip
  #     log_msg("Downloading work report zip")
  #     generate_report(preview = FALSE)
  #     zip_path <- ctmmweb::zip_folder(LOG_folder, "report.zip")
  #     file.copy(zip_path, file)
  #   }
  # )
}
