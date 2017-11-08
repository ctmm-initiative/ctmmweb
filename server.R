# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)
# enable more debugging and messages
debug_mode <- FALSE
# all helper functions are for server side
source("helpers.R", local = TRUE)

server <- function(input, output, session) {
  APP_local <- (isolate(session$clientData$url_hostname) == "127.0.0.1")
  # to copy from 07_report_save_load
  # global LOG variables ----
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
  log_add_rmd <- function(vec, on = TRUE) {
    if (!on) return()
    # used as function, will search variable in parent first, only go to global when not found. so need to make sure parent function don't have this
    LOG_rmd_vec <<- c(LOG_rmd_vec, vec)
  }
  # always do even switch is off. each session need to have individual folder.
  # use token as folder name, still create the timestamp folder as subfolder, so that the zip will have the timestamped folder
  # LOG_folder is session specific global variable inside server function, many functions need this as global variable and need to defined inside server instead of helpers.R
  create_log_folder <- function() {
    create_folder(file.path(session_tmpdir,
                            str_c("Report_", current_timestamp())))
  }
  # rely on several global variables. do side effect of console msg, and write string to global vector.
  # usually console content is same with markdown, except the data frame table need to be plain in console, table in markdown.
  log_msg_console <- function(msg, detail = "") {
    time_stamp <- str_c("[", Sys.time(), "]")
    if (detail != "") {
      detail <- str_c("\n\t", detail)
    }
    if (LOG_console) {
      cat(LOG_color_mappings$time_stamp(time_stamp),
          LOG_color_mappings$msg(msg),
          LOG_color_mappings$detail(detail), "\n")
    }
    return(time_stamp)
  }
  # setting default value of on because sometimes it was used internally. Make sure to assign on for every outside log call
  log_msg <- function(msg, detail = "", on = TRUE) {
    if (!on) return()
    time_stamp <- log_msg_console(msg, detail)
    # need extra new line for markdown
    log_add_rmd(str_c("`", time_stamp, "` ", msg, "\n\n\t", detail))
  }
  # common process for saving a plot
  log_prepare_plot <- function(f_name, f_ext = ".png") {
    pic_name <- str_c(f_name, "_", current_timestamp(), f_ext)
    log_msg("saving plot as", pic_name)
    log_add_rmd(str_c("![](", pic_name, ")"))
    return(file.path(LOG_folder, pic_name))
  }
  log_save_ggplot <- function(g, f_name, on = TRUE) {
    if (!on) return(g)
    print(system.time(ggsave(filename = log_prepare_plot(f_name), plot = g)))
    return(g)
  }
  # only used for variogram, with specific format and parameters, some came from input
  log_save_vario <- function(f_name, rows, cols, on = TRUE) {
    if (!on) return()
    dev.print(png, file = log_prepare_plot(f_name), units = "in", res = 220,
              width = cols * 4, height = rows * 3)
  }
  # pdf is better for home range, occurrence
  log_save_UD <- function(f_name, on = TRUE) {
    if (!on) return()
    dev.copy2pdf(file = log_prepare_plot(f_name, f_ext = ".pdf"))
  }
  # save dt into markdown table or csv. note the msg could be in different format
  log_dt_md <- function(dt, msg, on = TRUE) {
    if (!on) return()
    # need the extra \t because log_msg put \t before first line of detail
    time_stamp <- log_msg_console(msg,
                                  str_c(capture.output(dt), collapse = "\n\t"))
    log_add_rmd(c(str_c("`", time_stamp, "` ", msg, "\n"),
                  knitr::kable(dt, format = "markdown")))
  }
  # save dt in csv, need different msg format and a file name, so in independent function. f_name is used for part of csv file name, full name will be detail part of message
  log_dt_csv <- function(dt, msg, f_name, on = TRUE) {
    if (!on) return()
    csv_name <- str_c(f_name, "_", current_timestamp(), ".csv")
    fwrite(dt, file = file.path(LOG_folder, csv_name))
    log_msg(msg, detail = csv_name)
    log_add_rmd(str_c("[", csv_name, "](", csv_name, ")"))
  }
  # copy end ----
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
                     occurrence = "Occurrence",
                     map = "Map",
                     report = "Work Report")
  log_page <- function(title, on = TRUE) {
    if (!on) return()
    log_msg_console(str_c("## ", title))
    log_add_rmd(str_c("\n## ", title, "\n"))
  }
  # also notify the requirement of time subsetting. we want to show this everytime switched to this page. if put inside color_bin_animal it will only show once if switched back and forth.
  observeEvent(input$tabs, {
    req(values$data)
    # since we req data, so it will not record pages without data. This is good.
    log_page(page_title[[input$tabs]], on = input$record_on)
    if (input$tabs == "subset") {
      # must select single animal to proceed
      if (length(input$individuals_rows_selected) != 1) {
        updateTabItems(session, "tabs", "plots")
        showNotification(
          "Please select single individual first before time subsetting",
          type = "error", duration = 6)
      }
    }
  })
  # call outside of reactive context need isolate, they are also one time call only run when app started.
  log_msg("App started", on = isolate(input$record_on))
  # first page need to be added manually since no page switching event fired
  log_page(page_title$import, on = isolate(input$record_on))
  observeEvent(input$record_on, {
    # this call doesn't use the switch to turn off itself
    log_msg(str_c("Recording is ", if (input$record_on) "On" else "Off"))
  })
  # cache setup ----
  create_cache <- function() {
    create_folder(file.path(session_tmpdir, "cache"))
  }
  reset_cache <- function(cache_path) {
    cache_files <- list.files(cache_path, full.names = TRUE)
    file.remove(cache_files)
  }
  cache_path <- create_cache()
  para_ll_fit_mem <- memoise(para_ll_fit, cache = cache_filesystem(cache_path))
  akde_mem <- memoise(akde, cache = cache_filesystem(cache_path))
  para_ll_ud_mem <- memoise(para_ll_ud, cache = cache_filesystem(cache_path))
  # p1. import ----
  values <- reactiveValues()
  # run this after every modification on data and list separately. i.e. values$data$tele_list changes, or data not coming from merge_animals. this should got run automatically? no if not referenced. need reactive expression to refer values$.
  # this is a side effect reactive expression that depend on a switch.
  verify_global_data <- reactive({
    if (debug_mode) {
      match_tele_merged(values$data$tele_list, values$data$merged)
    }
  })
  # values$ ----
  values$data <- NULL  # 4 items need to be synced
  # important reactive value and expressions need special comments, use <--. the design need to well thought
  # input_tele_list: telemetry obj list from as.telemetry on input data: movebank download, local upload, package data. all reference of this value should wrap req around it. Once it's used, no need to keep the copy. thus add it with the new time subset. We don't need to keep the dt version because we can often just use existing dt and other info. do need to verify tele and dt is synced.
  # tele_list, merged: the telemetry version and merged data.table version of updated data reflected changes on outlier removal and time subsetting.
  # all_removed_outliers: records of all removed outliers. original - all removed = current. the table have id column so this can work across different individuals.
  # the time subset only live in time subsetting process, the result of the process update tele_list and merged.
  # the extra column of outliers only live in outlier page. the result of the process update whole data. note may need to use column subset when operating between dt with or without extra columns.
  # for any data source changes, need to update these 4 items together.
  # current_model_fit_res is updated in model fitting stage, need to be cleared when input change too.
  update_input_data <- function(tele_list) {
    values$data$input_tele_list <- tele_list
    values$data$tele_list <- tele_list
    values$data$merged <- merge_animals(tele_list)
    values$data$all_removed_outliers <- NULL
    values$selected_data_model_fit_res <- NULL
    # this need to be built with full data
    values$id_pal <- colorFactor(hue_pal()(nrow(values$data$merged$info)),
                                 unique(values$data$merged$data$id))
    updateTabItems(session, "tabs", "plots")
    # LOG input data updated
    log_msg("Input data updated", on = isolate(input$record_on))
  }
  # 1.1 csv to telemetry ----
  # call this function for side effect, set values$data
  data_import <- function(data) {
    # sometimes there is error: Error in <Anonymous>: unable to find an inherited method for function ‘span’ for signature ‘"shiny.tag"’. added tags$, not sure if it will fix it.
    note_import <- showNotification(
      shiny::span(icon("spinner fa-spin"), "Importing data..."),
      type = "message", duration = NULL)
    on.exit(removeNotification(note_import))
    # wrap it so even single individual will return a list with one item
    tele_list <- tryCatch(wrap_single_telemetry(as.telemetry(data)),
        error = function(e) {
          showNotification("Import error, check data again",
                           duration = 4, type = "error")
        })
    # only proceed if no error
    test_class <- lapply(tele_list, function(x) {"telemetry" %in% class(x)})
    req(all(unlist(test_class)))
    # sort list by identity. only sort list, not info table. that's why we need to sort it again after time subsetting.
    tele_list <- sort_tele_list(tele_list)
    update_input_data(tele_list)
  }
  # clicking browse button without changing radio button should also update, this is why we make the function to include all behavior after file upload.
  file_uploaded <- function(){
    data_import(input$tele_file$datapath)
    updateRadioButtons(session, "load_option", selected = "upload")
    updateTabItems(session, "tabs", "plots")
  }
  observeEvent(input$tele_file, {
    req(input$tele_file)
    # LOG file upload.
    log_msg("Importing file", input$tele_file$name,
            on = isolate(input$record_on))
    file_uploaded()
  })
  # abstract because need to do this in 2 places
  set_sample_data <- function() {
    data("buffalo")
    sample_data <- pick_m_tele_list(buffalo, input$sample_size)
    # LOG use sample
    log_msg("Using data", "buffalo sample from ctmm",
            on = isolate(input$record_on))
    update_input_data(sample_data)
  }
  # observe radio button changes
  observeEvent(input$load_option, {
    switch(input$load_option,
           ctmm = {
             data("buffalo")
             # LOG use buffalo
             log_msg("Using data", "buffalo from ctmm",
                     on = isolate(input$record_on))
             update_input_data(buffalo)
           },
           ctmm_sample = {
             # data("buffalo")
             # sample_data <- pick_m_tele_list(buffalo, input$sample_size)
             # # LOG use sample
             # log_msg("Using data", "buffalo sample from ctmm",
             #         on = isolate(input$record_on))
             # update_input_data(sample_data)
             set_sample_data()
           },
           upload = {
             # this doesn't do anything by itself so no log msg
             # need to check NULL input from source, stop error in downstream
             req(input$tele_file)
             file_uploaded()
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
  if (identical(sort(names(mb_env)), c("movebank_pass", "movebank_user")) &&
      all(nchar(mb_env) != 0)) {
    mb_user_env <- unname(mb_env["movebank_user"])
    mb_pass_env <- unname(mb_env["movebank_pass"])
    # the textinput value are always sync to date, so we can just use textinput everywhere which is reactive
    updateTextInput(session, "user", value = mb_user_env)
    updateTextInput(session, "pass", value = mb_pass_env)
    showNotification("Movebank login info found", duration = 1, type = "message")
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
    datatable({
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
    datatable(req(values$study_detail),
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
    datatable(req(values$study_preview), options = list(dom = 't')))
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
    res <- get_all_studies(input$user, input$pass)  # may generate error notification if failed
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
      log_msg("Movebank login failed", on = isolate(input$record_on))
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
      log_msg("Logged in Movebank as", input$user,
              on = isolate(input$record_on))
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
                             style = styles$external_link),
                 target = "_blank", href =
  paste0("https://www.movebank.org/movebank/#page=studies,path=study", mb_id()))
      })
      res <- get_study_detail(mb_id(), input$user, input$pass)
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
    res <- get_study_data(mb_id(), input$user, input$pass)
    removeNotification(note_data_download)
    # need to check response content to determine result type. the status is always success
    # read first rows to determine if download is successful. fread will guess sep so still can read html for certain degree, specify `,` will prevent this
    # sometimes the result is one line "<p>No data are available for download.</p>". fread and read.csv will take one line string as file name thus cannot find the input file, generate some warnings. To use string as input need at least one "\n". Adding "\n" will solve this error but get valid dt with 0 row, also we cannot use the nrows parameters.
    movebank_dt_preview <- try(fread(res$res_cont, sep = ",", nrows = 5))
    # the fread in ctmm can use == directly because it was reading in df only, only one class attributes. Here we need to use %in% instead
    if (!("data.table" %in% class(movebank_dt_preview))) {
      showNotification(
        h4("No data available -- or you need to agree to license term first."),
        type = "warning", duration = 5)
      msg <- html_to_text(res$res_cont)
      clear_mb_download(paste0(msg, collapse = "\n"))
      # LOG download movebank data failed
      log_msg("Movebank data download failed", mb_id(),
              on = isolate(input$record_on))
    } else {
      showNotification("Data downloaded", type = "message", duration = 2)
      note_parse <- showNotification(
        shiny::span(icon("spinner fa-spin"), "Parsing csv..."),
        type = "message", duration = NULL)
      move_bank_dt <- try(fread(res$res_cont, sep = ","))
      removeNotification(note_parse)
      row_count <- formatC(move_bank_dt[, .N], format = "d", big.mark = ",")
      individual_count <- length(unique(move_bank_dt[, individual_id]))
      values$study_data_response <- paste0(
          "Data downloaded with ", row_count, " rows, ",
          individual_count, " individuals. ", "Preview:")
      values$study_preview <- movebank_dt_preview
      values$move_bank_dt <- move_bank_dt
      # LOG download movebank data
      log_msg("Movebank data downloaded", mb_id(),
              on = isolate(input$record_on))
      # some detail table may have invalid characters that crash kable. disable this now.
      # log_dt_md(values$study_detail, "Downloaded study details",
      #           on = isolate(input$record_on))
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
      log_msg("Movebank data saved", mb_id(),
              on = isolate(input$record_on))
    }
  )
  observeEvent(input$import_movebank, {
    req(values$move_bank_dt[, .N] > 0)
    data_import(values$move_bank_dt)
    # LOG import movebank data
    log_msg("Movebank data imported", mb_id(),
            on = isolate(input$record_on))
    updateTabItems(session, "tabs", "plots")
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
    if (input$time_in_sec) {
      info_p <- values$data$merged$info[,
                  .(identity, start, end, interval_s, duration_s, points)]
    } else {
      info_p <- values$data$merged$info[,
                  .(identity, start, end, interval, duration, points)]
    }
    datatable(info_p, options = list(pageLength = 6,
                                     lengthMenu = c(2, 4, 6, 8, 10, 20))) %>%
      formatStyle('identity', target = 'row',
                  color = styleEqual(info_p$identity,
                                     hue_pal()(nrow(info_p)))
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
      freezeReactiveValue(input, "individuals_rows_selected")
      if (!is.null(values$data$all_removed_outliers)) {
        values$data$all_removed_outliers <- values$data$all_removed_outliers[
          !(identity %in% chosen_ids)
          ]
      }
      values$data$merged$data <- values$data$merged$data[
        !(identity %in% chosen_ids)
      ]
      remaining_indice <- !(values$data$merged$info$identity %in% chosen_ids)
      values$data$merged$info <- values$data$merged$info[remaining_indice]
      values$data$tele_list <- values$data$tele_list[remaining_indice]
      verify_global_data()
      # LOG delete inidividuals
      log_msg("Individuals deleted from data ",
              str_c(chosen_ids, collapse = ", "),
              on = isolate(input$record_on))
    }
  })
  proxy_individuals <- dataTableProxy("individuals")
  observeEvent(input$select_all, {
    selectRows(proxy_individuals, 1:nrow(values$data$merged$info))
  })
  observeEvent(input$deselect_all, {
    # use list() instead of NULL to avoid R 3.4 warning on I(NULL). After DT fixed this warning we can change back to NULL
    selectRows(proxy_individuals, list())
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
    animals_dt <- values$data$merged$data[identity %in% chosen_ids]
    subset_indice <- values$data$merged$info$identity %in% chosen_ids
    info <- values$data$merged$info[subset_indice]
    # need to clear model fit result, change to original mode instead of modeled mode
    values$selected_data_model_fit_res <- NULL
    updateRadioButtons(session, "vario_mode", selected = "empirical")
    # LOG current selected individuals
    log_dt_md(info[,
                   .(identity, start, end, interval, duration, points)],
              "Current selected individuals",
              on = isolate(input$record_on))
    # didn't verify data here since it's too obvious and used too frequently. if need verfication, need call function on subset.
    return(list(data = animals_dt,
                info = info,
                chosen_row_nos = chosen_row_nos,
                tele_list = values$data$tele_list[subset_indice]
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
    animals_dt <- req(select_data()$data)
    g <- ggplot() +
      {if (input$overlay_all) {
        geom_point(data = values$data$merged$data, aes(x, y),
                   size = input$point_size_1, alpha = 0.6, colour = "gray")
      }} +
      geom_point(data = animals_dt, aes(x, y, colour = id),
                 size = input$point_size_1, alpha = 0.7) +
      coord_fixed(xlim = location_plot_gg_range$x,
                  ylim = location_plot_gg_range$y) +
      factor_color(animals_dt$id) +  # the color is right because id is factor, its levels included all values from full dataset ids.
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      theme(legend.position = "top",
            legend.direction = "horizontal") +
      bigger_theme + bigger_key
    # LOG save pic
    log_save_ggplot(g, "plot_2_overview", on = isolate(input$record_on))
  }, height = function() { input$canvas_height }, width = "auto"
  )
  # 2.3 facet ----
  output$location_plot_facet_fixed <- renderPlot({
    # by convention animals_dt mean the data frame, sometimes still need some other items from list, use full expression
    animals_dt <- req(select_data()$data)
    g <- ggplot(data = animals_dt, aes(x, y)) +
      geom_point(size = 0.1, aes(colour = id)) +
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      factor_color(animals_dt$id) +
      facet_grid(id ~ .) +
      coord_fixed() +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
    # LOG save pic
    log_save_ggplot(g, "plot_3_facet", on = isolate(input$record_on))
  }, height = function() { input$canvas_height }, width = "auto")
  # 2.4 individual plot ----
  output$location_plot_individual <- renderPlot({
    animals_dt <- req(select_data()$data)
    new_ranges <- get_ranges_quantile_dt(animals_dt, input$include_level)
    id_vector <- select_data()$info$identity
    g_list <- vector("list", length = length(id_vector))
    for (i in seq_along(id_vector)) {
      data_i <- animals_dt[identity == id_vector[i]]
      new_ranges_i <- new_ranges[identity == id_vector[i]]
      g_list[[i]] <- ggplot(data = data_i, aes(x, y, color = id)) +
        geom_point(size = input$point_size_3, alpha = 0.7) +
        factor_color(data_i$id) +
        scale_x_continuous(labels = format_distance_f(data_i$x)) +
        scale_y_continuous(labels = format_distance_f(data_i$y)) +
        labs(title = id_vector[i]) +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none") +
        coord_fixed(xlim = c(new_ranges_i$x_start, new_ranges_i$x_end),
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
    gr <- grid.arrange(grobs = g_list, ncol = input$plot4_col)
    # LOG save pic
    log_save_ggplot(gr, "plot_4_individual", on = isolate(input$record_on))
  }, height = function() { input$canvas_height }, width = "auto")
  # 2.5 histogram facet ----
  output$histogram_facet <- renderPlot({
    animals_dt <- req(select_data()$data)
    g <- ggplot(data = animals_dt, aes(x = timestamp, fill = id)) +
      geom_histogram(bins = 60) +
      factor_fill(animals_dt$id) +
      facet_grid(id ~ .) +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
    # LOG save pic
    log_save_ggplot(g, "plot_5_histogram", on = isolate(input$record_on))
  }, height = styles$height_hist, width = "auto")
  # p3. outlier ----
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
    animals_dt <- outlier_page_data$data
    animals_dt <- calculate_distance(animals_dt)
    animals_dt <- calculate_speed(animals_dt, as.numeric(input$device_error))
    outlier_page_data$data <- animals_dt
    return(outlier_page_data)
  })
  # p3.a.1 distance histogram ----
  # note this also add bin factor column
  bin_by_distance <- reactive({
    # animals_dt <- req(select_data()$data)
    animals_dt <- req(calc_outlier()$data)
    return(color_break(input$distance_his_bins, animals_dt,
                       "distance_center", format_distance_f))
  })
  output$distance_histogram <- renderPlot({
    # need to get data from reactive, update by bin count
    distance_binned <- req(bin_by_distance())
    animals_dt <- distance_binned$animals_dt
    # use this to check if distance and speed data is synced
    # cat("dataset in distance page\n")
    # print(animals_dt[, .N, by = id])
    g <- ggplot(animals_dt, aes(x = distance_center)) +
      geom_histogram(breaks = distance_binned$color_bin_breaks,
                     # fill = hue_pal()(input$distance_his_bins),
                     aes(fill = distance_center_color_factor,
                       alpha = distance_center_color_factor)) +
      # need to exclude 0 count groups
      geom_text(stat = 'bin',aes(label = ifelse(..count.. != 0, ..count.., "")),
                vjust = -1, breaks = distance_binned$color_bin_breaks) +
      factor_fill(animals_dt$distance_center_color_factor) +
      factor_alpha(animals_dt$distance_center_color_factor) +
      scale_x_continuous(breaks = distance_binned$non_empty_breaks,
                         labels = distance_binned$vec_formatter) +
      # all counts above 20 are not shown, so it's easier to see the few outliers.
      coord_cartesian(ylim = c(0, input$distance_his_y_limit)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    # LOG save pic
    log_save_ggplot(g, "plot_distance_outlier_histogram",
                    on = isolate(input$record_on))
  })
  # need the whole range to get proper unit selection
  format_outliers <- function(animal_selected_data, animals_dt) {
    unit_distance <- pick_unit_distance(animals_dt$distance_center)
    unit_speed <- pick_unit_speed(animals_dt$speed)
    animal_selected_data[, .(id, row_no,
       timestamp = format_datetime(timestamp),
       distance_center = format(distance_center / unit_distance$scale,
                                digits = 3),
       distance_unit = unit_distance$name,
       speed = format(speed / unit_speed$scale, digits = 3),
       speed_unit = unit_speed$name
       # distance_center = format_distance_f(animals_dt[, distance_center])(
       #   distance_center),
       # distance_center_SI = format(distance_center, digits = 3),
       # speed = format_speed_f(animals_dt[, speed])(speed)
       # speed_SI = format(speed, digits = 3)
       )]
  }
  # brush selection function
  select_range <- function(his_type){
    return(reactive({
      # everything in outlier page should take animal_dt from binned version
      # the current data have distance/speed column, the binned version just create the color factors. in theory we could use the original data but we may need the color factor sometimes.
      switch(his_type,
             distance = {
               col_name = quote(distance_center)
               format_f <- format_distance_f
               unit_name <- " m"
               animals_dt <- req(bin_by_distance()$animals_dt)
             },
             speed = {
               col_name = quote(speed)
               format_f <- format_speed_f
               unit_name <- " m/s"
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
      format_raw <- function(value, unit_name) {
        str_c(format(value, digits = 3), unit_name)
      }
      dt <- data.table(Unit = c("Formated", "SI"),
        Start = c(format_f_value(select_start),
                  format_raw(select_start, unit_name)),
        End = c(format_f_value(select_end),
                format_raw(select_end, unit_name)))
      log_dt_md(dt,
                "Range Selected",
                on = isolate(input$record_on))
      log_msg("Points in Selected Range", nrow(animal_selected_data),
                on = isolate(input$record_on))
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
    g <- ggplot(animals_dt, aes(x, y)) +
      geom_point(size = 0.05, alpha = 0.6, colour = "gray") +
      geom_point(data = animal_selected_data,
                 size = ifelse(is.null(input$distance_his_brush),
                               0.2,
                               input$distance_point_size),
                 # alpha = ifelse(is.null(input$distance_his_brush),
                 #                0.6,
                 #                input$distance_alpha),
                 aes(colour = distance_center_color_factor,
                     alpha = distance_center_color_factor)) +
      {if (!is.null(input$points_in_distance_range_rows_selected)) {
        points_selected <- select_distance_range()$animal_selected_data[
          input$points_in_distance_range_rows_selected]
        geom_point(data = points_selected, size = 3.5, alpha = 1,
                   color = "blue", shape = 22)
      }} +
      geom_point(data = unique(animals_dt[, .(id, median_x, median_y)]),
                 aes(x = median_x, y = median_y, shape = id), color = "blue", size = 0.8) +
      factor_color(animal_selected_data$distance_center_color_factor) +
      # scale_alpha_discrete(breaks = bin_by_distance()$color_bin_breaks) +
      factor_alpha(animal_selected_data$distance_center_color_factor) +
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      coord_fixed(xlim = distance_outlier_plot_range$x,
                  ylim = distance_outlier_plot_range$y) +
      theme(legend.position = "top",
            legend.direction = "horizontal") + bigger_key
    # LOG save pic
    log_save_ggplot(g, "plot_distance_outlier_plot",
                    on = isolate(input$record_on))
  })
  # points in selected distance range
  output$points_in_distance_range <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data
    req(input$distance_his_brush)
    # cols <- c("row_no", "timestamp", "id", "distance_center")
    # datatable(select_distance_range()$animal_selected_data[, cols, with = FALSE],
    datatable(select_distance_range()$animal_selected_formatted,
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
    # removed_points <- values$data$merged$data[
    #   row_name %in% row_names_to_remove]
    # distance and speed color_break will add each own factor column, so two tab have different columns. we only need the extra columns minus these factor column in summary table
    points_to_remove <- points_to_remove[, timestamp:speed]
    values$data$all_removed_outliers <- rbindlist(list(
      values$data$all_removed_outliers, points_to_remove))
    animals_dt <- values$data$merged$data[
      !(row_name %in% values$data$all_removed_outliers[, row_name])]
    # update tele obj. more general apporach is update them according to data frame changes.
    changed <- unique(points_to_remove$identity)
    tele_list <- values$data$tele_list
    tele_list[changed] <- lapply(tele_list[changed], function(x) {
      x[!(row.names(x) %in% points_to_remove[, row_name]),]
    })
    tele_list <- tele_list[lapply(tele_list, nrow) != 0]
    info <- tele_list_info(tele_list)
    # distance/speed calculation need to be updated. row_no not updated.
    # animals_dt <- calculate_distance(animals_dt)
    # animals_dt <- calculate_speed(animals_dt)
    values$data$tele_list <- tele_list
    values$data$merged <- NULL
    values$data$merged <- list(data = animals_dt, info = info)
    verify_global_data()
  }
  proxy_points_in_distance_range <- dataTableProxy("points_in_distance_range",
                                                deferUntilFlush = FALSE)
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
    selectRows(proxy_points_in_distance_range, list())
    freezeReactiveValue(input, "distance_his_brush")
    session$resetBrush("distance_his_brush")
    # LOG points to remove
    log_dt_md(points_to_remove_formated, "Points to be Removed by Distance",
      on = isolate(input$record_on))
    remove_outliers(points_to_remove)
  })
  # p3.b.1 speed histogram ----
  # bin_by_speed() ----
  bin_by_speed <- reactive({
    # animals_dt <- req(select_data()$data)
    animals_dt <- req(calc_outlier()$data)
    # too large UERE value will result calculated speed in 0
    zero_speeds <- all(range(animals_dt$speed) == c(0,0))
    if (zero_speeds) {
      showNotification("Calculated Speed = 0, is device error too big?",
                       type = "error")
    }
    req(!zero_speeds)
    return(color_break(input$speed_his_bins, animals_dt,
                       "speed", format_speed_f))
  })
  output$speed_histogram <- renderPlot({
    speed_binned <- req(bin_by_speed())
    animals_dt <- speed_binned$animals_dt
    # cat("dataset in speed page\n")
    # print(animals_dt[, .N, by = id])
    g <- ggplot(animals_dt, aes(x = speed)) +
      geom_histogram(breaks = speed_binned$color_bin_breaks,
                     aes(fill = speed_color_factor,
                         alpha = speed_color_factor)) +
      # need to exclude 0 count groups
      geom_text(stat = 'bin', aes(label = ifelse(..count.. != 0, ..count.., "")),
                vjust = -1, breaks = speed_binned$color_bin_breaks) +
      factor_fill(animals_dt$speed_color_factor) +
      factor_alpha(animals_dt$speed_color_factor) +
      scale_x_continuous(breaks = speed_binned$non_empty_breaks,
                         labels = speed_binned$vec_formatter) +
      coord_cartesian(ylim = c(0, input$speed_his_y_limit)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    # LOG save pic
    log_save_ggplot(g, "plot_speed_outlier_histogram",
                    on = isolate(input$record_on))
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
    g <- ggplot(animals_dt, aes(x, y)) +
      geom_point(size = 0.05, alpha = 0.6, colour = "gray") +
      geom_point(data = animal_selected_data,
                 size = ifelse(is.null(input$speed_his_brush),
                               0.2,
                               input$speed_point_size),
                 # alpha = ifelse(is.null(input$speed_his_brush),
                 #                0.6,
                 #                input$speed_alpha),
                 aes(colour = speed_color_factor,
                     alpha = speed_color_factor)) +
      factor_color(animal_selected_data$speed_color_factor) +
      # scale_alpha_discrete(breaks = bin_by_speed()$color_bin_breaks) +
      factor_alpha(animal_selected_data$speed_color_factor) +
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      coord_fixed(xlim = speed_outlier_plot_range$x,
                  ylim = speed_outlier_plot_range$y) +
      theme(legend.position = "top",
            legend.direction = "horizontal") + bigger_key
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
        geom_point(data = selected_points, size = 3.5, alpha = 1,
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
          geom_point(data = animals_dt[(in_neighbor)],
                     color = "blue",
                     alpha = 0.8, size = 1, shape = 21) +
          # remove the warning of NA otherwise each zoom will have one warning.
          geom_segment(data = animals_dt[(in_neighbor)],
                       color = "blue", alpha = 0.3, na.rm = TRUE,
                       aes(xend = xend, yend = yend),
                       arrow = arrow(length = unit(2,"mm")))
        # add label in path if needed, this only work when path is selected.
        if ("add_label" %in% input$selected_details) {
          g <- g +
            geom_text(data = animals_dt[(in_neighbor)],
                      aes(label = row_no), alpha = 0.6, hjust = -0.1)
        }
      }
    }
    # LOG save pic
    log_save_ggplot(g, "plot_speed_outlier_plot",
                    on = isolate(input$record_on))
  })
  # outputOptions(output, "speed_outlier_plot", priority = 1)
  # points without valid speed values
  # output$points_speed_non_valid <- DT::renderDataTable({
  #   # only render table when there is a selection. otherwise it will be all data.
  #   animals_dt <- req(values$data$merged$data)
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
    datatable(select_speed_range()$animal_selected_formatted,
              options = list(pageLength = 6,
                             lengthMenu = c(6, 10, 20),
                             scrollX = TRUE,
                             searching = FALSE),
              rownames = FALSE)
  })
  # give it high priority so it will update in before the plot updates
  # outputOptions(output, "points_in_speed_range", priority = 10)
  # remove speed outliers ----
  proxy_points_in_speed_range <- dataTableProxy("points_in_speed_range",
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
    selectRows(proxy_points_in_speed_range, list())
    freezeReactiveValue(input, "speed_his_brush")
    session$resetBrush("speed_his_brush")
    # LOG points to remove
    log_dt_md(points_to_remove_formated, "Points to be Removed by Speed",
              on = isolate(input$record_on))
    remove_outliers(points_to_remove)
  })
  # all removed outliers ----
  output$all_removed_outliers <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data.
    req(values$data$all_removed_outliers)
    # animals_dt <- req(select_data()$data)
    animals_dt <- req(calc_outlier()$data)
    dt <- format_outliers(values$data$all_removed_outliers, animals_dt)
    log_dt_md(dt, "All Removed Outliers", on = isolate(input$record_on))
    datatable(dt,
              options = list(pageLength = 6,
                             lengthMenu = c(6, 10, 20),
                             searching = FALSE),
              rownames = FALSE)
  })
  # tried to add delete rows like the time range table, but that need to update a lot of values in proper order, the reset is easy because it just use original input. Not really need this complex operations.
  # reset outlier removal ----
  # method 1. merge data back, just reverse the remove outlier. that require add rows to tele which is not possible now? need that tele update function later. if this is doable, pros: merge dt is faster than merge_animals; time-subset don't need to update input tele, only need to maintain current tele/dt.
  # method 2. merge input. but time subset added new data. if we update input_tele with time subset, need to use the original input tele + new time subset, not the current tele which could have outlier removed. by merging tele we didn't keep two versions. but this could be expensive in merging.
  observeEvent(input$reset_outliers, {
    values$data$tele_list <- values$data$input_tele_list
    values$data$merged <- merge_animals(values$data$tele_list)
    values$data$all_removed_outliers <- NULL
    # LOG reset removal
    log_msg("All Removed Outliers Restored", on = isolate(input$record_on))
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
    data_i <- select_data()$data
    data_i[, color_bin_start :=
             cut_date_time(timestamp, input$time_color_bins)]  # a factor
    color_bin_start_vec_time <- ymd_hms(levels(data_i$color_bin_start))
    color_bin_breaks <- c(color_bin_start_vec_time,
                                     data_i[t == max(t), timestamp])
    # initital selection is full range
    # the manual set of date range triggered this whole expression to calculate again, and reset it to full range.
    isolate({
      values$selected_time_range <- list(
        select_start = data_i[t == min(t), timestamp],
        select_end = data_i[t == max(t), timestamp])
      updateDateRangeInput(session, "date_range",
                           start = values$selected_time_range$select_start,
                           end = values$selected_time_range$select_end)
    })
    # using id internally to make code shorter, in data frame id is factor
    return(list(identity = selected_id, data = data_i,
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
    g <- ggplot(data = animal_binned$data, aes(x = timestamp)) +
      geom_histogram(breaks = as.numeric(animal_binned$color_bin_breaks),
                     fill = hue_pal()(input$time_color_bins)) +
      scale_x_datetime(breaks = animal_binned$color_bin_breaks,
                       labels = date_format("%Y-%m-%d %H:%M:%S")) +
      ggtitle(animal_binned$data[1, identity]) + center_title +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # LOG save pic
    log_save_ggplot(g, "plot_time_subsetting_histogram",
                    on = isolate(input$record_on))
  })
  # select time range ----
  # brush selection and matching color bins
  observeEvent(input$time_sub_his_brush, {
    values$selected_time_range <- list(
      select_start = as_datetime(input$time_sub_his_brush$xmin),
      select_end = as_datetime(input$time_sub_his_brush$xmax))
  })
  observeEvent(input$set_date_range, {
    start <- as_datetime(input$date_range[1])
    end <- as_datetime(input$date_range[2])
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
      time_range_dt[, `:=`(start = format_datetime(select_start),
                           end = format_datetime(select_end),
                           length = format_diff_time(select_end - select_start))]
      return(time_range_dt[, .(start, end, length)])
    }
  }
  output$current_range <- DT::renderDataTable({
    req(!is.null(values$selected_time_range))
    dt <- format_time_range(as.data.frame(values$selected_time_range))
    # LOG selection
    log_dt_md(dt, "Current Selected Time Range",
              on = isolate(input$record_on))
    datatable(dt, options =
                list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
      formatStyle(1, target = 'row', color = "#00c0ef")
  })
  # 4.3 selected locations ----
  selected_loc_ranges <- add_zoom("selected_loc")
  output$selected_loc <- renderPlot({
    animal_binned <- color_bin_animal()
    time_range <- values$selected_time_range
    animal_selected_data <- animal_binned$data[
      (timestamp >= time_range$select_start) &
        (timestamp <= time_range$select_end)]
    g <- ggplot(data = animal_binned$data, aes(x, y)) +
      geom_point(size = 0.01, alpha = 0.5, colour = "gray") +
      geom_point(size = input$point_size_time_loc, alpha = 0.9,
                 data = animal_selected_data,
                 aes(colour = color_bin_start)) +
      factor_color(animal_selected_data$color_bin_start) +
      scale_x_continuous(labels = format_distance_f(animal_binned$data$x)) +
      scale_y_continuous(labels = format_distance_f(animal_binned$data$y)) +
      coord_fixed(xlim = selected_loc_ranges$x, ylim = selected_loc_ranges$y) +
      theme(legend.position = "top",
            legend.direction = "horizontal") + bigger_key
    # LOG save pic
    log_save_ggplot(g, "plot_time_subsetting_plot",
                    on = isolate(input$record_on))
  })
  # 4.4 time range table ----
  # time_subsets hold a table of time ranges for current individual, this should only live in one time subsetting process(clear in beginning, in color_bin_animal. clear after finish, when subset is generated), which is always on single individual. If user moved around pages without changing individual, the states are kept. Once generated, the new subset instance data and tele obj are inserted to values$current and kept there, which hold for all input session.
  observeEvent(input$add_time, {
    l <- list(values$time_ranges, as.data.frame(values$selected_time_range))
    values$time_ranges <- rbindlist(l)
    # LOG add
    log_dt_md(format_time_range(as.data.frame(values$selected_time_range)),
              "Time Range Added to List", on = isolate(input$record_on))
  })
  observeEvent(input$delete_time_sub_rows, {
    # with empty table the previous selected value is still there, need to check table too
    if (!is.null(input$time_ranges_rows_selected) &&
        (nrow(values$time_ranges) > 0)) {
      # LOG delete
      log_dt_md(values$time_ranges[
          as.numeric(input$time_ranges_rows_selected)],
        "Time Range Deleted", on = isolate(input$record_on))
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
    dt <- animal_binned$data[, timestamp:row_no]
    new_tele <- animal_binned$tele  # single tele obj from color_bin_animal
    res <- vector("list", length = nrow(values$time_ranges))
    for (i in 1:nrow(values$time_ranges)) {
      res[[i]] <- dt[(timestamp >= values$time_ranges[i, select_start]) &
                       (timestamp <= values$time_ranges[i, select_end])]
    }
    # note all ranges are combined. this is intended for a subset of non-overlapping sections. If need multiple subset, just generate several times.
    new_dt <- unique(rbindlist(res))
    setkey(new_dt, row_no)
    # new name
    matches <- str_match(values$data$merged$info$identity,
                         paste0(animal_binned$identity, "_subset_(\\d+)$"))
    matches[is.na(matches)] <- 0
    last_index <- max(as.numeric(matches[,2]))
    new_suffix <- paste0("_subset_", last_index + 1)
    new_id <- paste0(animal_binned$identity, new_suffix)
    new_dt[, identity := new_id]
    # subset tele by row_name before it changes
    new_tele <- new_tele[(row.names(new_tele) %in% new_dt[, row_name]),]
    new_tele@info$identity <- new_id
    # update other columns
    new_dt[, row_name := paste0(row_name, new_suffix)]
    # update the row name in tele data frame by new row_name column
    row.names(new_tele) <- new_dt$row_name
    # update data
    all_dt <- values$data$merged$data
    all_dt <- rbindlist(list(all_dt, new_dt))
    # ggplot sort id by name, to keep it consistent we also sort the info table. for data.table there is no need to change order (?), this can keep row_no mostly same
    all_dt[, id := factor(identity)]
    all_dt[, row_no := .I]
    values$data$merged$data <- all_dt
    # need to wrap single obj otherwise it was flattened by c
    values$data$tele_list <- c(values$data$tele_list,
                               wrap_single_telemetry(new_tele))
    # also update input tele from original input + new tele
    values$data$input_tele_list <- c(values$data$input_tele_list,
                                     wrap_single_telemetry(new_tele))
    # sort info list so the info table will have right order. we can also sort the info table, but we used the row index of table for selecting indidivuals(sometimes I used identity, sometimes maybe use id), it's better to keep the view sync with the data
    # sorted_names <- sort(names(values$data$tele_list))
    values$data$tele_list <- sort_tele_list(values$data$tele_list)
    values$data$input_tele_list <- sort_tele_list(values$data$input_tele_list)
    values$data$merged$info <- tele_list_info(values$data$tele_list)
    values$time_ranges <- NULL
    verify_global_data()
    # LOG subset added
    log_msg("New Time Range Subset Added", new_id,
            on = isolate(input$record_on))
    updateTabItems(session, "tabs", "plots")
    msg <- paste0(new_id, " added to data")
    showNotification(msg, duration = 2, type = "message")
  })
  output$time_ranges <- DT::renderDataTable({
    # it could be NULL from clear, or empty data.table from delete
    req(values$time_ranges)
    req(nrow(values$time_ranges) > 0)
    dt <- format_time_range(values$time_ranges)
    # LOG time range list
    log_dt_md(dt, "Time Range List", on = isolate(input$record_on))
    datatable(dt, options =
                list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  # p5. variogram ----
  callModule(click_help, "variogram", title = "Variograms",
             size = "l", file = "help/5_a_variograms.md")
  callModule(click_help, "vario_irregular", title = "Irregular Data",
             size = "l", file = "help/5_b_irregular_data.md")
  # values$selected_data_guess_list guessed parameters for current data, also can be manual adjusted from fine tune.
  values$selected_data_guess_list <- NULL
  # select_data_vg_list() ----
  # select_data_vg_list is variogram for current data.
  select_data_vg_list <- reactive({
    tele_list <- select_data()$tele_list
    # guess value need to be reactive so it can be modified in manual fit.
    values$selected_data_guess_list <- lapply(tele_list,
                    function(tele) ctmm.guess(tele, interactive = FALSE))
    res <- lapply(tele_list, variogram)
    names(res) <- names(tele_list)
    return(res)
  })
  # select_data_vg_layout() ----
  # modeled mode and home range etc share same layout, which could coexist with variograms. so we cannot use one layout for all of them.
  select_data_vg_layout <- reactive({
    fig_count <- length(select_data_vg_list())
    row_count <- ceiling(fig_count / input$vario_columns)
    height <- input$vario_height * row_count
    # return(list(layout_matrix = layout_matrix, height = height))
    return(list(row_count = row_count, height = height))
  })
  # variogram CTMM ----
  get_vario_ctmm_list <- reactive({
    switch(input$vario_mode,
           empirical = NULL,
           guesstimate = values$selected_data_guess_list,
           modeled = select_models()$models_list
           )
  })
  # plot variograms ----
  output$vario_plot_zoom <- renderPlot({
    # in modeled mode, draw selected subset of select_data_vg_list and selected models, using select_models_layout
    if (input$vario_mode != "modeled") {
      vg_lst <- select_data_vg_list()
      row_count <- select_data_vg_layout()$row_count
      title_vec <- names(select_data()$tele_list)
    } else {
      vg_lst <- select_models()$vg_list
      row_count <- select_models_layout()$row_count
      title_vec <- select_models()$names_dt$full_name
    }
    ctmm_list <- get_vario_ctmm_list()  # this adjust to selected models by self
    ctmm_color <- switch(input$vario_mode,
                         guesstimate = "green",
                         modeled = "purple")
    def.par <- par(no.readonly = TRUE)
    par(mfrow = c(row_count, input$vario_columns),
        mar = c(5, 5, 4, 1), ps = 18, cex = 0.72, cex.main = 0.9)
    if (input$vario_option == "absolute") {
      max.lag <- max(sapply(vg_lst, function(v){ last(v$lag) } ))
      xlim <- max.lag * (10 ^ input$zoom_lag_fraction)
      vg_zoomed_list <- lapply(vg_lst,
                             function(vario) vario[vario$lag <= xlim, ])
      extent_tele <- ctmm::extent(vg_zoomed_list)
      for (i in seq_along(vg_zoomed_list)) {
        plot(vg_zoomed_list[[i]], CTMM = ctmm_list[[i]],
             col.CTMM = ctmm_color, fraction = 1,
             xlim = c(0, extent_tele["max", "x"]),
             ylim = c(0, extent_tele["max", "y"]))
        title(title_vec[i])
        # if (!is.null(ctmm_list[[i]]) && ctmm_list[[i]]$error) {
        #   title(vg_zoomed_list[[i]]@info$identity, sub = "Error on",
        #         cex.sub = 0.85, col.sub = "red")
        # } else {
        #   title(title_vec[i])
        # }
      }
    } else {
      for (i in seq_along(vg_lst)) {
        plot(vg_lst[[i]], CTMM = ctmm_list[[i]],
             col.CTMM = ctmm_color,
             fraction = 10 ^ input$zoom_lag_fraction)
        # browser()
        title(title_vec[i])
        # if (!is.null(ctmm_list[[i]]) && ctmm_list[[i]]$error) {
        #   title(vg_lst[[i]]@info$identity, sub = "Error on",
        #         cex.sub = 0.85, col.sub = "red")
        # } else {
        #   title(vg_lst[[i]]@info$identity)
        # }
        # if (ctmm_list[[i]]$error) {
        #   title(sub = "Error on", cex.sub = 0.85, col.sub = "red")
        # }
      }
    }
    # LOG save pic
    log_save_vario("vario", row_count, input$vario_columns,
                   on = isolate(input$record_on))
    par(def.par)
  }, height = function() {
      if (input$vario_mode != "modeled") {
        select_data_vg_layout()$height
      } else {
        select_models_layout()$height
      }
    }
  )
  # select individual plot to fine tune
  output$fit_selector <- renderUI({
    tele_list <- req(select_data()$tele_list)
    if (input$vario_mode == "guesstimate") {
      identities <- sapply(tele_list, function(x) x@info$identity)
      selectInput("fit_selected", NULL,
                  c("Fine-tune" = "", identities))
    }
  })
  # fine tune fit start ----
  observeEvent(input$fit_selected, {
    if (input$fit_selected != "") {
      # LOG fine tune start
      log_msg("Fine-tune Parameters for", input$fit_selected,
              on = isolate(input$record_on))
      showModal(modalDialog(title = paste0("Fine-tune parameters for ",
                                           input$fit_selected),
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
                            style = styles$page_action)))
                            ))
    }
  })
  # init values of sliders ----
  init_slider_values <- reactive({
    req(select_data_vg_list())
    ids <- names(select_data_vg_list())
    vario <- select_data_vg_list()[ids == input$fit_selected][[1]]
    CTMM <- values$selected_data_guess_list[ids == input$fit_selected][[1]]
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
    list(tags$head(tags$script(HTML(JS.onload("vfit_z")))),
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
    log_msg("Apply Fine-tuned Parameters", on = isolate(input$record_on))
    removeModal()
    ids <- sapply(select_data_vg_list(), function(vario) vario@info$identity)
    values$selected_data_guess_list[ids == input$fit_selected][[1]] <- slider_to_CTMM()
  })
  # fine tune fit end ----
  # p5. model selection ----
  callModule(click_help, "model_selection", title = "Model Selection",
             size = "l", file = "help/5_c_model_selection.md")
  # $selected_model_fit_res ----
  # use value instead of reactive expression, because we used a button so need to use observeEvent, cannot start fit automatically by reactive expression.
  # this is the model fit(model selection in ctmm context, but we have a select model process, so use different names now) results for current animal subset. home range and occurence are based on further selected models
  # observeEvent(input$test_digest, {
  #   # cat("tele_guess_list: ", digest::digest(tele_guess_list), "\n")
  #   print(fit_models)
  #   cat("fit_models: ", digest::digest(fit_models), "\n")
  # })
  values$selected_data_model_fit_res <- NULL  # need to clear this at input change too
  # fit models ----
  observeEvent(input$fit_models, {
    # it's common to use existing table row selection in some reactives, until the correct selection updated and reactive evaluate again. With previous fitted models and selection rows, next fit on different animal will first try to plot with existing selection number. Freeze it so we can update the correct selection first. freeze halt the chain (like req), then thaw after other finished.
    freezeReactiveValue(input, "model_fit_summary_rows_selected")
    # guess_list is updated inside select_data_vg_list, but select_data_vg_list is not referenced here, if still in model mode, it was not referenced in UI too, so it didn't get updated.
    tele_guess_list <- align_list(select_data()$tele_list,
                                  values$selected_data_guess_list)
    # cat("tele_guess_list: ", digest::digest(tele_guess_list), "\n")
    # print(fit_models)
    # cat("fit_models: ", digest::digest(fit_models), "\n")
    # cat("test fun:", digest::digest(test_fun), "\n")
    # LOG fit models
    log_msg("Fitting models", on = isolate(input$record_on))
    withProgress(print(system.time(
      values$selected_data_model_fit_res <- para_ll_fit_mem(tele_guess_list))),
      message = "Fitting models to find the best ...")
    names(values$selected_data_model_fit_res) <- names(select_data()$tele_list)
    updateRadioButtons(session, "vario_mode", selected = "modeled")
    # we are selecting rows on a table just generated.
    selectRows(proxy_model_dt, summary_models()$first_models)
  })
  # summary_models() ----
  # summary table and model dt with model as list column
  summary_models <- reactive({
    # the dt with model in list column
    models_dt <- model_fit_res_to_model_list_dt(req(values$selected_data_model_fit_res))
    # the model summary table
    model_summary_dt <- model_list_dt_to_model_summary_dt(models_dt)
    formated_summary_dt <- format_model_summary_dt(model_summary_dt)
    if (input$hide_ci_model) {
      formated_summary_dt <- formated_summary_dt[!str_detect(estimate, "CI")]
      # formated_summary_dt[, estimate := NULL]
    }
    # need a full model table with identity(for base color), full name to create model color, basically a full version of selected model table
    model_full_names_dt <- unique(formated_summary_dt[,
                            .(identity, model_name, full_name)])
    # prepare model color, identity color function

    model_full_names_dt[, base_color := values$id_pal(identity)]
    model_full_names_dt[, variation_number := seq_len(.N), by = identity]
    model_full_names_dt[, color := vary_color(base_color, .N)[variation_number],
                        by = identity]
    # need ordered = TRUE for character vector not being factor yet.
    hr_pal <- colorFactor(model_full_names_dt$color,
                          model_full_names_dt$full_name, ordered = TRUE)
    # calculate the first model row number depend on table mode (hide/show CI)
    # we don't want the row number to show in the final table
    dt <- copy(formated_summary_dt)
    dt[, row_no := .I]
    model_position <- if (input$hide_ci_model) 1 else 2
    first_models <- dt[, row_no[model_position], by = identity]$V1
    return(list(models_dt = models_dt,
                summary_dt = formated_summary_dt,
                hr_pal = hr_pal,
                first_models = first_models))
  })
  # model summary ----
  output$model_fit_summary <- DT::renderDataTable({
    # should not need to use req on reactive expression if that expression have req inside.
    dt <- copy(summary_models()$summary_dt)
    # delete extra col here so it will not be shown, need to copy first otherwise it get modified.
    dt[, model_no := NULL]
    dt[, full_name := NULL]
    # LOG fitted models
    log_dt_md(dt, "Fitted Models", on = isolate(input$record_on))
    # need the full info table to keep the color mapping when only a subset is selected
    info_p <- values$data$merged$info
    # CI_colors <- color_CI(values$data$merged$info$identity)
    model_names <- sort(unique(dt$model_name))
    datatable(dt,options = list(scrollX = TRUE,
                                pageLength = 18, lengthMenu = c(18, 36, 72)),
              rownames = FALSE) %>%
      # majority cells in color by model
      formatStyle('model_name', target = 'row',
                  color = styleEqual(model_names,
                                     hue_pal()(length(model_names)))
      ) %>%
      # override the id col color
      formatStyle('identity', target = 'cell',
                  color = styleEqual(info_p$identity,
                                     hue_pal()(nrow(info_p)))
      ) %>%
      # override the low/high cols with background
      formatStyle(
        'estimate',
        target = 'row',
        backgroundColor = styleEqual(c("CI low", "ML" , "CI high"),
                           c("#FFFFFF", "#F7F7F7", "#F2F2F2"))
      )
  })
  proxy_model_dt <- dataTableProxy("model_fit_summary")
  observeEvent(input$clear_models, {
    # use list() instead of NULL to avoid R 3.4 warning on I(NULL). After DT fixed this warning we can change back to NULL
    selectRows(proxy_model_dt, list())
  })
  # select_models() ----
  # previously we use first model if no selection. now we select them automatically so the intent is more clear, and it's easier to modify selection based on this.
  select_models <- reactive({
    # req(!is.null(values$selected_data_model_fit_res))
    req(length(input$model_fit_summary_rows_selected) > 0)
    # sort the rows selected so same individual models are together
    rows_selected_sorted <- sort(input$model_fit_summary_rows_selected)
    # previous model selection value may still exist
    model_summary_dt <- summary_models()$summary_dt
    selected_names_dt <- unique(model_summary_dt[rows_selected_sorted,
                                           .(identity, model_name, full_name)])
    # selections can be any order, need to avoid sort to keep the proper model order
    selected_models_dt <- merge(selected_names_dt, summary_models()$models_dt,
                                by = c("identity", "model_name"), sort = FALSE)
    # the row click may be any order or have duplicate individuals, need to index by name instead of index
    selected_tele_list <- select_data()$tele_list[selected_names_dt$identity]
    selected_models_list <- selected_models_dt$model
    selected_vg_list <- select_data_vg_list()[selected_names_dt$identity]
    # selected_names_dt[, full_name := str_c(identity, " - ", model_name)]
    # LOG selected models
    log_dt_md(selected_names_dt, "Selected Models",
              on = isolate(input$record_on))
    # must make sure all items in same order
    return(list(names_dt = selected_names_dt,
                tele_list = selected_tele_list,
                models_list = selected_models_list,
                # id_model =
                #   selected_names_dt[, str_c(identity, " - ", model_name)],
                vg_list = selected_vg_list
                ))
  })
  # select_models_layout() ----
  # even the control parameter is same with variogram, the total number could be different
  select_models_layout <- reactive({
    fig_count <- length(select_models()$models_list)
    row_count <- ceiling(fig_count / input$vario_columns)
    height <- input$vario_height * row_count
    # return(list(layout_matrix = layout_matrix, height = height))
    return(list(row_count = row_count, height = height))
  })
  # p6. home range ----
  callModule(click_help, "home_range", title = "Home Range",
             size = "l", file = "help/6_home_range.md")
  # select_models_hranges ----
  select_models_hranges <- reactive({
    req(select_models())
    withProgress(print(system.time(
      res <- akde_mem(select_models()$tele_list,
                      CTMM = select_models()$models_list))),
      message = "Calculating Home Range ...")
    # res
    # withProgress(res <- akde_mem(select_models()$tele_list,
    #                          CTMM = select_models()$models_list),
    #              message = "Calculating home range ...")
    return(res)
  })
  # home range summary ----
  output$range_summary <- DT::renderDataTable({
    hrange_summary_dt <- model_list_dt_to_model_summary_dt(
      build_hrange_list_dt(select_models()$names_dt, select_models_hranges()))
    dt <- format_hrange_summary_dt(hrange_summary_dt)
    dt[, model_no := NULL]
    # LOG home range summary
    log_dt_md(dt, "Home Range Summary")
    if (input$hide_ci_hrange) {
      dt <- dt[!str_detect(estimate, "CI")]
      # dt[, estimate := NULL]
    }
    info_p <- values$data$merged$info
    model_names <- sort(unique(dt$model_name))
    datatable(dt, options = list(scrollX = TRUE,
                                 pageLength = 18, lengthMenu = c(18, 36, 72)),
              rownames = FALSE) %>%
      # majority cells in color by model
      formatStyle('model_name', target = 'row',
                  color = styleEqual(model_names,
                                     hue_pal()(length(model_names)))
      ) %>%
      # override the id col color
      formatStyle('identity', target = 'cell',
                  color = styleEqual(info_p$identity,
                                     hue_pal()(nrow(info_p)))
      ) %>%
      # override the low/high cols with background
      formatStyle(
        'estimate',
        target = 'row',
        # valueColumns = 'estimate',
        backgroundColor = styleEqual(c("CI low", "ML" , "CI high"),
                                     c("#FFFFFF", "#F7F7F7", "#F2F2F2"))
      )
  })
  # function on input didn't update, need a reactive expression?
  get_hr_levels <- reactive({
    parse_CI_levels(input$hr_level_text)
  })
  output$range_plot <- renderPlot({
    selected_tele_list <- select_models()$tele_list
    def.par <- par(no.readonly = TRUE)
    par(mfrow = c(select_models_layout()$row_count, input$vario_columns),
        mar = c(5, 5, 4, 1), ps = 18, cex = 0.72, cex.main = 0.9)
    lapply(seq_along(selected_tele_list), function(i) {
      plot(selected_tele_list[[i]], UD = select_models_hranges()[[i]],
           level.UD = get_hr_levels())
      title(select_models()$names_dt$full_name[i])
      # title(sub = "Error on", cex.sub = 0.85, col.sub = "red")
    })
    # LOG save pic
    log_save_vario("home_range", select_models_layout()$row_count,
                   input$vario_columns,
                   on = isolate(input$record_on))
    log_save_UD("home_range", on = isolate(input$record_on))
    par(def.par)
  }, height = function() { select_models_layout()$height })
  # export shapefiles ----
  output$export_hrange <- downloadHandler(
    filename = function() {
      # up to min so it should be consistent with the folder name inside zip
      current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M")
      paste0("Home_Range_", current_time, ".zip")
    },
    content = function(file) {
      # closure: create a function that take reactive parameters, return a function waiting for folder path. use it as parameter for build zip function, which provide folder path as parameter
      # functional::Curry is misnomer, and it's extra dependency.
      save_shapefiles <- function(hrange_list, ud_levels) {
        write_f <- function(folder_path) {
          # hrange_list came from select_models(), so the order should be synced
          for (i in seq_along(hrange_list)) {
            writeShapefile(hrange_list[[i]], level.UD = ud_levels,
                           folder = folder_path,
                           file = select_models()$names_dt$full_name[i])
          }
        }
        return(write_f)
      }
      build_shapefile_zip(file, save_shapefiles(select_models_hranges(),
                                                get_hr_levels()),
                          session_tmpdir)
      # LOG build shapefiles
      log_msg("Shapefiles built and downloaded")
    }
  )
  # p7. occurrence ----
  callModule(click_help, "occurrence", title = "Occurrence Distribution",
             size = "l", file = "help/7_occurrence.md")
  # select_models_occurrences() ----
  select_models_occurrences <- reactive({
    ud_para_list <- align_list(select_models()$tele_list,
                               select_models()$models_list)
    withProgress(print(system.time(
      res <- para_ll_ud_mem(ud_para_list))),
                 message = "Calculating Occurrence ...")
    res
  })
  # function on input didn't update, need a reactive expression?
  get_oc_levels <- reactive({
    parse_CI_levels(input$oc_level_text)
  })
  output$occurrence_plot <- renderPlot({
    # plot
    def.par <- par(no.readonly = TRUE)
    par(mfrow = c(select_models_layout()$row_count, input$vario_columns),
        mar = c(5, 5, 4, 1), ps = 18, cex = 0.72, cex.main = 0.9)
    lapply(seq_along(select_models_occurrences()), function(i) {
      tryCatch({
        # plot(select_models_occurrences()[[i]], level.UD = input$ud_level)
        plot(select_models_occurrences()[[i]], level.UD = get_oc_levels())
      }, error = function(e) {
        warning(select_models()$names_dt$full_name[i], ": ", e)
        plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10))
      })
      title(select_models()$names_dt$full_name[i])
    })
    # LOG save pic
    log_save_vario("occurrence", select_models_layout()$row_count,
                   input$vario_columns,
                   on = isolate(input$record_on))
    log_save_UD("occurrence", on = isolate(input$record_on))
    par(def.par)
  }, height = function() { select_models_layout()$height })
  # p8. map ----
  callModule(click_help, "map", title = "Map",
             size = "l", file = "help/8_map.md")
  CURRENT_map_path <- list(Point = NULL, Heatmap = NULL)
  # save map to html
  save_map <- function(leaf, map_type) {
    map_file_name <- str_c(map_type, "_", current_timestamp(), ".html")
    # LOG saving map
    log_msg(str_c("Saving map: ", map_type),
              on = isolate(input$record_on))
    map_path <- file.path(LOG_folder, map_file_name)
    saveWidget(leaf, file = map_path, selfcontained = TRUE)
    # add link in rmd, difficult to embed map itself.
    log_add_rmd(str_c("\n[", map_type, "](", map_file_name, ")\n"))
    # record the latest file path
    CURRENT_map_path[[map_type]] <<- map_path
  }
  # shared basemap
  tiles_info <- list(here = c("HERE.terrainDay", "HERE.satelliteDay",
                              "HERE.hybridDay"),
                     open = c("OpenTopoMap",
                              "Esri.WorldTopoMap", "Esri.WorldImagery"),
                     here_app_id = 'ehftALetcOLjvopsXsZP',
                     here_app_code = 'a5oE5ewb0eH9ojahDBLUzQ'
  )
  base_map <- init_base_maps(tiles_info)
  output$point_map_holder <- renderUI(
    leafletOutput("point_map",
                  height = input$map_height)
  )
  # point map ----
  get_point_map <- reactive({
    dt <- select_data()$data
    info <- select_data()$info
    # the color pallete need to be built upon full data set, not current subset
    # id_pal <- colorFactor(hue_pal()(nrow(values$data$merged$info)),
    #                       values$data$merged$data$identity)
    # we cannot put id_pal in same place with hr_pal because user may check map without fitting models, when summary_models doesn't exist.
    withProgress(leaf <- base_map %>% add_points(dt, info, values$id_pal),
                 message = "Building maps...")
    # there could be mismatch between individuals and available home ranges. it's difficult to test reactive value exist(which is an error when not validated), so we test select_models instead. brewer pallete have upper/lower limit on color number, use hue_pal with different parameters.
    if (reactive_validated(select_models_hranges())) {
      # color pallete need to be on full model name list, but we don't want to change the model summary table since it doesn't need to be displayed in app.
      # hr_pal <- model_pal(summary_models()$model_full_names_dt, id_pal)
      hr_pal <- summary_models()$hr_pal
      leaf <- leaf %>%
        add_home_range_list(select_models_hranges(), get_hr_levels(),
                            hr_pal(select_models()$names_dt$full_name),
                            select_models()$names_dt$full_name) %>%
        addLayersControl(
          baseGroups = c(tiles_info$here, tiles_info$open),
          overlayGroups = c(grid_group, info$identity,
                            select_models()$names_dt$full_name
                            # ,
                            # draw_group
          ),
          options = layersControlOptions(collapsed = FALSE)
        )
    } else {
      leaf <- leaf %>%
        addLayersControl(
          baseGroups = c(tiles_info$here, tiles_info$open),
          overlayGroups = c(grid_group, info$identity
                            # , draw_group
          ),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
    return(leaf)
  })
  # reactive get map, render function save map and count time
  output$point_map <- renderLeaflet({
    leaf <- get_point_map()
    print(system.time(
      save_map(leaf, "Point")
    ))
    return(leaf)
  })
  output$heat_map_holder <- renderUI(
    leafletOutput("heat_map",
                  height = input$map_height)
  )
  # heatmap ----
  get_heat_map <- reactive({
    base_map %>% add_heat(select_data()$data, tiles_info)
  })
  output$heat_map <- renderLeaflet({
    # dt <- select_data()$data
    # leaf <- base_map %>% add_heat(dt, tiles_info)
    leaf <- get_heat_map()
    print(system.time(
      save_map(leaf, "Heatmap")
    ))
    return(leaf)
  })
  output$cluster_map_holder <- renderUI(
    leafletOutput("cluster_map",
                  height = input$map_height)
  )
  # need a history list of tabs, from tab switching and page switching
  # values$map_tab_history <- NULL
  # first map page view ----
  # check data size, switch to heatmap if too big. this only happen when moving into map page. Later there is no limit
  observeEvent(input$tabs, {
    if (input$tabs == "map") {
      # buffalo 17k, gulls 32k start to be slow. set threshold to 25k.
      # cat(nrow(select_data()$data), "\n")
      if (nrow(select_data()$data) > 25000) {
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
      leafletProxy("point_map", session) %>%
        # fitBounds(input$heat_map_bounds$east, input$heat_map_bounds$north,
        #           input$heat_map_bounds$west, input$heat_map_bounds$south)
        # setView(NULL, NULL, zoom = input$heat_map_zoom) %>%
        apply_bounds(input$heat_map_bounds)
        # fitBounds(input$heat_map_bounds$east, input$heat_map_bounds$north,
        #           input$heat_map_bounds$west, input$heat_map_bounds$south)
    }
  })
  # reset map view ----
  observeEvent(input$reset_map_view, {
    # fitBounds will have some allowance so no need to add padding here.
    bounds <- get_bounds(select_data()$data)
    leafletProxy(map_name_by_tab[[input$map_tabs]], session) %>%
      fitBounds(bounds$lng1, bounds$lat1, bounds$lng2, bounds$lat2)
  })
  # download map ----
  output$download_map <- downloadHandler(
    filename = function() {
      paste0(input$map_tabs, "_", current_timestamp(), ".html")
    },
    content = function(file) {
      # LOG download map
      log_msg("Downloading map", on = isolate(input$record_on))
      # to save map with current view, update the map object with current bounds. the proxy only updated the in memory structure, not the map objec itself
      if (input$map_tabs == "Point") {
        leaf <- get_point_map() %>% apply_bounds(input$point_map_bounds)
        save_map(leaf, "Point")
      } else {
        leaf <- get_heat_map() %>% apply_bounds(input$heat_map_bounds)
        save_map(leaf, "Heatmap")
      }
      # leaf <- get_heat_map() %>%
      #   fitBounds(input$heat_map_bounds$east, input$heat_map_bounds$north,
      #             input$heat_map_bounds$west, input$heat_map_bounds$south)
      file.copy(CURRENT_map_path[[input$map_tabs]], file)
    }
  )
  # p9. report ----
  callModule(click_help, "report", title = "Work Report",
             size = "l", file = "help/9_work_report.md")
  # save session ----
  output$save_session <- downloadHandler(
    filename = function() {
      paste0("Session_", current_timestamp(), ".zip")
    },
    content = function(file) {
      # we are checking input data instead of select_data, which is the real condition that can cause error, because it's easier to check and should be in same status
      if (is.null(values$data$input_tele_list)) {
        showNotification("No data to save", duration = 7,
                         type = "error")
      } else {
        # LOG save session
        log_msg("Saving session data", on = isolate(input$record_on))
        # pack and save cache
        cache_zip_path <- compress_folder(cache_path, "cache.zip")
        # data in .rds format, pack multiple variables into list first.
        saved <- list(data = values$data,
                      chosen_row_nos = select_data()$chosen_row_nos,
                      selected_data_model_fit_res =
                        values$selected_data_model_fit_res,
                      selected_data_guess_list =
                        values$selected_data_guess_list,
                      # didn't sort this, need to sort it when restoring
                      model_fit_summary_rows_selected =
                        input$model_fit_summary_rows_selected
                      )
        saved_rds_path <- file.path(session_tmpdir, "saved.rds")
        saveRDS(saved, file = saved_rds_path)
        # also save report for reference
        generate_report(preview = FALSE)
        # move to same directory for easier packing. use rename to reduce effort
        # file.copy(values$html_path, file.path(session_tmpdir, "report.html"),
        #           overwrite = TRUE)
        file.rename(values$html_path, file.path(session_tmpdir, "report.html"))
        # pack to session.zip, this is a temp name anyway.
        session_zip_path <- compress_relative_files(
          session_tmpdir, c("cache.zip", "saved.rds", "report.html"),
          "session.zip")
        file.copy(session_zip_path, file)
      }
    }
  )
  # load session ----
  observeEvent(input$load_session, {
    # LOG load session
    log_msg("Loading session data", input$load_session$name,
            on = isolate(input$record_on))
    # session.zip -> cache.zip, saved.rds, report.html
    unzip(input$load_session$datapath, exdir = session_tmpdir)
    if (APP_local) {
      browseURL(file.path(session_tmpdir, "report.html"))
    }
    # first clear current cache.
    reset_cache(cache_path)
    # using hard coded file name, need to search all usage when changed. cache.zip have cache folder inside it, so need to extract one level up
    unzip(file.path(session_tmpdir, "cache.zip"), exdir = session_tmpdir)
    loaded <- readRDS(file.path(session_tmpdir, "saved.rds"))
    # restore variables in order, may need to freeze some
    values$data <- loaded$data
    values$selected_data_model_fit_res <- loaded$selected_data_model_fit_res
    values$selected_data_guess_list <- loaded$selected_data_guess_list
    # freezeReactiveValue(input, "model_fit_summary_rows_selected")
    # selectRows(proxy_model_dt, loaded$model_fit_summary_rows_selected)
    updateTabItems(session, "tabs", "plots")
    # freezeReactiveValue(input, "individuals_rows_selected")
    # cat("selecting rows\n")
    # selectRows(proxy_individuals, loaded$chosen_row_nos)
  })
  # view_report ----
  generate_report <- function(preview) {
    # LOG report generated, need to be placed before the markdown rendering, otherwise will not be included.
    log_msg("Work Report Generated", on = isolate(input$record_on))
    # write markdown file
    markdown_path <- file.path(LOG_folder, "report.rmd")
    writeLines(LOG_rmd_vec, con = markdown_path)
    # render markdown to html
    html_path <- file.path(LOG_folder, "report.html")
    rmarkdown::render(markdown_path, output_file = html_path, quiet = TRUE)
    # file.copy(html_path, "www/report.html", overwrite = TRUE)
    # non-encoded file path cannot have white space for browserURL
    if (preview) browseURL(html_path)
    values$html_path <- html_path
  }
  # preview in local mode, download in host mode
  output$view_report <- renderUI(
    if (APP_local) {
      actionButton("preview_report", "Preview Report",
                   icon = icon("file-text-o"),
                   style = styles$page_action)
    } else {
      downloadButton("download_report", "Download Report",
                     style = styles$download_button)
    }
  )
  observeEvent(input$preview_report, {
    generate_report(preview = TRUE)
  })
  # observeEvent(input$generate_report, {
  #   if (session$clientData$url_hostname == "127.0.0.1") {
  #     generate_report(preview = TRUE)
  #   } else {
  #     generate_report(preview = FALSE)
  #   }
  # })
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Report_", current_timestamp(), ".html")
    },
    content = function(file) {
      # LOG download report
      log_msg("Downloading work report", on = isolate(input$record_on))
      generate_report(preview = FALSE)
      file.copy(values$html_path, file)
      # if (is.null(values$html_path)) {
      #   showNotification("Report not generated yet", duration = 7,
      #                    type = "error")
      # } else {
      #   file.copy(values$html_path, file)
      # }
    }
  )
  output$download_report_zip <- downloadHandler(
    filename = function() {
      paste0("Report_", current_timestamp(), ".zip")
    },
    content = function(file) {
      # LOG download report zip
      log_msg("Downloading work report zip", on = isolate(input$record_on))
      generate_report(preview = FALSE)
      zip_path <- compress_folder(LOG_folder, "report.zip")
      file.copy(zip_path, file)
    }
  )
}
