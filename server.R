# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)
# all helper functions are for server side
source("helpers.R", local = TRUE)
source("cut_divide.R", local = TRUE)

server <- function(input, output, session) {
  # p1. import ----
  values <- reactiveValues()
  # run this after every modification on data and list separately. i.e. values$data$tele_list changes, or data not coming from merge_animals.
  verify_global_data <- reactive({
    if (debug_mode) {
      match_tele_merged(values$data$tele_list, values$data$merged)
    }
  })
  # values$ <----
  values$data <- NULL  # 4 items need to be synced
  # important reactive value and expressions need special comments, use <--. the design need to well thought
  # input_tele_list: telemetry obj list from as.telemetry on input data: movebank download, local upload, package data. all reference of this value should wrap req around it. Once it's used, no need to keep the copy. thus add it with the new time subset. We don't need to keep the dt version because we can often just use existing dt and other info. do need to verify tele and dt is synced.
  # tele_list, merged: the telemetry version and merged data.table version of updated data reflected changes on outlier removal and time subsetting.
  # all_removed_outliers: records of all removed outliers. original - all removed = current. the table have id column so this can work across different individuals.
  # the time subset only live in time subsetting process, the result of the process update tele_list and merged.
  # the extra column of outliers only live in outlier page. the result of the process update whole data. note may need to use column subset when operating between dt with or without extra columns.
  # for any data source changes, need to update these 4 items together.
  update_input_data <- function(tele_list) {
    values$data$input_tele_list <- tele_list
    values$data$tele_list <- tele_list
    values$data$merged <- merge_animals(tele_list)
    values$data$all_removed_outliers <- NULL
    values$model_select_res <- NULL
    updateTabItems(session, "tabs", "plots")
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
    data_import(input$file1$datapath)
    updateRadioButtons(session, "load_option", selected = "upload")
    updateTabItems(session, "tabs", "plots")
  }
  observeEvent(input$file1, {
    req(input$file1)
    file_uploaded()
  })
  # observe radio button changes
  observeEvent(input$load_option, {
    switch(input$load_option,
           ctmm = {
             data("buffalo")
             update_input_data(buffalo)
           },
           ctmm_sample = {
             data("buffalo")
             sample_data <- pick_m_tele_list(buffalo, input$sample_size)
             update_input_data(sample_data)
           },
           upload = {
             # need to check NULL input from source, stop error in downstream
             req(input$file1)
             file_uploaded()
           })
  })
  callModule(click_help, "import", title = "Data Import Options", size = "m",
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
    }
  })
  # 1.4 selected details ----
  # save file name need different column so didn't use this reactive
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
  observeEvent(input$download, {
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
    }
  })
  callModule(click_help, "download", title = "Download Movebank data",
             size = "l", file = "help/1_movebank_download.md")
  # 1.5 save, import data ----
  output$save <- downloadHandler(
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
    }
  )
  observeEvent(input$import, {
    req(values$move_bank_dt[, .N] > 0)
    data_import(values$move_bank_dt)
    updateTabItems(session, "tabs", "plots")
  })
  # p2. plots ----
  # input (upload, movebank, buffalo) -> current -> chose animal in table
  # current: merge telemetry to df, remove outliers if in quene, return df, info table, removed outliers full data
  # 2.3 data summary ----
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
    # cat("chosen animals:\n")
    # print(animals_dt[, .N, by = identity])
    subset_indice <- values$data$merged$info$identity %in% chosen_ids
    # didn't verify data here since it's too obvious and used too frequently. if need verfication, need call function on subset.
    return(list(data = animals_dt,
                info = values$data$merged$info[subset_indice],
                tele_list = values$data$tele_list[subset_indice]
                ))
  })
  # 2.4.1 overview plot ----
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
    ggplot() +
      {if (input$overlay_all) {
        geom_point(data = values$data$merged$data, aes(x, y),
                   size = input$point_size_1, alpha = 0.6, colour = "gray")
      }} +
      geom_point(data = animals_dt, aes(x, y, colour = id),
                 size = input$point_size_1, alpha = 0.7) +
      coord_fixed(xlim = location_plot_gg_range$x,
                  ylim = location_plot_gg_range$y) +
      factor_color(animals_dt$id) +
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      theme(legend.position = "top",
            legend.direction = "horizontal") +
      bigger_theme + bigger_key
  }
  # , height = 400, width = "auto"
  # , height = styles$height_plot_loc, width = "auto"
  , height = function() { input$canvas_height }
    , width = "auto"
  )
  # 2.4.2 facet ----
  output$location_plot_facet_fixed <- renderPlot({
    # by convention animals_dt mean the data frame, sometimes still need some other items from list, use full expression
    animals_dt <- req(select_data()$data)
    ggplot(data = animals_dt, aes(x, y)) +
      geom_point(size = 0.1, aes(colour = id)) +
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      factor_color(animals_dt$id) +
      facet_grid(id ~ .) +
      coord_fixed() +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
  }, height = function() { input$canvas_height }, width = "auto")
  # 2.4.3 individual plot ----
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
    grid.arrange(grobs = g_list, layout_matrix =
                   matrix(1:fig_count, nrow = fig_count / input$plot4_col,
                          ncol = input$plot4_col, byrow = TRUE))
  }, height = function() { input$canvas_height }, width = "auto")
  # 2.5 histogram facet ----
  output$histogram_facet <- renderPlot({
    animals_dt <- req(select_data()$data)
    ggplot(data = animals_dt, aes(x = timestamp, fill = id)) +
      geom_histogram(bins = 60) +
      factor_fill(animals_dt$id) +
      facet_grid(id ~ .) +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
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
    outlier_page_data <- req(select_data())  # data, info, tele_list
    animals_dt <- outlier_page_data$data
    animals_dt <- calculate_distance(animals_dt)
    animals_dt <- calculate_speed(animals_dt)
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
    ggplot(animals_dt, aes(x = distance_center)) +
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
  })
  # need the whole range to get proper unit selection
  format_outliers <- function(animal_selected_data, animals_dt) {
    animal_selected_data[, .(id, row_no,
       timestamp = format_datetime(timestamp),
       distance_center = format_distance_f(animals_dt[, distance_center])(
         distance_center),
       distance_center_SI = format(distance_center, digits = 3),
       speed = format_speed_f(animals_dt[, speed])(speed),
       speed_SI = format(speed, digits = 3))]
  }
  # brush selection function
  select_range <- function(his_type){
    return(reactive({
      # everything in outlier page should take animal_dt from binned version
      # the current data have distance/speed column, the binned version just create the color factors. in theory we could use the original data but we may need the color factor sometimes.
      switch(his_type,
             distance = {
               col_name = quote(distance_center)
               animals_dt <- req(bin_by_distance()$animals_dt)
             },
             speed = {
               col_name = quote(speed)
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
    ggplot(animals_dt, aes(x, y)) +
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

  })
  # points in selected distance range
  output$points_in_distance_range <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data.
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
    freezeReactiveValue(input, "points_in_distance_range_rows_selected")
    selectRows(proxy_points_in_distance_range, list())
    freezeReactiveValue(input, "distance_his_brush")
    session$resetBrush("distance_his_brush")
    remove_outliers(points_to_remove)
  })
  # p3.b.1 speed histogram ----
  bin_by_speed <- reactive({
    # animals_dt <- req(select_data()$data)
    animals_dt <- req(calc_outlier()$data)
    return(color_break(input$speed_his_bins, animals_dt,
                       "speed", format_speed_f))
  })
  output$speed_histogram <- renderPlot({
    speed_binned <- req(bin_by_speed())
    animals_dt <- speed_binned$animals_dt
    # cat("dataset in speed page\n")
    # print(animals_dt[, .N, by = id])
    ggplot(animals_dt, aes(x = speed)) +
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
    g
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
    # to ensure proper order of execution, need to clear the points in range table row selection, and the brush value of histogram, otherwise some reactive expressions will take the leftover value of them when plot are not yet updated fully.
    # freeze it so all expression accessing it will be put on hold until update finish, because the reset here just send message to client, didn't update immediately
    freezeReactiveValue(input, "points_in_speed_range_rows_selected")
    selectRows(proxy_points_in_speed_range, list())
    freezeReactiveValue(input, "speed_his_brush")
    session$resetBrush("speed_his_brush")
    remove_outliers(points_to_remove)
  })
  # all removed outliers ----
  output$all_removed_outliers <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data.
    req(values$data$all_removed_outliers)
    # animals_dt <- req(select_data()$data)
    animals_dt <- req(calc_outlier()$data)
    datatable(format_outliers(values$data$all_removed_outliers,
                              animals_dt),
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
  })
  # p4. time subset ----
  observeEvent(input$tabs, {
    req(values$data)
    if (input$tabs == "subset") {
      # must select single animal to proceed
      if (length(input$individuals_rows_selected) != 1) {
        showNotification("Please select single Animal", type = "error")
      }
    }
  })
  callModule(click_help, "time_subsetting", title = "Subset data by time",
             size = "l", file = "help/4_time_subsetting.md")
  # actually should not color by page 1 color because we will rainbow color by time
  output$selected_summary <- DT::renderDataTable({
    req(values$data)
    req(length(input$individuals_rows_selected) == 1)
    info <- values$data$merged$info
    dt <- info[input$individuals_rows_selected]
    datatable(dt, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  # color_bin_animal() ----
  # when putting brush in same reactive value, every brush selection updated the whole value which update the histogram then reset brush.
  color_bin_animal <- reactive({
    # ensure time range table are cleared even there is no suitable single individual
    values$time_ranges <- NULL
    req(values$data)
    req(length(input$individuals_rows_selected) == 1)
    # selected_id <- values$data$merged$info$identity[
    #   input$individuals_rows_selected]
    selected_id <- select_data()$info$identity
    # tele_ids <- sapply(values$data$tele_list, function(x) x@info$identity)
    # tele_i <- values$data$tele_list[tele_ids == selected_id][[1]]
    # data_i <- values$data$merged$data[identity == selected_id]
    data_i <- select_data()$data
    data_i[, color_bin_start :=
             cut_date_time(timestamp, input$time_color_bins)]  # a factor
    color_bin_start_vec_time <- ymd_hms(levels(data_i$color_bin_start))
    color_bin_breaks <- c(color_bin_start_vec_time,
                                     data_i[t == max(t), timestamp])
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
    ggplot(data = animal_binned$data, aes(x = timestamp)) +
      geom_histogram(breaks = as.numeric(animal_binned$color_bin_breaks),
                     fill = hue_pal()(input$time_color_bins)) +
      scale_x_datetime(breaks = animal_binned$color_bin_breaks,
                       labels = date_format("%Y-%m-%d %H:%M:%S")) +
      ggtitle(animal_binned$data[1, identity]) + center_title +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # selected time range ----
  # brush selection and matching color bins
  select_time_range <- reactive({
    animal_binned <- color_bin_animal()
    if (is.null(input$time_sub_his_brush)) {
      select_start <- animal_binned$data[t == min(t), timestamp]
      select_end <- animal_binned$data[t == max(t), timestamp]
    } else {
      # brush value in seconds
      select_start <- as_datetime(input$time_sub_his_brush$xmin)
      select_end <- as_datetime(input$time_sub_his_brush$xmax)
    }
    select_length <- select_end - select_start
    # use identity because id is factor. avoid surprises
    return(list(
      # identity = animal_binned$identity,
                select_start = select_start, select_end = select_end,
                select_length = select_length))
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
                           length = format_diff_time(select_length))]
      return(time_range_dt[, .(start, end, length)])
    }
  }
  output$current_range <- DT::renderDataTable({
    dt <- format_time_range(as.data.frame(select_time_range()))
    datatable(dt, options =
                list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
      formatStyle(1, target = 'row', color = "#00c0ef")
  })
  # 4.3 selected locations ----
  selected_loc_ranges <- add_zoom("selected_loc")
  output$selected_loc <- renderPlot({
    animal_binned <- color_bin_animal()
    time_range <- select_time_range()
    animal_selected_data <- animal_binned$data[
      (timestamp >= time_range$select_start) &
        (timestamp <= time_range$select_end)]
    ggplot(data = animal_binned$data, aes(x, y)) +
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
  })
  # 4.4 time range table ----
  # time_subsets hold a table of time ranges for current individual, this should only live in one time subsetting process(clear in beginning, in color_bin_animal. clear after finish, when subset is generated), which is always on single individual. If user moved around pages without changing individual, the states are kept. Once generated, the new subset instance data and tele obj are inserted to values$current and kept there, which hold for all input session.
  observeEvent(input$add_time, {
    l <- list(values$time_ranges, as.data.frame(select_time_range()))
    values$time_ranges <- rbindlist(l)
  })
  observeEvent(input$delete_time_sub_rows, {
    if (!is.null(input$time_ranges_rows_selected)) {
      values$time_ranges <- values$time_ranges[
        -as.numeric(input$time_ranges_rows_selected)
      ]
    }
  })
  observeEvent(input$reset_time_sub, {
    values$time_ranges <- NULL
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
    # organize
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
    # new_dt <- calculate_distance(new_dt)
    # new_dt <- calculate_speed(new_dt)
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
    updateTabItems(session, "tabs", "plots")
    msg <- paste0(new_id, " added to data")
    showNotification(msg, duration = 2, type = "message")
  })
  output$time_ranges <- DT::renderDataTable({
    # NULL is valid input when all rows removed, no need for req here.
    datatable(format_time_range(values$time_ranges), options =
                list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  # p5. variogram ----
  callModule(click_help, "variogram", title = "Variograms",
             size = "l", file = "help/5_a_variograms.md")
  callModule(click_help, "vario_irregular", title = "Irregular Data",
             size = "l", file = "help/5_b_irregular_data.md")
  # values$guess_list created from input data and save manual changes from fine tune.
  values$guess_list <- NULL
  # vg_list() ----
  vg_list <- reactive({
    tele_list <- select_data()$tele_list
    # guess value need to be reactive so it can be modified in manual fit.
    values$guess_list <- lapply(tele_list,
                    function(tele) ctmm.guess(tele, interactive = FALSE))
    res <- lapply(tele_list, variogram)
    names(res) <- names(tele_list)
    return(res)
  })
  # vg_layout() ----
  # modeled mode and home range etc share same layout, which could coexist with variograms. so we cannot use one layout for all of them.
  vg_layout <- reactive({
    fig_count <- length(vg_list())
    row_count <- ceiling(fig_count / input$vario_columns)
    height <- input$vario_height * row_count
    # return(list(layout_matrix = layout_matrix, height = height))
    return(list(row_count = row_count, height = height))
  })
  # variogram CTMM ----
  get_vario_ctmm_list <- reactive({
    switch(input$vario_mode,
           empirical = NULL,
           guesstimate = values$guess_list,
           # the plot will not shown when models are not fitted yet
           # modeled = lapply(req(values$model_select_res), function(x) {
           #   x[[1]]
           # })
           modeled = select_models()$models
           )
    # if (input$vario_mode == ) {
    #   guess_list <- values$guess_list
    #   # if ("error" %in% input$fit_vario) {
    #   #   # need to assign result back to list. lapply didn't change reference
    #   #   guess_list <- lapply(guess_list, function(x) {
    #   #     x$error <- TRUE
    #   #     x
    #   #   })
    #   # }
    # } else {
    #   guess_list <- NULL
    # }
    # return(guess_list)
  })
  # plot variograms ----
  output$vario_plot_zoom <- renderPlot({
    # in modeled mode, draw selected subset of vg_list and selected models, using select_models_layout
    if (input$vario_mode != "modeled") {
      vg_lst <- vg_list()
      row_count <- vg_layout()$row_count
      title_vec <- names(select_data()$tele_list)
    } else {
      vg_lst <- select_models()$vg_list
      row_count <- select_models_layout()$row_count
      title_vec <- select_models()$dt[, paste0(identity, " - ", model_name)]
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
    par(def.par)
  }, height = function() {
      if (input$vario_mode != "modeled") {
        vg_layout()$height
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
      showModal(modalDialog(title = paste0("Fine-tune parameters for ",
                                           input$fit_selected),
                            fluidRow(column(4, uiOutput("fit_sliders")),
                                     column(8, plotOutput("fit_plot"))),
                            size = "l",
                            footer = fluidRow(
        column(3, actionButton("center_slider", "Center current values",
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
  # init sliders ----
  init_guess <- reactive({
    req(vg_list())
    ids <- sapply(vg_list(), function(vario) vario@info$identity)
    vario <- vg_list()[ids == input$fit_selected][[1]]
    CTMM <- values$guess_list[ids == input$fit_selected][[1]]
    res <- list()
    # CTMM <- ctmm.guess(buffalo[[1]], interactive = FALSE)
    fraction <- 0.5
    # rename this variable
    # variogram <- SVFS[[1]]
    # above should be from reactive
    m <- 2 # slider length relative to point guestimate
    n <- length(vario$lag)
    # CTMM <- ctmm:::variogram.guess(vario,CTMM)
    # CTMM$circle <- 10
    # slider 1: zoom
    b <- 10
    min.step <- 10*vario$lag[2]/vario$lag[n]
    res$slider1 <- list(label = "fraction",
                        min = log(min.step,b), max = 0,
                        value = log(fraction,b), step = 0.001)
    # slider 2: sigma
    if(length(CTMM$tau)==1) { CTMM$tau[2] <- 0 }
    sigma <- mean(diag(CTMM$sigma))
    sigma.unit <- ctmm:::unit(sigma,"area",concise=TRUE)
    sigma <- sigma / sigma.unit$scale
    label_2 <- paste("sigma variance (",sigma.unit$name,")",sep="")
    res$slider2 <- list(label = label_2, min = 0, max = m*sigma,
                        value = sigma, step = 0.01)
    # slider 3_a, 3_b: tau
    tau <- CTMM$tau
    tau1.unit <- ctmm:::unit(tau[1],"time",2,concise=TRUE)
    tau2.unit <- ctmm:::unit(tau[2],"time",2,concise=TRUE)
    tau[1] <- tau[1] / tau1.unit$scale
    tau[2] <- tau[2] / tau2.unit$scale
    label_3_a <- paste("tau position (",tau1.unit$name,")",sep="")
    label_3_b <- paste("tau velocity (",tau2.unit$name,")",sep="")
    res$slider3_a <- list(label = label_3_a, min = 0, max = m*tau[1],
                          value = tau[1], step = 0.01)
    res$slider3_b <- list(label = label_3_b, min = 0, max = m*tau[2],
                          value = tau[2], step = 0.01)
    # optional slider: circulation
    circle <- CTMM$circle
    if (circle)
    {
      circle.unit <- ctmm:::unit(circle,"time",concise=TRUE)
      circle <- circle / circle.unit$scale
      label_cir <- paste("circulation (",circle.unit$name,")",sep="")
      res$slider_cir <- list(label = label_cir,
                             min = min(0,m*circle), max = max(0,m*circle),
                             value = circle, step = 0.01)
      res$circle.unit <- circle.unit
    } else {
      res$slider_cir <- NULL
    }
    # slider 5: error
    error <- CTMM$error
    e2 <- max(100,2*error)
    res$slider5 <- list(label = "error (m)", min = 0, max = e2,
                        value = as.numeric(error), step = 0.1)
    # assign here so we don't need to change too much in above
    res <- c(res, list(vario = vario, CTMM = CTMM,
                       b = b, sigma.unit = sigma.unit,
                       tau1.unit = tau1.unit, tau2.unit = tau2.unit))
    return(res)
  })
  # init sliders
  output$fit_sliders <- renderUI({
    # start from reactive value of selected variogram, guess obj
    # already have this in app, replace with variable or reactive values
    req(init_guess())
    build_slider <- function(id, para_list) {
      # have to round the number and limit the step otherwise too many digits
      sliderInput(id, label = para_list$label, min = round(para_list$min, 3),
                  max = round(para_list$max, 3),
                  value = round(para_list$value, 3),
                  step = para_list$step)
    }
    return(list(tags$head(tags$script(HTML(JS.onload("fit_1_zoom")))),
                build_slider("fit_1_zoom", init_guess()$slider1),
                build_slider("fit_2_sigma", init_guess()$slider2),
                build_slider("fit_3_tau_a", init_guess()$slider3_a),
                build_slider("fit_3_tau_b", init_guess()$slider3_b),
                if (!is.null(init_guess()$slider_cir))
                  build_slider("fit_opt_cir", init_guess()$slider_cir),
                build_slider("fit_5_error", init_guess()$slider5)))
  })
  observeEvent(input$center_slider, {
    extend_slider <- function(id) {
      # Shiny will complain for named vector
      updateSliderInput(session, id, max = round(input[[id]] * 2, 2))
    }
    extend_slider("fit_2_sigma")
    extend_slider("fit_3_tau_a")
    extend_slider("fit_3_tau_b")
    if (!is.null(init_guess()$slider_cir))
      extend_slider("fit_opt_cir")
    # extend_slider("fit_5_error")
  })
  # get CTMM from sliders
  updated_CTMM <- reactive({
    # variables need from reactive: b as log base, each unit, ctmm object
    CTMM <- req(init_guess()$CTMM)
    req(input$fit_2_sigma)
    if (length(CTMM$axes)==2) {
      CTMM$sigma <- CTMM$sigma@par
      CTMM$sigma[1] <- input$fit_2_sigma *
        init_guess()$sigma.unit$scale / cosh(CTMM$sigma[2]/2)
    } else {
      CTMM$sigma <- input$fit_2_sigma
    }
    CTMM$tau <- c(input$fit_3_tau_a * init_guess()$tau1.unit$scale,
                  input$fit_3_tau_b * init_guess()$tau2.unit$scale)
    if(CTMM$circle) {
      CTMM$circle <- input$fit_opt_cir * init_guess()$circle.unit$scale
    }
    CTMM$error <- input$fit_5_error
    CTMM <- as.list(CTMM)
    CTMM$info <- attr(init_guess()$vario, "info")
    CTMM <- do.call("ctmm",CTMM)
  })
  # update plot by sliders ----
  output$fit_plot <- renderPlot({
    req(updated_CTMM())
    fraction <- init_guess()$b ^ input$fit_1_zoom
    plot(init_guess()$vario,CTMM = updated_CTMM(),fraction=fraction)
  })
  observeEvent(input$tuned, {
    removeModal()
    ids <- sapply(vg_list(), function(vario) vario@info$identity)
    values$guess_list[ids == input$fit_selected][[1]] <- updated_CTMM()
  })
  # fine tune fit end ----
  # p5. model selection ----
  callModule(click_help, "model_selection", title = "Model Selection",
             size = "l", file = "help/5_c_model_selection.md")
  # $model_select_res ----
  values$model_select_res <- NULL  # need to clear this at input change too
  # fit models ----
  observeEvent(input$fit_models, {
    tele_guess_list <- align_list(select_data()$tele_list,
                                  values$guess_list)
    withProgress(print(system.time(
      values$model_select_res <- para_ll(tele_guess_list, model_select))),
      message = "Fitting models to find the best ...")
    names(values$model_select_res) <- names(select_data()$tele_list)
    updateRadioButtons(session, "vario_mode", selected = "modeled")
    # we are selecting rows on a table just generated.
    dt <- copy(summary_models()$summary_dt)
    dt[, row_no := .I]
    dt[, row_no[1], by = identity]$V1
    selectRows(proxy_model_dt, dt[, row_no[1], by = identity]$V1)
  })
  # summary_models() ----
  # summary table and model dt with model as list column
  summary_models <- reactive({
    # the dt with model in list column
    models_dt <- model_select_res_to_model_list_dt(req(values$model_select_res))
    # the model summary table
    model_summary_dt <- model_list_dt_to_model_summary_dt(models_dt)
    formated_summary_dt <- format_model_summary_dt(model_summary_dt)
    if (!input$show_ci_model) {
      formated_summary_dt <- formated_summary_dt[!str_detect(estimate, "CI")]
      # formated_summary_dt[, estimate := NULL]
    }
    return(list(models_dt = models_dt,
                summary_dt = formated_summary_dt))
  })
  # model table ----
  output$model_fit_summary <- DT::renderDataTable({
    # should not need to use req on reactive expression if that expression have req inside.
    dt <- summary_models()$summary_dt
    # delete extra col here so it will not be shown, but we can still use them in code because the reactive expression still have it.
    dt[, model_no := NULL]
    # need the full info table to keep the color mapping when only a subset is selected
    info_p <- values$data$merged$info
    # CI_colors <- color_CI(values$data$merged$info$identity)
    model_names <- sort(unique(dt$model_name))
    datatable(dt,options = list(scrollX = TRUE,
                                pageLength = 18, lengthMenu = c(6, 18, 36)),
              # class = 'table-bordered',
              rownames = FALSE) %>%
      # majority cells in color by model
      formatStyle('model_name', target = 'row',
                  color = styleEqual(model_names,
                                     hue_pal()(length(model_names)))
                  ,
                  backgroundColor = "#FFFFFF"
                  # lineHeight = '70%'
                  # color = styleEqual(CI_colors$levels, CI_colors$values)
      ) %>%
      # override the id col color
      formatStyle('identity', target = 'cell',
                  color = styleEqual(info_p$identity,
                                     hue_pal()(nrow(info_p)))
                  # color = styleEqual(CI_colors$levels, CI_colors$values)
      ) %>%
      # override the low/high cols
      formatStyle(
        # c('estimate', 'area', 'tau position', 'tau velocity', 'speed'),
        'estimate',
        target = 'row',
        # valueColumns = 'estimate',
        backgroundColor = styleEqual(c("CI low", "CI high"),
                           c("#E3E3E3", "#CCCCCC"))
        # color = styleEqual(CI_colors$levels, CI_colors$values)
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
    req(length(input$model_fit_summary_rows_selected) > 0)
    model_summary_dt <- summary_models()$summary_dt
    # if (length(input$model_fit_summary_rows_selected) > 0) {
    #   # unique is to remove duplicate selection in CI mode
    #   selected_dt <- unique(model_summary_dt[input$model_fit_summary_rows_selected,
    #                                          .(identity, model_name)])
    # } else {
    #   selected_dt <- model_summary_dt[, .(model_name = model_name[1]),
    #                                   by = identity]
    # }
    selected_dt <- unique(model_summary_dt[input$model_fit_summary_rows_selected,
                                           .(identity, model_name)])
    # selections can be any order, need to avoid sort to keep the proper model order
    selected_models_dt <- merge(selected_dt, summary_models()$models_dt,
                                by = c("identity", "model_name"), sort = FALSE)
    # the row click may be any order or have duplicate individuals, need to index by name instead of index
    selected_tele_list <- select_data()$tele_list[selected_dt$identity]
    selected_models <- selected_models_dt$model
    selected_vg_list <- vg_list()[selected_dt$identity]
    # must make sure all items in same order
    return(list(dt = selected_dt,
                tele_list = selected_tele_list,
                models = selected_models,
                vg_list = selected_vg_list
                ))
  })
  select_models_layout <- reactive({
    fig_count <- length(select_models()$models)
    row_count <- ceiling(fig_count / input$vario_columns)
    height <- input$vario_height * row_count
    # return(list(layout_matrix = layout_matrix, height = height))
    return(list(row_count = row_count, height = height))
  })
  # p6. home range ----
  callModule(click_help, "home_range", title = "Home Range",
             size = "l", file = "help/6_home_range.md")
  # selected_hrange_list ----
  selected_hrange_list <- reactive({
    withProgress(res <- akde(select_models()$tele_list,
                             CTMM = select_models()$models),
                 message = "Calculating home range ...")
    return(res)
  })
  output$range_summary <- DT::renderDataTable({
    hrange_summary_dt <- model_list_dt_to_model_summary_dt(build_hrange_list_dt(
      select_models()$dt, selected_hrange_list()))
    dt <- format_hrange_summary_dt(hrange_summary_dt)
    dt[, model_no := NULL]
    if (!input$show_ci_hrange) {
      dt <- dt[!str_detect(estimate, "CI")]
      dt[, estimate := NULL]
    }
    info_p <- values$data$merged$info
    datatable(dt, options = list(scrollX = TRUE), rownames = FALSE) %>%
      formatStyle('identity', target = 'row',
                  color = styleEqual(info_p$identity,
                                     hue_pal()(nrow(info_p)))
      )
  })
  output$range_plot <- renderPlot({
    selected_tele_list <- select_models()$tele_list
    def.par <- par(no.readonly = TRUE)
    par(mfrow = c(select_models_layout()$row_count, input$vario_columns),
        mar = c(5, 5, 4, 1), ps = 18, cex = 0.72, cex.main = 0.9)
    lapply(seq_along(selected_tele_list), function(i) {
      plot(selected_tele_list[[i]], UD = selected_hrange_list()[[i]])
      title(select_models()$dt[i, paste0(identity, " - ", model_name)])
      # title(sub = "Error on", cex.sub = 0.85, col.sub = "red")
    })
    par(def.par)
  }, height = function() { select_models_layout()$height })
  # p7. occurrence ----
  callModule(click_help, "occurrence", title = "Occurrence Distribution",
             size = "l", file = "help/7_occurrence.md")
  # selected_occurrence() ----
  select_occurrences <- reactive({
    selected_tele_list <- select_models()$tele_list
    ud_para_list <- align_list(selected_tele_list, select_models()$models)
    ud_calc <- function(ud_para_list) {
      occurrence(ud_para_list$a, ud_para_list$b)
    }
    withProgress(print(system.time(selected_occurrences <-
                                     para_ll(ud_para_list, ud_calc))),
                 message = "Calculating Occurrence ...")
    selected_occurrences
  })
  # function on input didn't update, need a reactive expression?
  parse_CI_levels <- reactive({
    if (str_trim(input$ud_level_text) == "") {
      return(NA)
    }
    else {
      items <- str_trim(str_split(input$ud_level_text, ",")[[1]])
      as.numeric(items[items != ""]) / 100
    }
  })
  output$occurrence_plot <- renderPlot({
    # plot
    def.par <- par(no.readonly = TRUE)
    par(mfrow = c(select_models_layout()$row_count, input$vario_columns),
        mar = c(5, 5, 4, 1), ps = 18, cex = 0.72, cex.main = 0.9)
    lapply(seq_along(select_occurrences()), function(i) {
      tryCatch({
        # plot(select_occurrences()[[i]], level.UD = input$ud_level)
        plot(select_occurrences()[[i]], level.UD = parse_CI_levels())
      }, error = function(e) {
        warning(select_models()$dt[i, paste0(identity, " - ", model_name)], ": ", e)
        plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10))
      })
      title(select_models()$dt[i, paste0(identity, " - ", model_name)])
    })
    par(def.par)
  }, height = function() { select_models_layout()$height })
}
