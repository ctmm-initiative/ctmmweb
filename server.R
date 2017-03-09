# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)
# all helper functions are for server side
source("helpers.R", local = TRUE)
# some variables need to be visible across different functions. modify with <<-
# valid_studies <- NULL
# move_bank_dt <- NULL
server <- function(input, output, session) {
  # p1. import ----
  # values got updated in observeEvent need this format.
  values <- reactiveValues()
  # all reference of this value should wrap req around it: req(values$input_data)
  values$input_data <- NULL
  # 1.1 local data ----
  data_import <- function(data) {
    note_import <- showNotification(span(icon("spinner fa-spin"), "Importing data..."), type = "message", duration = NULL)
    on.exit(removeNotification(note_import))
    values$input_data <- tryCatch(as.telemetry(data),
                                  error = function(e) {
                                    showNotification("Import error, check data again",
                                                     duration = 4, type = "error")
                                  })
  }
  # clicking browse button without changing radio button should also update
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
    if (input$load_option == "ctmm") {
      data("buffalo")
      values$input_data <- buffalo
      updateTabItems(session, "tabs", "plots")
    } else if (input$load_option == "upload") {
      # need to check NULL input from source, stop error in downstream
      req(input$file1)
      file_uploaded()
    }
  })
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
  observeEvent(input$login_help, {
    showModal(modalDialog(
      title = "Movebank Login", size = "l",
      fluidPage(includeMarkdown("help/movebank_login.md")),
      easyClose = TRUE
    ))
  })
  # 1.3 movebank studies ----
  # 1.3, 1.4, 1.5 are linked. Each content for rendering should be reactive but passive updated by observeEvent. Each action should check whether all other content need to be updated. with reactive we only need to update the variable, not really update rendering manually.
  # all studies box
  values$all_studies_stat <- NULL
  output$all_studies_stat <- renderText(req(values$all_studies_stat))
  values$studies <- NULL
  output$studies <- DT::renderDataTable(datatable(req(values$studies),
                                                  rownames = FALSE,
                                                  options = list(pageLength = 5),
                                                  selection = 'single'
  ))
  # selected data box
  values$study_detail <- NULL
  output$study_detail <- DT::renderDataTable(datatable(req(values$study_detail),
                                                       rownames = FALSE,
                                                       options = list(pageLength = 5),
                                                       selection = 'none'))
  # data preview box
  values$study_data_response <- NULL
  output$study_data_response <- renderText(req(values$study_data_response))
  values$study_preview <- NULL
  output$study_preview <- DT::renderDataTable(datatable(req(values$study_preview),
                                                        options = list(dom = 't')))
  values$move_bank_dt <- NULL  # the downloaded whole data table, not rendered anywhere
  # the whole data preview box should be cleared with all actions other than download, otherwise it could be confusing when there is a previous download and user made other actions
  clear_mb_download <- function(){
    values$study_data_response <- NULL
    values$study_preview <- NULL
    values$move_bank_dt <- NULL
  }
  observeEvent(input$login, {
    note_studies <- showNotification(span(icon("spinner fa-spin"), "Downloading studies..."), type = "message", duration = NULL)
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
      # all_studies[, i_am_owner := ifelse(i_am_owner == "true", TRUE, FALSE)]
      all_studies[, i_can_see_data := ifelse(i_can_see_data == "true", TRUE, FALSE)]
      # update the global variable with <<-
      valid_studies <- all_studies[(i_can_see_data)]
      new_names <- sub(".*_", "", studies_cols)
      setnames(valid_studies, studies_cols, new_names)
      setkey(valid_studies, name)
      selected_studies_cols <- c("id", "name",
                         "deployments", "events",
                         "individuals")
      values$studies <- valid_studies[, ..selected_studies_cols]
      # output$studies <- DT::renderDataTable(datatable(,
      #                                                 rownames = FALSE,
      #                                                 options = list(pageLength = 5),
      #                                                 selection = 'single'
      # ))
      values$all_studies_stat <- paste0("Total Studies Found: ", all_studies[, .N],
                                        "; Studies you can see data: ", values$studies[, .N])
      # output$all_studies_stat <- renderPrint({
      #   cat("Total Studies Found: ", studies[, .N],
      #       "; Studies you can see data: ", valid_studies[, .N])
      # })
      values$study_detail <- NULL
      clear_mb_download()
    }
  })
  # 1.4 selected details ----
  observeEvent(input$studies_rows_selected, {
    mb_id <- values$studies[input$studies_rows_selected, id]
    res <- get_study_detail(mb_id, input$user, input$pass)
    # It's easier to specify cols here to drop some cols and reorder cols at the same time
    detail_cols <- c("id", "name", "study_objective", "study_type", "license_terms", "principal_investigator_name", "principal_investigator_address", "principal_investigator_email", "timestamp_start", "timestamp_end", "bounding_box", "location_description", "main_location_lat", "main_location_long", "number_of_tags", "acknowledgements", "citation", "comments", "grants_used", "there_are_data_which_i_cannot_see")
    detail_dt <- try(fread(res$res_cont, select = detail_cols))
    req("data.table" %in% class(detail_dt))
    # need to check content in case something wrong and code below generate error on empty table
    # never had error here because the mb_id came from table itself. so no extra clear up boxes
    if (detail_dt[, .N] == 0) {
      showNotification("No study information downloaded", duration = 2, type = "error")
    } else{
      # exclude empty columns (value of NA)
      valid_cols <- names(detail_dt)[colSums(!is.na(detail_dt)) != 0]
      #  show table as rows. will have some warning of coercing different column types, ignored.
      detail_rows <- suppressWarnings(melt(detail_dt, id.vars = "id", na.rm = TRUE))
      detail_rows[, id := NULL]
      values$study_detail <- detail_rows
      # output$study_detail <- DT::renderDataTable(datatable(detail_rows,
      #                                                      rownames = FALSE,
      #                                                      options = list(pageLength = 5),
      #                                                      selection = 'none'))
      # any selection in studies table should clear downloaded data table
      # output$study_data_response <- renderText("")
      # output$study_preview <- DT::renderDataTable(NULL)
      # move_bank_dt <<- move_bank_dt[0]
      clear_mb_download()
    }
  })
  # 1.4 download data ----
  observeEvent(input$download, {
    req(input$studies_rows_selected)
    mb_id <- valid_studies[input$studies_rows_selected, id]
    note_data_download <- showNotification(span(icon("spinner fa-spin"), "Downloading data..."),
                                           type = "message", duration = NULL)
    # always take current form value
    res <- get_study_data(mb_id, input$user, input$pass)
    removeNotification(note_data_download)
    # need to check response content to determine result type. the status is always success
    # read first rows to determine if download is successful. fread will guess sep so still can read html for certain degree, specify `,` will prevent this
    # sometimes the result is one line "<p>No data are available for download.</p>". fread and read.csv will take one line string as file name thus cannot find the input file, generate some warnings. To use string as input need at least one "\n". Adding "\n" will solve this error but get valid dt with 0 row, also we cannot use the nrows parameters.
    movebank_dt_preview <- try(fread(res$res_cont, sep = ",", nrows = 5))
    # the fread in ctmm can use == directly because it was reading in df only, only one class attributes. Here we need to use %in% instead
    if (!("data.table" %in% class(movebank_dt_preview))) {
      showNotification("No data available for download", type = "warning", duration = 1.5)
      msg <- html_to_text(res$res_cont)
      output$study_data_response <- renderText(paste0(msg, collapse = "\n"))
      # data downloaded can be csv or html reponse, movebank_dt_preview can be dt or `try-error`, so we cannot just render movebank_dt_preview and use reactive values, need to clear the preview table and data dt manually. The logic is simple so this is still acceptable.
      output$study_preview <- DT::renderDataTable(NULL)
      # clear previous downloaded valid data
      move_bank_dt <<- move_bank_dt[0]
    } else {
      showNotification("Data downloaded", type = "message", duration = 2)
      note_parse <- showNotification(span(icon("spinner fa-spin"),
                                                   "Parsing csv..."),
                                              type = "message", duration = NULL)
      move_bank_dt <<- try(fread(res$res_cont, sep = ","))
      removeNotification(note_parse)
      row_count <- formatC(move_bank_dt[, .N], format = "d", big.mark = ",")
      individual_count <- length(unique(move_bank_dt[, individual_id]))
      output$study_data_response <- renderText(paste0(
        "Data downloaded with ", row_count, " rows, ", individual_count, " individuals. ",
        "Preview:"))
      output$study_preview <- DT::renderDataTable(datatable(movebank_dt_preview,
                                                            options = list(dom = 't')))
    }
  })
  observeEvent(input$download_help, {
    showModal(modalDialog(
      title = "Downloading Movebank data", size = "l",
      fluidPage(includeMarkdown("help/movebank_download.md")),
      easyClose = TRUE
    ))
  })
  # 1.5 save, import data ----
  output$save <- downloadHandler(
    filename = function() {
                  mb_id <- valid_studies[input$studies_rows_selected, id]
                  # avoid special characters that invalid for file name
                  study_name <- gsub('[^\\w]', ' ',
                                     valid_studies[input$studies_rows_selected, name],
                                     perl = TRUE)
                  paste0("Movebank ", mb_id, " - ", study_name, ".csv") },
    content = function(file) {
      req(move_bank_dt[, .N] > 0)
      fwrite(move_bank_dt, file)
    }
  )
  observeEvent(input$import, {
    req(move_bank_dt[, .N] > 0)
    data_import(move_bank_dt)
    updateTabItems(session, "tabs", "plots")
  })
  # p2. plots ----
  # merge obj list into data frame with identity column, easier for ggplot and summary
  merge_data <- reactive({
    req(values$input_data)
    # note_updating <- showNotification(span(icon("spinner fa-spin"), "Updating data..."), type = "message", duration = NULL)
    # on.exit(removeNotification(note_updating))
    merge_animals(values$input_data)
  })
  # 2.3 data summary ----
  output$data_summary <- DT::renderDataTable({
    info <- merge_data()$info_print
    datatable(info) %>%
      formatStyle('Identity', target = 'row',
                  color =
                    styleEqual(info$Identity,
                               hue_pal()(nrow(info)))
      )}
  )
  # 2.4.4 location basic plot
  output$location_plot_basic <- renderPlot({
    tele_objs <- req(values$input_data)
    plot(tele_objs, col = rainbow(length(tele_objs)))
  })
  # selected ids and color ----
  select_animal <- reactive({
    id_vec <- merge_data()$info_print[, Identity]
    color_vec <- hue_pal()(length(id_vec))
    # table can be sorted, but always return row number in column 1
    selected_ids <- id_vec[input$data_summary_rows_selected]
    selected_colors <- color_vec[id_vec %in% selected_ids]
    if (length(selected_ids) == 0) {
      # select all when there is no selection
      return(list(ids = id_vec, colors = color_vec))
    } else {
      return(list(ids = selected_ids, colors = selected_colors))
    }
  })
  # single selected ----
  values$selected_animal_no <- 1
  observeEvent(input$selected, {
    if (length(input$data_summary_rows_selected) != 1) {
      showNotification("Please select single Animal.", type = "error")
    } else {
      values$selected_animal_no <- input$data_summary_rows_selected
      updateTabItems(session, "tabs", "subset")
    }
  })
  # 1.4.1 overview plot ----
  values$ranges <- c(x = NULL, y = NULL)
  observeEvent(input$overview_dblclick, {
    brush <- input$overview_brush
    if (!is.null(brush)) {
      values$ranges$x <- c(brush$xmin, brush$xmax)
      values$ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      values$ranges$x <- NULL
      values$ranges$y <- NULL
    }
  })
  output$location_plot_gg <- renderPlot({
    animals <- merge_data()$data
    ggplot(data = animals, aes(x, y)) +
      geom_point(size = 0.1, alpha = 0.6, colour = "gray") +
      geom_point(size = 0.1, alpha = 0.7, data = animals[identity %in% select_animal()$ids], aes(colour = id)) +
      coord_fixed(xlim = values$ranges$x, ylim = values$ranges$y) +
      scale_color_manual(values = select_animal()$colors) +
      labs(x = "x (meters)", y = "y (meters)") +
      theme(legend.position = "top",
            legend.direction = "horizontal") +
      bigger_theme + bigger_key
  }, height = height_plot_loc, width = "auto")
  # 1.4.2 facet ----
  output$location_plot_facet_fixed <- renderPlot({
    animals <- merge_data()$data
    ggplot(data = animals, aes(x, y)) +
      geom_point(size = 0.1, alpha = 1/3, data = animals, aes(colour = id)) +
      labs(x = "x (meters)", y = "y (meters)") +
      facet_grid(id ~ .) +
      coord_fixed() +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
  }, height = height_plot_loc, width = "auto")
  # 1.4.3 individuals ----
  # TODO fix bug for single animal input.
  output$location_plot_individual <- renderPlot({
    merged <- merge_data()
    animals <- merged$data
    new_ranges <- get_ranges_quantile(req(values$input_data), animals, input$include_level)
    id_vector <- merged$info_print$Identity
    color_vec <- hue_pal()(length(id_vector))
    g_list <- vector("list", length = length(id_vector))
    for (i in seq_along(id_vector)) {
      data_i <- animals[identity == id_vector[i]]
      new_ranges_i <- new_ranges[identity == id_vector[i]]
      g_list[[i]] <- ggplot(data = data_i, aes(x, y)) +
        geom_point(size = 0.1, alpha = 1/3, color = color_vec[i]) +
        labs(title = id_vector[i], x = "x (meters)", y = "y (meters)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        # coord_fixed(xlim = zoom_in_range(new_ranges_i$x_start,
        #                                  new_ranges_i$x_end,
        #                                  input$zoom_ratio),
        #             ylim = zoom_in_range(new_ranges_i$y_start,
        #                                  new_ranges_i$y_end,
        #                                  input$zoom_ratio),
        #             expand = FALSE)
        coord_fixed(xlim = c(new_ranges_i$x_start, new_ranges_i$x_end),
                    ylim = c(new_ranges_i$y_start, new_ranges_i$y_end),
                    expand = FALSE)
      # no bigger theme and key here since no key involved. bigger theme could mess up the axis labels too.
    }
    grid.arrange(grobs = g_list)
  }, height = height_plot_3, width = "auto")
  # 1.5 histogram facet ----
  output$histogram_facet <- renderPlot({
    animals <- merge_data()$data
    ggplot(data = animals, aes(x = timestamp, fill = id)) +
      geom_histogram(bins = 60) +
      facet_grid(id ~ .) +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
  }, height = height_hist, width = "auto")
  # p2. subset ----
  # actually should not color by page 1 color because we will rainbow color by time
  output$selected_summary <- DT::renderDataTable({
    info <- merge_data()$info_print
    dt <- info[values$selected_animal_no]
    datatable(dt, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  }
  )
  # selected animal data and color bins
  # when putting brush in same reactive value, every brush selection updated the whole value which update the histogram then reset brush.
  color_bin_animal <- reactive({
    bin_count <- input$bin_count
    merged <- merge_data()
    animals <- merged$data
    id_vector <- merged$info_print$Identity
    color_vec <- hue_pal()(bin_count)
    data_i <- animals[identity == id_vector[values$selected_animal_no]]
    data_i[, color_bin_factor := cut(timestamp, bin_count)]
    color_bin_start_vec_time <- ymd_hms(levels(data_i$color_bin_factor))
    color_bin_breaks <- c(color_bin_start_vec_time, data_i[t == max(t), timestamp])
    return(list(data = data_i,
                color_vec = color_vec,
                color_bin_start_vec_time = color_bin_start_vec_time,
                color_bin_breaks = color_bin_breaks))
  })
  # 2.1 histogram subsetting ----
  output$histogram_subsetting <- renderPlot({
    animal_binned <- color_bin_animal()
    ggplot(data = animal_binned$data, aes(x = timestamp)) +
      geom_histogram(breaks = as.numeric(animal_binned$color_bin_breaks), fill = animal_binned$color_vec) +
      scale_x_datetime(breaks = animal_binned$color_bin_breaks,
                       labels = date_format("%Y-%m-%d %H:%M:%S")) +
      ggtitle(animal_binned$data[1, identity]) +
      center_title +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # brush selection and matching color bins
  select_time_range <- reactive({
    animal_binned <- color_bin_animal()
    if (is.null(input$histo_sub_brush)) {
      select_start <- animal_binned$data[t == min(t), timestamp]
      select_end <- animal_binned$data[t == max(t), timestamp]
    } else {
      # brush value in seconds
      select_start <- as_datetime(input$histo_sub_brush$xmin)
      select_end <- as_datetime(input$histo_sub_brush$xmax)
    }
    select_length <- select_end - select_start
    select_start_bin <- findInterval(select_start, animal_binned$color_bin_start_vec_time)
    select_end_bin <- findInterval(select_end, animal_binned$color_bin_start_vec_time)
    selected_color <- animal_binned$color_vec[select_start_bin:select_end_bin]
    return(list(select_start = select_start, select_end = select_end,
                select_length = select_length,
                selected_color = selected_color))
  })
  # 2.2 current range ----
  output$current_range <- DT::renderDataTable({
    dt <- data.frame(start = select_time_range()$select_start,
                     end = select_time_range()$select_end,
                     length = select_time_range()$select_length)
    datatable(dt, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
      formatStyle(1, target = 'row', color = "#00c0ef")
  })
  # 2.3 selected locations ----
  output$selected_loc <- renderPlot({
    animal_binned <- color_bin_animal()
    time_range <- select_time_range()
    ggplot(data = animal_binned$data, aes(x, y)) +
      geom_point(size = 0.01, alpha = 0.5, colour = "gray") +
      geom_point(size = 0.01, alpha = 0.9,
                 data = animal_binned$data[timestamp >= time_range$select_start &
                                             timestamp <= time_range$select_end],
                 aes(colour = color_bin_factor)) +
      scale_colour_manual(values = time_range$selected_color) +
      labs(x = "x (meters)", y = "y (meters)") +
      coord_fixed() +
      theme(legend.position = "top",
            legend.direction = "horizontal") +
      bigger_key
  })
  # 2.4 time range table ----
  empty_ranges <- data.frame(start = NULL, end = NULL, length = NULL)
  values$selected_time_ranges <- empty_ranges
  observeEvent(input$add_time, {
    l <- list(values$selected_time_ranges,
              data.frame(start = select_time_range()$select_start,
                         end = select_time_range()$select_end,
                         length = select_time_range()$select_length))
    values$selected_time_ranges <- rbindlist(l)
  })
  observeEvent(input$reset, {
    values$selected_time_ranges <- empty_ranges
  })
  # selected_times
  output$selected_ranges <- DT::renderDataTable({
    time_range <- select_time_range()
    # dt <- data.frame(start = time_range$select_start, end = time_range$select_end,
    #            length = time_range$select_length)
    datatable(values$selected_time_ranges, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  # p3. variogram ----
  vg.animal_1 <- reactive({
    animal_1 <- req(values$input_data)
    variogram(animal_1)
  })
  output$vario_plot_1 <- renderPlot({plot(vg.animal_1())})
  output$vario_plot_2 <- renderPlot({plot(vg.animal_1(), fraction = 0.1)})
  output$vario_plot_3 <- renderPlot({plot(vg.animal_1(), fraction = 10 ^ input$zoom, main = sprintf("%2.1f%s", (10 ^ input$zoom) * 100, "% of Total Time-lag" ))})
  # # take snapshot of variogram
  # observeEvent(input$snapBtn, {
  #   btn <- input$snapBtn
  #   insertUI(
  #     selector = '#varioholder',
  #     ## wrap element in a div with id for ease of removal
  #     ui = plotOutput(paste0("vario_plot_", btn))
  #   )})
  # model selection ----
  # right now with all default parameter, no user selection
  selected_model <- reactive({
    # debug
    # if (debug) {
    #   cat(file = stderr(), "fitting models\n")
    # }
    animal_1 <- req(values$input_data)
    guessed <- ctmm.guess(animal_1, interactive = FALSE)
    withProgress(ctmm.select(animal_1, CTMM = guessed),
                 message = "Fitting models to find the best ...")
  })
  # observe({
  #   print("observing")
  #   invisible(selected_model())
  # })
  output$model_summary <- renderPrint({
    fitted.mod <- selected_model()
    # TODO use validate
    if (is.null(fitted.mod))
      cat("No model selected yet")
    else{
      summary(fitted.mod)
    }
  })
  output$model_plot_1 <- renderPlot({
    ouf <- selected_model()
    plot(vg.animal_1(), CTMM = ouf, col.CTMM = "#1b9e77")
  })
  output$model_plot_2 <- renderPlot({
    ouf <- selected_model()
    plot(vg.animal_1(),
         CTMM = ouf,
         col.CTMM = "#1b9e77",
         fraction = 0.1)
  })
  # home range ----
  akde.animal_1 <- reactive({
    # debug
    # if (debug) {
    #   cat(file = stderr(), "akde running\n")
    # }
    animal_1 <- req(values$input_data)
    ouf <- selected_model()
    withProgress(akde(animal_1,CTMM = ouf), message = "Calculating home range ...")
  })
  output$range_summary <- renderPrint({
    akde1 <- akde.animal_1()
    # TODO use validate
    if (is.null(akde1))
      cat("No model selected yet")
    else{
      summary(akde1)
    }
  })
  output$range_plot_basic <- renderPlot({
    plot(req(values$input_data), UD = akde.animal_1())
  })
}
