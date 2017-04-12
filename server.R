# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)
# all helper functions are for server side
source("helpers.R", local = TRUE)
source("cut_divide.R", local = TRUE)

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
    # sometimes there is error: Error in <Anonymous>: unable to find an inherited method for function ‘span’ for signature ‘"shiny.tag"’. added tags$, not sure if it will fix it.
    note_import <- showNotification(shiny::span(icon("spinner fa-spin"), "Importing data..."), type = "message", duration = NULL)
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
  callModule(click_help, "login", title = "Movebank Login", file = "help/movebank_login.md")
  # 1.3 movebank studies ----
  # 1.3, 1.4, 1.5 are linked. Each content for rendering should be reactive but passive updated by observeEvent. Each action should check whether all other content need to be updated. with reactive we only need to update the variable, not really update rendering manually.
  # all studies box
  values$all_studies_stat <- NULL
  output$all_studies_stat <- renderText(req(values$all_studies_stat))
  values$studies <- NULL
  # values$studies hold complete data, only render part of it according to reactive input
  # only show selected cols because we don't want to show owner col. want to keep it insivibly so we can switch it on and off.
  output$studies <- DT::renderDataTable(
    datatable({
      req(values$studies)
      selected_studies_cols <- c("id", "name", "deployments", "events", "individuals")
      values$studies[owner == input$data_manager, selected_studies_cols, with = FALSE]
      },
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
  clear_mb_download <- function(res_msg = NULL){
    values$study_data_response <- res_msg
    values$study_preview <- NULL
    values$move_bank_dt <- NULL
  }
  observeEvent(input$login, {
    note_studies <- showNotification(shiny::span(icon("spinner fa-spin"), "Downloading studies..."), type = "message", duration = NULL)
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
      all_studies[, i_can_see_data := ifelse(i_can_see_data == "true", TRUE, FALSE)]
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
  # deselect row should clear detail table, so added ignoreNULL
  observeEvent(input$studies_rows_selected, ignoreNULL = FALSE, {
    if (length(input$studies_rows_selected) == 0) {
      values$study_detail <- NULL
      clear_mb_download()
    } else {
      mb_id <- values$studies[input$studies_rows_selected, id]
      # link to movebank
      output$open_study <- renderUI({
        req(input$studies_rows_selected)
        shiny::a(tags$button(icon("external-link"), "Open in Movebank",
                             class = "btn btn-default action-button",
                             style = external_link_style),
                 target = "_blank",
                 href = paste0("https://www.movebank.org/movebank/#page=studies,path=study",
                               mb_id))
      })
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
        # any selection in studies table should clear downloaded data table
        clear_mb_download()
      }
    }
  })
  # 1.4 download data ----
  observeEvent(input$download, {
    req(input$studies_rows_selected)
    mb_id <- values$studies[input$studies_rows_selected, id]
    note_data_download <- showNotification(shiny::span(icon("spinner fa-spin"), "Downloading data..."),
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
      showNotification(h4("No data available -- or you need to agree to license term first."), type = "warning", duration = 5)
      msg <- html_to_text(res$res_cont)
      clear_mb_download(paste0(msg, collapse = "\n"))
    } else {
      showNotification("Data downloaded", type = "message", duration = 2)
      note_parse <- showNotification(shiny::span(icon("spinner fa-spin"),
                                                   "Parsing csv..."),
                                              type = "message", duration = NULL)
      move_bank_dt <- try(fread(res$res_cont, sep = ","))
      removeNotification(note_parse)
      row_count <- formatC(move_bank_dt[, .N], format = "d", big.mark = ",")
      individual_count <- length(unique(move_bank_dt[, individual_id]))
      values$study_data_response <- paste0(
          "Data downloaded with ", row_count, " rows, ", individual_count, " individuals. ",
          "Preview:")
      values$study_preview <- movebank_dt_preview
      values$move_bank_dt <- move_bank_dt
    }
  })
  callModule(click_help, "download", title = "Downloading Movebank data",
             file = "help/movebank_download.md")
  # 1.5 save, import data ----
  output$save <- downloadHandler(
    filename = function() {
                  mb_id <- values$studies[input$studies_rows_selected, id]
                  # avoid special characters that invalid for file name
                  study_name <- gsub('[^\\w]', ' ',
                                     values$studies[input$studies_rows_selected, name],
                                     perl = TRUE)
                  paste0("Movebank ", mb_id, " - ", study_name, ".csv") },
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
  # merge_input <- reactive({
  #   req(values$input_data)
  #   # tried notification here, the timing is not accurate. This step usually don't need much time. if it does, the importing step will have notification which shoul serve the purpose.
  #   merge_animals(values$input_data)
  # })
  # input data may have outliers removed then return to visualization page. The outlier removal step is optional and can be repeated. current_data will replace merge_input, which always start from original input, apply filter needed.
  # this came from outliers added to quene. vector of row_no. Compare to the actual rows, it's easier to remove rows by row_no from original data.
  values$outliers_to_remove <- NULL
  # outliers removed will be added here. vector of row_no
  values$outliers_removed <- NULL
  # When removing outliers, always update the outliers_remove vector, then get current from input data - outliers removed. So always start from original, always keep the full list of outliers, every removal only update the removal list. This is to avoid the reactive building on previous reactive value and creating a loop.
  # merge_input get df from tele_objs. after we filtered the df, also need a updated tele_objs version to be used in some cases. this make it more complex. better remove the dependency. which is just the extent function.
  # current_animals() ----
  # merge input into a list of data frame and info frame. also apply the subset or filter.
  current_animals <- reactive({
    req(values$input_data)
    animals <- merge_animals(values$input_data)
    # TODO remove points ----
    # sync telemetry obj with the data frame. some individual could be removed in outlier removal, also return the telemetry obj
    if (!is.null(values$outliers_to_remove)) {

    }
    # distance/speed calculation need to be based on updated data
    # distance
    animals_dt <- animals$data
    animals_dt[, `:=`(median_x = median(x), median_y = median(y)),
               by = identity]
    animals_dt[, distance_center := sqrt((x - median_x) ** 2 +
                                           (y - median_y) ** 2)]
    # speed calculation ----
    # x[i] + inc[i] = x[i+1], note by id
    animals_dt[, `:=`(inc_x = shift(x, 1L, type = "lead") - x,
                      inc_y = shift(y, 1L, type = "lead") - y,
                      inc_t = shift(t, 1L, type = "lead") - t), by = id]
    animals_dt[, speed := sqrt(inc_x ^ 2 + inc_y ^ 2) / inc_t]
    animals_dt[is.infinite(speed), speed := NaN]
    # if last point n is outlier, n-1 will have high speed according to our definition, and n have no speed definition. assign n-1 speed to it. Then we don't need to clean up NA in speed too
    # this removed NaN too. The NA values caused speed outlier plot default subset to NULL. should not keep NA, NaN in speed, will cause too many troubles. could use negative value to mark
    for (i in animals_dt[is.na(speed), which = TRUE]) {
      animals_dt[i, speed := animals_dt[i - 1, speed]]
    }
    # may also need to return updated telemetry obj, for future modeling.
    return(list(data = animals_dt, info = animals$info))
  })
  # 2.3 data summary ----
  output$individuals <- DT::renderDataTable({
    if (input$time_unit == "normal") {
      info_p <- current_animals()$info[,
                      .(identity, start, end, interval, duration)]
    } else {
      info_p <- current_animals()$info[,
                      .(identity, start, end, interval_s, duration_s)]
    }
    datatable(info_p, options = list(pageLength = 6,
                                     lengthMenu = c(2, 4, 6, 8, 10, 20))) %>%
      formatStyle('identity', target = 'row',
                  color = styleEqual(info_p$identity,
                                     hue_pal()(nrow(info_p)))
      )}
  )
  # to outlier filtering ----
  observeEvent(input$outlier, {
    updateTabItems(session, "tabs", "filter")
  })
  # to time subsetting ----
  values$selected_animal_no <- 1
  observeEvent(input$time_subset, {
    # must select single animal to proceed
    if (length(input$individuals_rows_selected) != 1) {
      showNotification("Please select single Animal.", type = "error")
    } else {
      values$selected_animal_no <- input$individuals_rows_selected
      updateTabItems(session, "tabs", "subset")
    }
  })
  # 2.4.4 location basic plot
  # this is the only place that take original input data directly.
  output$location_plot_basic <- renderPlot({
    tele_objs <- req(values$input_data)
    plot(tele_objs, col = rainbow(length(tele_objs)))
  })
  # chose_animal() ----
  # when there are lots of animals, the color gradient is subtle for neighbors.
  chose_animal <- reactive({
    # need to wait the individual summary table initialization finish. otherwise the varible will be NULl and data will be an empty data.table but not NULL, sampling time histogram will have empty data input.
    req(input$individuals_rows_current)
    id_vec <- current_animals()$info[, identity]
    # color_vec <- hue_pal()(length(id_vec))
    # table can be sorted, but always return row number in column 1
    if (length(input$individuals_rows_selected) == 0) {
      # select all in current page when there is no selection
      chosen_row_nos <- input$individuals_rows_current
    } else {
      chosen_row_nos <- input$individuals_rows_selected
    }
    chosen_ids <- id_vec[chosen_row_nos]
    # chosen_colors <- color_vec[id_vec %in% chosen_ids]
    # we always use id to get data subset, so return data directly. range_quantile need tele obj, which need id row number to subset. include it here because the data may actually have subset, outlier removal applied, we cannot take from input directly anymore.
    return(list(data = current_animals()$data[identity %in% chosen_ids],
                info = current_animals()$info[identity %in% chosen_ids]
                # tele_objs = values$input_data[chosen_row_nos],
                # row_nos = chosen_row_nos,
                # colors = chosen_colors
                ))
  })
  # 2.4.1 overview plot ----
  # to add zoom in for a non-arranged plot, seem more in add_zoom.R and google group discussion
  # 1. add event id in ui, always use same naming pattern with plotid. we can use global variables to record the naming pattern, but that's extra complexity for simple things.
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
    animals_dt <- req(chose_animal()$data)
    ggplot() +
      {if (input$overlay_all) {
        geom_point(data = current_animals()$data, aes(x, y),
                   size = input$point_size_1, alpha = 0.6, colour = "gray")
      }} +
      geom_point(data = animals_dt, aes(x, y, colour = id),
                 size = input$point_size_1, alpha = 0.7) +
      coord_fixed(xlim = location_plot_gg_range$x,
                  ylim = location_plot_gg_range$y) +
      # scale_color_manual(values = chose_animal()$colors) +
      factor_color(animals_dt$id) +
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      theme(legend.position = "top",
            legend.direction = "horizontal") +
      bigger_theme + bigger_key
  }
  , height = styles$height_plot_loc, width = "auto"
  )
  # 2.4.2 facet ----
  output$location_plot_facet_fixed <- renderPlot({
    # by convention animals_dt mean the data frame, sometimes still need some other items from list, use full expression
    animals_dt <- req(chose_animal()$data)
    ggplot(data = animals_dt, aes(x, y)) +
      geom_point(size = 0.1, aes(colour = id)) +
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      # scale_color_manual(values = chose_animal()$colors) +
      factor_color(animals_dt$id) +
      facet_grid(id ~ .) +
      coord_fixed() +
      theme(strip.text.y = element_text(size = 12)) +
      bigger_theme + bigger_key
  }, height = styles$height_plot_loc, width = "auto")
  # 2.4.3 individuals ----
  output$location_plot_individual <- renderPlot({
    animals_dt <- req(chose_animal()$data)
    # new_ranges <- get_ranges_quantile(chose_animal()$tele_objs, animals_dt, input$include_level)
    new_ranges <- get_ranges_quantile_dt(animals_dt, input$include_level)
    id_vector <- chose_animal()$info$identity
    color_vec <- chose_animal()$colors
    g_list <- vector("list", length = length(id_vector))
    for (i in seq_along(id_vector)) {
      data_i <- animals_dt[identity == id_vector[i]]
      new_ranges_i <- new_ranges[identity == id_vector[i]]
      g_list[[i]] <- ggplot(data = data_i, aes(x, y, color = id)) +
        # geom_point(size = input$point_size_3, alpha = 0.7,
        #            color = color_vec[i]) +
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
    grid.arrange(grobs = g_list)
  }, height = styles$height_plot_3, width = "auto")
  # 2.5 histogram facet ----
  output$histogram_facet <- renderPlot({
    animals_dt <- req(chose_animal()$data)
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
             file = "help/outlier_distance.md")
  callModule(click_help, "outlier_speed", title = "Outliers in Speed",
             file = "help/outlier_speed.md")
  # p3.a.1 distance histogram ----
  bin_by_distance <- reactive({
    animals_dt <- req(chose_animal()$data)
    return(color_break(input$distance_his_bins, animals_dt,
                       "distance_center", format_distance_f))
  })
  output$distance_histogram <- renderPlot({
    # need to get data from reactive, update by bin count
    distance_binned <- req(bin_by_distance())
    animals_dt <- distance_binned$animals_dt
    ggplot(animals_dt, aes(x = distance_center)) +
      geom_histogram(breaks = distance_binned$color_bin_breaks,
                     fill = hue_pal()(input$distance_his_bins)) +
      # need to exclude 0 count groups
      geom_text(stat = 'bin',aes(label = ifelse(..count.. != 0, ..count.., "")),
                vjust = -1, breaks = distance_binned$color_bin_breaks) +
      scale_x_continuous(breaks = distance_binned$non_empty_breaks,
                         labels = distance_binned$vec_formatter) +
      # all counts above 20 are not shown, so it's easier to see the few outliers.
      coord_cartesian(ylim = c(0, input$distance_his_y_limit)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # brush selection function
  select_range <- function(his_type){
    return(reactive({
      animals_dt <- req(chose_animal()$data)
      brush <- input[[paste0(his_type, "_his_brush")]]
      col_name <- switch(his_type,
                         distance = quote(distance_center),
                         speed = quote(speed))
      unit_formatter <- switch(his_type,
                 distance = format_distance_f(animals_dt[, eval(col_name)]),
                 speed = format_speed_f(animals_dt[, eval(col_name)]))
      if (is.null(brush)) {
        select_start <- 0
        select_end <- max(animals_dt[, eval(col_name)])
      } else {
        select_start <- brush$xmin
        select_end <- brush$xmax
      }
      animal_selected_data <- animals_dt[(eval(col_name) >= select_start) &
                                           (eval(col_name) <= select_end)]
      list(select_start = select_start, select_end = select_end,
                  select_start_p = unit_formatter(select_start),
                  select_end_p = unit_formatter(select_end),
                  animal_selected_data = animal_selected_data)
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
                 alpha = ifelse(is.null(input$distance_his_brush),
                                0.6,
                                input$distance_alpha),
                 aes(colour = distance_center_color_factor)) +
      {if (!is.null(input$points_in_distance_range_rows_selected)) {
        points_selected <- select_distance_range()$animal_selected_data[
          input$points_in_distance_range_rows_selected]
        geom_point(data = points_selected, size = 3.5, alpha = 1,
                   color = "blue", shape = 22)
      }} +
      geom_point(data = unique(animals_dt[, .(median_x, median_y), by = id]),
                 aes(x = median_x, y = median_y), color = "blue", size = 0.8) +
      factor_color(animal_selected_data$distance_center_color_factor) +
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      coord_fixed(xlim = distance_outlier_plot_range$x,
                  ylim = distance_outlier_plot_range$y) +
      theme(legend.position = "top",
            legend.direction = "horizontal") + bigger_key

  })
  # points in selected distance range
  # TODO format table ----
  output$points_in_distance_range <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data.
    req(input$distance_his_brush)
    cols <- c("row_no", "timestamp", "id", "distance_center")
    datatable(select_distance_range()$animal_selected_data[, cols, with = FALSE],
              options = list(pageLength = 6,
                             lengthMenu = c(6, 10, 20),
                             searching = FALSE),
              rownames = FALSE)
  })
  # p3.b.1 speed histogram ----
  bin_by_speed <- reactive({
    animals_dt <- req(chose_animal()$data)
    return(color_break(input$speed_his_bins, animals_dt,
                       "speed", format_speed_f))
  })
  output$speed_histogram <- renderPlot({
    speed_binned <- req(bin_by_speed())
    animals_dt <- speed_binned$animals_dt
    ggplot(animals_dt, aes(x = speed)) +
      geom_histogram(breaks = speed_binned$color_bin_breaks,
                     fill = hue_pal()(input$speed_his_bins)) +
      # need to exclude 0 count groups
      geom_text(stat = 'bin', aes(label = ifelse(..count.. != 0, ..count.., "")),
                vjust = -1, breaks = speed_binned$color_bin_breaks) +
      scale_x_continuous(breaks = speed_binned$non_empty_breaks,
                         labels = speed_binned$vec_formatter) +
      coord_cartesian(ylim = c(0, input$speed_his_y_limit)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  })
  select_speed_range <- select_range("speed")
  # speed outlier plot ----
  speed_outlier_plot_range <- add_zoom("speed_outlier_plot")
  output$speed_outlier_plot <- renderPlot({
    animals_dt <- req(bin_by_speed()$animals_dt)
    animal_selected_data <- select_speed_range()$animal_selected_data
    g <- ggplot(animals_dt, aes(x, y)) +
      geom_point(size = 0.05, alpha = 0.6, colour = "gray") +
      geom_point(data = animal_selected_data,
                 size = ifelse(is.null(input$speed_his_brush),
                               0.2,
                               input$speed_point_size),
                 alpha = ifelse(is.null(input$speed_his_brush),
                                0.6,
                                input$speed_alpha),
                 aes(colour = speed_color_factor)) +
      # selected_geoms +
      factor_color(animal_selected_data$speed_color_factor) +
      scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
      scale_y_continuous(labels = format_distance_f(animals_dt$y)) +
      coord_fixed(xlim = speed_outlier_plot_range$x,
                  ylim = speed_outlier_plot_range$y) +
      theme(legend.position = "top",
            legend.direction = "horizontal") + bigger_key
    # prepare selected points data
    if (!is.null(input$points_in_speed_range_rows_selected)) {
      selected_points <- select_speed_range()$animal_selected_data[
        input$points_in_speed_range_rows_selected]
      # draw rectangle around selected points
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
        animals_dt[, c("xend", "yend") := NULL]
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


    # animals_dt <- req(chose_animal()$data)
    # ggplot(animals_dt, aes(x, y)) +
    #   geom_point(size = 0.5, alpha = 0.6, aes(color = speed)) +
    #   scale_colour_gradient(low = "gray", high = "red") +
    #   coord_fixed(xlim = speed_outlier_plot_range$x,
    #               ylim = speed_outlier_plot_range$y) +
    #   scale_x_continuous(labels = format_distance_f(animals_dt$x)) +
    #   scale_y_continuous(labels = format_distance_f(animals_dt$y))
      # facet_wrap(~ id, ncol = 2) +
  })
  # points without valid speed values
  # TODO format table
  output$points_speed_non_valid <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data.
    animals_dt <- req(current_animals()$data)
    cols <- c("row_no", "timestamp", "id", "speed")
    datatable(animals_dt[is.na(speed), cols, with = FALSE],
              options = list(pageLength = 6,
                             lengthMenu = c(6, 10, 20),
                             searching = FALSE),
              rownames = FALSE)
  })
  # points in selected speed range
  # TODO format table ----
  output$points_in_speed_range <- DT::renderDataTable({
    # only render table when there is a selection. otherwise it will be all data.
    req(input$speed_his_brush)
    cols <- c("row_no", "timestamp", "id", "speed")
    datatable(select_speed_range()$animal_selected_data[, cols, with = FALSE],
              options = list(pageLength = 6,
                             lengthMenu = c(6, 10, 20),
                             searching = FALSE),
              rownames = FALSE)
  })
  # p4. time subset ----
  # actually should not color by page 1 color because we will rainbow color by time
  output$selected_summary <- DT::renderDataTable({
    info <- current_animals()$info
    dt <- info[values$selected_animal_no]
    datatable(dt, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  # selected animal data and color bins
  # when putting brush in same reactive value, every brush selection updated the whole value which update the histogram then reset brush.
  color_bin_animal <- reactive({
    selected_id <- current_animals()$info$identity[values$selected_animal_no]
    data_i <- current_animals()$data[identity == selected_id]
    data_i[, color_bin_start :=
             cut_date_time(timestamp, input$time_color_bins)]  # a factor
    color_bin_start_vec_time <- ymd_hms(levels(data_i$color_bin_start))
    color_bin_breaks <- c(color_bin_start_vec_time,
                                     data_i[t == max(t), timestamp])
    return(list(data = data_i,
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
    return(list(select_start = select_start, select_end = select_end,
                select_start_p = format_datetime(select_start),
                select_end_p = format_datetime(select_end),
                select_length = select_length,
                select_length_p = format_diff_time(select_length)))
  })
  # 4.2 current range ----
  output$current_range <- DT::renderDataTable({
    dt <- data.frame(start = select_time_range()$select_start_p,
                     end = select_time_range()$select_end_p,
                     length = select_time_range()$select_length_p)
    datatable(dt, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
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
  empty_ranges <- data.frame(start = NULL, end = NULL, length = NULL)
  values$selected_time_ranges <- empty_ranges
  observeEvent(input$add_time, {
    l <- list(values$selected_time_ranges,
              data.frame(start = select_time_range()$select_start_p,
                         end = select_time_range()$select_end_p,
                         length = select_time_range()$select_length_p))
    values$selected_time_ranges <- rbindlist(l)
  })
  observeEvent(input$reset, {
    values$selected_time_ranges <- empty_ranges
  })
  # selected_times
  output$selected_ranges <- DT::renderDataTable({
    time_range <- select_time_range()
    datatable(values$selected_time_ranges, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  # p5. variogram ----
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
