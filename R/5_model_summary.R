# build ctmm model summary table ----
# each model object is a CTMM object, summary(ctmm_obj) give some information, which can be converted into a table. summary(ctmm_obj_list) give some comparison among models, which is additional info.
ctmm_obj_to_summary_dt <- function(model) {
  # convert the named vectors into data table, keep relevant info
  model_summary_list <- lapply(summary(model, units = FALSE), function(item) {
    data.table(t(data.frame(item)), keep.rownames = TRUE)
  })
  # any modification to dof_dt actually changed the parameter. this cause problems in rerun the function second time.
  dof_dt <- copy(model_summary_list[["DOF"]])
  ci_dt <- copy(model_summary_list[["CI"]])
  # need literal treatment by item name because every case is different
  # we need row name of CI, but not dof
  dof_dt[, rn := NULL]
  setnames(dof_dt, names(dof_dt), stringr::str_c("DOF ", names(dof_dt)))
  setnames(ci_dt, "rn", "estimate")
  ci_dt[estimate %in% c("low", "high"), estimate := stringr::str_c("CI ", estimate)]
  # with SI units, we only need to know which type of unit. the actual format need to happen after the whole table is built. so we only need a mapping from col name to proper unit function later.
  setnames(ci_dt, names(ci_dt), stringr::str_replace_all(names(ci_dt), "\\s\\(.*", ""))
  # two part need to bind together, add model name col, then add animal name col in last step. because of row number difference, it's easier to use merge, add common col first.
  dof_dt[, item := 1]
  ci_dt[, item := 1]
  res_dt <- merge(dof_dt, ci_dt, by = "item")
  res_dt[, item := NULL]
}
# para_ll_fit fit each animal with ctmm.fit, generate a model list for each animal, saved in a list of list, named by animal
# this result was converted into a data.table models_dt, with the model objects as a list column, note each list is various models for same animal, a summary on list was used to generate dAICc information. model name is just model type, not the full name with the animal name part. we need the separate model name col for coloring of model summary table.
model_fit_res_to_model_list_dt <- function(model_fit_res) {
  animal_names_dt <- data.table(identity = names(model_fit_res))
  model_type_list <- lapply(model_fit_res, names)
  # must use per row by to create list column, otherwise dt try to apply whole column to function
  animal_names_dt[, model_type_list :=
                    list(list(model_type_list[[identity]])),
                  by = 1:nrow(animal_names_dt)]
  models_dt <- animal_names_dt[, .(model_type = unlist(model_type_list)),
                               by = identity]
  models_dt[, model := list(list(model_fit_res[[identity]][[model_type]])),
            by = 1:nrow(models_dt)]
  models_dt[, model_no := .I]
  # also add the AICc col
  get_aicc_col <- function(model_list) {
    res <- summary(model_list, units = FALSE)
    data.frame(res)$dAICc
  }
  models_dt[, dAICc := get_aicc_col(model), by = identity]
  # add model name col so it can be used to create model color palette
  models_dt[, model_name := stringr::str_c(identity, " - ", model_type)]
}
# generate summary table for models. home range don't have dAICc column
model_list_dt_to_model_summary_dt <- function(models_dt, hrange = FALSE) {
  # make copy first because we will remove column later
  # a list of converted summary on each model
  model_summary_dt_list <- lapply(1:nrow(models_dt), function(i) {
    summary_dt <- ctmm_obj_to_summary_dt(models_dt$model[[i]])
    summary_dt[, model_no := i]
  })
  model_summary_dt <- rbindlist(model_summary_dt_list, fill = TRUE)
  # home range result also used this function, but there is no dAICc column from summary of list of home range.
  if (hrange) {
    res_dt <- merge(models_dt[, .(identity, model_type, model_name, model_no)],
                    model_summary_dt,
                    by = "model_no")
  } else {
    res_dt <- merge(models_dt[, .(identity, model_type, model_name, model_no,
                                  dAICc)],
                    model_summary_dt,
                    by = "model_no")
  }
  # res_dt[, color_target := stringr::str_c(identity, " - " , estimate)]
}
# apply units format functions list to columns
apply_format_f_list <- function(dt, format_f_list) {
  # it's easier to use a for loop since we can use i. with lapply and .SD we don't have col name available
  for (i in seq_along(format_f_list)) {
    # tried to use identity for cols don't need change, but we cannot update existing cols because col type changed
    if (!is.null(format_f_list[[i]])) {
      # the data table in shiny printed too many digits.
      dt[, paste0(names(dt)[i], "_units") := format_f_list[[i]](dt[[names(dt)[i]]])]
    }
  }
  new_cols <- names(dt)[stringr::str_detect(names(dt), "_units")]
  old_cols <- stringr::str_replace_all(new_cols, "_units", "")
  dt[, (old_cols) := NULL]
  setnames(dt, new_cols, old_cols)
}
# the model summary table need to be formatted for units
format_model_summary_dt <- function(model_summary_dt) {
  # data.table modify reference, use copy so we can rerun same line again
  dt <- copy(model_summary_dt)
  # speed is m/day, need manual adjust before ctmm update on this
  # dt[, speed := speed / (24 * 3600)]
  # round up dof mean, area
  dt[, `DOF mean` := round(`DOF mean`, 3)]
  dt[, `DOF area` := round(`DOF area`, 3)]
  dt[, dAICc := round(dAICc, 3)]
  format_f_list <- lapply(names(dt), function(col_name) {
    switch(col_name,
           area = format_area_f(dt[[col_name]], round = TRUE),
           `tau position` = format_seconds_f(dt[[col_name]], round = TRUE),
           `tau velocity` = format_seconds_f(dt[[col_name]], round = TRUE),
           speed = format_speed_f(dt[[col_name]], round = TRUE),
           error = format_distance_f(dt[[col_name]], round = TRUE)
    )
  })
  # not really used, but easier to debug
  names(format_f_list) <- names(dt)
  res_dt <- apply_format_f_list(dt, format_f_list)
  # NA cells should have units removed or just empty values
  res_dt[stringr::str_detect(`tau velocity`, "^NA "),
         c("tau velocity", "speed") := ""]
  res_dt[stringr::str_detect(estimate, "CI"),
         c("DOF mean", "DOF area") := NA_real_]
}
# combined steps to make usage easier, otherwise the function name could be confusing
# Generate Formated Model Summary Table From Model List Table
#
# models_dt a `data.table` holding model information and models objects
#   as list column
#
# return formated model summary table
model_list_dt_to_formated_model_summary_dt <- function(models_dt) {
  model_summary_dt <- model_list_dt_to_model_summary_dt(models_dt,
                                                        hrange = FALSE)
  format_model_summary_dt(model_summary_dt)
}
# exported version, make the interface simpler. our internal version need intermediate steps because we need the intermediate data

#' Generate formated model summary table from model fit results
#'
#' @param model_fit_res list of applying `ctmm::ctmm.select` on telemetry objects
#'
#' @return A `data.table` of model summary
#' @export
summary_model_fit <- function(model_fit_res) {
  models_dt <- model_fit_res_to_model_list_dt(model_fit_res)
  # use [] to make sure calling function directly will print in console.
  model_list_dt_to_formated_model_summary_dt(models_dt)[]
}
#' Convert nested [par_fit_tele] result into flatten list with model names
#'
#' @param model_fit_res
#'
#' @return A single level list of models with names
#' @export
list_model_fit <- function(model_fit_res) {
  models_dt <- model_fit_res_to_model_list_dt(model_fit_res)
  model_list <- models_dt$model
  names(model_list) <- models_dt$model_name
  return(model_list)
}
# it requires more manual code to assemble a table for home range, temporarily not exporting these function untill requested
# Build Home Range list table
#
# The table structure is similar to model list table, with model information
# from model summary table, and home range objects as list column
#
# return a data.table holding model info and home range
# export
#
build_hrange_list_dt <- function(selected_model_names_dt, selected_hrange_list) {
  dt <- copy(selected_model_names_dt)
  dt[, model := list(selected_hrange_list)]
  dt[, model_no := .I]
}
format_hrange_summary_dt <- function(hrange_summary_dt) {
  # data.table modify reference, use copy so we can rerun same line again
  dt <- copy(hrange_summary_dt)
  dt[, `DOF area` := round(`DOF area`, 3)]
  dt[, `DOF bandwidth` := round(`DOF bandwidth`, 3)]
  format_f_list <- lapply(names(dt), function(col_name) {
    switch(col_name,
           area = format_area_f(dt[[col_name]], round = TRUE)
    )
  })
  # not really used, but easier to debug
  # names(format_f_list) <- names(dt)
  res_dt <- apply_format_f_list(dt, format_f_list)
  res_dt[stringr::str_detect(estimate, "CI"),
         c("DOF area", "DOF bandwidth") := NA_real_]
}
# Generate Formated Home Range Summary Table From Home Range List Table
#
# param hrange_list_dt a data.table holding model info and home range objects
#
# return formated home range summary table
hrange_list_dt_to_formated_range_summary_dt <- function(hrange_list_dt) {
  hrange_summary_dt <- model_list_dt_to_model_summary_dt(hrange_list_dt,
                                                         hrange = TRUE)
  format_hrange_summary_dt(hrange_summary_dt)
}
# it's difficult to get a home range summary table function, because we reused same summary function and need a model table, which is borrowed from model table. so we have to reuse same model table in home range summary. From user's perspective we can use selected model list, and go a long way inside function to get the table.
# rebuild model_fit_res from selected_model_list? then build summary. too much hassles. ask user to use regular summary?

# summary_home_range <- function(hrange_list) {
#   hrange_list_dt <- build_hrange_list_dt(names(hrange_list),
#                                          hrange_list)
#   dt <- ctmmweb:::hrange_list_dt_to_formated_range_summary_dt(hrange_list_dt)
#   models_dt <- model_fit_res_to_model_list_dt(model_fit_res)
#   # use [] to make sure calling function directly will print in console.
#   model_list_dt_to_formated_model_summary_dt(models_dt)[]
# }
