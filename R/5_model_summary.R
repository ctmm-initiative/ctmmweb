# build ctmm model summary table ----
# each model object is a CTMM object, summary(ctmm_obj) give some information in a list, which is converted into a table. summary(ctmm_obj_list) give some comparison among models, dAIIc col.
# only convert the summary list as we need more flexibility in summary call
ctmm_summary_to_dt <- function(ctmm_summary) {
  # convert the named vectors into data table, keep relevant info
  model_summary_list <- lapply(ctmm_summary, function(item) {
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
# par_try_tele_guess try multiple models on each animal with ctmm.select, generate a model list for each animal, saved in a list of list, named by animal
# this result was converted into a data.table model_list_dt, with the model objects as a list column, note each list is various models for same animal, a summary on list was used to generate dAICc information. model name is just model type, not the full name with the animal name part. we need the separate model name col for coloring of model summary table.
# when multiple copies of same animal data with different init conditions are tried, the result will be unnamed item with sub item of models named by model type. previously we use animal name as item name when they are unique. This is still the  default option, but we need to provide animal names in a separate vector when duplicate exist, as they cannot be used as item names because of dupliates.
# previously used identity as key, now we need to switch to model_no
model_try_res_to_model_list_dt <- function(model_try_res, animal_names = NULL) {
  if (is.null(animal_names)) animal_names <- names(model_try_res)
  animal_names_dt <- data.table(res_list_index = seq_along(model_try_res),
                                identity = animal_names)
  # each row map to one list item, named by item names and they can be duplicated
  # model_type_list <- lapply(model_try_res, names)
  # must use per row by to create list column, otherwise dt try to apply whole column to function and have error of no such index at level 2. not sure why previous usage worked (identity indexing should not work), and why model_type_list[[res_list_index]] doesn't work, have to use names function call directly
  # animal_names_dt[, model_type_list := list(list(model_type_list[[res_list_index]])),
  #                 by = res_list_index]
  animal_names_dt[, model_type_list :=
                    list(list(names(model_try_res[[res_list_index]]))),
                  by = res_list_index]
  # spread list items
  model_list_dt <- animal_names_dt[, .(identity = identity,
                                       model_type = unlist(model_type_list)),
                                   by = res_list_index]
  model_list_dt[, model_no := .I]
  # previously we used identity to access res list items, now we use item index
  model_list_dt[, model :=
                  list(list(model_try_res[[res_list_index]][[model_type]])),
                by = model_no]
  get_aicc_col <- function(model_list) {
    res <- summary(model_list, units = FALSE)
    data.frame(res)$dAICc
  }
  # AICc come from the summary of a group models, by each list item, for sub items. it's grouped by res_list_index now. previously we can just ready by identity, now need to read it by res_list_index. any other name?
  model_list_dt[, dAICc := get_aicc_col(model), by = res_list_index]
  # need a col that represent each model uniquely so it can be used to create home range color palette, which need to separate for each possible models across animals and model types. It need to be "global" for full table no matter what subset is selected.
  model_list_dt[, model_name := stringr::str_c(model_no, ". ", identity, " - ", model_type)]
  # additional columns needed by app
  # the init condition of this model, to be used for variogram plot. value is empty but the col type need to be right
  model_list_dt[, init_ctmm_base := vector('list', nrow(model_list_dt))]
  # name of the init condition model. guess/modified guess for auto fit, model name for refit.
  model_list_dt[, init_ctmm_base_name := NA_character_]
  # init_ctmm_next to be used as init for next refit
  model_list_dt[, init_ctmm_next := model]
  model_list_dt[, fine_tuned := FALSE]
}
# generate summary table for models. too much difference between model table and home range table, make separate functions
model_list_dt_to_model_summary_dt <- function(model_list_dt) {
  # make copy first because we will remove column later
  # a list of converted summary on each model
  model_summary_dt_list <- lapply(1:nrow(model_list_dt), function(i) {
    summary_dt <- ctmm_summary_to_dt(summary(model_list_dt$model[[i]],
                                             units = FALSE))
    summary_dt[, model_no := i]
  })
  model_summary_dt <- rbindlist(model_summary_dt_list, fill = TRUE)
  # individual usage in command line don't have fit_no, but app need that. previously list column name to pick subset (skip models column), also give a proper order. if we leave col order code to outside, the package usage need more adjustment, and the model summary columns can be dynamic, it's easier just put model info cols in left side. use conditional col subsetting instead
  export_cols <- c("identity", "model_type", "model_name", "model_no", "dAICc")
  # if ("fit_no" %in% names(model_list_dt)) export_cols <- c("fit_no", export_cols)
  # res_dt <- merge(model_list_dt[, .(identity, model_type, model_name, model_no,
  #                               dAICc)],
  #                 model_summary_dt,
  #                 by = "model_no")
  res_dt <- merge(model_list_dt[, ..export_cols], model_summary_dt,
                  by = "model_no")
}
# home range don't have dAICc column, need level.UD for CI areas. with level vec, will return more rows. default usage use single input, then remove the ci number column
hrange_list_dt_to_model_summary_dt <- function(model_list_dt, level.UD = 0.95) {
  # make copy first because we will remove column later
  # a list of converted summary on each model. now we have additional level by level, need to combine first
  model_summary_dt_list <- lapply(1:nrow(model_list_dt), function(i) {
    dt_list <- lapply(level.UD, function(level_value) {
      summary_dt <- ctmm_summary_to_dt(summary(model_list_dt$model[[i]],
                                               units = FALSE,
                                               level.UD = level_value))
      summary_dt[, model_no := i]
      summary_dt[, quantile := level_value * 100]
    })
    rbindlist(dt_list, fill = TRUE)
    # summary_dt <- ctmm_summary_to_dt(summary(model_list_dt$model[[i]],
    #                                          units = FALSE))
    # summary_dt[, model_no := i]
  })
  model_summary_dt <- rbindlist(model_summary_dt_list, fill = TRUE)
  # there is no dAICc column from summary of list of home range.
  res_dt <- merge(model_list_dt[, .(identity, model_type, model_name, model_no)],
                  model_summary_dt,
                  by = "model_no")
  # move level.UD col to after estimate
  setcolorder(res_dt,
              c(names(res_dt)[1:(ncol(res_dt) - 2)], "quantile", "area"))
}
# given 3 values of CI, round them properly, keep 2 significant digit on difference
round_CIs <- function(vec, digits = 2) {
  # if NA in input, need remove. if all NA, there will be warnings.
  # remove negative sign
  minimal_diff <- min(abs(diff(vec)), na.rm = TRUE)
  if (minimal_diff > 1) {
    # don't need to worry digits.
    round(vec, 2)
  } else {
    formated_diff <- format(minimal_diff, digits = 1, scientific = FALSE)
    # if exactly 0, will not match 0.xx pattern
    if (formated_diff == "0") return(vec)
    # get 0.000, -2 to get the count of 0 after decimal point
    zeros <- nchar(stringr::str_extract(formated_diff, "0\\.0*")) - 2
    # need 2 more digits after zeros
    round(vec, zeros + digits)
  }
}
# given a col name -> unit formation function map, format a dt to scale the value, add unit label to col name. For model summary table, CI rows need to be round properly by each model. There are other tables that don't have CI rows and no model_no column. we have two usage for regular tables: data summary, outlier summary, and two usage of model tables here, make the other usage default as they are spreaded.
format_dt_unit <- function(dt, name_unit_list, round_by_model = FALSE) {
  # the col name list have error, which may not exist in some cases
  valid_col_names <- intersect(names(dt), names(name_unit_list))
  lapply(valid_col_names, function(col_name) {
    best_unit <- name_unit_list[[col_name]](dt[[col_name]])
    # creating new cols, delete old later is easier to check result. though that will cause col order changes, since new cols added in end, old cols removed. updating existing col instead
    # when using pick unit do the convert in dt, need to round digits, this was taken care of in format functions
    # dt[, (col_name) := round(dt[[col_name]] / best_unit$scale, 2)]
    # cannot use col_name variable in round_CI call, so use a temp col instead
    dt[, temp := dt[[col_name]] / best_unit$scale]
    if (round_by_model) {
      # could be warnings for NA input
      suppressWarnings(
        # note the individual rows rounded at individual points, but data frame print in R used highest precision. checking subset of dt showing proper digits. DT in app is showing them properly
        dt[, (col_name) := round_CIs(temp), by = model_no]
      )
    } else {
      dt[, (col_name) := round(temp, 2)]
    }
    dt[, temp := NULL]
    # \n will cause the table in work report render messed up in html. sometimes DT render colunmn name with \n as same line anyway.
    setnames(dt, col_name, paste0(col_name, " (", best_unit$name, ")"))
    # dt[, paste0(col_name, "\n(", best_unit$name, ")") :=
    #      round(dt[[col_name]] / best_unit$scale, 2) ]
  })
  return(dt)
  # dt[, (valid_col_names) := NULL]
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
  # NA cells should have units removed or just empty values. apply this before the column names changed with units
  # these columns are numeric now, will have real NA instead of text of NA. not needed
  # dt[stringr::str_detect(`tau velocity`, "^NA "),
  #        c("tau velocity", "speed") := NA_real_]
  # remove the duplicated values in CI rows to reduce cluter
  dt[stringr::str_detect(estimate, "CI"),
         c("dAICc", "DOF mean", "DOF area") := NA_real_]
  # need a list to hold function as element, c have same effect but list is more verbose
  name_unit_list <- list("area" = pick_unit_area,
                         "tau position" = pick_unit_seconds,
                         "tau velocity" = pick_unit_seconds,
                         "speed" = pick_unit_speed,
                         "error" = pick_unit_distance)
  format_dt_unit(dt, name_unit_list, round_by_model = FALSE)
}
# combined steps to make usage easier, otherwise the function name could be confusing
# Generate Formated Model Summary Table From Model List Table
#
# model_list_dt a `data.table` holding model information and models objects
#   as list column
#
# return formated model summary table
model_list_dt_to_formated_model_summary_dt <- function(model_list_dt) {
  model_summary_dt <- model_list_dt_to_model_summary_dt(model_list_dt)
  format_model_summary_dt(model_summary_dt)
}
# exported version, make the interface simpler. our internal version need intermediate steps because we need the intermediate data

#' Generate formated model summary table from tried models results
#'
#' @param model_try_res list of applying `ctmm::ctmm.select` on telemetry objects
#'
#' @return A `data.table` of model summary
#' @export
summary_tried_models <- function(model_try_res) {
  model_list_dt <- model_try_res_to_model_list_dt(model_try_res)
  # use [] to make sure calling function directly will print in console.
  model_list_dt_to_formated_model_summary_dt(model_list_dt)[]
}
#' Flatten model list
#'
#' Convert nested [par_fit_models()] result into flatten list with model names
#'
#' @param model_try_res result from [par_fit_models()]
#'
#' @return A single level list of models with names
#' @export
flatten_models <- function(model_try_res) {
  model_list_dt <- model_try_res_to_model_list_dt(model_try_res)
  model_list <- model_list_dt$model
  names(model_list) <- model_list_dt$model_name
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
  dt[stringr::str_detect(estimate, "CI"),
     c("DOF area", "DOF bandwidth") := NA_real_]
  name_unit_list <- list("area" = pick_unit_area)
  format_dt_unit(dt, name_unit_list, round_by_model = FALSE)
}
# Generate Formated Home Range Summary Table From Home Range List Table
#
# param hrange_list_dt a data.table holding model info and home range objects
#
# return formated home range summary table
hrange_list_dt_to_formated_range_summary_dt <- function(hrange_list_dt,
                                                        level.UD = 0.95) {
  hrange_summary_dt <- hrange_list_dt_to_model_summary_dt(hrange_list_dt,
                                                          level.UD)
  format_hrange_summary_dt(hrange_summary_dt)
}
# it's difficult to get a home range summary table function, because we reused same summary function and need a model name table, which is borrowed from model summary table, see build_hrange_list_dt. unless we put model name in hrange_list names, get id names, now they are combined. only build when needed. From user's perspective we can use selected model list, and go a long way inside function to get the table.
# rebuild model_try_res from selected_model_list? then build summary. too much hassles. ask user to use regular summary?

# summary_home_range <- function(hrange_list) {
#   hrange_list_dt <- build_hrange_list_dt(names(hrange_list),
#                                          hrange_list)
#   dt <- ctmmweb:::hrange_list_dt_to_formated_range_summary_dt(hrange_list_dt)
#   model_list_dt <- model_try_res_to_model_list_dt(model_try_res)
#   # use [] to make sure calling function directly will print in console.
#   model_list_dt_to_formated_model_summary_dt(model_list_dt)[]
# }

# convert ctmm::overlap result matrix into data.table ----

#' Convert overlap result matrix to data.table
#'
#' @param mat_3d the 3d matrix result from [ctmm::overlap()]
#' @param clear_half whether to clear the lower triangular part, including the
#'   diagonal
#'
#' @return a `data.table` of overlap results
#' @export
overlap_matrix_to_dt <- function(mat_3d, clear_half = TRUE) {
  matrix_to_dt <- function(mat, estimate_level, clear_half = TRUE) {
    if (clear_half) {
      # clear lower triangular part
      mat[lower.tri(mat, diag = TRUE)] <- NA
    }
    # rownames need to kept explicitly, and it become a column.
    matrix_dt <- data.table(mat, keep.rownames = TRUE)
    matrix_dt[, estimate := estimate_level]
  }
  # need the data.table of full data, for overview table. 3 versions in columns can only work by tags which is not reliable. add another column of low/ML/high and save 3 version in rows, just like the model summary table
  overlap_matrix_dt <- rbindlist(list(
    matrix_to_dt(mat_3d[ , , 1], "CI low", clear_half),
    matrix_to_dt(mat_3d[ , , 2], "ML", clear_half),
    matrix_to_dt(mat_3d[ , , 3], "CI high", clear_half)))
  setorder(overlap_matrix_dt, "rn")
  setnames(overlap_matrix_dt, "rn", "home_range")
  # move estimate col to 2nd
  col_count <- ncol(overlap_matrix_dt)
  setcolorder(overlap_matrix_dt, c(1, col_count, 2:(col_count - 1)))
}
# convert 2d matrix table to 1d row table

#' Convert overlap result 2d table to combination rows
#'
#' @param overlap_matrix_dt 2d data.table from [overlap_matrix_to_dt] applied on
#'   overlap matrix
#'
#' @return a data.table with overlap pair combinations in rows
#' @export
overlap_2d_to_1d <- function(overlap_matrix_dt) {
  # COPY from overlap.Rmd - plot point range --
  # rows format instead of 2d table. need to avoid factor so we can compare string
  overlap_rows_dt <- melt(overlap_matrix_dt,
                          id.vars = c("home_range", "estimate"),
                          variable.factor = FALSE, na.rm = TRUE)
  # < removes both duplicate combination and same animal combination, also make sure the combination is sorted.
  overlap_rows_dt_unique <- overlap_rows_dt[home_range < variable,
                                            .(v1 = home_range, v2 = variable,
                                              estimate, overlap = value)]
  # ggplot need the low/ML/high value in columns, now it's not totaly tidy
  overlap_dt <- dcast(overlap_rows_dt_unique, ... ~ estimate,
                      value.var = "overlap")
  setcolorder(overlap_dt, c("v1", "v2", "CI low", "ML", "CI high"))
  overlap_dt[, Combination := paste(v1, v2, sep = " / ")]
  # COPY end --
  # the right side need to be a list to be assigned to multiple columns. need as.list to convert a vector into separate list items.
  overlap_dt[, c("CI low", "ML", "CI high") :=
               as.list(round_CIs(c(`CI low`, ML, `CI high`))),
             by = 1:nrow(overlap_dt)]
}
