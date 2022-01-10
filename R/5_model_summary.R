# helper functions ----
# newly created column are always in the end. If it's created based on a reference column, we may want to move to right after reference column. not considering new column are just updating and in the middle.
move_last_col_after_ref <- function(dt, col_name) {
  all_col_names <- names(dt)
  current_col_index <- match(col_name, all_col_names)
  last_col_index <- length(all_col_names)
  # if reference col was last column, no need to reorder (the last part of new_index_vec will be like 16:15, which will not work)
  if (last_col_index != current_col_index + 1) {
    new_index_vec <- c(1:current_col_index,  # up to current one
                       last_col_index,  # the last one, just created
                       (current_col_index + 1):(last_col_index - 1))
    setcolorder(dt, new_index_vec)
  }  # no need to return as it modify input
}
# build ctmm model summary table ----
# names in each step of workflow
## model_list_dt -> model_list_dt_compared -> model_summary_dt -(format)> summary_dt
# all model related table use same key columns, so merge will be easier. otherwise need to specify all common columns.
# model_list_dt, model_summary_dt using this. use this in merge, but not setkey, which will sort the table, but we want to sort by identity and aicc.
model_dt_id_cols <- c("model_no", "identity", "model_type", "model_name")
# each model object is a CTMM object, summary(ctmm_obj) give some information in a list, which is converted into a table. summary(ctmm_obj_list) give some comparison among models, dAIIc col.
# only convert the summary list as we need more flexibility in summary call
# summary on single model
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
  # replace tau column name
  names(ci_dt) <- stringr::str_replace_all(names(ci_dt), "\u03C4", "tau")
  ci_dt[estimate %in% c("low", "high"), estimate := stringr::str_c("CI ", estimate)]
  # with SI units, we only need to know which type of unit. the actual format need to happen after the whole table is built. so we only need a mapping from col name to proper unit function later.
  setnames(ci_dt, names(ci_dt), stringr::str_replace_all(names(ci_dt), "\\s\\(.*", ""))
  # two part need to bind together, add model name col, then add animal name col in last step. because of row number difference, it's easier to use merge, add common col first.
  dof_dt[, item := 1]
  ci_dt[, item := 1]
  res_dt <- merge(dof_dt, ci_dt, by = "item")
  res_dt[, item := NULL]
}
# update model_no and model_name for dt. will be used in app so abstracted as function, in case we want to change the model_name notation. Note this modify dt in place.
update_model_no <- function(model_list_dt) {
  model_list_dt[, model_no := .I]
  # need a col that represent each model uniquely so it can be used to create home range color palette, which need to separate for each possible models across animals and model types. It need to be "global" for full table no matter what subset is selected.
  model_list_dt[, model_name := stringr::str_c(model_no, ". ", identity, " - ", model_type)]
}
# convert ctmm.select/try_tele_guess result into data.table with model in list column.
# par_try_tele_guess try multiple models on each animal with ctmm.select, generate a model list for each animal, saved in a list of list, unnamed item with sub item of models named by model type. previously we use animal name as item name since they are natural group condition. Now multiple copies of same animal data with different init conditions can be tried to refit, animal name as item name will have duplicate and indexing problem.
# always use unique name for list items, so animal name in auto fit, model name in refit and provide animal names in a separate vector (with duplicates).
# previously used identity as key, now we need to switch to model_no. a summary on each list item was used to generate \u0394AICc information, res_list_index is important.
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
  # add model_no, model_name
  update_model_no(model_list_dt)
  # previously we used identity to access res list items, now we use item index
  model_list_dt[, model :=
                  list(list(model_try_res[[res_list_index]][[model_type]])),
                by = model_no]
  # prepare additional columns needed by app.
  # the init condition of this model, to be used for variogram plot. value is empty but the col type need to be right
  # this doesn't init column correctly when nrow = 1. the "correct" result when nrow > 1 might accidental
  # model_list_dt[, init_ctmm := vector('list', nrow(model_list_dt))]
  model_list_dt[, init_ctmm := list(list(NULL))]
  # name of the init condition model. guess/modified guess for auto fit, model name for refit.
  model_list_dt[, init_ctmm_name := NA_character_]
  # init_ctmm_next to be used as init for next refit. to copy a list column need to wrap with list(), otherwise it will unpack when nrow = 1
  model_list_dt[, model_current := list(model)]
  model_list_dt[, model_tuned := FALSE]
}
# aicc column can only generated for group of models of same animal. when multiple pass model results merged, need to generate this info again, also sort again. put it in separate function, so that we always compare and sort model_lisst_dt before summary. if we put this inside summary function, the modification to model_list_dt is not obvious(new column added to input parameter). better to make this transition step obvious.
# use this after try_res conversion, after merge of two model_list_dt. i.e. after auto fit and refit, each fit, merge result.
# add some comparison columns to model_list_dt. this is only used once so put inside model_list -> modesl summary step
compare_models <- function(model_list_dt, IC_chosen) {
  # summary on model list always give results sorted, with row names of list item names. reorder result to be same order of input, and function call is simpler
  get_IC_vec <- function(model_list) {
    # the model list is the list column of models for same animal, don't have names - model names in other column, we may not add names to list column
    # after we assigned names, the result now have names (character format of numbers)
    names(model_list) <- seq_along(model_list)
    res <- summary(model_list, IC = IC_chosen, units = FALSE)
    # res is data.frame. the index need to be character(to use row names), if using numbers will use row number which is not correct
    # need to use unicode here as the original summary has it.
    # just chose the first column as IC column is always the first
    # res[as.character(seq_along(model_list)), "\u0394AICc"]
    res[as.character(seq_along(model_list)), 1]
  }
  # AICc come from the summary of a group models, always by animal, even models may came from different fit passes
  # model_list_dt[, "dAICc" := get_IC_vec(model), by = identity]
  # column name need to add a "d" prefix, all later column name need to be in this pattern.
  model_list_dt[, (paste0("d", IC_chosen)) := get_IC_vec(model), by = identity]
  # sort it, so model_list_dt and summary are always sorted by same criteria
  # setorder(model_list_dt, identity, "\u0394AICc")
  # need to take column name in quote. backtick doesn't work with unicode, need double quote.
  # setorderv(model_list_dt, c("identity", "dAICc"))
  setorderv(model_list_dt, c("identity", paste0("d", IC_chosen)))
}
# generate summary table for models. too much difference between model table and home range table, make separate functions. use model_summary_dt for unformatted summary, summary_dt as formatted summary to match app usage of summary_dt. the unformatted summary is only intermediate stage, not used in app.
# it's the summary on model that create CI columns, expand one model into 3 rows. we sort model_list_dt and summary_dt by identity and \u0394AICc through compare_models, and keep the order in merge, always use sort = false if merging different order tables.
# expect \u0394AICc column, always compare model before summary.
# model_list_dt -> add compared columns -> to model_summary_dt
# in app we added compared columns right after tried models, then do the remaining in other steps, better separate at same point.
compared_model_list_dt_to_model_summary_dt <- function(compared_model_list_dt, IC_chosen) {
  # a list of converted summary on each model
  ctmm_summary_dt_list <- lapply(1:nrow(compared_model_list_dt), function(i) {
    summary_dt <- ctmm_summary_to_dt(summary(compared_model_list_dt$model[[i]],
                                             units = FALSE))
    # model_no must be taken from compared_model_list_dt. lots of assumption changed. need to assign here, because each model have 3 rows, share same model_no
    summary_dt[, model_no := compared_model_list_dt[i, model_no]]
  })
  ctmm_summary_dt <- rbindlist(ctmm_summary_dt_list, fill = TRUE)
  # export_cols <- c(model_dt_id_cols, "dAICc")
  export_cols <- c(model_dt_id_cols, paste0("d", IC_chosen))
  # merge by common columns, keep the order. the summary table only has model_no
  model_summary_dt <- merge(compared_model_list_dt[, ..export_cols],
                            ctmm_summary_dt, by = "model_no",
                            sort = FALSE)
}
# home range don't have \u0394AICc column, and no compare step. need level.UD for CI areas. with level vec, will return more rows. default usage use single input, then remove the ci number column
hrange_list_dt_to_model_summary_dt <- function(hrange_list_dt, level.UD = 0.95) {
  # make copy first because we will remove column later
  # a list of converted summary on each model. now we have additional level by level, need to combine first
  ctmm_summary_dt_list <- lapply(1:nrow(hrange_list_dt), function(i) {
    dt_list <- lapply(level.UD, function(level_value) {
      summary_dt <- ctmm_summary_to_dt(summary(hrange_list_dt$model[[i]],
                                               units = FALSE,
                                               level.UD = level_value))
      summary_dt[, model_no := hrange_list_dt[i, model_no]]
      summary_dt[, quantile := level_value * 100]
    })
    rbindlist(dt_list, fill = TRUE)
  })
  ctmm_summary_dt <- rbindlist(ctmm_summary_dt_list, fill = TRUE)
  # there is no \u0394AICc column from summary of list of home range.
  res_dt <- merge(hrange_list_dt[, ..model_dt_id_cols],
                  ctmm_summary_dt,
                  by = "model_no", sort = FALSE)
  # move level.UD col to after estimate
  setcolorder(res_dt,
              c(names(res_dt)[1:(ncol(res_dt) - 2)], "quantile", "area"))
}
# given 3 values of CI, round them properly, keep 2 significant digit on difference
# round_CIs <- function(vec, digits = 2) {
#   # if NA in input, need remove. if all NA, there will be warnings.
#   # remove negative sign
#   minimal_diff <- min(abs(diff(vec)), na.rm = TRUE)
#   if (minimal_diff > 1) {
#     # don't need to worry digits.
#     round(vec, 2)
#   } else {
#     formated_diff <- format(minimal_diff, digits = 1, scientific = FALSE)
#     # if exactly 0, will not match 0.xx pattern
#     if (formated_diff == "0") return(vec)
#     # get 0.000, -2 to get the count of 0 after decimal point
#     zeros <- nchar(stringr::str_extract(formated_diff, "0\\.0*")) - 2
#     # need 2 more digits after zeros
#     round(vec, zeros + digits)
#   }
# }
# given a col name -> unit formation function map, format a dt to scale the value, add unit label to col name. For model summary table, CI rows need to be round properly by each model. There are other tables that don't have CI rows and no model_no column. we have two usage for regular tables: data summary, outlier summary, and two usage of model tables here, make the other usage default as they are spreaded.
# the round CI values by model feature may not be expected, turned off now. if need to turn it on, need to assign it in calling functions. The function is also used in info table formatting, note round_by_CI will not work with that usage because no model column
format_dt_unit <- function(dt, name_unit_list) {
  # the col name list have error, which may not exist in some cases
  valid_col_names <- intersect(names(dt), names(name_unit_list))
  lapply(valid_col_names, function(col_name) {
    # get col specific function to apply to col
    best_unit <- name_unit_list[[col_name]](dt[[col_name]])
    # creating new cols, delete old later is easier to check result. though that will cause col order changes, since new cols added in end, old cols removed. updating existing col instead
    # cannot use col_name variable in round_CI call, so use a temp col instead
    # calculate first, round according to need later
    dt[, temp := dt[[col_name]] / best_unit$scale]
    dt[, (col_name) := round(temp, 2)]
    dt[, temp := NULL]
    # \n will cause the table in work report render messed up in html. sometimes DT render colunmn name with \n as same line anyway.
    setnames(dt, col_name, paste0(col_name, " (", best_unit$name, ")"))
  })
  return(dt)
  # dt[, (valid_col_names) := NULL]
}
# just take all columns, will check if numerical before round up
round_cols <- function(dt, digits = 2) {
  lapply(names(dt), function(col_name) {
    if (class(dt[[col_name]]) == "numeric") {
      dt[, (col_name) := round(dt[[col_name]], digits)]
    }
  })
}
# the model summary table need to be formatted for units
format_model_summary_dt <- function(model_summary_dt) {
  # data.table modify reference, use copy so we can rerun same line again
  dt <- copy(model_summary_dt)
  # with Sys.setlocale("LC_CTYPE", "English_United States.1252"), it start to have problem here already, just print dt give warning, other symbol became ascii one, except tau don't have counterpart.
  # https://github.com/ctmm-initiative/ctmmweb/issues/86
  # only OUf have a new column called tau, but it's just tau position and tau velocity, copy to them and remove tau.
  # if (any(stringr::str_detect(names(dt), "^\u03C4$"))) {
  #   browser()
  # }
  # we have to use write tau directly within data.table call. we need to reference the column, not some value.
  if (any(stringr::str_detect(dt$model_type, "OUf"))) {
    dt[stringr::str_detect(model_type, "OUf"), `:=`("tau[position]" = tau,
                                                    "tau[velocity]" = tau)]
    dt[, tau := NULL]
  }
  if (any(stringr::str_detect(dt$model_type, "OUO"))) {
    dt[stringr::str_detect(model_type, "OUO"), `:=`("tau[position]" = tau[decay],
                                                    "tau[velocity]" = tau[decay])]
    dt[, tau[decay] := NULL]
  }
  # should round all numeric values. there are new columns after ctmm update.
  # cols_roundup <- c("DOF mean", "DOF area", "DOF speed", "\u0394AICc")
  # cols_roundup <- names(dt)[5:ncol(dt)]
  # except estimate level column which is not numerical
  # cols_roundup <- cols_roundup[!cols_roundup == "estimate"]
  round_cols(dt)
  # empty cells will have NA since they are numeric columns.
  # remove the duplicated values in CI rows to reduce cluter. - this is not needed with the 1 row design, but leave it in comment in case we want to switch back.
  # dt[stringr::str_detect(estimate, "CI"),
  #        c("\u0394AICc", "DOF mean", "DOF area") := NA_real_]
  # need a list to hold function as element, c have same effect but list is more verbose
  # CI columns will be combined and created later
  # check model summary columns. there are new columns added with ctmm. need to check the model list first by animal, then by specific model. we only need to pick the main metric, CI columns will be added in the function.
  name_unit_list <- list("area" = pick_unit_area,
                         # "\u03C4[position]" = pick_unit_seconds,
                         # "\u03C4[velocity]" = pick_unit_seconds,
                         # "\u03C4" = pick_unit_seconds,
                         "speed" = pick_unit_speed,
                         "error" = pick_unit_distance,
                         "diffusion" = pick_unit_diffusion)
  # all tau columns are time
  if (any(stringr::str_detect(names(dt), "tau"))) {
    name_unit_list[stringr::str_subset(names(dt), "tau")] <- list(pick_unit_seconds)
  }
  format_dt_unit(dt, name_unit_list)
}
# combine ci rows in formatted model summary table, get single row table
combine_summary_ci <- function(summary_dt, hrange = FALSE) {
  dt <- copy(summary_dt)  # we add columns then take subset, better make copy
  # only apply to the sub dt for each model, there are only 3 rows
  get_ci_col_name <- function(col_name) {
    stringr::str_replace(col_name, stringr::fixed("("), "CI (")
  }
  # model page always have one model, each 3 rows. In home range one model can have multiple copy on different level.UD.
  get_ci_col_value <- function(dt, col_name) {
    ci_values <- dt[estimate %in% c("CI low", "CI high")][[col_name]]
    if (all(is.na(ci_values))) {
      NA_character_
    } else {
      paste0("(", paste0(ci_values, collapse = " - "), ")")
    }
  }
  # columns with (, i.e. with units, the column with CI. DOF columns don't have it. note run this function on already converted function will also include ci cols as target cols. no need to check that since no that usage for now.
  target_cols <- stringr::str_subset(names(dt), stringr::fixed("("))
  model_id_cols <- if (hrange) {
    c("model_no", "quantile")
  } else {
    "model_no"
  }
  lapply(target_cols, function(col_name) {
    new_col_name <- get_ci_col_name(col_name)
    dt[, (new_col_name) := get_ci_col_value(.SD, col_name),
       by = model_id_cols]
    move_last_col_after_ref(dt, col_name)
    return(dt)  # the else branch of if clause will be NULL if use if clause as last expression.
  })
  # CRAN version ctmm 0.5.6 using ML, later development version using est
  res <- dt[estimate %in% c("ML", "est")]
  res[, estimate := NULL]
}
# combined steps to make usage easier, otherwise the function name could be confusing, use summary_dt to represent formated modle_summary, the final shape. didn't use this pattern for home range.
# Generate Formated Model Summary Table From Model List Table
#
# model_list_dt a `data.table` holding model information and models objects
#   as list column
#
# return final formated model summary table, with ci columns combined. need this because in app we put compare model right after try model, manipulate model summary table. there are some operations after refit that need to update table, so maybe cannot move compare model into this and use single operation to generate table. no need to change.
# exported parameter use IC, internal functions use IC_chosen
compared_model_list_dt_to_final_summary_dt <- function(compared_model_list_dt, IC_chosen) {
  compared_model_list_dt %>%
    compared_model_list_dt_to_model_summary_dt(IC_chosen = IC_chosen) %>%
    format_model_summary_dt %>%
    combine_summary_ci
}
# exported functions ----
# make the interface simpler. our internal version need intermediate steps because we need the intermediate data

#' Generate formated model summary table from tried models results
#'
#' @param model_try_res list of applying `ctmm::ctmm.select` on telemetry objects
#' @param IC information criteria used in model selection, possible values are
#'   "AICc", "AIC", "BIC", "LOOCV", and "HSCV"
#'
#' @return A `data.table` of model summary
#' @export
summary_tried_models <- function(model_try_res, IC = "AICc") {
  # the pipe line change object: model_res -> model_list_dt -> compared_model_list_dt -> final summary
  res <- model_try_res %>%
    model_try_res_to_model_list_dt %>%
    compare_models(IC_chosen = IC) %>%
    compared_model_list_dt_to_final_summary_dt(IC_chosen = IC)
    # compared_model_list_dt_to_model_summary_dt(IC_chosen = IC) %>%
    # format_model_summary_dt %>%
    # combine_summary_ci
  res[]
  # model_list_dt <- model_try_res_to_model_list_dt(model_try_res)
  # # use [] to make sure calling function directly will print in console.
  # compared_model_list_dt_to_final_summary_dt(model_list_dt)[]
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
# Build Home Range list table ----
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
  # dt[, model_no := .I]  # use model_no from last table
}
format_hrange_summary_dt <- function(hrange_summary_dt) {
  # data.table modify reference, use copy so we can rerun same line again
  dt <- copy(hrange_summary_dt)
  dt[, `DOF area` := round(`DOF area`, 3)]
  dt[, `DOF bandwidth` := round(`DOF bandwidth`, 3)]
  dt[stringr::str_detect(estimate, "CI"),
     c("DOF area", "DOF bandwidth") := NA_real_]
  name_unit_list <- list("area" = pick_unit_area)
  format_dt_unit(dt, name_unit_list)
}
# Generate Formated Home Range Summary Table From Home Range List Table
#
# param hrange_list_dt a data.table holding model info and home range objects
#
# return formated home range summary table
hrange_list_dt_to_formated_range_summary_dt <- function(hrange_list_dt,
                                                        level.UD = 0.95) {
  hrange_list_dt_to_model_summary_dt(hrange_list_dt, level.UD) %>%
    format_hrange_summary_dt %>%
    combine_summary_ci(hrange = TRUE)
  # format_hrange_summary_dt(hrange_summary_dt)
}
# it's difficult to get a home range summary table function, because we reused same summary function and need a model name table, which is borrowed from model summary table, see build_hrange_list_dt. unless we put model name in hrange_list names, get id names, now they are combined. only build when needed. From user's perspective we can use selected model list, and go a long way inside function to get the table.
# rebuild model_try_res from selected_model_list? then build summary. too much hassles. ask user to use regular summary?

# summary_home_range <- function(hrange_list) {
#   hrange_list_dt <- build_hrange_list_dt(names(hrange_list),
#                                          hrange_list)
#   dt <- ctmmweb:::hrange_list_dt_to_formated_range_summary_dt(hrange_list_dt)
#   model_list_dt <- model_try_res_to_model_list_dt(model_try_res)
#   # use [] to make sure calling function directly will print in console.
#   compared_model_list_dt_to_final_summary_dt(model_list_dt)[]
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
    # we are assigning this by location, so it can be changed
    matrix_to_dt(mat_3d[ , , 2], "est", clear_half),
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
  setcolorder(overlap_dt, c("v1", "v2", "CI low", "est", "CI high"))
  overlap_dt[, Combination := paste(v1, v2, sep = " / ")]
  # COPY end --
  # the right side need to be a list to be assigned to multiple columns. need as.list to convert a vector into separate list items.
  round_cols(overlap_dt)
  return(overlap_dt)
  # overlap_dt[, c("CI low", "ML", "CI high") :=
  #              as.list(round_CIs(c(`CI low`, ML, `CI high`))),
  #            by = 1:nrow(overlap_dt)]
}
# home range level input ----
# parse text input of comma separated values
parse_comma_text_input <- function(comma_text, default_value) {
  items <- stringr::str_trim(stringr::str_split(comma_text, ",")[[1]])
  parsed_values <- as.numeric(items[items != ""])
  # non valid input is checked, rejected, show message
  if ((length(parsed_values) == 0) || (is.na(parsed_values))) {
    shiny::showNotification("Only number or comma separated numbers are accepted",
                            duration = 5, type = "error")
    return(default_value)
  } else {
    return(parsed_values)
  }
}
# for home range/occur level input, divid by 100, take default value when no valid input
parse_levels.UD <- function(levels_text) {
  parse_comma_text_input(levels_text, default_value = 95) / 100
}
