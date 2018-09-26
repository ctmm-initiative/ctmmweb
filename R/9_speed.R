speed_res_to_dt <- function(res) {
  res_cleaned <- lapply(res, function(x) {
    if (class(x) == "try-error") {
      data.frame(rn = "error", low = Inf, ML = Inf, high = Inf)
    } else {
      data.table(x, keep.rownames = TRUE)
    }
  })
  dt <- rbindlist(res_cleaned)
  # format by unit, round values --
  # get unit name and ratio, update table manually. the existing format_dt_unit apply to vector, that could result in different units for low and high.
  # pick unit apply to median, so we only need to look at ML column
  unit_picked <- pick_unit_speed(dt$ML)
  dt[, `:=`(low = low / unit_picked$scale,
            ML = ML / unit_picked$scale,
            high = high / unit_picked$scale)]
  # round after conversion, otherwise need to round again
  round_cols(dt, c("low", "ML", "high"))
  setnames(dt, "ML", paste0("speed (", unit_picked$name, ")"))
  ci_col_name <- paste0("speed CI (", unit_picked$name, ")")
  dt[, (ci_col_name) := paste0("(", paste0(c(low, high), collapse = " – "), ")"),
     by = 1:nrow(dt)]
  # keep the low high column for plot
  dt[, c("rn") := NULL]
}
