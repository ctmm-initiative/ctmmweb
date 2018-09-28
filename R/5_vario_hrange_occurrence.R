# see documentation and example in 05_1_model_variogram.Rmd / align list
# this is slightly different from align_lists as the input is not each list, but a list of list. we have parameter in list already in this usage.
align_curve_lists <- function(list_lst) {
  dimension <- length(list_lst)
  # no 1st sublist so need to separate processing
  if (dimension == 0) return(NULL)
  # by length of sublist 1
  lapply(seq_along(list_lst[[1]]), function(i) {
    lapply(list_lst, getElement, i)  # function format of `[[`
  })
}
# kmeans detection ----
detect_clusters <- function(diff_t, k){
  cl <- try(stats::kmeans(diff_t, k))
  if (class(cl) == "try-error") {
    shiny::showNotification("Error in kmeans, check error messages",
                            duration = 5, type = "error")
    return(NULL)
  }
  dtv <- sapply(1:k, function(i) {
    round(median(diff_t[which(cl$cluster == i)]), 2)
  })
  return(sort(dtv))
}
# need to be in a function so it's easier to run per individual
filter_inc_t <- function(inc_t, prob = 0.05) {
  range_to_keep <- range(quantile(inc_t, probs = c(prob, 1 - prob),
                                  na.rm = TRUE))
  # need to keep same number of values, so assign NA instead of return subset
  inc_t[inc_t < range_to_keep[1] | inc_t > range_to_keep[2]] <- NA_real_
  return(inc_t)
}

# plot ----
# the app can use a 0.72 cex but package functions may need a smaller default.
# S3 generic on list https://github.com/ctmm-initiative/ctmm/blob/master/R/generic.R#L109
# plot.list <- ctmm:::plot.list
# S3 example, https://github.com/ctmm-initiative/ctmm/blob/master/R/1.R#L34
# plot <- function(object,...) UseMethod("plot")

#' Plot a group of empirical or modeled variograms.
#'
#' This is a wrapper over [ctmm::plot.variogram()] to make group plot easier.
#'
#' @param vario_list list of [ctmm::variogram()]. The names of list may be
#' needed for figure titles.
#' @param model_list `CTMM` models list. The `names` of list may be needed for
#'   figure titles. Draw modeled variogram if provided. The models list should
#'   match `vario_list` in length and animal, so that `i`th model is for `i`th
#'   variogram.
#'   Possible values:
#'   - It can be skipped to use default value `NULL`, and only empirical
#'   variograms are drawn. `vario_list` names will be used as figure title.
#'   Better use named parameters for others in this case.
#'   - list of guesstimated models from [ctmm::ctmm.guess()] on telemetry data,
#'   overlay guesstimate variogram.
#'   - list of fitted models from [ctmm::ctmm.select()] on `vario_list`, overlay
#'   modeled variogram.
#'   - It can be a list of list, with each item provide list of `CTMM` objects
#'   that to be plotted in each variogram. `model_color` can be a vector
#'   to specify color for each model.
#' @param title_vec vector of figure titles. If not provided, names of
#' `vario_list` or `model_list` will be used.
#' @param fraction Fraction of time-lag range, 0 ~ 1.
#' @param relative_zoom
#' - Relative mode zoom every plot by fraction of their own Time-lag range. The
#' X, Y axes are not synced.
#' - Absolute mode operate on the max Time-lag range individual in group, and
#' all others scaled with same X, Y axes for easier comparison.
#' @param cex The magnification factor of plot text and symbols. See [par()].
#' You may need a smaller value if figure margins is too large
#' @param model_color The color of model variogram. It'll be same for every
#' plot.
#' @param columns The columns of the group plot layout.
#'
#' @export
plot_vario <- function(vario_list, model_list = NULL, title_vec = NULL,
                       fraction = 0.5, relative_zoom = TRUE, cex = 0.65,
                       model_color = "blue", columns = 2){
  if (is.null(title_vec)) {
    title_vec <- if (is.null(model_list)) names(vario_list)
    else names(model_list)
  }
  # # if two curves are identical, only draw one to avoid color mixing from 2 colors. this doesn't work in fine-tune page because the model constructed from slider values will have tiny difference with original, thus not equal. but this acceptable. this only check 1 and 2, didn't check 3rd.
  # for (i in 1:length(model_list)) {
  #   if (identical(model_list[[i]][[1]], model_list[[i]][[2]])) {
  #     model_list[[i]][[2]] <- NULL
  #   }
  # }
  row_count <- ceiling(length(vario_list) / columns)
  # the shared group code is not much, and it involves env setup and restoration, would need on.exit if abstracted to function. just copy, basically 3 lines.
  def.par <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(row_count, columns),
                mar = c(5, 5, 4, 1), ps = 18, cex = cex, cex.main = 0.9)
  if (!relative_zoom) {
    max.lag <- max(sapply(vario_list, function(v){ last(v$lag) } ))
    xlim <- max.lag * fraction
    vario_zoomed_list <- lapply(vario_list,
                                function(vario) vario[vario$lag <= xlim, ])
    extent_tele <- ctmm::extent(vario_zoomed_list)
    for (i in seq_along(vario_zoomed_list)) {
      plot(vario_zoomed_list[[i]], CTMM = model_list[[i]],
           col.CTMM = model_color, fraction = 1,
           xlim = c(0, extent_tele["max", "x"]),
           ylim = c(0, extent_tele["max", "y"]))
      graphics::title(title_vec[i])
    }
  } else {
    for (i in seq_along(vario_list)) {
      plot(vario_list[[i]], CTMM = model_list[[i]],
           col.CTMM = model_color,
           fraction = fraction)
      graphics::title(title_vec[i])
    }
  }
  graphics::par(def.par)
}

# S3 generic, https://github.com/ctmm-initiative/ctmm/blob/master/R/emulate.R
# plot.variogram

#' Plot a group of home ranges or occurrences
#'
#' This is a wrapper over [ctmm::plot.telemetry()] to make group plot easier.
#'
#' @param UD_list `ctmm` `UD` object list, which can be home range from
#'   [ctmm::akde()] or occurrences from [ctmm::occurrence()]. The names of list
#'   are needed for title of figures.
#' @param level_vec The vector of `level.UD` in [ctmm::plot.telemetry()]. To be
#'   consistent with `ctmm` they are values 0 ~ 1 (for example 0.95). Note the
#'   app UI take percentage values 0 ~ 100 (for example 95) for easier input.
#' @param color_vec The colors for contour, density and location points in each
#'   plot by order. Single color is used for all plots.
#' @param option Whether to show contour, interval or location points. Note
#'   interval only take effect when contour exsits.
#' @inheritParams plot_vario
#' @param tele_list [ctmm::as.telemetry()] telemetry list. When provided, animal
#'   locations are overlayed in plot. This should only be used for home range
#'   since it can interfere with occurrence plot.
#'
#' @export
plot_ud <- function(UD_list, level_vec = 0.95, color_vec = "blue",
                    option = c("contour", "interval", "location"),
                    columns = 2, cex = 0.65,
                    tele_list = NULL) {
  if (length(color_vec) == 1) color_vec <- rep(color_vec, length(UD_list))
  row_count <- ceiling(length(UD_list) / columns)
  def.par <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(row_count, columns),
                mar = c(5, 5, 4, 1), ps = 18, cex = cex, cex.main = 0.9)
  lapply(seq_along(UD_list), function(i) {
    tryCatch({
      # without location, also no col parameter. all parameter differences in first line.
      if (is.null(tele_list) || !("location" %in% option)) {
        plot(UD_list[[i]],
             level.UD = level_vec, col.DF = color_vec[i],
             col.level = if ("contour" %in% option) color_vec[i] else NA,
             level = if ("interval" %in% option) 0.95 else NA)
      } else {
        # must use named parameter of UD here, since the 2nd parameter by position is for CTMM
        plot(tele_list[[i]], UD = UD_list[[i]], col = color_vec[i],
             level.UD = level_vec, col.DF = color_vec[i],
             col.level = if ("contour" %in% option) color_vec[i] else NA,
             level = if ("interval" %in% option) 0.95 else NA)
      }
    }, error = function(e) {
      warning(names(UD_list)[i], ": ", e)
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10))
    })
    graphics::title(names(UD_list)[i])
  })
  graphics::par(def.par)
}
# plot home range pairs. only used pair in app, the function also work on multiple so named as group. note plot_ud take a list but plot one by one.
# level.UD: Contour level, level: Confidence intervals placed on the contour
# different from plot_ud: group is list of 2 items, level.UD is single value. all group parameter are list by order. later group_list is list of groups
# we only use this internally so always have tele input. plotting pairs so grid is off.
plot_hr_group <- function(hr_group, tele_group, color_group,
                          level.UD = 0.95,
                          option = c("contour", "interval", "location")) {
  if ("location" %in% option) { # all parameter differences in first line.
    plot(tele_group, UD = hr_group, col = color_group,
         col.DF = color_group, col.grid = NA, level.UD = level.UD,
         col.level = if ("contour" %in% option) color_group else NA,
         level = if ("interval" %in% option) 0.95 else NA)
  } else {
    plot(hr_group,
         col.DF = color_group, col.grid = NA, level.UD = level.UD,
         col.level = if ("contour" %in% option) color_group else NA,
         level = if ("interval" %in% option) 0.95 else NA)
  }
  # adjust cex.main in group plot
  # graphics::title(paste(names(hr_group), collapse = ", "))
  # multi_color_title(color_group, names(hr_group))
  multi_color_multi_line_title(color_group, names(hr_group))
}

# plot a list of pairs
plot_hr_group_list <- function(hr_group_list, tele_group_list, color_group_list,
                               level.UD = 0.95,
                               option = c("contour", "interval", "location"),
                               columns = 2, cex = 0.65) {
  def.par <- graphics::par(no.readonly = TRUE)
  row_count <- ceiling(length(hr_group_list) / columns)
  graphics::par(mfrow = c(row_count, columns),
                # cex: font size, cex.main: title relative to font size
                mar = c(5, 5, 4, 1), ps = 18, cex = cex, cex.main = 1)
  lapply(seq_along(hr_group_list), function(i) {
    # must use named parameter of UD here, since the 2nd parameter by position is for CTMM
    plot_hr_group(hr_group_list[[i]], tele_group_list[[i]],
                  color_group = color_group_list[[i]],
                  level.UD = level.UD,
                  option = option)
  })
  graphics::par(def.par)
}
# use multiple color in base plot title. original version use color and string interwined. the function only check parameter name of color, any other name will be for the text.

#' Multiple Colored Plot Title
#'
#' Make multiple colored base plot title.
#'
#' @describeIn multi_title The original function taken from
#' [mailing list by Barry Rowlingson](http://r.789695.n4.nabble.com/title-words-in-different-colors-td878698.html#a878700).
#' The only change is the `NULL` return value was made invisible.
#' @param ... Color and text interwoven in order. See example. Note some spaces
#' need to be placed around text.
#'
#' @export
#'
#' @examples
#' plot(1, 2)
#' multi_title(color="red", "Traffic",
#'            color="orange", " light ",
#'            color="green", "signal")
multi_title <- function(...){
  ###
  ### multi-coloured title
  ###
  ### examples:
  ###  multiTitle(color="red", "Traffic",
  ###             color="orange"," light ",
  ###             color="green","signal")
  ###
  ### - note triple backslashes needed for embedding quotes:
  ###
  ###  multiTitle(color="orange","Hello ",
  ###             color="red"," \\\"world\\\"!")
  ###
  ### Barry Rowlingson <[hidden email]>
  ###
  l = list(...)
  ic = names(l)=='color'
  colors = unique(unlist(l[ic]))

  for(i in colors){
    color=par()$col.main
    strings=c()
    for(il in 1:length(l)){
      p = l[[il]]
      if(ic[il]){ # if this is a color:
        if(p==i){  # if it's the current color
          current=TRUE
        }else{
          current=FALSE
        }
      }else{ # it's some text
        if(current){
          # set as text
          strings = c(strings,paste('"',p,'"',sep=""))
        }else{
          # set as phantom
          strings = c(strings,paste("phantom(\"",p,"\")",sep=""))
        }
      }
    } # next item
    ## now plot this color
    prod=paste(strings,collapse="*")
    express = paste("expression(",prod,")",sep="")
    e=eval(parse(text=express))
    graphics::title(e,col.main=i)
  } # next color
  # remove this otherwise print NULL in console
  return(invisible())
}
# we will want color vector and string vector as parameter. the text parameter name is not needed by multiTitle but easier for us to organize.
# will need to insert ", " after first n-1 names.

#' @describeIn multi_title Instead of taking interwoven color and text input,
#'   color vector and text vector are used.
#' @param color_vec Vector of colors. Should have same length with `text_vec`
#' @param text_vec Vector of text segments. Will have the cooresponding color in
#'   `color_vec`.
#' @param sep Separator to be placed among the text segments
#'
#' @export
multi_color_title <- function(color_vec, text_vec, sep = ", ")  {
  # our color_vec in app is vector with names like "v1.Cilla". when converted to list then unlist then as.list, that names are reserved instead of "color".
  names(color_vec) <- NULL
  text_vec[1:(length(text_vec) - 1)] <- paste0(
    text_vec[1:(length(text_vec) - 1)], sep)
  para_list <- lapply(seq_along(color_vec), function(i) {
    c(color = color_vec[i], text = text_vec[i])
  })
  # list() will convert to list with one item holding the vector.
  do.call(multiTitle, as.list(unlist(para_list)))
}
# above functions make title in one line, our names could be quite long
# print each color in separate line. this probably will not work for n > 2 because not enough margin.

#' @describeIn multi_title Place each text segment in a new line.
#' @inheritParams multi_color_title
#'
#' @export
multi_color_multi_line_title <- function(color_vec, text_vec, sep = "\n\n") {
  # add new line after first n-1 names. need two new lines otherwise not enough space(could depend on font size)
  text_vec[1:(length(text_vec) - 1)] <- paste0(
    text_vec[1:(length(text_vec) - 1)], sep)
  for (i in seq_along(color_vec)) {
    graphics::title(text_vec[i], col.main = color_vec[i])
  }
}
