# parallel ----
# ... version is not what we need in curves. we have input in list already. this can be used for parallel usage if needed, not sure which of list()/NULL is needed

#' Combine multiple lists into one list by aligning each item
#'
#' The generic parallel function [par_lapply()] can only apply a function with
#' single parameter to a list. Thus function with multiple parameters need to be
#' wrapped into a function with single parameter list.
#'
#' @param ... multiple lists with same length, to be combined into a master list
#'   with item from each list aligned
#'
#' @return A list of same length of each input list, and each input parameter should have same length. Each item in result is a list of each `i`th item in input lists.
#' @export
#' @examples
#' align_lists(letters[1:3], 1:3, rep_len(FALSE, length.out = 3))
#'
align_lists <- function(...) {
  list_lst <- list(...)
  len_vec <- sapply(list_lst, length)
  stopifnot(length(unique(len_vec)) == 1)
  # by length of sublist 1. with list(...) even NULL input have 1 item.
  res <- lapply(seq_along(list_lst[[1]]), function(i) {
    lapply(list_lst, getElement, i)  # function format of `[[`
  })
  # if input is NULL, res will be list(), will error on [[i]] call, which is our most usage. change to NULL so [[i]] will get NULL
  if (length(res) == 0) res <- NULL
  return(res)
}
# used to write parallel as last parameter, then para_ll(ll, fun, parallel = TRUE) is interpreted as win_init = {parallel = TRUE}

#' Parallel apply function to list in all platforms
#'
#' This is a generic parallel lapply that work across all major platforms.
#'
#' In Windows [parallel::parLapplyLB()] is used, which is a socket cluster and
#' need to initialize each session manually with `win_init` if needed. In
#' Linux/Mac [parallel::mclapply()] is used, where each worker will inherit the
#' current environment through forking, so no additional setup is required.
#'
#' @param lst Input list.
#' @param fun Function to be applied on `lst`. Note only single parameter
#'   function is accepted, otherwise it's difficult to determine how to assign
#'   input parameters to each list item and worker. You need to convert multiple
#'   parameter function into a function take single list parameter, and assign
#'   parameters in that list accordingly. [align_lists()] is a helper function
#'   to align multiple lists.
#' @param cores the core count to be used for cluster. Could be a positive
#'  integer or
#'   - Default `NULL` value will indicate to use a heuristic value based on detected cores, which is roughly `min(input_size, physical_cores_count * n)`,
#'   `n` being 2 for windows, 4 for Mac/Linux. See [parallel::detectCores()] for
#'    more information on physical/logical cores in different platforms.
#'   - A negative value like `-2` will use `all available cores - 2`, so that 2
#'    cores are reserved for user's other tasks.
#' @param parallel Use regular [lapply()] when FALSE. You may notice more console
#'  messages in this mode because the console messages in parallel mode are
#'  lost by default as they happened in other threads.
#' @param win_init Expression to be initialized in Windows. Since all parameters
#'   should be included in the input list already, this usually means library
#'   calls, like `{library(ctmm)}` for ctmm related operations, which has been
#'   taken care of with the default value. Note [requireNamespace()] is used
#'   because that's more appropriate inside a package.
#'
#' @return List of applied results
#' @export
#'
par_lapply <- function(lst, fun,
                       cores = NULL,
                       parallel = TRUE,
                       win_init = expression({
                         requireNamespace("ctmm", quietly = TRUE)
                         })) {
  if (parallel) {
    # cores value has 3 cases, 2 of them are platform independent when it is a explicit value instead of NULL. we didn't check all possible values like 0.
    if (!is.null(cores) && cores > 0) { # NULL>0 become logical(0), not FALSE
      cluster_size <- cores
    }
    # negative value, reserve cores. must be explicit not just else, because NULL is valid input
    if (!is.null(cores) && cores < 0) {
      # use max(x, 1) to avoid invalid values like 4 - 5, 4 - 4
      cluster_size <- max(parallel::detectCores(logical = FALSE) + cores, 1)
    }
    sysinfo <- Sys.info()
    # catch parallel error and suggest to restart R session
    tryCatch({
      # windows ----
      if (sysinfo["sysname"] == "Windows")  {# Darwin for Mac
        if (is.null(cores)) {# no input, use heuristic.
          cluster_size <- min(length(lst),
                              parallel::detectCores(logical = FALSE) * 2)
        }
        cat(crayon::inverse("running parallel in SOCKET cluster of",
                            cluster_size, "\n"))
        cl <- parallel::makeCluster(cluster_size, outfile = "")
        # have to export parameter too because it's not available in remote
        parallel::clusterExport(cl, c("win_init"), envir = environment())
        parallel::clusterEvalQ(cl, eval(win_init))
        res <- parallel::parLapplyLB(cl, lst, fun)
        parallel::stopCluster(cl)
      } else {# Mac/Linux ----
        # log("a")  # to test error handling
        if (is.null(cores)) {# no input, use heuristic
          cluster_size <- min(length(lst),
                              parallel::detectCores(logical = FALSE) * 4)
        }
        cat(crayon::inverse("running parallel with mclapply in cluster of",
                            cluster_size, "\n"))
        res <- parallel::mclapply(lst, fun, mc.cores = cluster_size)
      }
    },
    error = function(e) {
      cat(crayon::bgRed$white("Parallel Error, try restart R session\n"))
      cat(e)
      }
     )

  } else {# non-parallel mode
    res <- lapply(lst, fun)
  }
  return(res)
}
# test single error object or a list with error objects as items
has_error <- function(result) {
  if (inherits(result, "try-error")) {
    TRUE
  } else {
    sapply(result, function(x) {
      inherits(x, "try-error")
    })
  }
}
# all par_ functions have additional cores, parallel parameters that will be used in app
# the real function used in app as we need to customize guess list.
# app need more control since we may want adjusted guess list instead of automatic guess. don't want to add this in package help index, so do not use roxygen format.
# ctmm.fit: return single best model for each
# try: ctmm.select, verbose = TRUE, all attempted models with model type as name, models for same animal as sub items of animal node
# ctmm.select verbose = FALSE: same structure but no model type as name, with one extra layer compare to ctmm.fit. also the object content is different. there is no sense to use verbose = FALSE. though there may be a need for parallel ctmm.fit
# trace will print progress, but console output is lost in parallel mode since they are not in master r process. it will be shown in non-parallel mode.
# didn't add animal names to list because the aligned list lost model name information anyway. we added the names in calling code instead. It was only called once.
# try: the ctmm.select, select: the manual select rows in model summary table. cannot use select_models name since that was a reactive expression to select model results by rows. use internal function for better locality, less name conflict. fit is also not optimal since it hint ctmm.fit
# param list have same length with animals, each item have 3 items for 3 parameters.
par_try_tele_guess_IC <- function(tele_guess_IC_list, cores = NULL, parallel = TRUE) {
  # tele_guess_IC_list is list by animals. each item have two sub items of tele, CTMM.
  # function process single animal with all parameters, may use ctmm parallel if single animal, otherwise use par apply on this function.
  try_models <- function(tele_guess_IC) {
    # only difference is pNewton method. internal_cores is outside value(not defined right now, but has value when try_models was called), referenced invisibly because the par_lapply need single parameter function
    fall_back(ctmm::ctmm.select,
              list(tele_guess_IC[[1]], CTMM = tele_guess_IC[[2]], IC = tele_guess_IC[[3]],
                   control = list(method = "pNewton",
                                  cores = internal_cores),
                   trace = TRUE, verbose = TRUE),
              ctmm::ctmm.select,
              list(tele_guess_IC[[1]], CTMM = tele_guess_IC[[2]], IC = tele_guess_IC[[3]],
                   control = list(cores = internal_cores),
                   trace = TRUE, verbose = TRUE),
              "ctmm.select() failed with pNewton, switching to Nelder-Mead")
  }
  # process multiple animals on multiple cores, single animal on multiple cores using ctmm.select internal parallel option. internal cores are value in environment.
  if (length(tele_guess_IC_list) == 1) {
    internal_cores <- if (parallel) -1 else 1
    cores_reported <- if (parallel) "all but one" else 1
    cat(crayon::white$bgBlack("trying models on single animal with",
                              cores_reported, "cores\n"))
    # the result is a list of models, named by model type. need to wrap into a list of animals. this internal function doesn't provide animal name as it may not have information. par_try_models as external functions will assign names, the app calling code also assign names.
    res <- list(try(try_models(tele_guess_IC_list[[1]])))
  } else {
    internal_cores <- 1
    res <- try(par_lapply(tele_guess_IC_list, try_models, cores, parallel))
  }
  # in serial mode whole res become error with one individual error. in parallel mode only list items with error are errors.
  if (any(has_error(res))) {
    cat(crayon::bgYellow$red("Error in model selection\n"))
  }
  # return value could be error object or list with error object as items. This is fine in command line(and keep most information), but need to prevent next in app as model as error object cannot work.
  return(res)
}
# convenience wrapped to take telemetry list, guess them, fit models. In app we need modified guess list so didn't use par_try_models and par_fit_models. They are used in vignette for quick fitting without touching on variogram.
# try: ctmm.select, verbose = TRUE, all attempted models with model type as name, models for same animal as sub items of animal node

#' Parallel fitting models on telemetry list
#'
#' par_try_models run [ctmm::ctmm.select()] on each object of list on parallel.
#'
#' @param tele_list [ctmm::as.telemetry()] telemetry list
#' @param IC information criteria used in model selection, possible values are
#'   "AICc", "AIC", "BIC", "LOOCV", and "HSCV"
#' @inheritParams par_lapply
#'
#' @return `par_try_models`: list of items named by animal names, each item hold
#'   the attempted models as sub items with model type as name.
#' @export
par_try_models <- function(tele_list, IC = "AICc",
                           cores = NULL, parallel = TRUE) {
  tele_guess_IC_list <- align_lists(tele_list,
                                lapply(tele_list, function(x) {
                                  ctmm::ctmm.guess(x, interactive = FALSE)
                                }),
                                rep.int(IC, length(tele_list)))
  # printing this caused problem in cran check vignette, even there is no problem knitting the package_usage.rmd. removing it now as we have system.time call in app already.
  # print(system.time(
    model_try_res <- par_try_tele_guess_IC(tele_guess_IC_list, cores, parallel)
    # ))
  if (!inherits(model_try_res, "try-error")) {
    names(model_try_res) <- names(tele_list)
  }
  return(model_try_res)
}
# ctmm.fit: return single best model for each. less used so didn't implement the parallel for single model feature.

#' Parallel fitting models on telemetry list
#'
#' par_fit_models run [ctmm::ctmm.fit()] on each object of list on parallel.
#' @describeIn par_try_models
#'
#' @return `par_fit_models`: list of models named by animal names.
#' @export
par_fit_models <- function(tele_list,
                           cores = NULL,
                           parallel = TRUE) {
  tele_guess_list <- align_lists(tele_list,
                                lapply(tele_list, function(x) {
                                  ctmm::ctmm.guess(x, interactive = FALSE)
                                }))
  # fit single model, no plural
  fit_model <- function(tele_guess) {
    ctmm::ctmm.fit(tele_guess[[1]], CTMM = tele_guess[[2]], trace = TRUE)
  }
  # print(system.time(
    model_fit_res <- par_lapply(tele_guess_list, fit_model,
                                 cores, parallel)
                    # ))
  names(model_fit_res) <- names(tele_list)
  return(model_fit_res)
}

#' Parallel calculate home range separately
#'
#' Sometimes you may want to calculate home range individually, because
#' calculate them in same grid could take significant memory when individuals
#' are spread out.
#'
#' @param tele_list [ctmm::as.telemetry()] telemetry list
#' @param model_list Corresponding `ctmm` model list for `tele_list`
#' @param weight_list List of True/False in same order of tele_list. Used for
#'   optimal weighting parameter in [ctmm::akde()].
#' @inheritParams par_try_models
#'
#' @return List of home ranges
#' @export
par_hrange_each <- function(tele_list, model_list, weight_list,
                                  cores = NULL,
                                  parallel = TRUE) {
  tele_model_weight_list <- align_lists(tele_list, model_list, weight_list)
  hrange_calc <- function(tele_model_weight_list) {
    ctmm::akde(tele_model_weight_list[[1]], tele_model_weight_list[[2]],
               weights = tele_model_weight_list[[3]])
  }
  par_lapply(tele_model_weight_list, hrange_calc, cores, parallel)
}

#' Parallel calculate occurrence from telemetry and model list
#'
#' @param tele_list [ctmm::as.telemetry()] telemetry list
#' @param model_list Corresponding `ctmm` model list for `tele_list`
#' @inheritParams par_lapply
#'
#' @return occurrence results list
#' @export
par_occur <- function(tele_list, model_list,
                      cores = NULL,
                      parallel = TRUE) {
  tele_model_list <- align_lists(tele_list, model_list)
  occur_calc <- function(tele_model_list) {
    ctmm::occurrence(tele_model_list[[1]], tele_model_list[[2]])
  }
  par_lapply(tele_model_list, occur_calc, cores, parallel)
}
# calculate speed in parallel
par_speed <- function(para_list,
                      cores = NULL,
                      parallel = TRUE) {
  speed_calc <- function(para_list) {
    ctmm::speed(para_list[[1]], para_list[[2]],
                level = para_list[[3]], robust = para_list[[4]],
                units = FALSE)$CI
  }
  par_lapply(para_list, speed_calc, cores, parallel)
}
# sample telemetry data ----

#' Pick subset from telemetry object/list
#'
#' A dataset subset can have models fitted much quicker. This is used to reduce
#' waiting time in developing code that involved time consuming modeling
#' processes. After code is tested and stablized, full size dataset can be used.
#'
#' @param tele Either a list of telemetry object or single telemetry object
#' @param m m even spaced points are taken from each object. If m > data size,
#'   all points are taken.
#'
#' @export
pick <- function(tele, m) {UseMethod("pick")}

#' pick subset from [ctmm::as.telemetry()] telemetry object
#'
#' @param tele [ctmm::as.telemetry()] telemetry object
#' @describeIn pick
#'
#' @return `pick.telemetry`: telemetry object with m data points
#' @export
#' @import ctmm
pick.telemetry <- pick_tele <- function(tele, m) {
  # Rely on ctmm S3 method to treat telemetry object as a `data.frame`, thus ctmm need to be imported in NAMESPACE.
  tele[floor(seq(from = 1, to = nrow(tele), length.out = min(nrow(tele), m))), ]
}
#' pick subset from each [ctmm::as.telemetry()] telemetry object
#'   in list
#'
#' @param tele [ctmm::as.telemetry()] telemetry list
#' @describeIn pick
#'
#' @return `pick.list`: telemetry list of subsets
#' @export
pick.list <- pick_tele_list <- function(tele, m) {
  # R-CMD-Check require S3 method to use same argument
  lapply(tele, function(x) {
    pick(x, m)
  })
}
