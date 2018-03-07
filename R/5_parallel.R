# parallel ----

#' Combine two lists into one list by aligning each item
#'
#' The generic parallel function [par_lapply()] can only apply a function with
#' single parameter to a list. Thus function with multiple parameters need to be
#' wrapped into a function with single list which hold all the parameters.
#'
#' @param list_a list_a. `list_a` and `list_b` need to have same length.
#' @param list_b list_b
#'
#' @return A list of same length of input list. Each item is a list of \itemize{
#'   \item \code{a: list_a[[i]]} \item \code{b: list_b[[i]]} }
#'
#' @export
#'
align_list <- function(list_a, list_b) {
  stopifnot(length(list_a) == length(list_b))
  # use lapply instead of for only because we can get a list without initialization
  lapply(seq_along(list_a), function(i) {
    list(a = list_a[[i]], b = list_b[[i]])
  })
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
#'   parameters in that list accordingly. [align_list()] is a helper function to
#'   align two lists.
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
# app need this with more control since we may want adjusted guess list instead of automatic guess. don't want to add this in package help index, so do not use roxygen format.
# ctmm.fit: return single best model for each
# ctmm.select, verbose = TRUE, all attempted models with model type as name, models for same animal as sub items of animal node
# ctmm.select verbose = FALSE: same structure but no model type as name, with one extra layer compare to ctmm.fit. also the object content is different. there is no sense to use verbose = FALSE. though there may be a need for parallel ctmm.fit
# trace will print progress, but console output is lost in parallel mode since they are not in master r process. it will be shown in non-parallel mode.
# didn't add animal names to list because the aligned list lost model name information anyway. we added the names in calling code instead. It was only called once.
par_try_tele_guess <- function(tele_guess_list,
                               cores = NULL,
                               parallel = TRUE) {
  # cannot use select_models name since that was a reactive expression to select model results by rows. use internal function for better locality, less name conflict. fit is also not optimal since it hint ctmm.fit
  # use try to refer the ctmm.select, use select to refer the manual select rows in model summary table.
  try_models <- function(tele_guess) {
    ctmm::ctmm.select(tele_guess$a, CTMM = tele_guess$b,
                      trace = TRUE, verbose = TRUE)
  }
  par_lapply(tele_guess_list, try_models, cores, parallel)
}
# convenience wrapped to take telemetry list, guess them, fit models. In app we want more control and didn't use this.

#' Parallel fitting models on telemetry list
#'
#' @describeIn par_try_models Run [ctmm::ctmm.select()] on each object of list
#'   on parallel.
#' @param tele_list [ctmm::as.telemetry()] telemetry list
#' @inheritParams par_lapply
#'
#' @return `par_try_models`: list of items named by animal names, each item hold
#'   the attempted models as sub items with model type as name.
#' @export
par_try_models <- function(tele_list,
                           cores = NULL,
                              parallel = TRUE) {
  tele_guess_list <- align_list(tele_list,
                                lapply(tele_list, function(x) {
                                  ctmm::ctmm.guess(x, interactive = FALSE)
                                }))
  print(system.time(model_try_res <-
                      par_try_tele_guess(tele_guess_list,
                                         cores,
                                         parallel)))
  names(model_try_res) <- names(tele_list)
  return(model_try_res)
}

#' @describeIn par_try_models Run [ctmm::ctmm.fit()] on each object of list on
#'   parallel.
#' @inheritParams par_try_models
#'
#' @return `par_fit_models`: list of models named by animal names.
#' @export
par_fit_models <- function(tele_list,
                           cores = NULL,
                           parallel = TRUE) {
  tele_guess_list <- align_list(tele_list,
                                lapply(tele_list, function(x) {
                                  ctmm::ctmm.guess(x, interactive = FALSE)
                                }))
  # fit single model, no plural
  fit_model <- function(tele_guess) {
    ctmm::ctmm.fit(tele_guess$a, CTMM = tele_guess$b, trace = TRUE)
  }
  print(system.time(model_fit_res <-
                      par_lapply(tele_guess_list, fit_model,
                                 cores, parallel)
                    ))
  names(model_fit_res) <- names(tele_list)
  return(model_fit_res)
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
  tele_model_list <- align_list(tele_list, model_list)
  occur_calc <- function(tele_model_list) {
    ctmm::occurrence(tele_model_list$a, tele_model_list$b)
  }
  par_lapply(tele_model_list, occur_calc, cores, parallel)
}
# sample telemetry data ----

#' Pick subset from telemetry object/list
#'
#' A dataset subset can have models fitted much quicker. This is used to reduce
#' waiting time in developing code that involved time consuming modeling
#' processes. After code is tested and stablized, full size dataset can be used.
#'
#' @param m m even spaced points are taken from each object.
#'
#' @export
pick <- function(object, m) {UseMethod("pick")}

#' @describeIn pick subset from [ctmm::as.telemetry()] telemetry object
#'
#' @param tele [ctmm::as.telemetry()] telemetry object
#' @inheritParams pick
#'
#' @return `pick.telemetry`: telemetry object with m data points
#' @export
#' @import ctmm
pick.telemetry <- pick_tele <- function(tele, m) {
  # Rely on ctmm S3 method to treat telemetry object as a `data.frame`, thus ctmm need to be imported in NAMESPACE.
  tele[floor(seq(from = 1, to = nrow(tele), length.out = m)), ]
}

#' @describeIn pick pick subset from each [ctmm::as.telemetry()] telemetry object
#'   in list
#'
#' @param tele_list [ctmm::as.telemetry()] telemetry list
#' @inheritParams pick
#'
#' @return `pick.list`: telemetry list of subsets
#' @export
pick.list <- pick_tele_list <- function(tele_list, m) {
  lapply(tele_list, function(x) {
    pick(x, m)
  })
}
