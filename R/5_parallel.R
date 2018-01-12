# parallel ----

# Expression to be initialized in windows for `ctmm` related parallel
# operations
#
# Parallel cluter in Windows is a socket cluster, which need to initialize each
# session manually. Since all parameters should be included in single list
# parameter, only stuff needed is init libraries.
#
# For ctmm related parallel operations, `ctmm` package need to be loaded.
# Instead of `library(ctmm)`, the expression of `requireNamespace("ctmm",
# quietly = TRUE)` is more appropriate inside a package.
WIN_INIT_ctmm <- expression({
  # library(ctmm)
  requireNamespace("ctmm", quietly = TRUE)
})

#' Combine two lists into one list by aligning each item
#'
#' The generic parallel function [para_ll] can only apply a function with single
#' parameter to a list. Thus function with multiple parameters need to be
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
#' In Windows `parallel::parLapplyLB` is used, which is a socket cluster and
#' need to initialize each session manually with `win_init` if needed. In
#' Linux/Mac `parallel::mclapply` is used, where each worker will inherit the
#' current environment through forking, so no additional setup is required.
#'
#' @param lst Input list.
#' @param fun Function to be applied on `lst`. Note only single parameter
#'   function is accepted, otherwise it's difficult to determine how to assign
#'   input parameters to each list item and worker. You need to convert multiple
#'   parameter function into a function take single list parameter, and assign
#'   parameters in that list accordingly. [align_list()] is a helper function to
#'   align two lists.
#' @param reserved_cores reserve some cores so that not all cores are used.
#'   Check your platform's core count with
#'   `parallel::detectCores(logical = FALSE)`. `?parallel::detectCores`
#'   has more details about physical/logical cores in different platforms.
#' @param parallel Use regular `lapply` when FALSE
#' @param win_init Expression to be initialized in Windows. Because all
#'   parameters should be included in the input list already, this usually means
#'   library calls, like `{library(ctmm)}` for ctmm related operations, which
#'   has been taken care of with the default value `ctmmweb:::WIN_INIT_ctmm`.
#'
#' @return List of applied results
#' @export
#'
par_lapply <- function(lst, fun,
                       reserved_cores = 0,
                       parallel = TRUE,
                       win_init = ctmmweb:::WIN_INIT_ctmm
) {
  if (parallel) {
    sysinfo <- Sys.info()
    if (sysinfo["sysname"] == "Windows")  {  # Darwin / Windows
      if (reserved_cores == 0) {
        win_cluster_size <- min(length(lst), parallel::detectCores())
      } else {
        win_cluster_size <-
          max(parallel::detectCores(logical = FALSE) - reserved_cores, 1)
      }
      cat(crayon::inverse("running parallel in SOCKET cluster of",
                          win_cluster_size, "\n"))
      cl <- parallel::makeCluster(win_cluster_size, outfile = "")
      # have to export parameter too because it's not available in remote
      parallel::clusterExport(cl, c("win_init"), envir = environment())
      parallel::clusterEvalQ(cl, eval(win_init))
      res <- parallel::parLapplyLB(cl, lst, fun)
      parallel::stopCluster(cl)
    } else {
      if (reserved_cores == 0) {
        cluster_size <- min(length(lst),
                            parallel::detectCores(logical = FALSE) * 4)
      } else {
        cluster_size <-
          max(parallel::detectCores(logical = FALSE) - reserved_cores, 1)
      }
      cat(crayon::inverse("running parallel with mclapply in cluster of",
                          cluster_size, "\n"))
      res <- parallel::mclapply(lst, fun, mc.cores = cluster_size)
    }
  } else {
    res <- lapply(lst, fun)
  }
  return(res)
}
# app need this with more control since we may want adjusted guess list instead of automatic guess. don't want to add this in package help index, so do not use roxygen format.
# ctmm.fit: return single best model for each
# ctmm.select, verbose = TRUE, all attempted models with model type as name, models for same animal as sub items of animal node
# ctmm.select verbose = FALSE: same structure but no model type as name, with one extra layer compare to ctmm.fit. also the object content is different. there is no sense to use verbose = FALSE. though there may be a need for parallel ctmm.fit
# didn't add animal names to list because the aligned list lost model name information anyway. we added the names in calling code instead. It was only called once.
par_select_tele_guess <- function(tele_guess_list,
                               reserved_cores = 0,
                               parallel = TRUE) {
  # cannot use select_models name since that was a reactive expression to select model results by rows. use internal function for better locality, less name conflict. Note this is ctmm.select, not ctmm.fit
  fit_models <- function(tele_guess) {
    ctmm::ctmm.select(tele_guess$a, CTMM = tele_guess$b,
                      trace = TRUE, verbose = TRUE)
  }
  par_lapply(tele_guess_list, fit_models, reserved_cores, parallel)
}
# convenience wrapped to take telemetry list, guess them, fit models. In app we want more control and didn't use this.

#' Parallel selecting models on telemetry list
#'
#' `ctmm::ctmm.select` fit multiple models on telemetry object and output list
#' of all attempted models. This can be parallelized on each object in list.
#'
#' @param tele_list telemetry list
#' @inheritParams par_lapply
#'
#' @return list of items named by animal names, each item hold the attempted
#'   models as sub items with model type as name
#' @export
par_select_models <- function(tele_list,
                         reserved_cores = 0,
                         parallel = TRUE) {
  tele_guess_list <- align_list(tele_list,
                                lapply(tele_list, function(x) {
                                  ctmm.guess(x, interactive = FALSE)
                                }))
  print(system.time(model_select_res <-
                      par_select_tele_guess(tele_guess_list,
                                         reserved_cores,
                                         parallel)))
  names(model_select_res) <- names(tele_list)
  return(model_select_res)
}
#' Parallel calculate occurrence from telemetry and model list
#'
#' @param tele_list `ctmm` `telemetry` list
#' @param model_list Corresponding `ctmm` model list for `tele_list`
#' @inheritParams par_lapply
#'
#' @return occurrence results list
#' @export
par_occur <- function(tele_list, model_list,
                      reserved_cores = 0,
                      parallel = TRUE) {
  tele_model_list <- align_list(tele_list, model_list)
  occur_calc <- function(tele_model_list) {
    ctmm::occurrence(tele_model_list$a, tele_model_list$b)
  }
  par_lapply(tele_model_list, occur_calc, reserved_cores, parallel)
}
# sample telemetry data ----

#' Pick subset from telemetry object/list
#'
#' A dataset subset can have models fitted much quicker. This is used to reduce
#' waiting time in developing code that involved time consuming modeling
#' processes. After code is tested and stablized, full size dataset can be used.
#'
#' `m` even spaced points are taken from each object.
#'
#' @export
pick <- function(object, m) {UseMethod("pick")}

#' @describeIn pick subset from telemetry object
#'
#' @param tele telemetry object
#' @param m subset size
#'
#' @return telemetry object with m data points
#' @export
#' @import ctmm
pick.telemetry <- pick_tele <- function(tele, m) {
  # Rely on ctmm S3 method to treat telemetry object as a `data.frame`, thus ctmm need to be imported in NAMESPACE.
  tele[floor(seq(from = 1, to = nrow(tele), length.out = m)), ]
}

#' @describeIn pick list of subset from each telemetry object
#'
#' @param tele_list telemetry list
#' @inheritParams pick
#'
#' @return telemetry list of subsets
#' @export
pick.list <- pick_tele_list <- function(tele_list, m) {
  lapply(tele_list, function(x) {
    pick(x, m)
  })
}
