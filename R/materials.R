#' Locate shared Methods and supplementary materials
#'
#' Returns filesystem paths to reusable text shipped with **FLBacktest**:
#' the peer-review Methods fragment and the modular supplementary R Markdown
#' notes (conditioning, observation error, management procedure, performance
#' statistics). Application papers should prefer these files over maintaining
#' parallel copies.
#'
#' @param what Character; which materials to list.
#'   \code{"all"} (default) returns a named list of paths;
#'   \code{"methods"} the LaTeX Methods fragment;
#'   \code{"vignette"} the Methods vignette source (if installed with the
#'   package source / build);
#'   \code{"rmd"} the supplementary \code{.Rmd} directory;
#'   \code{"example"} the commented erp_clean-style example script;
#'   or a stem such as \code{"01_conditioning"} for a single supplementary file.
#'
#' @return A named character vector of paths, or a list when \code{what = "all"}.
#'
#' @examples
#' backtestMaterials("rmd")
#' backtestMaterials("01_conditioning")
#'
#' @export
backtestMaterials <- function(what = c("all", "methods", "vignette", "rmd",
                                       "example", "01_conditioning",
                                       "02_observation_error",
                                       "03_management_procedure",
                                       "04_performance_statistics")) {
  what <- what[1]
  pkg <- "FLBacktest"

  methods_tex <- system.file("methods", "methods-backtest.tex", package = pkg)
  rmd_dir <- system.file("rmd", package = pkg)
  example <- system.file("examples", "erp_clean-style.R", package = pkg)
  vignette <- system.file("doc", "methods-backtest.Rmd", package = pkg)
  if (!nzchar(vignette)) {
    vignette <- system.file("vignettes", "methods-backtest.Rmd", package = pkg)
  }

  rmd_files <- c(
    "01_conditioning" = "01_conditioning.Rmd",
    "02_observation_error" = "02_observation_error.Rmd",
    "03_management_procedure" = "03_management_procedure.Rmd",
    "04_performance_statistics" = "04_performance_statistics.Rmd"
  )

  if (identical(what, "all")) {
    return(list(
      methods = methods_tex,
      vignette = vignette,
      rmd = rmd_dir,
      rmd_files = file.path(rmd_dir, rmd_files),
      example = example
    ))
  }

  if (identical(what, "methods")) {
    return(c(methods = methods_tex))
  }
  if (identical(what, "vignette")) {
    return(c(vignette = vignette))
  }
  if (identical(what, "rmd")) {
    return(c(rmd = rmd_dir))
  }
  if (identical(what, "example")) {
    return(c(example = example))
  }
  if (what %in% names(rmd_files)) {
    path <- file.path(rmd_dir, rmd_files[[what]])
    return(setNames(path, what))
  }

  stop("Unknown 'what': ", what, call. = FALSE)
}
