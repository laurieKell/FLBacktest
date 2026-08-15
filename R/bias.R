#' @rdname retroBias
#' @param years Integer vector of peel terminal years (default \code{2006:2020}).
#'
#' @return A \code{data.frame} with columns \code{.id}, \code{iYr}, \code{now},
#'   \code{then}.
#'
#' @export
setMethod("retroBias", signature(object = "list"),
          function(object, years = 2006:2020, ...) {

            if (!requireNamespace("plyr", quietly = TRUE))
              stop("retroBias() requires package plyr")

            ## Single set of peels (FLStocks or list of FLStock)
            if (is(object, "FLStocks") ||
                (length(object) > 0 && is(object[[1]], "FLStock"))) {
              object <- list(OM = object)
            }

            plyr::ldply(object, function(x) {
              plyr::mdply(data.frame(iYr = years), function(iYr) {
                data.frame(
                  now = c(ssb(x[[ac(iYr)]])[, ac(iYr - 1), drop = TRUE]),
                  then = c(ssb(x[[ac(iYr - 1)]])[, ac(iYr - 1), drop = TRUE]))
              })
            })
          })

#' @rdname retroBias
#' @export
setMethod("retroBias", signature(object = "FLStocks"),
          function(object, years = 2006:2020, ...) {
            retroBias(list(OM = object), years = years, ...)
          })

#' @title Summarise retrospective bias ratios
#'
#' @param object A \code{data.frame} from \code{\link{retroBias}}.
#' @param minYear Drop peels at or before this year (default 2005).
#'
#' @return A \code{data.frame} of mean, median and SD of \code{now/then} by OM.
#'
#' @export
biasSummary <- function(object, minYear = 2005) {

  if (!requireNamespace("plyr", quietly = TRUE))
    stop("biasSummary requires package plyr")

  dat <- object[object$iYr > minYear, , drop = FALSE]
  plyr::ddply(dat, ".id", function(df) {
    r <- df$now / df$then
    data.frame(
      Mean_Ratio = mean(r, na.rm = TRUE),
      Median_Ratio = stats::median(r, na.rm = TRUE),
      SD_Ratio = stats::sd(r, na.rm = TRUE),
      N = length(r))
  })
}

##' @rdname retroBias
##' @export
biasFromRetros <- function(retros, years = 2006:2020, ...) {
  retroBias(retros, years = years, ...)
}
