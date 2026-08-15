#' @rdname backtestResults
#' @param metrics Named list of functions of an \code{FLStock}, or \code{NULL}
#'   for defaults (SSB, recruits, yield, F).
#' @param drop Drop singleton dimensions when building the data.frame.
#'
#' @return A \code{data.frame} with year, iter and metric columns.
#'
#' @export
setMethod("backtestResults", signature(object = "FLStock"),
          function(object, metrics = NULL, drop = TRUE, ...) {

            if (is.null(metrics))
              metrics <- list(
                SSB = ssb,
                Recruits = function(x) stock.n(x)[1],
                Yield = catch,
                F = fbar)

            qts <- FLQuants(lapply(metrics, function(f) f(object)))
            names(qts) <- names(metrics)
            model.frame(qts, drop = drop)
          })

#' @rdname backtestResults
#' @export
setMethod("backtestResults", signature(object = "list"),
          function(object, metrics = NULL, drop = TRUE, ...) {

            if (length(object) < 1 || !is(object[[1]], "FLStock"))
              stop("list must be an hcrICES/backtest result (FLStock in [[1]])")
            backtestResults(object[[1]], metrics = metrics, drop = drop, ...)
          })
