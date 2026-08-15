#' @rdname looResiduals
#' @param log Logical; if \code{TRUE} (default), residuals are
#'   \code{log(rec) - log(hat)}, matching \code{FLSR} with \code{logerror=TRUE}.
#' @param years Optional character or numeric years to leave out. Default is
#'   all years with finite \code{rec} and \code{ssb}.
#' @param quiet Suppress \code{fmle} console output.
#'
#' @return An \code{FLQuant} of LOO residuals (same dims as \code{rec(object)}).
#'   Years that fail to refit are \code{NA}.
#'
#' @examples
#' \dontrun{
#' data(ple4)
#' sr <- fmle(as.FLSR(ple4, model = "geomean"))
#' loo <- looResiduals(sr)
#' plot(loo)
#' }
#'
#' @export
setMethod("looResiduals", signature(object = "FLSR"),
          function(object, log = TRUE, years = NULL, quiet = TRUE, ...) {

            rec0 <- rec(object)
            ssb0 <- ssb(object)
            out <- rec0
            out[] <- NA

            yrs <- if (is.null(years)) {
              dimnames(rec0)$year
            } else {
              ac(years)
            }

            ok <- yrs[
              is.finite(c(rec0[, yrs])) &
                is.finite(c(ssb0[, yrs])) &
                c(rec0[, yrs]) > 0 &
                c(ssb0[, yrs]) > 0
            ]

            fitOne <- function(sr) {
              if (quiet) {
                suppressWarnings(invisible(capture.output(
                  sr <- fmle(sr, ...))))
              } else {
                sr <- fmle(sr, ...)
              }
              sr
            }

            for (y in ok) {
              sr <- object
              rec(sr)[, y] <- NA
              sr <- tryCatch(fitOne(sr), error = function(e) NULL)
              if (is.null(sr)) next

              hat <- tryCatch(
                c(predict(sr, ssb = ssb0[, y])),
                error = function(e) NA_real_)
              if (!is.finite(hat) || hat <= 0) next

              obs <- c(rec0[, y])
              out[, y] <- if (isTRUE(log)) log(obs) - log(hat) else obs - hat
            }

            units(out) <- if (isTRUE(log)) "log" else units(rec0)
            out
          })

#' @rdname looResiduals
#' @param model SR model passed to \code{as.FLSR} (default \code{"geomean"}).
#' @export
setMethod("looResiduals", signature(object = "FLStock"),
          function(object, model = "geomean", log = TRUE, years = NULL,
                   quiet = TRUE, ...) {

            sr <- as.FLSR(object, model = model)
            if (quiet) {
              suppressWarnings(invisible(capture.output(
                sr <- fmle(sr))))
            } else {
              sr <- fmle(sr)
            }
            looResiduals(sr, log = log, years = years, quiet = quiet, ...)
          })
