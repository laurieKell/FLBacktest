#' @rdname shortcut
#' @param start First year of simulated error.
#' @param end Last year of simulated error.
#' @param nits Number of iterations.
#' @param drop Number of leading bias observations to drop (default 5).
#' @param seed Optional random seed.
#'
#' @return An \code{FLQuant} of multiplicative assessment error.
#'
#' @examples
#' \dontrun{
#' err <- shortcut(exp(rnorm(20, 0, 0.1)), start = 2005, end = 2020, nits = 100)
#' err <- shortcut(bias, id = "M=0.15", start = 2005, end = 2020, nits = 100)
#' }
#'
#' @export
setMethod("shortcut", signature(object = "numeric"),
          function(object, start, end, nits = 100, drop = 5, seed = NULL, ...) {

            if (!is.null(seed))
              set.seed(seed)

            x <- as.numeric(object)
            if (drop > 0 && length(x) > drop)
              x <- x[-(seq_len(drop))]

            nsim <- nits * (end - start + 1)

            if (requireNamespace("forecast", quietly = TRUE)) {
              fit <- forecast::auto.arima(stats::ts(x))
              sims <- as.numeric(stats::simulate(fit, nsim = nsim))
            } else {
              lx <- log(pmax(x, 1e-6))
              lx <- lx - mean(lx, na.rm = TRUE)
              phi <- if (length(lx) > 2)
                stats::cor(lx[-1], lx[-length(lx)]) else 0.5
              phi <- max(min(phi, 0.95), -0.95)
              s <- stats::sd(lx, na.rm = TRUE)
              if (!is.finite(s) || s == 0) s <- 0.1
              e <- numeric(nsim)
              e[1] <- stats::rnorm(1, 0, s)
              for (i in 2:nsim)
                e[i] <- phi * e[i - 1] + stats::rnorm(1, 0, s * sqrt(1 - phi^2))
              sims <- exp(e)
            }

            FLQuant(sims, dimnames = list(year = start:end, iter = seq_len(nits)))
          })

#' @rdname shortcut
#' @param id Optional \code{.id} level when \code{object} is a bias data.frame.
#' @export
setMethod("shortcut", signature(object = "data.frame"),
          function(object, start, end, nits = 100, drop = 5, seed = NULL,
                   id = NULL, ...) {

            if (!is.null(id)) {
              if (!".id" %in% names(object))
                stop("bias data.frame has no .id column")
              object <- object[object$.id == id, , drop = FALSE]
            }
            if (!all(c("now", "then") %in% names(object)))
              stop("bias data.frame must have columns 'now' and 'then'")

            shortcut(object$now / object$then,
                     start = start, end = end, nits = nits,
                     drop = drop, seed = seed, ...)
          })

##' @rdname shortcut
##' @export
shortCutErr <- function(bias, start, end, nits = 100, drop = 5, seed = NULL, ...) {
  shortcut(bias, start = start, end = end, nits = nits,
           drop = drop, seed = seed, ...)
}
