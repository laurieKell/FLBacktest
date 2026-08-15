utils::globalVariables(c("refpts<-", "index<-", "landings.n<-", "catch.n<-",
                         "catch<-", "stock.n<-"))

#' @title ICES hockey-stick HCR feedback simulation
#'
#' @description
#' Runs a closed-loop backtest of an ICES-style hockey-stick harvest control
#' rule over a historical period. Each year the management procedure observes
#' stock status (perfectly, with short-cut error, or via an assessment),
#' applies the HCR to set a TAC, and projects the operating model forward.
#'
#' @param object An \code{FLStock} operating model.
#' @param eql An \code{FLBRP} with a stock–recruitment relationship used for
#'   projection.
#' @param sr_deviances \code{FLQuant} of multiplicative recruitment deviations.
#' @param params \code{FLPar} HCR parameters: \code{blim}, \code{btrig},
#'   \code{bmin}, \code{ftar}, \code{fmin}.
#' @param wg Optional \code{FLStock} used as the assessment working-group stock
#'   when \code{err} is an \code{FLIndex} (full MSE). Defaults to \code{object}.
#' @param start First year of the feedback loop.
#' @param end Last year of the feedback loop.
#' @param interval Assessment / advice interval (years).
#' @param lag Assessment data lag (years).
#' @param err Observation model. \code{NULL} is perfect information (true OM
#'   status and short-term forecast with recruitment deviations). An
#'   \code{FLQuant} of multiplicative SSB assessment error is the short-cut
#'   OEM; an \code{FLIndex} is a full assessment-based MSE (requires FLfse /
#'   stockassessment).
#' @param implErr Implementation error as an \code{FLQuant}, or \code{0}.
#' @param bndTac Bounds on year-to-year TAC change, e.g. \code{c(0.8, 1.25)}.
#'   Default \code{c(0, Inf)} disables bounds.
#' @param bndWhen Parameter name in \code{params} used as the biomass
#'   threshold above which TAC bounds apply (default \code{"btrig"}).
#' @param bndCap If the advised TAC change exceeds this fraction, bounds are
#'   dropped for that year. Default \code{1e6} effectively disables.
#' @param ... Unused; \code{perfect} is ignored if supplied (\code{err = NULL}
#'   is perfect information).
#'
#' @return A \code{list} with:
#' \itemize{
#'   \item \code{[[1]]} — the projected \code{FLStock}
#'   \item \code{[[2]]} — a \code{data.frame} tracking HCR SSB, F and catch
#' }
#'
#' @author Laurence Kell
#'
#' @seealso \code{\link{backtest}}, \code{\link{shortcut}}
#'
#' @export
#' @rdname hcrICES
setMethod("hcrICES", signature(object = "FLStock", eql = "FLBRP"),
          function(object, eql, sr_deviances, params,
                   wg = NULL,
                   start = max(dimnames(object)$year) - 10,
                   end = start + 10,
                   interval = 1,
                   lag = 1,
                   err = NULL,
                   implErr = 0,
                   bndTac = c(0, Inf),
                   bndWhen = "btrig",
                   bndCap = 1e6, ...) {

            if ("perfect" %in% names(list(...)))
              warning("'perfect' is ignored; err = NULL is perfect information.",
                      call. = FALSE)
            perfect <- is.null(err)

            biasOm <- function(object, eql, err, iYr, lag = 1) {
              stock.n(object) <- stock.n(object) %*% err[, ac(iYr)]
              if (lag > 0)
                object <- FLasher::fwd(object,
                              catch = catch(object)[, ac(iYr - (lag:0))],
                              sr = eql)
              window(object, end = iYr + 1)
            }

            chk <- NULL
            cat("\n==")
            for (iYr in seq(start, end - 1, interval)) {
              cat(iYr, ", ", sep = "")

              stkYrs <- iYr - lag
              refYrs <- iYr
              hcrYrs <- iYr + 1

              refpts(eql)[] <- 1
              refpts(eql) <- propagate(refpts(eql), dim(object)[6])
              
              if ("FLQuant" %in% is(err)) {
                mp <- biasOm(object, eql, err, iYr, lag = hcrYrs - stkYrs - 1)
              } else if ("FLIndex" %in% is(err)) {
                if (!requireNamespace("FLfse", quietly = TRUE))
                  stop("Full MSE with FLIndex requires package FLfse")

                index(err)[, ac(iYr - lag)] <-
                  stock.n(object)[, ac(iYr - lag)] %*% index(err)[, ac(iYr - lag)]

                if (is.null(wg)) {
                  stk <- window(object, end = iYr - lag)
                } else {
                  om <- window(object, end = iYr - lag)
                  stk <- window(wg, end = iYr - lag)
                  landings.n(stk) <- landings.n(om)
                  catch.n(stk) <- catch.n(om)
                  catch(stk) <- computeCatch(stk, slot = "all")
                }

                idx <- err[, ac(dims(err)$minyear:(iYr - lag))]
                mp <- FLfse::FLR_SAM(stk, idx, NA_rm = TRUE)
                mp <- FLfse::SAM2FLStock(mp)
                mp <- fwdWindow(mp, end = hcrYrs)
                mp <- FLasher::fwd(mp,
                         catch = catch(object)[, ac((iYr - lag + 1):hcrYrs)],
                         sr = eql)
              } else {
                mp <- object
              }
              
              if ("FLPar" %in% is(params))
                par <- params[, min(iYr - start + 1, dims(params)$iter), drop = TRUE]
              else
                par <- params
       
              res <- hcrFn(mp, eql, par,
                           stkYrs, refYrs, hcrYrs,
                           perfect = perfect,
                           sr_deviances = sr_deviances)
       
              flag <- c(ssb(mp)[, ac(stkYrs)] > c(FLPar(params)[bndWhen])[1])
              rtn <- res[[1]]
              
              rtn[,,,,, flag] <-
                qmax(rtn, catch(object)[, ac(refYrs)] * bndTac[1])[,,,,, flag]
              rtn[,,,,, flag] <-
                qmin(rtn, catch(object)[, ac(refYrs)] * bndTac[2])[,,,,, flag]

              if (bndCap < 1) {
                ctcRatio <- res[[1]] %/% (catch(object)[, ac(refYrs)])
                flag <- flag & (ctcRatio < (1 - bndCap) | ctcRatio > (1 + bndCap))
                if (any(flag))
                  rtn[,,,,, flag] <- res[[1]][,,,,, flag]
              }

              if ("FLQuant" %in% is(implErr))
                rtn <- rtn %*% (1 + implErr[, dimnames(rtn)$year])

              object <- FLasher::fwd(object, catch = rtn, sr = eql,
                            residuals = sr_deviances)

              chk <- rbind(chk, data.frame(
                iter = factor(seq(dim(rtn)[6]),
                              levels = seq(dim(rtn)[6]),
                              labels = seq(dim(rtn)[6])),
                res[[2]],
                catch = c(rtn[, 1])))
            }

            list(object, chk)
          })
