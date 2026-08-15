#' @rdname hcrParams
#' @param fmin Multiplier or value for \code{fmin} relative to \code{ftar}
#'   when built from named reference \code{FLQuants} (default 0.005).
#' @param bmin Value for \code{bmin} (default 0).
#'
#' @details
#' Expected names in \code{FLQuants} / list: \code{fmsy} (or \code{ftar}),
#' \code{msybtrigger} (or \code{btrig}), \code{blim}.
#'
#' @return An \code{FLPar} with params \code{ftar}, \code{btrig}, \code{fmin},
#'   \code{bmin}, \code{blim}.
#'
#' @export
setMethod("hcrParams", signature(object = "FLQuants"),
          function(object, fmin = 0.005, bmin = 0, ...) {

            grab <- function(cands) {
              hit <- intersect(cands, names(object))
              if (length(hit) < 1)
                stop("hcrParams needs one of: ", paste(cands, collapse = ", "))
              object[[hit[1]]]
            }

            ftar <- grab(c("ftar", "fmsy", "Fmsy"))
            btrig <- grab(c("btrig", "msybtrigger", "MSYBtrigger"))
            blim <- grab(c("blim", "Blim"))

            ## Align years then build FLPar (params x year-as-iter)
            qts <- mcf(FLQuants(
              ftar = ftar,
              btrig = btrig,
              fmin = ftar %=% fmin,
              bmin = blim %=% bmin,
              blim = blim))

            yrs <- dimnames(qts[[1]])$year
            mat <- rbind(
              ftar = c(qts[["ftar"]]),
              btrig = c(qts[["btrig"]]),
              fmin = c(qts[["fmin"]]),
              bmin = c(qts[["bmin"]]),
              blim = c(qts[["blim"]]))
            dimnames(mat) <- list(params = rownames(mat), iter = yrs)
            par <- FLPar(mat)

            for (p in c("ftar", "btrig", "blim")) {
              v <- c(par[p])
              if (any(is.na(v)))
                par[p][is.na(par[p])] <- min(v, na.rm = TRUE)
            }

            par
          })

#' @rdname hcrParams
#' @export
setMethod("hcrParams", signature(object = "list"),
          function(object, fmin = 0.005, bmin = 0, ...) {
            hcrParams(FLQuants(object), fmin = fmin, bmin = bmin, ...)
          })

#' @rdname lorenzenM
#' @param params \code{FLPar} with \code{m1} and \code{m2}.
#'
#' @return An \code{FLQuant} of natural mortality.
#'
#' @export
setMethod("lorenzenM", signature(object = "FLQuant"),
          function(object, params = FLPar(m1 = 0.15 / (0.300^-0.288),
                                          m2 = -0.288), ...) {
            params["m1"] %*% (object %^% params["m2"])
          })

#' @rdname lorenzenM
#' @param replaceAge0 If \code{TRUE}, set age-0 stock weights from catch weights
#'   before computing M.
#' @export
setMethod("lorenzenM", signature(object = "FLStock"),
          function(object,
                   params = FLPar(m1 = 0.15 / (0.300^-0.288), m2 = -0.288),
                   replaceAge0 = TRUE, ...) {

            if (replaceAge0 && "0" %in% dimnames(stock.wt(object))$age)
              stock.wt(object)["0"] <- catch.wt(object)["0"]

            lorenzenM(stock.wt(object), params = params, ...)
          })

#' @rdname recDevs
#' @param nits Number of iterations.
#' @param trend If \code{TRUE}, add a lowess trend (epistemic); if
#'   \code{FALSE}, return AR(1) residuals about the trend (aleatory).
#' @param years Optional year window for the ARIMA / AR(1) fit.
#'
#' @return An \code{FLQuant} of log recruitment deviations. Use
#'   \code{exp(recDevs(...))} as \code{sr_deviances} in \code{hcrICES}.
#'
#' @export
setMethod("recDevs", signature(object = "FLQuant"),
          function(object, nits = 100, trend = FALSE, years = NULL, ...) {

            rsd <- object
            if (!is.null(years))
              rsd <- window(rsd, start = min(years), end = max(years))

            dat <- as.data.frame(rsd, drop = TRUE)
            fit <- stats::lowess(dat$data ~ dat$year)
            dat$trend <- fit$y
            dat$residual <- dat$data - dat$trend

            nsim <- nits * dim(rsd)[2]
            fitAr1 <- stats::arima(dat$residual, order = c(1, 0, 0),
                                   include.mean = TRUE)
            simAr1 <- stats::arima.sim(
              model = list(ar = fitAr1$coef["ar1"]),
              n = nsim,
              mean = fitAr1$coef["intercept"],
              sd = sqrt(fitAr1$sigma2))

            recDev <- FLQuant(simAr1,
                              dimnames = list(year = dimnames(rsd)$year,
                                              iter = seq_len(nits)))
            if (trend) {
              tr <- FLQuant(dat$trend, dimnames = dimnames(rsd))
              recDev <- recDev + tr
            }
            recDev
          })

#' @rdname recDevs
#' @export
setMethod("recDevs", signature(object = "FLSR"),
          function(object, nits = 100, trend = FALSE, years = NULL, ...) {
            recDevs(residuals(object), nits = nits, trend = trend,
                    years = years, ...)
          })

#' @rdname omIters
#' @param n Number of iterations.
#' @param idx Optional \code{FLIndices} / \code{FLIndex} for the assessment.
#'   Default uses a near-perfect SSB index via \code{\link{samIndexSSB}}.
#'
#' @details
#' Requires \pkg{FLfse}. Fits SAM, draws uncertainty, and returns a propagated
#' \code{FLStock}.
#'
#' @return An \code{FLStock} with \code{n} iterations.
#'
#' @export
setMethod("omIters", signature(object = "FLStock"),
          function(object, n = 100, idx = NULL, ...) {

            if (!requireNamespace("FLfse", quietly = TRUE))
              stop("omIters() requires package FLfse")

            stock.n(object)[is.na(stock.n(object))] <- 1e3
            stock.n(object) <- qmax(stock.n(object), 1e-2)

            if (is.null(idx)) {
              idxSsb <- samIndexSSB(object)
              index.var(idxSsb) <- index(idxSsb) * 0 + 1e-6
              idx <- FLIndices(ssbIdx = idxSsb)
            }

            fit <- FLfse::FLR_SAM(stk = object, idx = idx, ...)
            unc <- FLfse::SAM_uncertainty(fit = fit, n = n)
            om <- FLfse::SAM2FLStock(fit)
            om <- propagate(om, n)

            stock.n(om) <- unc$stock.n
            stock(om) <- computeStock(om)
            catch.n(om) <- unc$catch.n
            catch(om) <- computeCatch(om)
            harvest(om) <- unc$harvest
            units(harvest(om)) <- "f"

            om
          })
