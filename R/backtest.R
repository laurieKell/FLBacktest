#' @rdname backtest
#' @param sr_deviances Multiplicative recruitment deviations.
#' @param params \code{FLPar} of HCR parameters.
#' @param method Character: \code{"perfect"}, \code{"shortcut"}, or \code{"full"}.
#' @param err Short-cut error (\code{FLQuant}) when \code{method = "shortcut"}.
#' @param start,end,interval,lag Timing arguments.
#' @param bndTac TAC bounds.
#' @param wg,wgName Working-group stock for full MSE.
#' @param indexCv Index CV for full MSE.
#' @param nits Iterations for full MSE.
#'
#' @export
setMethod("backtest", signature(object = "FLStock", eql = "FLBRP"),
          function(object, eql, sr_deviances, params,
                   method = c("perfect", "shortcut", "full"),
                   err = NULL,
                   start = max(dimnames(object)$year) - 10,
                   end = start + 10,
                   interval = 1, lag = 1,
                   bndTac = c(0, Inf),
                   wg = NULL, wgName = "M=0.15",
                   indexCv = 0.1, nits = NULL, ...) {

            method <- match.arg(method)

            if (method == "perfect") {
              return(hcrICES(object, eql, sr_deviances = sr_deviances,
                             params = params, start = start, end = end,
                             interval = interval, lag = lag,
                             err = NULL, bndTac = bndTac, ...))
            }

            if (method == "shortcut") {
              if (is.null(err))
                stop("method = 'shortcut' requires err (FLQuant)")
              return(hcrICES(object, eql, sr_deviances = sr_deviances,
                             params = params, start = start, end = end,
                             interval = interval, lag = lag,
                             err = err, bndTac = bndTac, ...))
            }

            ## full
            localNits <- if (is.null(nits)) max(1L, dims(sr_deviances)$iter) else nits
            stock.n(object)[is.na(stock.n(object))] <- 1e3
            stock.n(object) <- qmax(stock.n(object), 1e-2)
            object <- propagate(object, localNits)

            idx <- samIndex(object)
            index(idx)[, ac((start - lag):end)] <- 1
            index(idx) <- index(idx) %*%
              rlnorm(localNits, index(idx) %=% 0, indexCv) %*%
              rlnorm(localNits, index(idx)[1] %=% 0, indexCv)

            hcrICES(object, eql, sr_deviances = sr_deviances,
                    params = params, wg = wg,
                    start = start, end = end,
                    interval = interval, lag = lag,
                    err = idx, bndTac = bndTac, ...)
          })

#' @rdname backtest
#' @param omIters Named list of \code{FLStock} OMs.
#' @param eqs Nested list of \code{FLBRP}, \code{eqs[[om]][[regime]]}.
#' @param parallel,ncores,tryIt Parallel control.
#'
#' @export
setMethod("backtest", signature(object = "data.frame", eql = "missing"),
          function(object, eql, omIters, eqs, sr_deviances, params,
                   method = c("perfect", "shortcut", "full"),
                   err = NULL,
                   start = 2005, end = 2020,
                   interval = 1, lag = 1,
                   bndTac = c(0, Inf),
                   wg = NULL, wgName = "M=0.15",
                   indexCv = 0.1, nits = NULL,
                   parallel = TRUE, ncores = NULL,
                   tryIt = TRUE, ...) {

            method <- match.arg(method)
            scen <- object

            if (!"om" %in% names(scen) && "OM" %in% names(scen))
              scen$om <- scen$OM
            if (!"regime" %in% names(scen) && "SRR" %in% names(scen))
              scen$regime <- scen$SRR
            if (!all(c("om", "regime") %in% names(scen)))
              stop("scenario data.frame must have columns 'om' and 'regime'")

            if (method == "shortcut" && is.null(err))
              stop("method = 'shortcut' requires err (FLQuant)")

            runOne <- function(id) {
              om <- omIters[[as.character(scen[id, "om"])]]
              eq <- eqs[[as.character(scen[id, "om"])]][[
                as.character(scen[id, "regime"])]]
              recDev <- if (is(sr_deviances, "FLQuant"))
                sr_deviances else exp(sr_deviances)

              localWg <- wg
              if (method == "full" && is.null(localWg)) {
                if (!wgName %in% names(omIters))
                  stop("wg not supplied and wgName '", wgName, "' not in omIters")
                localWg <- omIters[[wgName]]
              }

              args <- list(object = om, eql = eq,
                           sr_deviances = recDev, params = params,
                           method = method, err = err,
                           start = start, end = end,
                           interval = interval, lag = lag,
                           bndTac = bndTac, wg = localWg,
                           indexCv = indexCv, nits = nits)
              args <- c(args, list(...))

              if (tryIt)
                try(do.call(backtest, args), silent = TRUE)
              else
                do.call(backtest, args)
            }

            ids <- seq_len(nrow(scen))
            canParallel <- parallel &&
              requireNamespace("foreach", quietly = TRUE) &&
              requireNamespace("doParallel", quietly = TRUE) &&
              requireNamespace("parallel", quietly = TRUE)

            if (!canParallel)
              return(lapply(ids, runOne))

            if (is.null(ncores))
              ncores <- max(1L, parallel::detectCores() - 1L)

            cl <- parallel::makeCluster(ncores)
            on.exit(parallel::stopCluster(cl), add = TRUE)
            doParallel::registerDoParallel(cl)

            `%dopar%` <- foreach::`%dopar%`
            foreach::foreach(
              id = ids,
              .packages = c("FLCore", "FLBRP", "FLasher", "FLBacktest")
            ) %dopar% runOne(id)
          })

##' Convenience wrappers that call \code{\link{backtest}}
##' @rdname backtest
##' @export
runMSE <- function(scen, omIters, eqs, sr_deviances, params, ...) {
  backtest(scen, omIters = omIters, eqs = eqs,
           sr_deviances = sr_deviances, params = params, ...)
}

##' @rdname backtest
##' @export
runShortCut <- function(scen, omIters, eqs, sr_deviances, params, err, ...) {
  backtest(scen, omIters = omIters, eqs = eqs,
           sr_deviances = sr_deviances, params = params,
           method = "shortcut", err = err, ...)
}

##' @rdname backtest
##' @export
runFull <- function(scen, omIters, eqs, sr_deviances, params, ...) {
  backtest(scen, omIters = omIters, eqs = eqs,
           sr_deviances = sr_deviances, params = params,
           method = "full", ...)
}
