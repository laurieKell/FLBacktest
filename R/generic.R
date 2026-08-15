# S4 generics for FLBacktest
# Applications dispatch on FLR classes; package supplies the methods.

#' @rdname hcrICES
#' @export
setGeneric("hcrICES", function(object, eql, ...) standardGeneric("hcrICES"))

#' @title Backtest MSE
#'
#' @description
#' Generic for running a historical-period feedback MSE. Methods are provided
#' for a single operating model (`FLStock` + `FLBRP`) and for a scenario grid
#' (`data.frame`).
#'
#' @param object An \code{FLStock}, or a scenario \code{data.frame}.
#' @param eql An \code{FLBRP}, or missing when \code{object} is a scenario grid.
#' @param ... Passed to methods (e.g. \code{sr_deviances}, \code{params},
#'   \code{err}, \code{method = c("perfect", "shortcut", "full")}).
#'
#' @export
setGeneric("backtest", function(object, eql, ...) standardGeneric("backtest"))

#' @title Short-cut assessment error
#'
#' @description
#' Generic for building multiplicative short-cut assessment error from
#' retrospective bias ratios or a bias \code{data.frame}.
#'
#' @param object Numeric bias ratios, or a bias \code{data.frame}.
#' @param ... Passed to methods (\code{start}, \code{end}, \code{nits}, ...).
#'
#' @export
setGeneric("shortcut", function(object, ...) standardGeneric("shortcut"))

#' @title Retrospective assessment bias
#'
#' @description
#' Generic for computing retrospective SSB bias from peels. Named
#' \code{retroBias} to avoid masking \code{FLCore::bias}.
#'
#' @param object A list of peels (\code{FLStocks}) or a list of such lists by OM.
#' @param ... Passed to methods.
#'
#' @export
setGeneric("retroBias", function(object, ...) standardGeneric("retroBias"))

#' @title SAM survey index from an FLStock
#' @param object An \code{FLStock}.
#' @param ... Passed to methods.
#' @export
setGeneric("samIndex", function(object, ...) standardGeneric("samIndex"))

#' @title SAM SSB index from an FLStock
#' @param object An \code{FLStock}.
#' @param ... Passed to methods.
#' @export
setGeneric("samIndexSSB", function(object, ...) standardGeneric("samIndexSSB"))

#' @title SAM recruitment index from an FLStock
#' @param object An \code{FLStock}.
#' @param ... Passed to methods.
#' @export
setGeneric("samIndexRec", function(object, ...) standardGeneric("samIndexRec"))

#' @title Summarise backtest trajectories
#' @param object An \code{FLStock} or \code{hcrICES} / \code{backtest} result list.
#' @param ... Passed to methods.
#' @export
setGeneric("backtestResults", function(object, ...)
  standardGeneric("backtestResults"))

#' @title Build ICES hockey-stick HCR parameters
#'
#' @description
#' Generic for constructing \code{FLPar} HCR parameters
#' (\code{ftar}, \code{btrig}, \code{fmin}, \code{bmin}, \code{blim}) from
#' reference-point objects.
#'
#' @param object An \code{FLQuants} or \code{list} of reference points.
#' @param ... Passed to methods.
#'
#' @export
setGeneric("hcrParams", function(object, ...) standardGeneric("hcrParams"))

#' @title Lorenzén mass-at-age natural mortality
#'
#' @description
#' Generic for \eqn{M = m_1 W^{m_2}} natural mortality from mass-at-age.
#'
#' @param object An \code{FLQuant} of weights, or an \code{FLStock}.
#' @param ... Passed to methods (\code{params} as \code{FLPar} with \code{m1},
#'   \code{m2}).
#'
#' @export
setGeneric("lorenzenM", function(object, ...) standardGeneric("lorenzenM"))

#' @title Simulate recruitment deviations
#'
#' @description
#' Generic for simulating aleatory / epistemic recruitment deviations from
#' stock–recruit residuals.
#'
#' @param object An \code{FLQuant} of log residuals, or an \code{FLSR}.
#' @param ... Passed to methods.
#'
#' @export
setGeneric("recDevs", function(object, ...) standardGeneric("recDevs"))

#' @title OM iterations from assessment uncertainty
#'
#' @description
#' Generic for expanding an \code{FLStock} with assessment uncertainty
#' (e.g. from SAM).
#'
#' @param object An \code{FLStock}, or a SAM fit object.
#' @param ... Passed to methods.
#'
#' @export
setGeneric("omIters", function(object, ...) standardGeneric("omIters"))

#' @title Leave-one-out residuals
#'
#' @description
#' Generic for leave-one-out (LOO) residuals. For an \code{FLSR}, each year is
#' dropped in turn, the stock–recruit model is re-fitted, and the residual for
#' the held-out year is computed from the predicted recruitment at that year's
#' SSB.
#'
#' @param object An \code{FLSR} or \code{FLStock}.
#' @param ... Passed to methods.
#'
#' @export
setGeneric("looResiduals", function(object, ...)
  standardGeneric("looResiduals"))
