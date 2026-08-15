#' @keywords internal
"_PACKAGE"

##' FLBacktest: Backtest MSE for FLR
##'
##' Retrospective Management Strategy Evaluation via S4 generic methods that
##' applications can reuse. The design follows the short-cut versus full-MP
##' backtest workflow.
##'
##' **Core MSE**
##' \itemize{
##'   \item \code{\link{hcrICES}} — year-by-year ICES hockey-stick feedback
##'   \item \code{\link{backtest}} — single OM or scenario-grid runner
##' }
##'
##' **Observation / error**
##' \itemize{
##'   \item \code{\link{shortcut}} — short-cut assessment error
##'   \item \code{\link{retroBias}} — retrospective SSB bias from peels
##'   \item \code{\link{samIndex}}, \code{\link{samIndexSSB}}, \code{\link{samIndexRec}}
##' }
##'
##' **Conditioning helpers**
##' \itemize{
##'   \item \code{\link{hcrParams}} — HCR \code{FLPar} from reference points
##'   \item \code{\link{lorenzenM}} — mass-at-age natural mortality
##'   \item \code{\link{recDevs}} — simulated recruitment deviations
##'   \item \code{\link{omIters}} — OM iterations from assessment uncertainty
##' }
##'
##' **Methods and supplementary materials for papers**
##' \itemize{
##'   \item Vignette \code{methods-backtest} — peer-review Methods text
##'   \item \code{inst/methods/methods-backtest.tex} — LaTeX fragment to
##'     \code{\\input\{\}} into journal manuscripts
##'   \item \code{inst/rmd/01}–\code{04_*.Rmd} — shared supplementary notes
##'     (conditioning, OEM, MP, performance statistics)
##'   \item \code{\link{backtestMaterials}} — locate installed paths
##' }
##'
##' @author Laurence Kell
##' @name FLBacktest-package
##' @aliases FLBacktest FLBacktest-package
NULL
