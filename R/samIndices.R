#' @rdname samIndex
#' @param idx Optional \code{FLQuant} of survey index values; default uses
#'   \code{stock.n(object)}.
#' @param name Character name for the index.
#' @param index_q Optional catchability \code{FLQuant}.
#' @param sel_pattern Optional selectivity \code{FLQuant}.
#'
#' @return An \code{FLIndex} suitable for SAM.
#'
#' @export
setMethod("samIndex", signature(object = "FLStock"),
          function(object, idx = NULL, name = "Survey",
                   index_q = NULL, sel_pattern = NULL, ...) {

            ages <- dimnames(stock.n(object))$age
            years <- dimnames(stock.n(object))$year
            iters <- dims(object)$iter

            if (is.null(idx)) {
              idx <- stock.n(object)
              message("Using stock.n as index values; replace with survey data.")
            }

            if (!identical(dimnames(idx)$age, ages) ||
                !identical(dimnames(idx)$year, years))
              stop("idx dimensions must match stock age and year dimensions")

            if (is.null(index_q)) {
              index_q <- FLQuant(0.1,
                                dimnames = list(age = ages, year = years,
                                                iter = seq(iters)))
              if (iters > 1 && dims(index_q)$iter == 1)
                index_q <- propagate(index_q, iters)
            }

            if (is.null(sel_pattern)) {
              sel_pattern <- FLQuant(1.0,
                                     dimnames = list(age = ages, year = years,
                                                     iter = seq(iters)))
              if (iters > 1 && dims(sel_pattern)$iter == 1)
                sel_pattern <- propagate(sel_pattern, iters)
            }

            age_min <- min(as.numeric(ages))
            age_max <- max(as.numeric(ages))

            out <- FLIndex(
              name = name,
              index = idx,
              index.q = index_q,
              sel.pattern = sel_pattern,
              range = c(min = age_min, max = age_max,
                        minfbar = age_min, maxfbar = age_max,
                        startf = 0.5, endf = 0.5))

            if (iters > 1 && dims(out)$iter == 1)
              out <- propagate(out, iters)
            else if (iters == 1 && dims(out)$iter > 1)
              out <- iter(out, 1)

            out
          })

#' @rdname samIndexSSB
#' @param name Index name.
#' @param desc Index description.
#' @param startf,endf Survey timing fractions.
#' @param age Age label for the aggregated index.
#' @export
setMethod("samIndexSSB", signature(object = "FLStock"),
          function(object, name = "SSB", desc = "Index equal to SSB",
                   startf=0.0, endf=0.01, ...) {
            idxSsb <- ssb(object)
            
            years <- dimnames(idxSsb)$year
            iters <- dims(object)$iter

            index_var <- FLQuant(1e-6,
                                dimnames = list(age  = "all",
                                                year = years,
                                                iter = seq(iters)))
            if (iters > 1 && dims(index_var)$iter == 1)
              index_var <- propagate(index_var, iters)

            out <- FLIndex(
              index     = idxSsb,
              index.var = index_var,
              range     = c(startf= startf, endf=endf),
              name = name, desc = desc)

            if (iters > 1 && dims(out)$iter == 1)
              out <- propagate(out, iters)
            else if (iters == 1 && dims(out)$iter > 1)
              out <- iter(out, 1)

            out
          })

#' @rdname samIndexRec
#' @param name Index name.
#' @param desc Index description.
#' @param startf,endf Survey timing fractions.
#' @param age Age label for recruitment.
#' @export
setMethod("samIndexRec", signature(object = "FLStock"),
          function(object, name = "Recs",
                   desc = "Index equal to Recruitments",
                   startf = 0.0, endf = 0.01, age = 1, ...) {

            idxRec <- rec(object)
            dimnames(idxRec)[["age"]] <- as.character(age)
            years <- dimnames(idxRec)$year
            iters <- dims(object)$iter

            index_var <- FLQuant(1e-6,
                                dimnames = list(age = as.character(age),
                                                year = years,
                                                iter = seq(iters)))
            if (iters > 1 && dims(index_var)$iter == 1)
              index_var <- propagate(index_var, iters)

            out <- FLIndex(
              index = idxRec,
              index.var = index_var,
              range = c(min = age, max = age, startf = startf, endf = endf),
              name = name, desc = desc)

            if (iters > 1 && dims(out)$iter == 1)
              out <- propagate(out, iters)
            else if (iters == 1 && dims(out)$iter > 1)
              out <- iter(out, 1)

            out
          })

#' @title Validate FLIndex structure for SAM
#' @param idx An \code{FLIndex} or \code{FLIndices}.
#' @return \code{TRUE} if valid.
#' @export
validateSamIndex <- function(idx) {

  if (!inherits(idx, "FLIndex") && !inherits(idx, "FLIndices"))
    stop("idx must be FLIndex or FLIndices")

  if (inherits(idx, "FLIndices")) {
    for (i in seq_along(idx))
      validateSamIndex(idx[[i]])
    return(TRUE)
  }

  if (is.null(index(idx)))
    stop("FLIndex must have 'index' slot with survey index values")

  na_count <- sum(is.na(index(idx)) | is.nan(index(idx)))
  if (na_count > 0) {
    total <- length(index(idx))
    warning("Index contains ", na_count, " NA/NaN values (",
            round(100 * na_count / total, 2), "%)")
  }

  zero_count <- sum(index(idx) <= 0, na.rm = TRUE)
  if (zero_count > 0)
    warning("Index contains ", zero_count, " zero or negative values")

  if (is.na(range(idx)["min"]) || is.na(range(idx)["max"]))
    warning("FLIndex range (min/max ages) not properly set")

  TRUE
}
