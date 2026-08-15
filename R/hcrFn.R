# Internal hockey-stick HCR: set F from SSB status, convert to TAC via short-term fwd

hcrFn <- function(object, eql, params,
                  stkYrs = max(as.numeric(dimnames(stock(object))$year)) - 2,
                  refYrs = max(as.numeric(dimnames(catch(object))$year)) - 1,
                  hcrYrs = max(as.numeric(dimnames(stock(object))$year)),
                  maxF = 2,
                  nyrs = 3,
                  sr_deviances = NULL,
                  perfect = FALSE,
                  ...) {

  setTarget <- function(params, stk, hcrYrs) {

    if (length(dim(params)) == 2) {
      flag <- c(1, 2)[1 + as.numeric(c(stk) > c(params["btrig", "lower"]))]
    } else {
      params <- propagate(FLPar(params), dim(stk)[6])
      flag <- rep(1, dim(stk)[6])
    }

    a <- FLPar(a = array(
      (params["ftar", flag] - params["fmin", flag]) /
        (params["btrig", flag] - params["bmin", flag]),
      c(1, dim(stk)[6])))
    b <- FLPar(b = array(
      params["ftar", flag] - a * params["btrig", flag],
      c(1, dim(stk)[6])))

    rtn <- (stk %*% a)
    rtn <- FLCore::sweep(rtn, 2:6, b, "+")

    fmin <- FLQuant(c(params["fmin", flag]),
                    dimnames = list(iter = seq(dim(stk)[6])))
    ftar <- FLQuant(c(params["ftar", flag]),
                    dimnames = list(iter = seq(dim(stk)[6])))

    for (i in seq(dims(object)$iter)) {
      FLCore::iter(rtn, i)[] <- max(FLCore::iter(rtn, i), FLCore::iter(fmin, i))
      FLCore::iter(rtn, i)[] <- min(FLCore::iter(rtn, i), FLCore::iter(ftar, i))
    }

    rtn <- window(rtn, end = max(hcrYrs))
    if (hcrYrs > (stkYrs + 1))
      rtn[, ac((stkYrs + 1):(hcrYrs - 1))] <-
        fbar(object)[, ac((stkYrs + 1):(hcrYrs - 1))]

    for (i in hcrYrs)
      rtn[, ac(i)] <- rtn[, dimnames(rtn)$year[1]]

    chk <- data.frame(
      hcrYrs = hcrYrs,
      ssb = c(stk),
      f = c(rtn[, ac(hcrYrs)]))

    list(rtn[, ac(hcrYrs)], chk)
  }

  ## Window first so status years exist. Perfect information (err = NULL)
  ## uses true SSB in the advice year when it is on the stock; otherwise
  ## the assessment year. An OEM still uses lagged SSB and mean-filled STF.
  object <- window(object, end = max(as.numeric(hcrYrs)))
  yrs <- dimnames(ssb(object))$year
  status_yr <- ac(stkYrs)
  if (perfect) {
    hcr_yr <- ac(min(as.numeric(hcrYrs)))
    if (hcr_yr %in% yrs && any(is.finite(c(ssb(object)[, hcr_yr]))))
      status_yr <- hcr_yr
  }
  status <- FLCore::apply(ssb(object)[, status_yr], 6, mean)

  res <- setTarget(params, status, hcrYrs)
  rtn <- res[[1]]
  hvt <- rtn

  ## Short-term projection: fill biology with recent means
  if (!perfect) {

    for (i in slotNames(FLStock())[-c(1, 4, 7, 10, 18:20)]){
      slot(object, i)[, ac(min(hcrYrs))] <-
        apply(slot(object, i)[, ac(stkYrs - (seq(nyrs) - 1))], c(1, 6), mean)}

    if (length(hcrYrs) > 1)
      for (i in slotNames(FLStock())[-c(1, 4, 7, 10, 18:20)])
        for (j in hcrYrs[-1])
          slot(object, i)[, ac(j)] <- slot(object, i)[, ac(hcrYrs[1])]

    if (stkYrs < (hcrYrs - 1))
      object <- FLasher::fwd(object,
                    fbar = fbar(object)[, ac(max(stkYrs + 1):min(as.numeric(hcrYrs) - 1))],
                    sr = eql)
  }

  hvt[is.na(hvt)] <- 6.6666

  if (!perfect)
    rtn <- catch(FLasher::fwd(object, fbar = hvt, sr = eql))[, ac(hcrYrs)]
  else
    rtn <- catch(FLasher::fwd(object, fbar = hvt, sr = eql,
                     residuals = sr_deviances))[, ac(hcrYrs)]

  rtn[] <- rep(c(apply(rtn, c(3:6), mean)), each = dim(rtn)[2])

  list(rtn, res[[2]])
}
