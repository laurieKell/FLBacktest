## Application pattern: reuse FLBacktest generics (erp_clean-style)

library(FLCore)
library(FLBRP)
library(FLasher)
library(FLBacktest)

## --- Conditioning helpers (generic methods) ---
# par <- hcrParams(refs)                    # FLQuants of fmsy, msybtrigger, blim
# m(om) <- lorenzenM(om)                    # mass-based M on FLStock
# omU   <- omIters(om, n = 100)             # SAM uncertainty iters
# rsd   <- recDevs(residuals(sr), nits = 100, trend = FALSE)

## --- Short-cut error from retrospective peels ---
# b <- retroBias(retros)                    # list of FLStocks peels by OM
# err <- shortcut(b, id = "M=0.15", start = 2005, end = 2020, nits = 100)

## --- Single-OM backtest ---
# res <- backtest(om, eq, sr_deviances = exp(rsd), params = par,
#                 method = "shortcut", err = err,
#                 start = 2005, end = 2020, bndTac = c(0.8, 1.25))

## --- Scenario grid ---
# scen <- expand.grid(om = names(eqs), regime = names(eqs[[1]]))
# sCut <- backtest(scen, omIters = omIters, eqs = eqs,
#                  sr_deviances = exp(recDevs_random), params = par,
#                  method = "shortcut", err = err, bndTac = c(0.8, 1.25))
# full <- backtest(scen, omIters = omIters, eqs = eqs,
#                  sr_deviances = exp(recDevs_random), params = par,
#                  method = "full", wgName = "M=0.15", bndTac = c(0.8, 1.25))

# backtestResults(sCut[[1]])
