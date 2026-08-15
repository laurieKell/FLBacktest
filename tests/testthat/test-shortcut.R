test_that("shortcut numeric method returns FLQuant", {
  err <- shortcut(exp(rnorm(20, 0, 0.1)), start = 2005, end = 2010,
                  nits = 5, drop = 2, seed = 1)
  expect_s4_class(err, "FLQuant")
  expect_equal(dim(err)[2], 6)
  expect_equal(dim(err)[6], 5)
})

test_that("hcrParams builds FLPar from FLQuants", {
  refs <- FLQuants(
    fmsy = FLQuant(0.2, dimnames = list(year = 2020)),
    msybtrigger = FLQuant(1e5, dimnames = list(year = 2020)),
    blim = FLQuant(5e4, dimnames = list(year = 2020)))
  par <- hcrParams(refs)
  expect_s4_class(par, "FLPar")
  expect_true(all(c("ftar", "btrig", "fmin", "bmin", "blim") %in%
                    dimnames(par)$params))
})

test_that("lorenzenM works on FLQuant", {
  wt <- FLQuant(0.3, dimnames = list(age = 0:3, year = 2000))
  m <- lorenzenM(wt)
  expect_s4_class(m, "FLQuant")
  expect_equal(dim(m), dim(wt))
})

test_that("generics are registered", {
  expect_true(isGeneric("hcrICES"))
  expect_true(isGeneric("backtest"))
  expect_true(isGeneric("shortcut"))
  expect_true(isGeneric("retroBias"))
  expect_true(isGeneric("samIndex"))
  expect_true(isGeneric("hcrParams"))
  expect_true(isGeneric("lorenzenM"))
  expect_true(isGeneric("recDevs"))
  expect_true(isGeneric("omIters"))
  expect_true(isGeneric("backtestResults"))
})
