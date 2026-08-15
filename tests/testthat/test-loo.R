test_that("looResiduals returns FLQuant for FLSR", {
  data(ple4, package = "FLCore")
  invisible(capture.output(
    sr <- suppressWarnings(fmle(as.FLSR(ple4, model = "geomean")))))
  loo <- looResiduals(sr, years = 1990:1995)
  expect_s4_class(loo, "FLQuant")
  expect_true(any(is.finite(c(loo))))
})

test_that("generics include looResiduals", {
  expect_true(isGeneric("looResiduals"))
})
