# The bank that make_xts() and addf() fill when no dates are given. The
# snapshots were recorded with the bank the package had built in until
# version 0.0.0.9044.
withr::local_options(
  list(
    fcutils.bnk_start = as.Date("1970-01-01"),
    fcutils.bnk_end = as.Date("2100-12-31")
  ),
  .local_envir = testthat::teardown_env()
)
