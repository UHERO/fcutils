# The data behind plot_fc(), with fixed ranges so the result does not depend
# on the date of the run.

test_that("plot_fc() data today", {
  out <- plot_fc(
    q_wide,
    rng_start = "2015-01-01", rng_end = "2020-01-01",
    table_start = "2015-01-01", table_end = "2020-01-01"
  )
  plt <- out[[1]][[1]]
  tbl <- out[[1]][[2]]
  snap(list(
    data = plt$x$data,
    labels = plt$x$attrs$labels,
    series = plt$x$attrs$series,
    table = tbl$x$tag$attribs$data
  ))
})
