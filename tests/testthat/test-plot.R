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

test_that("plot_fc(rate = TRUE) shows the change in points", {
  ur <- make_xts(2020.1, val = c(4.0, 4.4, 4.2, 4.6, 5.0))
  dy <- function(...) {
    plot_fc(ur, rng_start = "2020-01-01", rng_end = "2021-01-01", add_table = FALSE, ...)[[1]]
  }
  pts <- dy(rate = TRUE)
  expect_equal(round(pts$x$data[[3]][5], 10), 1.0) # 5.0 - 4.0, y/y
  expect_equal(pts$x$attrs$labels[3], "value chg")
  pc <- dy()
  expect_equal(round(pc$x$data[[3]][5], 10), 25) # (5.0 / 4.0 - 1) * 100
  expect_equal(pc$x$attrs$labels[3], "value%")
})

test_that("save_plot_list() saves without pandoc and says so", {
  local_mocked_bindings(find_pandoc = function() FALSE)
  f <- withr::local_tempfile(fileext = ".html")
  plt <- plot_fc(q_wide, rng_start = "2015-01-01", rng_end = "2020-01-01", add_table = FALSE)
  expect_message(save_plot_list(list(plt), f), "pandoc not found")
  expect_true(file.exists(f))
  expect_true(dir.exists(file.path(dirname(f), sub("\\.html$", "_files", basename(f)))))
})
