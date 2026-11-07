# Step 4 of the plan: extend() reproduces extsol's anchor_path(),
# anchor_growth(), compound_forward() and smooth_loess(). The expected
# results were computed with those helpers in forecastr and saved in
# fixtures/extend_ref.rds (inputs and outputs).

ref <- function() readRDS(test_path("fixtures", "extend_ref.rds"))

# extsol's spline filled the years before history with the first value;
# extend() leaves them empty, so compare from the start of history
from_start <- function(out, x) out[paste0(find_start(x), "/")]

test_that("type = 'level' with the spline is anchor_path(method = 'stine')", {
  ref_bank()
  r <- ref()
  out <- extend(r$inputs$ur, val = r$inputs$ur_asm, interp = "stine")
  same_values(out, from_start(r$expected$ur_stine, r$inputs$ur))
  expect_true(all(is.na(out[paste0("/", find_start(r$inputs$ur) - 1)])))
})

test_that("type = 'level', linear, is anchor_path()", {
  ref_bank()
  r <- ref()
  same_values(extend(r$inputs$ur, val = r$inputs$ur_asm), r$expected$ur_linear)
  # the same anchors as at and val
  same_values(
    extend(r$inputs$ur, at = names(r$inputs$ur_asm), val = unname(r$inputs$ur_asm)),
    r$expected$ur_linear
  )
})

test_that("stale anchors are dropped with a message", {
  ref_bank()
  r <- ref()
  expect_message(
    out <- extend(r$inputs$ur, val = c("2025Q4" = 9, r$inputs$ur_asm)),
    "ignoring anchors"
  )
  same_values(out, r$expected$ur_stale)
})

test_that("annual spline without holding the last anchor", {
  ref_bank()
  r <- ref()
  out <- extend(r$inputs$lfp_jp_a, val = r$inputs$lfp_asm, interp = "stine", hold_last = FALSE)
  same_values(out, from_start(r$expected$lfp_a_stine_nohold, r$inputs$lfp_jp_a))
})

test_that("anchors on a zero series with a given last date", {
  ref_bank()
  r <- ref()
  zero <- make_xts(per = "quarter", val = 0)
  out <- suppressMessages(
    extend(zero, val = r$inputs$gr_add_asm, last = r$inputs$last_lf, interp = "stine")
  )
  same_values(out, r$expected$gr_add)
})

test_that("type = 'growth' is anchor_growth()", {
  ref_bank()
  r <- ref()
  out <- extend(r$inputs$cpi, val = r$inputs$cpi_asm, type = "growth")
  same_values(out, r$expected$cpi_growth)
})

test_that("type = 'chain', join = 'shift', smoothed, is extsol's N_AT_JP path", {
  ref_bank()
  r <- ref()
  out <- extend(
    r$inputs$n_jp,
    path = r$inputs$un_q,
    type = "chain",
    join = "shift",
    smooth = list(span = 0.2)
  )
  same_values(out, r$expected$n_shift_smooth)
})

test_that("type = 'chain' on annual data, with and without smoothing", {
  ref_bank()
  r <- ref()
  x <- r$inputs$n15_jp_a
  same_values(
    extend(x, path = r$inputs$un15_a, type = "chain", smooth = list(span = 0.2)),
    r$expected$n15_chain_smooth
  )
  same_values(
    extend(x, path = r$inputs$un15_a, type = "chain"),
    r$expected$n15_chain
  )
})

test_that("extend() keeps the input's class and name", {
  ref_bank()
  x <- make_xts(2024.1, val = c(3.8, 4.0, 4.2, 4.1))
  colnames(x) <- "UR"
  tbl <- tsbox::ts_tbl(x)
  out <- extend(tbl, at = 2026.4, val = 4.4)
  expect_s3_class(out, "tbl_df")
  wide <- extend(tibble::tibble(time = zoo::index(x), UR = as.numeric(x)), at = 2026.4, val = 4.4)
  expect_named(wide, c("time", "UR"))
  expect_equal(colnames(extend(x, at = 2026.4, val = 4.4)), "UR")
})

test_that("extend() explains wrong input", {
  ref_bank()
  x <- make_xts(2024.1, val = c(3.8, 4.0, 4.2, 4.1))
  expect_error(extend(x), "anchors")
  expect_error(extend(x, type = "chain", val = c("2026Q4" = 1)), "needs a path")
  expect_error(extend(x, at = c(2026.4, 2027.4), val = 1), "differ in length")
  expect_error(extend(x, val = c(1, 2)), "name the values")
})
