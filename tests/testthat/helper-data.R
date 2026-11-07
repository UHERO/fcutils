# Small inputs shared by the tests. They come from the package's example data
# so the snapshots do not depend on the network or the date of the run.

q_wide <- quarterly_data_example[, c("time", "E_NF_HI", "ECT_HI", "EMN_HI")]
q_long <- tsbox::ts_long(q_wide)
q_xts <- tsbox::ts_xts(q_long)
m_wide <- monthly_data_example
a_xts <- tsbox::ts_frequency(tsbox::ts_pick(q_xts, "E_NF_HI"), "year")

# Make an object safe to serialize: data frames (tsbox may return
# data.tables, whose internal pointer does not survive serialization) become
# plain data frames, with their original class kept beside them.
plain <- function(x) {
  if (is.data.frame(x)) {
    list(class = class(x), value = as.data.frame(lapply(x, identity)))
  } else if (is.list(x) && !inherits(x, "xts")) {
    lapply(x, plain)
  } else {
    x
  }
}

# snapshot an object by value; "serialize" keeps classes, attributes and dates
snap <- function(x) {
  testthat::expect_snapshot_value(plain(x), style = "serialize")
}

# snapshot short results as readable text
snap_text <- function(x) testthat::expect_snapshot(print(x))

# run quietly: several functions print or message as they work
quiet <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(suppressWarnings(expr)))
  out
}
