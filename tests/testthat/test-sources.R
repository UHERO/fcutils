# Step 4 of the plan: get_series_src() and set_api_key(). The network tests
# compare with values extsol downloaded (fixtures/sources_ref.rds), for
# series that are not revised: unadjusted CPIs, observed prices and rates.
# They run only online, and only with the source's key.

src_ref <- function() readRDS(test_path("fixtures", "sources_ref.rds"))

skip_without_key <- function(var) {
  skip_if(!nzchar(Sys.getenv(var)), paste(var, "is not set"))
}

expect_matches_ref <- function(new, ref) {
  both <- dplyr::inner_join(new, ref, by = "time")
  expect_equal(nrow(both), nrow(ref))
  expect_equal(both$value.x, both$value.y, tolerance = 1e-8)
}

test_that("set_api_key() sets the key for the session", {
  withr::local_envvar(EIA_API_KEY = NA)
  set_api_key("eia", "test-key")
  expect_equal(Sys.getenv("EIA_API_KEY"), "test-key")
  expect_error(set_api_key("nonsense", "x"), "should be one of")
  expect_error(set_api_key("eia", ""), "non-empty")
})

test_that("a missing key gives a message saying how to set it", {
  withr::local_envvar(EIA_API_KEY = NA)
  local_mocked_bindings(key_list = function(...) stop("no keyring"), .package = "keyring")
  expect_error(api_key("eia"), "Add EIA_API_KEY to .Renviron")
})

test_that("keys never appear in error messages", {
  expect_equal(
    mask_keys("https://x.org/a?appId=SECRET1&b=1&api_key=SECRET2"),
    "https://x.org/a?appId=<key>&b=1&api_key=<key>"
  )
  warns <- character()
  err <- withCallingHandlers(
    tryCatch(
      read_json_retry("https://invalid.invalid/x?api_key=SECRET", tries = 1),
      error = function(e) conditionMessage(e)
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(grepl("SECRET", err))
  expect_match(err, "api_key=<key>", fixed = TRUE)
  expect_false(any(grepl("SECRET", warns)))
})

test_that("get_series_src() checks its arguments", {
  expect_error(get_series_src("X", "fred", freq = "quarter"), "freq")
  expect_error(get_series_src("X", "nowhere"), "should be one of")
})

test_that("FRED: levels and monthly means of daily prices", {
  skip_if_offline()
  skip_without_key("FRED_API_KEY")
  ref <- src_ref()
  cpi <- get_series_src("CPIAUCNS", "fred", start = 2019, format = "long")
  expect_named(cpi, c("id", "time", "value"))
  expect_matches_ref(cpi, ref$fred_CPIAUCNS)
  wti <- get_series_src("DCOILWTICO", "fred", freq = "month", start = 2020, format = "long")
  expect_matches_ref(wti, ref$fred_DCOILWTICO_month)
  expect_true(max(wti$time) < lubridate::floor_date(Sys.Date(), "month"))
  wide <- get_series_src(c("CPIAUCNS", "CPIAUCSL"), "fred", start = 2019)
  expect_named(wide, c("time", "CPIAUCNS", "CPIAUCSL"))
})

test_that("StatCan and the Bank of Canada", {
  skip_if_offline()
  ref <- src_ref()
  expect_matches_ref(
    get_series_src("v41690973", "statcan", start = 2019, format = "long"),
    ref$statcan_v41690973
  )
  expect_matches_ref(
    get_series_src("FXMUSDCAD", "boc", start = 2019, format = "long"),
    ref$boc_FXMUSDCAD
  )
})

test_that("e-Stat", {
  skip_if_offline()
  skip_without_key("ESTAT_API_KEY")
  ref <- src_ref()
  cpi <- get_series_src(
    "0004052037", "estat",
    filters = list(cdTab = "1", cdCat01 = "0001", cdArea = "00000"),
    format = "long"
  )
  expect_matches_ref(cpi, ref$estat_cpi_nsa)
  expect_error(
    get_series_src("0004052037", "estat", filters = list(cdTab = "1", cdArea = "00000")),
    "several values per date"
  )
})

test_that("EIA", {
  skip_if_offline()
  skip_without_key("EIA_API_KEY")
  ref <- src_ref()
  wti <- get_series_src(
    "WTIPUUS", "eia",
    filters = list(route = "steo", frequency = "monthly"),
    format = "long"
  )
  expect_matches_ref(wti, ref$eia_steo_WTIPUUS)
  aeo <- get_series_src(
    "prce_NA_NA_NA_cr_wti_usa_ndlrpbrl", "eia",
    filters = list(route = "aeo/2025", frequency = "annual", scenario = "ref2025"),
    format = "long"
  )
  expect_equal(lubridate::month(aeo$time[1]), 1)
  expect_true(all(aeo$value > 0))
  expect_error(get_series_src("WTIPUUS", "eia"), "route")
})

test_that("rename_udaman() takes the location just before the frequency", {
  expect_equal(
    rename_udaman(c("SH_US@HI.M", "SH_JP__HON_M", "SH_US_HI_M", "E_NF_AT_HI_Q")),
    c("SH_US@HI.M", "SH_JP@HON.M", "SH_US@HI.M", "E_NF@HI.Q")
  )
  expect_equal(rename_udaman("ECT_HI", freq = "Q"), "ECT@HI.Q")
  expect_length(rename_udaman("E_NF_HI5", freq = "M"), 5)
})
