test_that("write_tsd() writes the TSD layout", {
  x <- quarterly_data_example[, c("time", "E_NF_HI", "ECT_HI")]
  file <- withr::local_tempfile(fileext = ".tsd")
  write_tsd(x, file)
  lines <- strsplit(readr::read_file(file), "\r\n")[[1]]
  expect_true(all(nchar(lines) == 80))
  expect_equal(trimws(lines[1]), "E_NF_HI")
  # second line: write date, first and last period, frequency
  n <- sum(!is.na(x$E_NF_HI))
  first <- min(x$time[!is.na(x$E_NF_HI)])
  expect_match(lines[2], paste0(lubridate::year(first), "0", lubridate::quarter(first), "00"))
  expect_match(lines[2], "Q  0")
  # values: five per line, so the second series starts after ceiling(n / 5) lines
  expect_equal(trimws(lines[3 + ceiling(n / 5)]), "ECT_HI")
  expect_equal(
    as.numeric(substr(lines[3], 1, 15)),
    signif(x$E_NF_HI[!is.na(x$E_NF_HI)][1], 7)
  )
})

test_that("write_tsd() puts descriptions in columns 17-80", {
  x <- quarterly_data_example[, c("time", "E_NF_HI", "ECT_HI")]
  file <- withr::local_tempfile(fileext = ".tsd")
  write_tsd(x, file, descriptions = c(E_NF_HI = "Nonfarm payroll jobs"))
  lines <- strsplit(readr::read_file(file), "\r\n")[[1]]
  expect_equal(substr(lines[1], 1, 16), "E_NF_HI         ")
  expect_equal(trimws(substr(lines[1], 17, 80)), "Nonfarm payroll jobs")
  # a series with no description gets a blank field
  ect <- grep("^ECT_HI", lines)
  expect_equal(trimws(substr(lines[ect], 17, 80)), "")
  expect_warning(
    write_tsd(x, file, descriptions = c(E_NF_HI = strrep("x", 70))),
    "exceeds 64 characters"
  )
})

test_that("write_tsd() keeps interior NAs in place as the TSD missing value", {
  x <- conv_xts(quarterly_data_example)[, "E_NF_HI"]["2019/2021"]
  x[c(1, 6)] <- NA # leading NA dropped, 2020Q2 kept as missing
  file <- withr::local_tempfile(fileext = ".tsd")
  write_tsd(x, file)
  lines <- strsplit(readr::read_file(file), "\r\n")[[1]]
  expect_match(lines[2], "2019020020210400Q")
  vals <- as.numeric(unlist(strsplit(trimws(paste(lines[3:5], collapse = " ")), " +")))
  expect_length(vals, 11)
  expect_equal(vals[5], 1e15)
  expect_equal(vals[11], signif(as.numeric(x["2021-10-01"]), 7))
})
