#' start of data range in the data bank
#' @name bnk_start
#' @format scalar yyyy-mm-dd date
#' @docType data
#' @author Peter Fuleky \email{fuleky@hawaii.edu}
#' @source author
#' @references \url{uhero.hawaii.edu}
#' @keywords data
# NULL
"bnk_start"
bnk_start <- lubridate::ymd("1970-01-01")

#' end of data range in the data bank
#' @name bnk_end
#' @format scalar yyyy-mm-dd date
#' @docType data
#' @author Peter Fuleky \email{fuleky@hawaii.edu}
#' @source author
#' @references \url{uhero.hawaii.edu}
#' @keywords data
# NULL
"bnk_end"
# bnk_end <- lubridate::ymd("2060-12-31")
bnk_end <- lubridate::ymd("2100-12-31")


#' pattern in series names that should be aggregate as sum
#' @name sum_pattern
#' @format string of patterns in series names that should be aggregated via sum
#' @docType data
#' @author Peter Fuleky \email{fuleky@hawaii.edu}
#' @source author
#' @references \url{uhero.hawaii.edu}
#' @keywords data
# NULL
"sum_pattern"
sum_pattern <- stringr::str_flatten(
  c(
    "^KB",
    "KN",
    "KP",
    "KR",
    "NT",
    "PC(?=(DM|IT))",
    "TD",
    "TG",
    "TR(?!MS)",
    "UI",
    "VA(?!DC)",
    "VDAY",
    "VEXP(?!P)",
    "VIS",
    "VP",
    "VS",
    "VX(?!P)"
  ),
  collapse = "|^"
)

#' colors defined in the UHERO Style Guide
#' @name uh_colors
#' @format vector of hex color codes
#' @docType data
#' @author Peter Fuleky \email{fuleky@hawaii.edu}
#' @source author
#' @references \url{uhero.hawaii.edu}
#' @keywords data
# NULL
# plot <- plot +
#   geom_point(
#     data = data,
#     aes(x = Low, y = Category),
#     color = uh_colors["uhcyan"],
#     size = point_size
#   ) +
#   geom_point(
#     data = data,
#     aes(x = Median, y = Category),
#     color = uh_colors["uhblue"],
#     size = point_size
#   ) +
#   geom_point(
#     data = data,
#     aes(x = High, y = Category),
#     color = uh_colors["uhorange"],
#     size = point_size
#   )
"uh_colors"
uh_colors <- c(
  "#1D667F",
  "#F6A01B",
  "#9BBB59",
  "#8064A2",
  "#7EC4CA",
  "#505050",
  "red"
) |>
  magrittr::set_names(c(
    "uhblue",
    "uhorange",
    "uhgreen",
    "uhpurple",
    "uhcyan",
    "uhgray",
    "uhred"
  ))

#' lighter versions of UHERO colors
#' @name uh_colors_light
#' @format vector of hex color codes
#' @docType data
#' @author Peter Fuleky \email{fuleky@hawaii.edu}
#' @source author
#' @references \url{uhero.hawaii.edu}
#' @keywords data
# NULL
# oi <- uh_colors
# colorspace::swatchplot(
#   "-60%" = colorspace::lighten(oi, 0.6), # light color
#   "-40%" = colorspace::lighten(oi, 0.4),
#   "-20%" = colorspace::lighten(oi, 0.2),
#   "  0%" = oi,
#   " 20%" =  colorspace::darken(oi, 0.2),
#   " 40%" =  colorspace::darken(oi, 0.4),
#   off = c(0, 0)
# )
"uh_colors_light"
uh_colors_light <- c(
  "#90C3DC",
  "#FFD9BD",
  "#CAEA8D",
  "#D2B8F6",
  "#AAEEF4",
  "#B5B5B5",
  "#FFB8B8"
) |>
  magrittr::set_names(c(
    "uhlblue",
    "uhlorange",
    "uhlgreen",
    "uhlpurple",
    "uhlcyan",
    "uhlgray",
    "uhlred"
  ))

#' transparent versions of UHERO colors
#' @name uh_colors_50
#' @format vector of hex color codes
#' @docType data
#' @author Peter Fuleky \email{fuleky@hawaii.edu}
#' @source author
#' @references \url{uhero.hawaii.edu}
#' @keywords data
# NULL
# colorspace::adjust_transparency(uh_colors, alpha = 0.5)
"uh_colors_50"
uh_colors_50 <- c(
  "#1D667F80",
  "#F6A01B80",
  "#9BBB5980",
  "#8064A280",
  "#7EC4CA80",
  "#50505080",
  "#FF000080"
) |>
  magrittr::set_names(c(
    "uhtblue",
    "uhtorange",
    "uhtgreen",
    "uhtpurple",
    "uhtcyan",
    "uhtgray",
    "uhtred"
  ))

#' quarterly data for examples
#' @name quarterly_data_example
#' @format tibble of quarterly data
#' @docType data
#' @author Peter Fuleky \email{fuleky@hawaii.edu}
#' @source author
#' @references \url{uhero.hawaii.edu}
#' @keywords data
# NULL
# quarterly_data_example <- get_series_exp(74) |> tidyr::drop_na()
# quarterly_data_example |> save(file = "data/quarterly_data_example.rda")
"quarterly_data_example"

#' monthly data for examples
#' @name monthly_data_example
#' @format tibble of monthly data
#' @docType data
#' @author Peter Fuleky \email{fuleky@hawaii.edu}
#' @source author
#' @references \url{uhero.hawaii.edu}
#' @keywords data
# NULL
# monthly_data_example <- get_series(c(
#   "VISNS@HI.M",
#   "VAPNS@HI.M",
#   "VADCNS@HI.M"
# )) |>
#   tidyr::drop_na()
# monthly_data_example |> save(file = "data/monthly_data_example.rda")
"monthly_data_example"

#' daily data for examples
#' @name daily_data_example
#' @format tibble of daily data
#' @docType data
#' @author Peter Fuleky \email{fuleky@hawaii.edu}
#' @source author
#' @references \url{uhero.hawaii.edu}
#' @keywords data
# NULL
# daily_data_example <- get_series(c("VISPNS@HI.D", "VAPNS@HI.D")) |>
#   tidyr::drop_na()
# daily_data_example |> save(file = "data/daily_data_example.rda")
"daily_data_example"

# usethis::use_data(
#   bnk_start,
#   bnk_end,
#   sum_pattern,
#   uh_colors,
#   uh_colors_light,
#   uh_colors_50,
#   internal = FALSE,
#   overwrite = TRUE
# )
# usethis::use_data(
#   quarterly_data_example,
#   monthly_data_example,
#   daily_data_example,
#   internal = FALSE,
#   overwrite = TRUE
# )
