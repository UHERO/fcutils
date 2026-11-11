# **************************
# plotting functions ----
# **************************
#' Interactive plot with level and growth rate
#'
#' @param x ts-boxable object to plot (e.g. time series of history, oldsol, sol)
#' @param rng_start start of zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr TRUE (default) for growth on the same period a year earlier,
#'   FALSE for growth on the previous period
#' @param gr_1 only show growth for the first series (default)
#'
#' @return a browsable htmltools tag list holding the dygraph plots
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   plot_1()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_1()
plot_1 <- function(
  x,
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  height = 300,
  width = 900,
  yoy_gr = TRUE,
  gr_1 = TRUE
) {
  rng_start <- to_ymd(rng_start)
  rng_end <- to_ymd(rng_end)
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  ser_names <- attr(x_mod, "ser_names")

  ser_names_pct <- if (gr_1) {
    stringr::str_glue("{ser_names}%")[1]
  } else {
    stringr::str_glue("{ser_names}%")
  }

  ser_plot <- x_mod %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) {
        tsbox::ts_c(., tsbox::ts_pcy(.))
      } else {
        tsbox::ts_c(., tsbox::ts_pc(.))
      }
    } %>%
    # magrittr::extract(, 1:length(c(ser_names, ser_names_pct))) %>%
    tsbox::ts_pick(1:length(c(ser_names, ser_names_pct))) %>%
    magrittr::set_names(c(ser_names, ser_names_pct)) %>%
    tsbox::ts_dygraphs(
      main = ser_names[1],
      group = "comp",
      height = height,
      width = width
    ) %>%
    dygraphs::dyAxis("y", label = "% change") %>%
    dygraphs::dyAxis(
      "y2",
      label = "level",
      drawGrid = FALSE,
      independentTicks = TRUE
    ) %>%
    {
      if (length(ser_names_pct) > 1) {
        dygraphs::dyGroup(., ser_names_pct, axis = "y") %>%
          dygraphs::dyMultiColumnGroup(ser_names_pct)
      } else {
        dygraphs::dyBarSeries(., ser_names_pct, axis = "y")
      }
    } %>%
    {
      if (length(ser_names) > 1) {
        dygraphs::dyGroup(., ser_names, strokeWidth = 2, axis = "y2")
      } else {
        dygraphs::dySeries(., ser_names, strokeWidth = 2, axis = "y2")
      }
    } %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set1")) %>%
    dygraphs::dyOptions(
      colors = uh_colors_light[1:length(ser_names_pct)] %>%
        as.vector() %>%
        c(uh_colors[1:length(ser_names)] %>% as.vector())
    ) %>%
    dygraphs::dyLegend(show = "follow", labelsSeparateLines = TRUE) %>%
    # dygraphs::dyLegend(width = 0.9 * width, show = "auto", labelsSeparateLines = FALSE) %>%
    dygraphs::dyRangeSelector(
      dateWindow = c(rng_start, rng_end),
      height = 30,
      strokeColor = "red"
    )

  # render the dygraphs objects using htmltools
  ser_plot %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Interactive lineplot with two axes
#'
#' @param x ts-boxable object to plot. The first series goes on the left
#'   axis and all others on the right axis; a single series gets the left
#'   axis only.
#' @param rng_start start of zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#'
#' @return a browsable htmltools tag list holding the dygraph plot
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   plot_2ax()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_2ax()
plot_2ax <- function(
  x,
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  height = 300,
  width = 900
) {
  rng_start <- to_ymd(rng_start)
  rng_end <- to_ymd(rng_end)
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  ser_names <- attr(x_mod, "ser_names")

  ser_plot <- x_mod %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(
      main = ser_names[1] %>% stringr::str_replace_all("@.*", ""),
      group = "comp",
      height = height,
      width = width
    ) %>%
    dygraphs::dyAxis("y", label = "series 1") %>%
    dygraphs::dySeries(ser_names[1], strokeWidth = 2, axis = "y") %>%
    {
      rest <- ser_names[-1]
      if (length(rest) == 0) {
        .
      } else {
        g <- dygraphs::dyAxis(
          .,
          "y2",
          label = "series 2+",
          drawGrid = FALSE,
          independentTicks = TRUE
        )
        if (length(rest) > 1) {
          dygraphs::dyGroup(g, rest, strokeWidth = 2, axis = "y2")
        } else {
          dygraphs::dySeries(g, rest, strokeWidth = 2, axis = "y2")
        }
      }
    } %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set1")) %>%
    dygraphs::dyOptions(
      colors = uh_colors[1:length(ser_names)] %>% as.vector()
    ) %>%
    dygraphs::dyLegend(show = "follow", labelsSeparateLines = TRUE) %>%
    # dygraphs::dyLegend(width = 0.9 * width, show = "auto", labelsSeparateLines = FALSE) %>%
    dygraphs::dyRangeSelector(
      dateWindow = c(rng_start, rng_end),
      height = 30,
      strokeColor = "red"
    )

  # render the dygraphs objects using htmltools
  ser_plot %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Interactive plot with level and growth rate for forecast series
#'
#' @param x ts-boxable object to plot (e.g. current fcst, old fcst, history).
#'   The first three series get their own line styles; later ones get the
#'   default style.
#' @param rng_start start of zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param add_table should a data table be appended to the plot? (default = TRUE)
#' @param table_start start of table range ("YYYY-MM-DD"; default rng_start;
#'   NULL for the start of the data)
#' @param table_end end of table range ("YYYY-MM-DD"; default rng_end; NULL
#'   for the end of the data)
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr TRUE (default) for growth on the same period a year earlier,
#'   FALSE for growth on the previous period
#' @param rate if TRUE, the bars show the change in points instead of the %
#'   change (for rates such as the unemployment rate; default = FALSE)
#'
#' @return a browsable htmltools tag list holding the dygraph plots
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   plot_fc()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_fc()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI") |>
#'   plot_fc(rate = TRUE)
plot_fc <- function(
  x,
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  add_table = TRUE,
  table_start = rng_start,
  table_end = rng_end,
  height = 300,
  width = 900,
  yoy_gr = TRUE,
  rate = FALSE
) {
  rng_start <- to_ymd(rng_start)
  rng_end <- to_ymd(rng_end)
  if (!is.null(table_start)) table_start <- to_ymd(table_start)
  if (!is.null(table_end)) table_end <- to_ymd(table_end)

  # formatting of the date axis
  getQuarter <- 'function(d) {
      d = d || new Date();
      var n = [1,2,3,4];
      var qr = "Q" + n[Math.floor(d.getMonth() / 3)];
//      var twoDigitsYear = parseInt(d.getFullYear().toString().substr(2,2), 10);
      var Year = parseInt(d.getFullYear().toString(), 10);
//     return [twoDigits+qr];
      return [Year+qr];
}'

  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  ser_names <- attr(x_mod, "ser_names")

  # date axis labels by frequency: 2026 for years, 2026Q1 for quarters, the
  # dygraphs default for months and shorter periods
  days <- stats::median(as.numeric(diff(sort(unique(x_mod$time)))))
  date_label <- if (is.na(days) || days < 80) {
    NULL
  } else if (days < 300) {
    getQuarter
  } else {
    "function(d) { return d.getFullYear().toString(); }"
  }

  # bars: % change, or the change in points for rates
  gr_fun <- if (rate) {
    if (yoy_gr) tsbox::ts_diffy else tsbox::ts_diff
  } else {
    if (yoy_gr) tsbox::ts_pcy else tsbox::ts_pc
  }
  gr_name <- if (rate) {
    stringr::str_glue("{ser_names[1]} chg")
  } else {
    stringr::str_glue("{ser_names[1]}%")
  }

  # series to plot
  ser_to_plot <- x_mod %>%
    tsbox::ts_xts() %>%
    {
      tsbox::ts_c(., gr_fun(.[, 1]))
    } %>%
    magrittr::set_names(c(ser_names, gr_name))

  # generate the plot
  ser_plot <- ser_to_plot %>%
    tsbox::ts_dygraphs(
      main = ser_names[1],
      group = "comp",
      height = height,
      width = width
    ) %>%
    dygraphs::dyAxis("y", label = "level") %>%
    dygraphs::dyAxis(
      "y2",
      label = if (rate) "Chg" else "% Chg",
      drawGrid = FALSE,
      independentTicks = TRUE
    ) %>%
    {
      if (is.null(date_label)) {
        .
      } else {
        dygraphs::dyAxis(., "x", axisLabelFormatter = htmlwidgets::JS(date_label))
      }
    } %>%
    dygraphs::dySeries(
      stringr::str_glue("{ser_names[1]}"),
      axis = "y",
      strokeWidth = 2,
      color = uh_colors[1]
    ) %>%
    {
      if (length(ser_names) > 1) {
        dygraphs::dySeries(
          .,
          stringr::str_glue("{ser_names[2]}"),
          axis = "y",
          strokePattern = "dashed",
          strokeWidth = 2,
          color = uh_colors[2]
        )
      } else {
        .
      }
    } %>%
    {
      if (length(ser_names) > 2) {
        dygraphs::dySeries(
          .,
          stringr::str_glue("{ser_names[3]}"),
          axis = "y",
          strokePattern = "dashed",
          strokeWidth = 2,
          color = uh_colors[3]
        )
      } else {
        .
      }
    } %>%
    dygraphs::dySeries(
      gr_name,
      axis = "y2",
      stepPlot = TRUE,
      fillGraph = TRUE,
      color = uh_colors[1]
    ) %>%
    dygraphs::dyRangeSelector(
      dateWindow = c(rng_start, rng_end),
      height = 30,
      strokeColor = "red"
    ) %>%
    dygraphs::dyLegend(show = "follow", labelsSeparateLines = TRUE)

  if (add_table) {
    # generate a table with the data
    ser_tbl <- ser_to_plot %>%
      tsbox::ts_tbl() %>%
      tidyr::drop_na() %>%
      tsbox::ts_span(table_start, table_end) %>%
      dplyr::mutate(value = round(.data$value, 2)) %>%
      tsbox::ts_wide() %>%
      reactable::reactable(
        highlight = TRUE,
        compact = TRUE,
        height = height,
        width = width,
        defaultPageSize = 1000
      )
    ser_plot <- list(ser_plot, ser_tbl)
  }

  # render the dygraphs objects and tables using htmltools
  ser_plot %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Two-panel plot of levels and growth rates
#'
#' @param x ts-boxable object to plot
#' @param rng_start start of the zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr TRUE (default) for growth on the same period a year earlier,
#'   FALSE for growth on the previous period
#' @param gr_bar show bars or line (default) for the growth series
#'
#' @return a browsable htmltools tag list with two dygraph plots (level,
#'   growth)
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   plot_comp_2()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_comp_2()
plot_comp_2 <- function(
  x,
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  height = 300,
  width = 900,
  yoy_gr = TRUE,
  gr_bar = FALSE
) {
  plot_comp(x, NULL, rng_start, rng_end, height, width, yoy_gr, gr_bar)
}


#' Three-panel plot of levels, index, and growth rates
#'
#' @param x ts-boxable object to plot
#' @param base_date base period for the index panel, where the index equals 1
#'   ("YYYY-MM-DD"; default 10 years before today). It works as `base_per` in
#'   [index_to_base()]: the first observation on or after the date.
#' @param rng_start start of the zoom range ("YYYY-MM-DD")
#' @param rng_end end of the zoom range ("YYYY-MM-DD")
#' @param height height of a single panel (px)
#' @param width width of a single panel (px)
#' @param yoy_gr TRUE (default) for growth on the same period a year earlier,
#'   FALSE for growth on the previous period
#' @param gr_bar show bars or line (default) for the growth series
#'
#' @return a browsable htmltools tag list with three dygraph plots (level,
#'   index, growth)
#' @export
#'
#' @examples
#' monthly_data_example |>
#'   plot_comp_3()
#' quarterly_data_example |>
#'   tsbox::ts_long() |>
#'   tsbox::ts_pick("E_TU_HI", "ECT_HI", "EMN_HI") |>
#'   plot_comp_3()
plot_comp_3 <- function(
  x,
  base_date = as.character(Sys.Date() - lubridate::years(10)),
  rng_start = as.character(Sys.Date() - lubridate::years(10)),
  rng_end = as.character(Sys.Date() + lubridate::years(2)),
  height = 300,
  width = 900,
  yoy_gr = TRUE,
  gr_bar = FALSE
) {
  plot_comp(x, base_date, rng_start, rng_end, height, width, yoy_gr, gr_bar)
}


#' Comparison plots: level, optionally an index, and growth (plot_comp_2()
#' and plot_comp_3())
#'
#' @param base_date NULL for no index panel, else its base period
#' @noRd
plot_comp <- function(
  x,
  base_date,
  rng_start,
  rng_end,
  height,
  width,
  yoy_gr,
  gr_bar
) {
  if (!is.null(base_date)) {
    base_date <- to_ymd(base_date)
  }
  rng_start <- to_ymd(rng_start)
  rng_end <- to_ymd(rng_end)
  # convert to long format and return additional details
  x_mod <- conv_long(x, ser_info = TRUE)

  ser_names <- attr(x_mod, "ser_names")

  plot_level <-
    x_mod %>%
    tsbox::ts_xts() %>%
    tsbox::ts_dygraphs(
      main = "Level",
      group = "comp",
      height = height,
      width = width
    ) %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(colors = uh_colors[1:length(ser_names)] %>% as.vector()) # %>%
  # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  # plot_level[["elementId"]] <- ser_names %>% extract(1) %>% str_extract("^.*@")
  plot_index <- if (!is.null(base_date)) {
    x_mod %>%
      tsbox::ts_xts() %>%
      tsbox::ts_index(base = base_date) %>%
      tsbox::ts_dygraphs(
        main = "Index",
        group = "comp",
        height = height,
        width = width
      ) %>%
      # dygraphs::dyRebase(value = 100) %>%
      dygraphs::dyLegend(width = width * 0.90) %>%
      dygraphs::dyOptions(colors = uh_colors[1:length(ser_names)] %>% as.vector()) # %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2"))
  }
  plot_growth <-
    x_mod %>%
    tsbox::ts_xts() %>%
    {
      if (yoy_gr) tsbox::ts_pcy(.) else tsbox::ts_pc(.)
    } %>%
    tsbox::ts_dygraphs(
      main = "Growth",
      group = "comp",
      height = height,
      width = width
    ) %>%
    {
      if (gr_bar) dygraphs::dyBarChart(.) else .
    } %>%
    dygraphs::dyLegend(width = width * 0.90) %>%
    dygraphs::dyOptions(
      colors = uh_colors[1:length(ser_names)] %>% as.vector()
    ) %>%
    # dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(ser_names), "Set2")) %>%
    dygraphs::dyRangeSelector(
      dateWindow = c(rng_start, rng_end),
      height = 30,
      strokeColor = "red"
    )

  # render the dygraphs objects using htmltools
  # list() keeps a NULL plot_index; purrr::compact() drops it
  list(plot_level, plot_index, plot_growth) %>%
    purrr::compact() %>%
    htmltools::tagList() %>%
    htmltools::browsable()
}


#' Make pandoc available to rmarkdown, falling back on Quarto's copy
#'
#' @return TRUE if pandoc is available
#' @noRd
find_pandoc <- function() {
  if (rmarkdown::pandoc_available()) {
    return(TRUE)
  }
  quarto <- Sys.which("quarto")
  if (quarto == "") {
    return(FALSE)
  }
  # Quarto ships pandoc under bin/tools/<arch>/ (bin/tools/ on some builds)
  bin <- dirname(normalizePath(quarto))
  for (dir in file.path(bin, "tools", c(R.version$arch, ""))) {
    rmarkdown::find_pandoc(cache = FALSE, dir = dir)
    if (rmarkdown::pandoc_available()) {
      return(TRUE)
    }
  }
  FALSE
}


#' Save a list of interactive plots to html
#'
#' @param plot_list a list of plots generated by fcutils::plot_xxxx() functions
#' @param save_loc location to save the plots to, including file name
#'
#' @return the path of the saved file, invisibly
#' @export
#'
#' @details The page embeds its scripts with pandoc. When rmarkdown finds no
#' pandoc (as under Rscript or VS Code, where RStudio's copy is not known),
#' the function looks for the copy that ships with Quarto. Without either, it
#' saves the page with its scripts in a folder next to it and says so.
#'
#' @examples
#' # hold the plots in a list
#' plot_out <- list()
#' for (i in monthly_data_example[2:3] |> names()) {
#'   plot_out[[i]] <- plot_1(
#'     monthly_data_example |> tsbox::ts_long() |>
#'       dplyr::filter(stringr::str_detect(id, i)),
#'     rng_start = as.character(Sys.Date() - lubridate::years(5)),
#'     rng_end = as.character(Sys.Date() + lubridate::years(7)),
#'     width = 1500, height = 650, yoy_gr = TRUE
#'   )
#' }
#' # specify location of the output
#' save_loc <- file.path(tempdir(), stringr::str_c("plots_", Sys.Date(), ".html"))
#' # combine a list of plots into a single html
#' @examplesIf interactive()
#' plot_out |> save_plot_list(save_loc)
save_plot_list <- function(plot_list, save_loc) {
  if (!find_pandoc()) {
    lib_dir <- stringr::str_c(
      stringr::str_remove(basename(save_loc), "\\.[^.]*$"),
      "_files"
    )
    message(
      "pandoc not found: ", save_loc, " needs the folder ", lib_dir,
      " next to it. Install Quarto or run from RStudio to embed the scripts."
    )
    plot_list %>%
      htmltools::tagList() %>%
      htmltools::browsable() %>%
      htmltools::save_html(file = save_loc, libdir = lib_dir)
    return(invisible(save_loc))
  }
  # save list of plots to html
  plot_list %>%
    htmltools::tagList() %>%
    htmltools::browsable() %>%
    htmltools::save_html(file = save_loc)
  # fix the html
  readr::read_lines(file = save_loc)[-1] %>%
    stringr::str_replace_all(c(
      "<style>" = "\\\n<title>Plots</title>\\\n<style>",
      "\\}</style>" = "font-family:Arial,Helvetica,sans-serif;font-size:medium;}</style>"
    )) %>%
    readr::write_lines(file = save_loc)
  # incorporate dependencies into html
  rmarkdown::pandoc_self_contained_html(input = save_loc, output = save_loc)
  # check out htmlwidgets
}
