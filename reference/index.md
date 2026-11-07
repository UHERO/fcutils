# Package index

## Get data

- [`get_series()`](https://uhero.github.io/fcutils/reference/get_series.md)
  : Download a set of series from udaman using series names
- [`get_series_exp()`](https://uhero.github.io/fcutils/reference/get_series_exp.md)
  : Download series listed in an export table from udaman
- [`get_series_src()`](https://uhero.github.io/fcutils/reference/get_series_src.md)
  : Download series from outside sources
- [`set_api_key()`](https://uhero.github.io/fcutils/reference/set_api_key.md)
  : Set an API key for a data source
- [`set_udaman_token()`](https://uhero.github.io/fcutils/reference/set_udaman_token.md)
  : Set udaman token for API access
- [`get_var()`](https://uhero.github.io/fcutils/reference/get_var.md) :
  Construct a series name from variable components and retrieve the
  series
- [`rename_udaman()`](https://uhero.github.io/fcutils/reference/rename_udaman.md)
  : Format series names to udaman format (mnemonic@loc.freq)
- [`extract_data()`](https://uhero.github.io/fcutils/reference/extract_data.md)
  : Parse gets output and extract underlying data (GETS model
  development)

## Create and extend series

- [`make_xts()`](https://uhero.github.io/fcutils/reference/make_xts.md)
  : Create xts and fill with values

- [`addf()`](https://uhero.github.io/fcutils/reference/addf.md) : Create
  xts addfactor

- [`extend()`](https://uhero.github.io/fcutils/reference/extend.md) :
  Extend a series past its last observation

- [`multi_chain()`](https://uhero.github.io/fcutils/reference/multi_chain.md)
  :

  Extend "ts-boxable" objects by chaining (extension of
  [`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html))

## Dates and spans

- [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md) :
  Read dates written as years, quarters, months or days
- [`ymd_to_yQq()`](https://uhero.github.io/fcutils/reference/ymd_to_yQq.md)
  : Convert dates from yyyy-mm-dd to yyyyQqq format
- [`p()`](https://uhero.github.io/fcutils/reference/p.md) : Concatenate
  dates to obtain period
- [`py()`](https://uhero.github.io/fcutils/reference/py.md) :
  Concatenate dates formatted as yyyy to obtain period
- [`pq()`](https://uhero.github.io/fcutils/reference/pq.md) :
  Concatenate dates formatted as yyyyQq or yyyy.q to obtain period
- [`pm()`](https://uhero.github.io/fcutils/reference/pm.md) :
  Concatenate dates formatted as yyyyMm or yyyy.m to obtain period
- [`span()`](https://uhero.github.io/fcutils/reference/span.md) :
  Specify span of time series (wrapper around tsbox::ts_span())
- [`find_start()`](https://uhero.github.io/fcutils/reference/find_start.md)
  : Find the date of the first observation (NAs are dropped)
- [`find_end()`](https://uhero.github.io/fcutils/reference/find_end.md)
  : Find the date of the last observation (NAs are dropped)
- [`nqtrs()`](https://uhero.github.io/fcutils/reference/nqtrs.md) :
  Calculate number of quarters between two dates yyyyQq, yyyy.q or
  yyyy-mm-dd
- [`nmons()`](https://uhero.github.io/fcutils/reference/nmons.md) :
  Calculate number of months between two dates yyyyMm or yyyy-mm-dd
- [`qtrs()`](https://uhero.github.io/fcutils/reference/qtrs.md) :
  Convert period in quarters to period in months
- [`set_tsrange()`](https://uhero.github.io/fcutils/reference/set_tsrange.md)
  : Set tsrange for behavioral equations to available data range

## Frequency conversion and interpolation

- [`aggr()`](https://uhero.github.io/fcutils/reference/aggr.md) :
  Aggregate univariate or multivariate time series from low to high
  frequency
- [`faggr()`](https://uhero.github.io/fcutils/reference/faggr.md) :
  Aggregate univariate or multivariate fiscal time series from low to
  high frequency
- [`disagg()`](https://uhero.github.io/fcutils/reference/disagg.md) :
  Disaggregate univariate or multivariate time series from low to high
  frequency
- [`interpol()`](https://uhero.github.io/fcutils/reference/interpol.md)
  : Interpolate univariate or multivariate time series with missing
  values
- [`AtoQ()`](https://uhero.github.io/fcutils/reference/AtoQ.md) : Linear
  interpolation based on AREMOS command reference page 292 (superseded
  by disagg())
- [`QtoA()`](https://uhero.github.io/fcutils/reference/QtoA.md) :
  Aggregate from quarterly to annual frequency (superseded by
  tsbox::ts_frequency())
- [`sum_pattern`](https://uhero.github.io/fcutils/reference/sum_pattern.md)
  : pattern in series names that should be aggregate as sum

## Growth rates and transformations

- [`pcmp()`](https://uhero.github.io/fcutils/reference/pcmp.md) :
  Calculate multi-period average growth
- [`pc_to_pca()`](https://uhero.github.io/fcutils/reference/pc_to_pca.md)
  : Convert quarterly growth to annualized growth
- [`pca_to_pc()`](https://uhero.github.io/fcutils/reference/pca_to_pc.md)
  : Convert annualized growth to quarterly growth
- [`cagr()`](https://uhero.github.io/fcutils/reference/cagr.md) :
  Calculate compund annual growth
- [`yoy_to_lev()`](https://uhero.github.io/fcutils/reference/yoy_to_lev.md)
  : Extend a series using year over year growth
- [`ma()`](https://uhero.github.io/fcutils/reference/ma.md) : Backward
  looking moving average
- [`ts_rebase()`](https://uhero.github.io/fcutils/reference/ts_rebase.md)
  : Rebase series to an index (wrapper around tsbox::ts_index())
- [`fytd_cum()`](https://uhero.github.io/fcutils/reference/fytd_cum.md)
  : Fiscal year to date sum or average
- [`mtd_cum()`](https://uhero.github.io/fcutils/reference/mtd_cum.md) :
  Month to date sum or average
- [`ptd_cum()`](https://uhero.github.io/fcutils/reference/ptd_cum.md) :
  Period to date sum or average
- [`ytd_cum()`](https://uhero.github.io/fcutils/reference/ytd_cum.md) :
  Year to date sum or average
- [`fytd_gr()`](https://uhero.github.io/fcutils/reference/fytd_gr.md) :
  Fiscal year to date growth rate
- [`mtd_gr()`](https://uhero.github.io/fcutils/reference/mtd_gr.md) :
  Month to date growth rate
- [`ptd_gr()`](https://uhero.github.io/fcutils/reference/ptd_gr.md) :
  Period to date growth rate
- [`ytd_gr()`](https://uhero.github.io/fcutils/reference/ytd_gr.md) :
  Year to date growth rate

## Formats and conversion

- [`conv_long()`](https://uhero.github.io/fcutils/reference/conv_long.md)
  :

  Convert "ts-boxable" objects to long format (extension of
  [`tsbox::ts_long()`](https://docs.ropensci.org/tsbox/reference/ts_long.html))

- [`conv_wide()`](https://uhero.github.io/fcutils/reference/conv_wide.md)
  :

  Convert "ts-boxable" objects to wide format (extension of
  [`tsbox::ts_wide()`](https://docs.ropensci.org/tsbox/reference/ts_long.html))

- [`conv_xts()`](https://uhero.github.io/fcutils/reference/conv_xts.md)
  :

  Convert "ts-boxable" objects to xts format (extension of
  [`tsbox::ts_xts()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html))

- [`conv_tslist()`](https://uhero.github.io/fcutils/reference/conv_tslist.md)
  :

  Convert "ts-boxable" objects to tslist (extension of
  [`tsbox::ts_tslist()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html))

- [`is_wide()`](https://uhero.github.io/fcutils/reference/is_wide.md) :
  Check if a data frame is in wide format

- [`copy_tbl()`](https://uhero.github.io/fcutils/reference/copy_tbl.md)
  : Copy a data frame to clipboard (only works on MacOS)

- [`set_attr_tslist()`](https://uhero.github.io/fcutils/reference/set_attr_tslist.md)
  : Set class attribute to tslist

- [`write_tsd()`](https://uhero.github.io/fcutils/reference/write_tsd.md)
  : Save a ts-boxable object in tsd format

## Plots and tables

- [`plot_1()`](https://uhero.github.io/fcutils/reference/plot_1.md) :
  Interactive plot with level and growth rate
- [`plot_2ax()`](https://uhero.github.io/fcutils/reference/plot_2ax.md)
  : Interactive lineplot with two axes
- [`plot_comp_2()`](https://uhero.github.io/fcutils/reference/plot_comp_2.md)
  : Two-panel plot of levels and growth rates
- [`plot_comp_3()`](https://uhero.github.io/fcutils/reference/plot_comp_3.md)
  : Three-panel plot of levels, index, and growth rates
- [`plot_fc()`](https://uhero.github.io/fcutils/reference/plot_fc.md) :
  Interactive plot with level and growth rate for forecast series
- [`gen_table()`](https://uhero.github.io/fcutils/reference/gen_table.md)
  : Generate a table with time series
- [`save_plot_list()`](https://uhero.github.io/fcutils/reference/save_plot_list.md)
  : Save a list of interactive plots to html
- [`uh_colors`](https://uhero.github.io/fcutils/reference/uh_colors.md)
  : colors defined in the UHERO Style Guide
- [`uh_colors_50`](https://uhero.github.io/fcutils/reference/uh_colors_50.md)
  : transparent versions of UHERO colors
- [`uh_colors_light`](https://uhero.github.io/fcutils/reference/uh_colors_light.md)
  : lighter versions of UHERO colors

## Model equations

- [`model_equation()`](https://uhero.github.io/fcutils/reference/model_equation.md)
  : Parse lm() output and convert into bimets equation (GETS model
  development)
- [`update_eqs()`](https://uhero.github.io/fcutils/reference/update_eqs.md)
  : Update a bimets model with new/modified equations

## Example data

- [`daily_data_example`](https://uhero.github.io/fcutils/reference/daily_data_example.md)
  : daily data for examples
- [`monthly_data_example`](https://uhero.github.io/fcutils/reference/monthly_data_example.md)
  : monthly data for examples
- [`quarterly_data_example`](https://uhero.github.io/fcutils/reference/quarterly_data_example.md)
  : quarterly data for examples

## Helpers

- [`` `%+=%` ``](https://uhero.github.io/fcutils/reference/grapes-plus-equals-grapes.md)
  : In place addition
- [`` `%>%` ``](https://uhero.github.io/fcutils/reference/pipe.md) :
  Pipe operator
- [`fcutils`](https://uhero.github.io/fcutils/reference/fcutils-package.md)
  [`fcutils-package`](https://uhero.github.io/fcutils/reference/fcutils-package.md)
  : fcutils: Time Series and Forecast Utilities
