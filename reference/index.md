# Package index

## All functions

- [`AtoQ()`](https://uhero.github.io/fcutils/reference/AtoQ.md) : Linear
  interpolation based on AREMOS command reference page 292 (superseded
  by disagg())

- [`QtoA()`](https://uhero.github.io/fcutils/reference/QtoA.md) :
  Aggregate from quarterly to annual frequency (superseded by
  tsbox::ts_frequency())

- [`addf()`](https://uhero.github.io/fcutils/reference/addf.md) : Create
  xts addfactor

- [`aggr()`](https://uhero.github.io/fcutils/reference/aggr.md) :
  Aggregate univariate or multivariate time series from low to high
  frequency

- [`bnk_end`](https://uhero.github.io/fcutils/reference/bnk_end.md) :
  end of data range in the data bank

- [`bnk_start`](https://uhero.github.io/fcutils/reference/bnk_start.md)
  : start of data range in the data bank

- [`cagr()`](https://uhero.github.io/fcutils/reference/cagr.md) :
  Calculate compund annual growth

- [`conv_long()`](https://uhero.github.io/fcutils/reference/conv_long.md)
  :

  Convert "ts-boxable" objects to long format (extension of
  [`tsbox::ts_long()`](https://docs.ropensci.org/tsbox/reference/ts_long.html))

- [`conv_tslist()`](https://uhero.github.io/fcutils/reference/conv_tslist.md)
  :

  Convert "ts-boxable" objects to tslist (extension of
  [`tsbox::ts_tslist()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html))

- [`conv_wide()`](https://uhero.github.io/fcutils/reference/conv_wide.md)
  :

  Convert "ts-boxable" objects to wide format (extension of
  [`tsbox::ts_wide()`](https://docs.ropensci.org/tsbox/reference/ts_long.html))

- [`conv_xts()`](https://uhero.github.io/fcutils/reference/conv_xts.md)
  :

  Convert "ts-boxable" objects to xts format (extension of
  [`tsbox::ts_xts()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html))

- [`copy_tbl()`](https://uhero.github.io/fcutils/reference/copy_tbl.md)
  : Copy a data frame to clipboard (only works on MacOS)

- [`daily_data_example`](https://uhero.github.io/fcutils/reference/daily_data_example.md)
  : daily data for examples

- [`disagg()`](https://uhero.github.io/fcutils/reference/disagg.md) :
  Disaggregate univariate or multivariate time series from low to high
  frequency

- [`extract_data()`](https://uhero.github.io/fcutils/reference/extract_data.md)
  : Parse gets output and extract underlying data (GETS model
  development)

- [`faggr()`](https://uhero.github.io/fcutils/reference/faggr.md) :
  Aggregate univariate or multivariate fiscal time series from low to
  high frequency

- [`find_end()`](https://uhero.github.io/fcutils/reference/find_end.md)
  : Find the date of the last observation (NAs are dropped)

- [`find_start()`](https://uhero.github.io/fcutils/reference/find_start.md)
  : Find the date of the first observation (NAs are dropped)

- [`fytd_cum()`](https://uhero.github.io/fcutils/reference/fytd_cum.md)
  : Fiscal year to date sum or average

- [`fytd_gr()`](https://uhero.github.io/fcutils/reference/fytd_gr.md) :
  Fiscal year to date growth rate

- [`gen_table()`](https://uhero.github.io/fcutils/reference/gen_table.md)
  : Generate a table with time series

- [`get_series()`](https://uhero.github.io/fcutils/reference/get_series.md)
  : Download a set of series from udaman using series names

- [`get_series_exp()`](https://uhero.github.io/fcutils/reference/get_series_exp.md)
  : Download series listed in an export table from udaman

- [`get_var()`](https://uhero.github.io/fcutils/reference/get_var.md) :
  Construct a series name from variable components and retrieve the
  series

- [`` `%+=%` ``](https://uhero.github.io/fcutils/reference/grapes-plus-equals-grapes.md)
  : In place addition

- [`index()`](https://uhero.github.io/fcutils/reference/index-topic.md)
  : Get indexed series (wrapper around tsbox::ts_index())

- [`interpol()`](https://uhero.github.io/fcutils/reference/interpol.md)
  : Interpolate univariate or multivariate time series with missing
  values

- [`interpol_1()`](https://uhero.github.io/fcutils/reference/interpol_1.md)
  : Interpolate a single vector containing missing values based on a
  pattern

- [`is_wide()`](https://uhero.github.io/fcutils/reference/is_wide.md) :
  Check if a data frame is in wide format

- [`ma()`](https://uhero.github.io/fcutils/reference/ma.md) : Backward
  looking moving average

- [`make_xts()`](https://uhero.github.io/fcutils/reference/make_xts.md)
  : Create xts and fill with values

- [`model_equation()`](https://uhero.github.io/fcutils/reference/model_equation.md)
  : Parse lm() output and convert into bimets equation (GETS model
  development)

- [`monthly_data_example`](https://uhero.github.io/fcutils/reference/monthly_data_example.md)
  : monthly data for examples

- [`mtd_cum()`](https://uhero.github.io/fcutils/reference/mtd_cum.md) :
  Month to date sum or average

- [`mtd_gr()`](https://uhero.github.io/fcutils/reference/mtd_gr.md) :
  Month to date growth rate

- [`multi_chain()`](https://uhero.github.io/fcutils/reference/multi_chain.md)
  :

  Extend "ts-boxable" objects by chaining (extension of
  [`tsbox::ts_chain()`](https://docs.ropensci.org/tsbox/reference/ts_bind.html))

- [`nmons()`](https://uhero.github.io/fcutils/reference/nmons.md) :
  Calculate number of months between two dates yyyyMm, yyyy.m or
  yyyy-mm-dd

- [`nqtrs()`](https://uhero.github.io/fcutils/reference/nqtrs.md) :
  Calculate number of quarters between two dates yyyyQq, yyyy.q or
  yyyy-mm-dd

- [`p()`](https://uhero.github.io/fcutils/reference/p.md) : Concatenate
  dates to obtain period

- [`pc_to_pca()`](https://uhero.github.io/fcutils/reference/pc_to_pca.md)
  : Convert quarterly growth to annualized growth

- [`pca_to_pc()`](https://uhero.github.io/fcutils/reference/pca_to_pc.md)
  : Convert annualized growth to quarterly growth

- [`pcmp()`](https://uhero.github.io/fcutils/reference/pcmp.md) :
  Calculate multi-period average growth

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

- [`pm()`](https://uhero.github.io/fcutils/reference/pm.md) :
  Concatenate dates formatted as yyyyMm or yyyy.m to obtain period

- [`pq()`](https://uhero.github.io/fcutils/reference/pq.md) :
  Concatenate dates formatted as yyyyQq or yyyy.q to obtain period

- [`ptd_cum()`](https://uhero.github.io/fcutils/reference/ptd_cum.md) :
  Period to date sum or average

- [`ptd_gr()`](https://uhero.github.io/fcutils/reference/ptd_gr.md) :
  Period to date growth rate

- [`py()`](https://uhero.github.io/fcutils/reference/py.md) :
  Concatenate dates formatted as yyyy to obtain period

- [`qtrs()`](https://uhero.github.io/fcutils/reference/qtrs.md) :
  Convert period in quarters to period in months

- [`quarterly_data_example`](https://uhero.github.io/fcutils/reference/quarterly_data_example.md)
  : quarterly data for examples

- [`rename_udaman()`](https://uhero.github.io/fcutils/reference/rename_udaman.md)
  : Format series names to udaman format (mnemonic@loc.freq)

- [`save_plot_list()`](https://uhero.github.io/fcutils/reference/save_plot_list.md)
  : Save a list of interactive plots to html

- [`set_attr_tslist()`](https://uhero.github.io/fcutils/reference/set_attr_tslist.md)
  : Set class attribute to tslist

- [`set_tsrange()`](https://uhero.github.io/fcutils/reference/set_tsrange.md)
  : Set tsrange for behavioral equations to available data range

- [`set_udaman_token()`](https://uhero.github.io/fcutils/reference/set_udaman_token.md)
  : Set udaman token for API access

- [`span()`](https://uhero.github.io/fcutils/reference/span.md) :
  Specify span of time series (wrapper around tsbox::ts_span())

- [`sum_pattern`](https://uhero.github.io/fcutils/reference/sum_pattern.md)
  : pattern in series names that should be aggregate as sum

- [`to_ymd()`](https://uhero.github.io/fcutils/reference/to_ymd.md) :
  Parse strings into dates in yyyy-mm-dd format

- [`uh_colors`](https://uhero.github.io/fcutils/reference/uh_colors.md)
  : colors defined in the UHERO Style Guide

- [`uh_colors_50`](https://uhero.github.io/fcutils/reference/uh_colors_50.md)
  : transparent versions of UHERO colors

- [`uh_colors_light`](https://uhero.github.io/fcutils/reference/uh_colors_light.md)
  : lighter versions of UHERO colors

- [`update_eqs()`](https://uhero.github.io/fcutils/reference/update_eqs.md)
  : Update a bimets model with new/modified equations

- [`write_tsd()`](https://uhero.github.io/fcutils/reference/write_tsd.md)
  : Save a ts-boxable object in tsd format

- [`ymd_to_yQq()`](https://uhero.github.io/fcutils/reference/ymd_to_yQq.md)
  : Convert dates from yyyy-mm-dd to yyyyQqq format

- [`yoy_to_lev()`](https://uhero.github.io/fcutils/reference/yoy_to_lev.md)
  : Extend a series using year over year growth

- [`ytd_cum()`](https://uhero.github.io/fcutils/reference/ytd_cum.md) :
  Year to date sum or average

- [`ytd_gr()`](https://uhero.github.io/fcutils/reference/ytd_gr.md) :
  Year to date growth rate
