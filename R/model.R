#' Parse lm() output and convert into bimets equation (GETS model development)
#'
#' @param model a model estimated by lm() (lm object)
#' @param ... arguments to format the coefficients e.g. digits = 3
#'
#' @return a character vector containing the estimated equation (1) and bimets components (2:5)
#' @export
#'
#' @examplesIf interactive()
#' # this function combines coefficient estimates and variable names into an equation
#' # in vector element 1 and into bimets components in vector elements 2-5.
#' # https://stats.stackexchange.com/questions/63600/
#' # how-to-translate-the-results-from-lm-to-an-equation
#' data("UKDriverDeaths", package = "datasets")
#' uk <- log10(UKDriverDeaths)
#' dfm <- dynlm::dynlm(uk ~ L(uk, 1:3) + L(log(uk), c(5:6, 12)))
#' model_equation(dfm)
#' # (1) "uk = - 0.12255631 + 0.42870091 * L(uk, 1:3)1 + 0.06306114 * L(uk, 1:3)2 - 0.09778518 *
#' # L(uk, 1:3)3 + 0.37480999 * L(log(uk), c(5:6, 12))5 - 0.22709846 * L(log(uk), c(5:6, 12))6 +
#' # 1.62340449 * L(log(uk), c(5:6, 12))12"
#' # (2) "BEHAVIORAL> uk"
#' # (3) "TSRANGE 1990 1 2010 4"
#' # (4) "EQ> uk = b0 + b1 * TSLAG(uk, 1) + b2 * TSLAG(uk, 2) + b3 * TSLAG(uk, 3) + b4 *
#' # TSLAG(LOG(uk), 5) + b5 * TSLAG(LOG(uk), 6) + b6 * TSLAG(LOG(uk), 12)"
#' # (5) "COEFF> b0 b1 b2 b3 b4 b5 b6"
#' ## regression on multiple lags in a single L() call
#' dfm <- dynlm::dynlm(d(log(uk)) ~ L(uk, c(1, 11, 12)), start = c(1975, 1), end = c(1982, 12))
#' model_equation(dfm)
#' # (1) "d(log(uk)) = 0.1018542 - 0.2379287 * L(uk, c(1, 11, 12))1 + 0.0368355 *
#' # L(uk, c(1, 11, 12))11 + 0.1689896 * L(uk, c(1, 11, 12))12"
#' # (2) "BEHAVIORAL> uk"
#' # (3) "TSRANGE 1990 1 2010 4"
#' # (4) "EQ> TSDELTA(LOG(uk)) = b0 + b1 * TSLAG(uk, 1) + b2 * TSLAG(uk, 11) + b3 * TSLAG(uk, 12)"
#' # (5) "COEFF> b0 b1 b2 b3"
model_equation <- function(model, ...) {
  format_args <- list(...)

  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- dplyr::case_when(
    model_coeff_sign == -1 ~ " - ",
    model_coeff_sign == 1 ~ " + ",
    model_coeff_sign == 0 ~ " + "
  )

  # components of TSRANGE from date stamps of residuals
  model_tsr <- model$residuals %>%
    names() %>%
    lubridate::parse_date_time(c("ymd", "my", "yq", "y")) %>%
    lubridate::ymd()
  model_eqn_tsr <- stringr::str_flatten(
    c(
      "TSRANGE",
      lubridate::year(model_tsr %>% utils::head(1)),
      lubridate::quarter(model_tsr %>% utils::head(1)),
      lubridate::year(model_tsr %>% utils::tail(1)),
      lubridate::quarter(model_tsr %>% utils::tail(1))
    ),
    collapse = " "
  )

  # model equation with estimated coefficients
  model_eqn <- stringr::str_c(
    stringr::str_split(as.character(model$terms), "~")[[2]], # 'y'
    " = ",
    stringr::str_c(
      model_coeff_prefix,
      do.call(format, format_args),
      " * ",
      names(model_coeff),
      collapse = ""
    )
  ) %>%
    stringr::str_squish() %>%
    stringr::str_replace("= \\+ ", "= ") %>%
    stringr::str_replace("\\* c |\\* CONST |\\* \\(Intercept\\) ", "* CONST ")

  # formal model equation with b coefficients
  # if there is a constant term in the model (Intercept, CONST, c)
  # move it to the first position with b0 coefficient
  if (
    names(model_coeff) %>% stringr::str_detect("CONST|Intercept|^c$") %>% any()
  ) {
    const_index <- which(
      names(model_coeff) %>% stringr::str_detect("CONST|Intercept|^c$")
    )
    model_eqn_bim <- stringr::str_c(
      stringr::str_split(as.character(model$terms), "~")[[2]], # 'y'
      " = ",
      stringr::str_c(
        "b0",
        stringr::str_c(
          stringr::str_c(" + b", 1:length(model_coeff[-1]), sep = ""),
          " * ",
          names(model_coeff[-const_index]),
          collapse = ""
        )
      )
    ) %>%
      stringr::str_squish() %>%
      stringr::str_replace("= \\+ ", "= ")
  } else {
    model_eqn_bim <- stringr::str_c(
      stringr::str_split(as.character(model$terms), "~")[[2]], # 'y'
      " = ",
      stringr::str_c(
        stringr::str_c(" + b", 1:length(model_coeff), sep = ""),
        " * ",
        names(model_coeff),
        collapse = ""
      )
    ) %>%
      stringr::str_squish() %>%
      stringr::str_replace("= \\+ ", "= ")
  }

  # format model equation in bimets format
  model_eqn_beh <- stringr::str_extract(
    model_eqn_bim,
    "[_.\\(\\)[:alnum:]]+"
  ) %>% # extract the target variable name
    stringr::str_replace_all(
      "(?<![:alpha:])DL_([_.[:alnum:]]+)",
      "TSDELTALOG(\\1)"
    ) %>% # replace DL_ with TSDELTALOG()
    stringr::str_replace_all("(?<![:alpha:])L_([_.[:alnum:]]+)", "LOG(\\1)") %>% # replace L_ with LOG()
    stringr::str_replace_all(
      "(?<![:alpha:])D_([_.[:alnum:]]+)",
      "TSDELTA(\\1)"
    ) %>% # replace D_ with TSDELTA()
    stringr::str_replace_all("log\\(([_.[:alnum:]]+)\\)", "LOG(\\1)") %>% # replace log() with LOG()
    stringr::str_replace_all(
      "d\\(([_., \\(\\)[:alnum:]]+)\\)",
      "TSDELTA(\\1)"
    ) %>% # dynlm::d() = diff()
    stringr::str_replace_all("[\\(]+", "_") %>% # drop parentheses from equation name
    stringr::str_replace_all("[., \\)[:digit:]]+", "") %>% # drop parentheses and extra items from equation name
    stringr::str_replace_all(c(
      "^TSDELTALOG_" = "",
      "^TSDELTA_" = "",
      "^LOG_" = ""
    )) # clean equation name
  # first deal with lags then with other transformations
  model_eqn_bim <- stringr::str_replace_all(
    model_eqn_bim,
    "([_[:alpha:]]+)(\\.)([[:digit:]]{1,2})",
    "TSLAG(\\1, \\3)"
  ) # replace dot notation for lags with TSLAG()
  model_eqn_bim <- stringr::str_replace_all(
    model_eqn_bim,
    "L\\(([_\\(\\) [:alpha:]]+)([, ]+)([:c\\(, [:digit:]\\)]+)\\)([[:digit:]]{1,2})",
    "TSLAG(\\1, \\4)"
  ) # dealing with dynlm::L(x, 1:4) or dynlm::L(x, c(2,4))
  model_eqn_bim <- stringr::str_replace_all(
    model_eqn_bim,
    "L\\(([_., \\(\\)[:alnum:]]+)\\)",
    "TSLAG(\\1)"
  ) # dynlm::L(x, k) = lag(x, -k)
  model_eqn_bim <- model_eqn_bim %>%
    stringr::str_replace_all(
      "(?<![:alpha:])DL_([_.[:alnum:]]+)",
      "TSDELTALOG(\\1)"
    ) %>% # replace DL_ with TSDELTALOG()
    stringr::str_replace_all("(?<![:alpha:])L_([_.[:alnum:]]+)", "LOG(\\1)") %>% # replace L_ with LOG()
    stringr::str_replace_all(
      "(?<![:alpha:])D_([_.[:alnum:]]+)",
      "TSDELTA(\\1)"
    ) %>% # replace D_ with TSDELTA()
    stringr::str_replace_all("log\\(([_.[:alnum:]]+)\\)", "LOG(\\1)") %>% # replace log() with LOG()
    stringr::str_replace_all(
      "d\\(([_., \\(\\)[:alnum:]]+)\\)",
      "TSDELTA(\\1)"
    ) %>% # dynlm::d() = diff()
    stringr::str_replace_all("lag\\(([_., \\(\\)[:alnum:]]+)\\)", "TSLAG(\\1)") # lag() with TSLAG()
  model_eqn_bim <- stringr::str_replace_all(
    model_eqn_bim,
    "S_(\\d{4})_(\\d{2})_(\\d{2})",
    "S_\\1-\\2-\\3"
  ) %>%
    stringr::str_replace_all(
      "(\\d{4}-\\d{2}-\\d{2})",
      ymd_to_yQq
    ) # convert IIS_1980_10_01 to IIS_1980Q4 and SIS_1980_10_01 to SIS_1980Q4
  model_eqn_coe <- stringr::str_extract_all(
    model_eqn_bim,
    "(b[[:digit:]]+)",
    simplify = TRUE
  ) %>%
    paste(collapse = " ") # # extract coefficients
  model_eqn_beh <- stringr::str_replace_all(model_eqn_beh, "^", "BEHAVIORAL> ") # add a line for BEHAVIORAL>
  model_eqn_bim <- stringr::str_replace_all(model_eqn_bim, "^", "EQ> ") # add EQ> to start of line
  model_eqn_coe <- stringr::str_replace_all(model_eqn_coe, "^", "COEFF> ") # add COEFF> to start of line

  return(c(
    model_eqn,
    model_eqn_beh,
    model_eqn_tsr,
    model_eqn_bim,
    model_eqn_coe
  ))
}


#' Parse gets output and extract underlying data (GETS model development)
#'
#' @param model_in a model estimated by arx, isat, or getsm
#' @param y_name the actual name of the y variable
#'
#' @return an xts containing the model variables
#' @export
#'
#' @examplesIf interactive()
#' # save the data associated with a gets model
extract_data <- function(model_in, y_name) {
  data_out <- gets::eviews(model_in, print = FALSE, return = TRUE)$data %>%
    dplyr::rename_with(~y_name, .cols = 2) %>%
    dplyr::rename_with(
      ~ stringr::str_replace(., "ar", stringr::str_glue("{y_name}."))
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(., c("iis" = "IIS_", "sis" = "SIS_"))
    ) %>%
    dplyr::rename_with(~ stringr::str_replace_all(., "-", "_")) %>%
    dplyr::mutate(
      time = lubridate::ymd(.data$index),
      .before = 1,
      .keep = "unused"
    ) %>%
    tsbox::ts_long() %>%
    tsbox::ts_xts()

  return(data_out)
}


#' Update a bimets model with new/modified equations
#'
#' @param model_1 original estimated bimets model
#' @param model_2 estimated bimets model containing updates (only updated
#'   equations need to be estimated)
#' @param eqList names of updated behavioral equations (vector of strings),
#'   others taken from model_1 (equations missing from model_2 are removed)
#'
#' @return estimated bimets model containing updates
#' @export
#'
#' @details Start by making a copy of the original model's equations (txt file).
#' Re-specify some equations, add new equations, and remove not needed equations.
#' Load the new model as model_2 and estimate the modified/new equations (ok to estimate all).
#' Replace the equations in model_2 that should remain the same as in model_1
#' by the estimated equations from model_1. Equations that are to remain unchanged
#' have to be present in both model_1 and model_2, and not present in eqList.
#'
#' @examplesIf interactive()
#' update_eqs(scen_model_1_est, scen_model_2_est, c("E_NF_AT_HI_Q", "Y_R_AT_HI_Q"))
update_eqs <- function(model_1, model_2, eqList) {
  # initialize the result with model_2 (contains all equations we want)
  scen_model_est <- model_2

  # equations to keep from original model_1 (if they are present in the updated model_2)
  keep_eqList <- model_1$behaviorals %>%
    names() %>%
    intersect(model_2$behaviorals %>% names()) %>%
    setdiff(eqList)

  # keep the unmodified behavioral equations from the original model_1
  scen_model_est$behaviorals[keep_eqList] <- model_1$behaviorals[keep_eqList]

  return(scen_model_est)
}


#' Set tsrange for behavioral equations to available data range
#'
#' @param model_w_dat bimets model (with data) to be estimated
#' @param max_lag the largest lag (default = 4) in the model (to offset starting point for estimation)
#' @param eqns names of behavioral equations to set tsrange for (default = NULL: all equations)
#'
#' @return bimets model with tsrange set for estimation
#' @export
#'
#' @details Find periods where all variables in the equation are available.
#' Shift beginning of the sample by max_lag periods.
#' Set the tsrange for each equation (used in estimation).
#'
#' @examplesIf interactive()
#' set_tsrange(scen_model_dat, 4)
set_tsrange <- function(model_w_dat, max_lag = 4, eqns = NULL) {
  # if no equations are specified, use all behavioral equations
  if (is.null(eqns)) {
    eqns <- model_w_dat$vendogBehaviorals
  }

  # for each behavioral equation, set the tsrange to the available data
  # bimets::GETRANGE() accomplishes a similar task but does not offset the start by max_lag
  for (eq_i in eqns) {
    model_w_dat$behaviorals[[eq_i]][["tsrange"]] <- model_w_dat$modelData %>%
      magrittr::extract(model_w_dat$behaviorals[[eq_i]][[
        "eqComponentsNames"
      ]]) %>%
      set_attr_tslist() %>%
      tsbox::ts_tbl() %>%
      tsbox::ts_wide() %>%
      tidyr::drop_na() %>%
      dplyr::slice(1 + max_lag, dplyr::n()) %>%
      dplyr::pull(.data$time) %>%
      purrr::map(~ c(lubridate::year(.x), lubridate::quarter(.x))) %>%
      purrr::reduce(c)
  }

  return(model_w_dat)
}


# **************************
# other utility functions ----
# **************************

#' In place addition
#'
#' Warning: Typing x %+=% y/2 returns x <- (x + y)/2.
#' Adding parentheses, i.e. x %+=% (y/2) solves the problem.
#'
#' @param e1 first addend (and returned sum)
#' @param e2 second addend
#'
#' @return sum of the two addends replacing the values in the first addend
#' @export
#'
#' @examplesIf interactive()
#' add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] <- add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] +
#'   c(0.01, -0.04, rep(-0.025, 4))
#' add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] %+=% c(0.01, -0.04, rep(-0.025, 4)) # easier on the eye
`%+=%` <- function(e1, e2) eval.parent(substitute(e1 <- e1 + e2))
