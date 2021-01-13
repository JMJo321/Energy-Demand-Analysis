# Description
# : A Header Script for Energy Demand Analysis Project.

# --------------------------------------------------
# To load required libraries
# --------------------------------------------------
# (Not Applicable)

# --------------------------------------------------
# To clear the worksapce
# --------------------------------------------------
rm(list = setdiff(ls(), c("PATH_PROJ")))


# --------------------------------------------------
# To set working directory
# --------------------------------------------------
# ------- To set working directory -------
# (In each script)


# --------------------------------------------------
# To define parameter(s)
# --------------------------------------------------
# ------- To define basic paths for "03_Note" ------
PATH_NOTE <- "03_Note"
PATH_NOTE_DESCRIPTIVE.ANALYSIS <-
  paste(PATH_NOTE, "01_Descriptive-Analysis", sep = "/")


# ------- To define basic paths for "04_Data" -------
PATH_DATA              <- "04_Data"
PATH_DATA_RAW          <- paste(PATH_DATA, "01_Raw-Data", sep = "/")
PATH_DATA_RAW_ORIGINAL <- paste(PATH_DATA_RAW, "01_Original", sep = "/")
PATH_DATA_RAW_USE      <- paste(PATH_DATA_RAW, "02_Use", sep = "/")
PATH_DATA_INTERMEDIATE <- paste(PATH_DATA, "02_Intermediate-Data", sep = "/")
PATH_DATA_ANALYSIS     <- paste(PATH_DATA, "03_For-Analysis", sep = "/")


# ------- To define basic paths for "05_Code" -------
PATH_CODE          <- "05_Code"
PATH_CODE_BUILD    <- paste(PATH_CODE, "01_Build", sep= "/")
PATH_CODE_ANALYSIS <- paste(PATH_CODE, "02_Analysis", sep= "/")


# ------- To generate the foldes defined above -------
list_path <- c(
    PATH_NOTE,
    PATH_NOTE_DESCRIPTIVE.ANALYSIS,
    PATH_DATA,
    PATH_DATA_RAW,
    PATH_DATA_RAW_ORIGINAL,
    PATH_DATA_RAW_USE,
    PATH_DATA_INTERMEDIATE,
    PATH_DATA_ANALYSIS,
    PATH_CODE,
    PATH_CODE_BUILD,
    PATH_CODE_ANALYSIS
)
for (path in list_path) {
    if (!dir.exists(path)) {
        dir.create(path)
    }
}


# --------------------------------------------------
# To define function(s)
# --------------------------------------------------
# ------- To run regressions for a specific bandwidth by using "felm" -------
# # 1. To estimate the treatment effect at monthly level
estimate.wBW_terms_felm_for.monthly.subsample <-
  function (
    dt, model, bw, rate.codes, year_lower, year_upper, month_response,
    suffix_tier, suffix_period
  ) {
    tmp_condition <- generate.condition_to.subset_month(
      bw = bw, rate.codes = rate.codes,
      year_lower = year_lower, year_upper = year_upper,
      month_response = month_response,
      suffix_tier = suffix_tier, suffix_period = suffix_period
    )
    return(
      felm(formula = model, data = dt[eval(parse(text = tmp_condition))])
    )
  }

# # 2. To estimate the treatment effect at season level
estimate.wBW_terms_felm_for.seasonal.subsample <-
  function (
    dt, model, bw, rate.codes, year_lower, year_upper, season,
    suffix_tier, suffix_period
  ) {
    tmp_condition <- generate.condition_to.subset_season(
      bw = bw, rate.codes = rate.codes,
      year_lower = year_lower, year_upper = year_upper,
      season = season,
      suffix_tier = suffix_tier, suffix_period = suffix_period
    )
    return(
      felm(formula = model, data = dt[eval(parse(text = tmp_condition))])
    )
  }

# # 3. To combine the two functions
estimate.wBW_terms_felm_for.subsample <- function (
  dt, model, bw, rate.codes, year_lower, year_upper,
  season = NULL, month_response = NULL,
  suffix_tier, suffix_period
) {
  if (is.null(season) & !is.null(month_response)) {
    return(
      estimate.wBW_terms_felm_for.monthly.subsample(
        dt = dt, model = model, bw = bw, rate.codes = rate.codes,
        month_response = month_response,
        year_lower = year_lower, year_upper = year_upper,
        suffix_tier = suffix_tier, suffix_period = suffix_period
      )
    )
  } else if (!is.null(season) & is.null(month_response)) {
    return(
      estimate.wBW_terms_felm_for.seasonal.subsample(
        dt = dt, model = model, bw = bw, rate.codes = rate.codes,
        season = season,
        year_lower = year_lower, year_upper = year_upper,
        suffix_tier = suffix_tier, suffix_period = suffix_period
      )
    )
  }
}


# ------- To label a data.table's columns -------
label.cols <- function(dt, data.dictionary) {
    # # To generate a list containing labels
    tmp_list_labels <- list(NULL)
    for (col in names(dt)) {
        tmp_list_labels[[col]] <- data.dictionary[[col]]
    }
    tmp_list_labels[1] <- NULL

    # # To label columns of a data.table
    mapply(setattr, dt, name= "label", tmp_list_labels, SIMPLIFY= FALSE)
}


# ------- To explot a ggplot object as PNG format -------
plot.save <-
    function(
      path.w.file.name, plot.obj, width= NULL, height= NULL, units= NULL
    ) {
        ggsave(
            filename  = path.w.file.name,
            plot      = plot.obj,
            dpi       = 320,
            device    = "png",
            width     = width,
            height    = height,
            units     = units,
            limitsize = FALSE
        )
    }


# ------- To load Parquet file and convert to DT -------
pq.to.dt <-
  function(path.to.pq, reg.ex_date = NULL, is_drop.index_cols = NULL) {
    dt <-
      arrow::read_parquet(path.to.pq, as_data_frame = TRUE)
    data.table::setDT(dt)

    if (!is.null(reg.ex_date)) {
        vct_cols_date <- names(dt)[stringr::str_detect(names(dt), reg.ex_date)]
        for (col in vct_cols_date) {
            dt[, (col) := as.Date(get(col), "%Y-%m-%d", tz= "UTC")]
        }
    }

    if (!is.null(is_drop.index_cols)) {
        if (is_drop.index_cols) {
            col.name_idx <-
                names(dt)[stringr::str_detect(names(dt), "^__index_")]
            if (length(col.name_idx) != 0) {
                dt[, (col.name_idx) := NULL]
            }
        }
    }

    return(dt)
  }


# ------- To generate a DT showing Cross-Section of Prices -------
# # 1. To breakdown a consumption amount into tier quantities
qty.allocation <- function(quantity, thresholds) {
  # ## To make a list of possible cases
  list_cases <-
    list(
      "1" = c(quantity, 0, 0),
      "2" =
        c(thresholds[1], sum(quantity, thresholds[1] * -1, na.rm = TRUE), 0),
      "3" =
        c(
          thresholds[1],
          sum(thresholds[2], thresholds[1] * -1, na.rm = TRUE),
          sum(quantity, thresholds[2] * -1, na.rm = TRUE)
        )
    )

  if (quantity <= thresholds[1]) {
    qty_allocated <- list_cases[[1]]
  } else if (!is.na(thresholds[2]) & thresholds[2] < quantity) {
    qty_allocated <- list_cases[[3]]
  } else {
    qty_allocated <- list_cases[[2]]
  }

  return(qty_allocated)
}

# # 2. To generate a DT illustrating Cross-Section of Prices
price.cross.section <-
  function(
    dt_rrs.panel, date.for.cs, rate.code, qty_upper.limit_in.percentage,
        resolution
  ) {
    # ## To make an empty DT
    dt <- as.data.table(seq(0, qty_upper.limit_in.percentage, resolution))
    names(dt) <- "qty_in.percentage"

    # ## To get values from the panel dataset of RRS
    qty_tier_1 <-
      dt_rrs.panel[
        date == date.for.cs & rate_code == rate.code, tier_1_qty_upto_in_kwh
      ]
    qty_tier_2 <-
      dt_rrs.panel[
        date == date.for.cs & rate_code == rate.code, tier_2_qty_upto_in_kwh
      ]
    charge_fixed <-
      dt_rrs.panel[
        date == date.for.cs & rate_code == rate.code, fixed_charge_in_usd
      ]
    charge_tier1 <-
      dt_rrs.panel[
        date == date.for.cs & rate_code == rate.code, tier_1_charge_in_usd
      ]
    charge_tier2 <-
      dt_rrs.panel[
        date == date.for.cs & rate_code == rate.code, tier_2_charge_in_usd
      ]
    charge_tier3 <-
      dt_rrs.panel[
        date == date.for.cs & rate_code == rate.code, tier_3_charge_in_usd
      ]

    # ## Add columns to the empty DT
    dt[, qty_in.kwh := qty_in.percentage / 100 * qty_tier_1]
    cols <- c("qty_for.tier1", "qty_for.tier2", "qty_for.tier3")
    dt[
       ,
      (cols) :=
        mclapply(qty_in.kwh, qty.allocation, c(qty_tier_1, qty_tier_2)) %>%
          transpose(.)
    ]
    dt[
      ,
      `:=` (
        usd_fixed = charge_fixed,
        rate_for.teir1 = charge_tier1,
        rate_for.teir2 = charge_tier2,
        rate_for.teir3 = charge_tier3
      )
    ]
    dt[
      ,
      `:=` (
        usd_for.tier1 = qty_for.tier1 * rate_for.teir1,
        usd_for.tier2 = qty_for.tier2 * rate_for.teir2,
        usd_for.tier3 = qty_for.tier3 * rate_for.teir3
      )
    ]
    cols_rowsums <- names(dt)[str_detect(names(dt), "^usd_")]
    dt[, usd_total := rowSums(.SD, na.rm = TRUE), .SDcols = cols_rowsums]
    dt[, price_marginal := rate_for.teir1]
    dt[qty_for.tier2 > 0, price_marginal := rate_for.teir2]
    dt[qty_for.tier3 > 0, price_marginal := rate_for.teir3]
    dt[, price_average := usd_total / qty_in.kwh]

    return(dt)
  }


# ------- To generate a string that will be used to subset a DT -------
# # 1. To subset a DT at month level
generate.condition_to.subset_month <- function(
  bw, rate.codes, year_lower, year_upper, month_response, suffix_tier,
  suffix_period
) {
    if (is.na(bw)) {
      condition <- paste(
        paste0(
          paste0("rate_code_normalize_", suffix_period, " %in% c(\""),
          str_c(rate.codes, collapse = "\", \""),
          "\")"
        ),
        paste0(
          paste0("billing.year_mid_", suffix_period, " %in% c("),
          year_lower, ":", year_upper,
          ")"
        ),
        paste0("billing.month_mid == ", month_response),
        sep = " & "
      )
    } else {
      condition <- paste(
        paste0(
          bw * -1, " <= kwh_total_in.percent_normalize_", suffix_tier, "_",
          suffix_period, " & ",
          "kwh_total_in.percent_normalize_", suffix_tier, "_",
          suffix_period, " <= ", bw
        ),
        paste0(
          paste0("rate_code_normalize_", suffix_period, " %in% c(\""),
          str_c(rate.codes, collapse = "\", \""),
          "\")"
        ),
        paste0(
          paste0("billing.year_mid_", suffix_period, " %in% c("),
          year_lower, ":", year_upper,
          ")"
        ),
        paste0("billing.month_mid == ", month_response),
        sep = " & "
      )
    }

  return(condition)
}

# # 2. To subset a DT at season level
generate.condition_to.subset_season <- function(
  bw, rate.codes, season, year_lower, year_upper, suffix_tier, suffix_period
) {
  if (is.na(bw) & is.na(season)) {
    condition <- paste(
      paste0(
        paste0("rate_code_normalize_", suffix_period, " %in% c(\""),
        str_c(rate.codes, collapse = "\", \""),
        "\")"
      ),
      paste0(
        paste0("billing.year_mid_", suffix_period, " %in% c("),
        year_lower, ":", year_upper,
        ")"
      ),
      sep = " & "
    )
  } else if (is.na(bw) & !is.na(season)) {
    condition <- paste(
      paste0(
        paste0("rate_code_normalize_", suffix_period, " %in% c(\""),
        str_c(rate.codes, collapse = "\", \""),
        "\")"
      ),
      paste0(
        paste0("billing.year_mid_", suffix_period, " %in% c("),
        year_lower, ":", year_upper,
        ")"
      ),
      paste0("season_before_", suffix_period, " == \"", season, "\""),
      paste0(
        "season_before_", suffix_period, " == season_after_", suffix_period
      ),
      sep = " & "
    )
  } else if (!is.na(bw) & is.na(season)) {
    condition <- paste(
      paste0(
        bw * -1, " <= kwh_total_in.percent_normalize_", suffix_tier, "_",
        suffix_period, " & ",
        "kwh_total_in.percent_normalize_", suffix_tier, "_",
        suffix_period, " <= ", bw
      ),
      paste0(
        paste0("rate_code_normalize_", suffix_period, " %in% c(\""),
        str_c(rate.codes, collapse = "\", \""),
        "\")"
      ),
      paste0(
        paste0("billing.year_mid_", suffix_period, " %in% c("),
        year_lower, ":", year_upper,
        ")"
      ),
      sep = " & "
    )
  } else {
    condition <- paste(
      paste0(
        bw * -1, " <= kwh_total_in.percent_normalize_", suffix_tier, "_",
        suffix_period, " & ",
        "kwh_total_in.percent_normalize_", suffix_tier, "_",
        suffix_period, " <= ", bw
      ),
      paste0(
        paste0("rate_code_normalize_", suffix_period, " %in% c(\""),
        str_c(rate.codes, collapse = "\", \""),
        "\")"
      ),
      paste0(
        paste0("billing.year_mid_", suffix_period, " %in% c("),
        year_lower, ":", year_upper,
        ")"
      ),
      paste0("season_before_", suffix_period, " == \"", season, "\""),
      paste0(
        "season_before_", suffix_period, " == season_after_", suffix_period
      ),
      sep = " & "
    )
  }

  return(condition)
}

# ------- To extract required information from a FELM object -------
extract_reg.results_felm_for.subsample <-
  function(
    model, model.description, is_clustering,
    bw, rate.codes, season = NULL, month_response = NULL,
    year_lower, year_upper, suffix_tier, suffix_period
  ) {
    # # 1) Create temporary objects that will be used later
    tmp_functional.form <-
      str_extract(model.description, "(Linear)|(Quadratic)|(Cubic)")
    tmp_model <- str_replace(model.description, ",.+$", "")
    tmp_rate.codes <- str_c(rate.codes, collapse = " & ")
    tmp_term_btw.periods <-
      str_extract(model.description, "P[0-9]$") %>% str_extract("[0-9]$") %>%
        as.numeric(.)
    tmp_condition <- paste0(
        "!is.na(kwh_daily.avg_", suffix_period, ") & is.finite(kwh_daily.avg_",
        suffix_period, ")"
      )

    # # 2) Change independent variables in the model
    tmp_exp_treat.var_from <- paste(
      "is_treated_t1", suffix_period, sep = "_"
    )
    tmp_exp_treat.var_to <- paste(
      "is_treated", suffix_tier, suffix_period, sep = "_"
    ) %>% str2lang(.) %>% as.list(.)
    names(tmp_exp_treat.var_to) <- tmp_exp_treat.var_from
    tmp_exp_running.var_from <- paste(
      "kwh_total_in.percent_normalize_t1", suffix_period, sep = "_"
    )
    tmp_exp_running.var_to <- paste(
      "kwh_total_in.percent_normalize", suffix_tier, suffix_period, sep = "_"
    ) %>% str2lang(.) %>% as.list(.)
    names(tmp_exp_running.var_to) <- tmp_exp_running.var_from

    TMP_model_for.reg <-
      do.call("substitute", list(model, tmp_exp_treat.var_to))
    tmp_model_for.reg <-
      do.call(
        "substitute",
        list(TMP_model_for.reg, tmp_exp_running.var_to)
      ) %>% as.formula(.)

    # # 3) Create FELM object by running a regression
    tmp_felm.obj = estimate.wBW_terms_felm_for.subsample(
      dt = dt_for.reg[eval(parse(text = tmp_condition))],
      model = tmp_model_for.reg,
      bw = bw, rate.codes = rate.codes,
      season = season, month_response = month_response,
      year_lower = year_lower, year_upper = year_upper,
      suffix_tier = suffix_tier, suffix_period = suffix_period
    )

    # # 5) Extract information from the FELM object
    tmp_dt_vars_independent <-
      tmp_felm.obj$coefficients %>% row.names(.) %>% as.data.table(.) %>%
        setnames(., ".", "var_independent")
    tmp_n_indepent.var <- tmp_dt_vars_independent[, .N]
    tmp_var_dependent <- tmp_felm.obj$lhs
    tmp_method <-
      tmp_felm.obj$call %>% .[[1]] %>% as.character(.)
    # ## Create a DT including base information
    if (is.na(season) | is.null(season)) {
      tmp_season <- NA
    } else {
      tmp_season <- season
    }
    if (is.null(month_response)) {
      tmp_month_response <- NA
    } else {
      tmp_month_response <- month_response
    }

    tmp_dt_base <- cbind(
      data.table(
        functional.form = rep(tmp_functional.form, times = tmp_n_indepent.var),
        model = rep(tmp_model, times = tmp_n_indepent.var),
        rate.codes = rep(tmp_rate.codes, times = tmp_n_indepent.var),
        season = rep(tmp_season, times = tmp_n_indepent.var),
        month_response = rep(tmp_month_response, times = tmp_n_indepent.var),
        term_btw.periods = rep(tmp_term_btw.periods, times = tmp_n_indepent.var),
        method = rep(tmp_method, times = tmp_n_indepent.var),
        is_clustering = rep(is_clustering, times = tmp_n_indepent.var),
        var_dependent = rep(tmp_var_dependent, times = tmp_n_indepent.var)
      ),
      tmp_dt_vars_independent
    )
    tmp_dt_base[
      ,
      `:=` (
        bw_in.str = paste0(bw, "%"),
        bw_in.percent = bw
      )
    ]

    tmp_dt_coef <-
      tmp_felm.obj$coefficients %>% as.data.table(.) %>%
        setnames(., tmp_var_dependent, "estimates")
    if (is_clustering) {
      tmp_dt_se_standard <-
        tmp_felm.obj %>% summary(., robust = FALSE) %>%
          .$coefficients %>% .[, "Std. Error"] %>% as.data.table(.) %>%
          setnames(., ".", "se_standard")
      tmp_dt_se_cluster <-
        tmp_felm.obj %>% summary(., robust = TRUE) %>%
          .$coefficients %>% .[, "Cluster s.e."] %>% as.data.table(.) %>%
          setnames(., ".", "se_cluster")
      tmp_dt_se_hetero <-
        rep(NA, times = tmp_n_indepent.var) %>% as.data.table(.) %>%
          setnames(., ".", "se_hetero")

      tmp_dt_ci <-
        confint(tmp_felm.obj) %>% as.data.table(.) %>%
          setnames(
            ., c("2.5 %", "97.5 %"), c("ci_lower", "ci_upper")
          )
    } else {
      tmp_dt_se_standard <-
        tmp_felm.obj %>% summary(., robust = FALSE) %>%
          .$coefficients %>% .[, "Std. Error"] %>% as.data.table(.) %>%
          setnames(., ".", "se_standard")
      tmp_dt_se_cluster <-
        rep(NA, times = tmp_n_indepent.var) %>% as.data.table(.) %>%
          setnames(., ".", "se_cluster")
      tmp_dt_se_hetero <-
        tmp_felm.obj %>% summary(., robust = TRUE) %>%
          .$coefficients %>% .[, "Robust s.e"] %>% as.data.table(.) %>%
          setnames(., ".", "se_hetero")

      tmp_dt_ci <-
        confint(tmp_felm.obj) %>% as.data.table(.) %>%
          setnames(
            ., c("2.5 %", "97.5 %"), c("ci_lower", "ci_upper")
          )
    }

    tmp_dt_binded <- cbind(
      tmp_dt_base,
      tmp_dt_coef,
      tmp_dt_se_standard,
      tmp_dt_se_cluster,
      tmp_dt_se_hetero,
      tmp_dt_ci
    )

    return(tmp_dt_binded)
  }
