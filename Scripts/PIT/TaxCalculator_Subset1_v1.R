library(data.table)
library(dplyr)
library(tidyr)

setDTthreads(threads = 8)

# I. Parameter helper --------------------------------------------------------

get_params_for_year <- function(params_dt, year) {
  
  params_dt <- as.data.table(params_dt)
  
  if (!"Year" %in% names(params_dt)) {
    stop("Parameter table must contain column: Year")
  }
  
  out <- params_dt[Year == year]
  
  if (nrow(out) == 0L) {
    stop(paste0("No PIT simulation parameters found for year: ", year))
  }
  
  out
}

get_param_fun <- function(params_dt, param_name) {
  
  params_dt <- as.data.table(params_dt)
  
  if (!"Parameters" %in% names(params_dt)) {
    stop("Parameter table must contain column: Parameters")
  }
  
  if (!"Value" %in% names(params_dt)) {
    stop("Parameter table must contain column: Value")
  }
  
  out <- params_dt[Parameters == param_name, Value]
  
  if (length(out) == 0L) {
    stop(paste0("Missing parameter: ", param_name))
  }
  
  out[1]
}

# II. Progressive PIT function ----------------------------------------------

calc_progressive_tax <- function(taxable, rate1, rate2, rate3, rate4,
                                 tbrk1, tbrk2, tbrk3) {
  
  taxable <- pmax(taxable, 0)
  
  bw1 <- pmax(tbrk2 - tbrk1, 0)
  bw2 <- pmax(tbrk3 - tbrk2, 0)
  
  tax <- rate1 * pmin(taxable, tbrk1) +
    rate2 * pmin(bw1, pmax(0, taxable - tbrk1)) +
    rate3 * pmin(bw2, pmax(0, taxable - tbrk2)) +
    rate4 * pmax(0, taxable - tbrk3)
  
  tax
}

# III. Growth factor helper --------------------------------------------------

get_growth_factor_row <- function(scenario, growth_factors, scenarios, vars_to_grow) {
  
  growth_factors <- as.data.table(growth_factors)
  
  row_id <- match(scenario, scenarios)
  
  if (is.na(row_id)) {
    stop(paste0("Scenario not found in scenarios vector: ", scenario))
  }
  
  if (row_id > nrow(growth_factors)) {
    stop(
      paste0(
        "growth_factors has only ", nrow(growth_factors),
        " rows, but scenario ", scenario,
        " needs row ", row_id
      )
    )
  }
  
  gf_row <- growth_factors[row_id]
  
  out <- numeric(length(vars_to_grow))
  names(out) <- vars_to_grow
  
  for (v in vars_to_grow) {
    
    gf_col <- sub("_adjusted", "", v)
    
    if (!gf_col %in% names(gf_row)) {
      stop(paste0("Missing growth factor column: ", gf_col))
    }
    
    out[v] <- gf_row[[gf_col]]
  }
  
  out
}

# IV. Apply growth only ------------------------------------------------------

apply_growth_only <- function(dt_scn, gf_values, vars_to_grow) {
  
  dt_scn <- as.data.table(dt_scn)
  
  for (v in vars_to_grow) {
    if (v %in% names(dt_scn)) {
      dt_scn[, (v) := get(v) * gf_values[v]]
    }
  }
  
  dt_scn
}

# V. Scenario extraction -----------------------------------------------------

get_scenario_df <- function(year, horizons, scenarios, table_list) {
  
  stopifnot(all(scenarios %in% names(table_list)))
  
  scn <- scenarios[horizons == year]
  
  stopifnot(length(scn) == 1L)
  
  table_list[[scn]]
}

# VI. Add missing numeric columns -------------------------------------------

add_missing_numeric_cols <- function(dt, cols) {
  
  dt <- as.data.table(dt)
  
  for (cc in cols) {
    if (!cc %in% names(dt)) {
      dt[, (cc) := 0]
    }
  }
  
  dt
}

# VII. Weighted summary for compatibility -----------------------------------

summarize_block_list <- function(PIT_list, suffix, value_cols = "pitax") {
  
  summary_list <- lapply(names(PIT_list), function(scenario_name) {
    
    dt <- as.data.table(PIT_list[[scenario_name]])
    
    if (!"weight" %in% names(dt)) {
      dt[, weight := 1]
    }
    
    dt <- add_missing_numeric_cols(dt, value_cols)
    
    out <- dt[, lapply(.SD, function(x) {
      sum(x * weight, na.rm = TRUE)
    }), .SDcols = value_cols]
    
    out[, scenarios := scenario_name]
    
    setcolorder(out, c("scenarios", value_cols))
    
    out
  })
  
  result <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
  
  old_names <- setdiff(names(result), "scenarios")
  new_names <- paste0(old_names, suffix)
  
  setnames(result, old_names, new_names)
  
  as.data.frame(result)
}

# VIII. Format merged object for downstream scripts -------------------------

format_merged_compat <- function(merged_dt, forecast_horizon) {
  
  merged_dt <- as.data.frame(merged_dt)
  
  if (!"year" %in% names(merged_dt)) {
    merged_dt$year <- as.character(forecast_horizon[seq_len(nrow(merged_dt))])
  }
  
  merged_dt <- merged_dt[, c(
    "year",
    setdiff(names(merged_dt), "year")
  )]
  
  numeric_columns <- sapply(merged_dt, is.numeric)
  numeric_columns["year"] <- FALSE
  
  merged_dt[, numeric_columns] <- merged_dt[, numeric_columns] / 1e06
  
  as.data.table(merged_dt)
}

# IX. Build GUI summary ------------------------------------------------------

build_pit_summary <- function(merged_dt, forecast_horizon, macro_dt) {
  
  merged_dt <- as.data.frame(merged_dt)
  
  if (!"year" %in% names(merged_dt)) {
    merged_dt$year <- as.character(forecast_horizon[seq_len(nrow(merged_dt))])
  }
  
  merged_dt <- merged_dt[, c(
    "year",
    setdiff(names(merged_dt), "year")
  )]
  
  numeric_columns <- sapply(merged_dt, is.numeric)
  numeric_columns["year"] <- FALSE
  
  merged_dt[, numeric_columns] <- merged_dt[, numeric_columns] / 1e06
  
  pit_summary_df <- merged_dt %>%
    pivot_longer(
      cols = -c(year, scenarios),
      names_to = c("variable", ".value"),
      names_pattern = "(.*)_(bu|sim)"
    ) %>%
    mutate(difference = sim - bu) %>%
    mutate(across(c(bu, sim, difference), ~ round(., 1))) %>%
    filter(variable == "pitax") %>%
    select(year, bu, sim, difference) %>%
    dplyr::rename(
      "Current law (LCU Mil)" = "bu",
      "Simulation (LCU Mil)" = "sim",
      "Fiscal impact (LCU Mil)" = "difference"
    )
  
  macro_dt <- as.data.frame(macro_dt)
  macro_dt$Year <- as.character(macro_dt$Year)
  
  pit_summary_df <- pit_summary_df %>%
    left_join(macro_dt, by = c("year" = "Year")) %>%
    select(
      year,
      "Current law (LCU Mil)",
      "Simulation (LCU Mil)",
      "Fiscal impact (LCU Mil)",
      Nominal_GDP
    ) %>%
    mutate(
      `Current law (Pct of GDP)` =
        round(`Current law (LCU Mil)` / Nominal_GDP * 100, 2),
      `Simulation (Pct of GDP)` =
        round(`Simulation (LCU Mil)` / Nominal_GDP * 100, 2),
      `Fiscal impact (Pct of GDP)` =
        round(`Fiscal impact (LCU Mil)` / Nominal_GDP * 100, 2)
    ) %>%
    select(-Nominal_GDP)
  
  as.data.table(pit_summary_df)
}

# X. Input data --------------------------------------------------------------

dt <- copy(dt1)
dt <- as.data.table(dt)

weights_pit <- weights_pit1

base_year <- unique(dt$Year)[1]
end_year <- base_year + 4

forecast_horizon <- seq(base_year, end_year)
scenario_years <- forecast_horizon

scenarios <- c("t0", "t1", "t2", "t3", "t4")

pit_simulation_parameters_raw <- as.data.table(pit_simulation_parameters_raw)
pit_simulation_parameters_updated <- as.data.table(pit_simulation_parameters_updated)

if (!all(forecast_horizon %in% pit_simulation_parameters_raw$Year)) {
  missing_years_raw <- setdiff(forecast_horizon, pit_simulation_parameters_raw$Year)
  stop(
    paste0(
      "pit_simulation_parameters_raw is missing year(s): ",
      paste(missing_years_raw, collapse = ", ")
    )
  )
}

if (!all(forecast_horizon %in% pit_simulation_parameters_updated$Year)) {
  missing_years_updated <- setdiff(forecast_horizon, pit_simulation_parameters_updated$Year)
  stop(
    paste0(
      "pit_simulation_parameters_updated is missing year(s): ",
      paste(missing_years_updated, collapse = ", ")
    )
  )
}

# XI. Wage tax function ------------------------------------------------------

tax_calc_wage_fun <- function(dt_scn, params_dt) {
  
  dt_scn <- as.data.table(dt_scn)
  params_dt <- as.data.table(params_dt)
  
  rate1 <- get_param_fun(params_dt, "rate1")
  rate2 <- get_param_fun(params_dt, "rate2")
  rate3 <- get_param_fun(params_dt, "rate3")
  rate4 <- get_param_fun(params_dt, "rate4")
  
  tbrk1 <- get_param_fun(params_dt, "tbrk1")
  tbrk2 <- get_param_fun(params_dt, "tbrk2")
  tbrk3 <- get_param_fun(params_dt, "tbrk3")
  
  ins_prem_art36_par6   <- get_param_fun(params_dt, "ins_prem_art36_par6")
  per_ex_art33_par1     <- get_param_fun(params_dt, "per_ex_art33_par1")
  per_ex_inc_art33_par2 <- get_param_fun(params_dt, "per_ex_inc_art33_par2")
  ex_spouse_art34_par2  <- get_param_fun(params_dt, "ex_spouse_art34_par2")
  ex_dep_art35_par1     <- get_param_fun(params_dt, "ex_dep_art35_par1")
  ex_dep_dis_art35_par2 <- get_param_fun(params_dt, "ex_dep_dis_art35_par2")
  
  tax_credit <- get_param_fun(params_dt, "tax_credit")
  personal_allowance <- get_param_fun(params_dt, "personal_allowance")
  
  wage_cols <- c(
    "wage_base_prog",
    "pit_wage_flat",
    "pit_ials21_sal",
    "pit_wage_prog_standalone",
    "pitax_flat",
    "pitax"
  )
  
  dt_scn <- add_missing_numeric_cols(dt_scn, wage_cols)
  
  dt_scn[, (wage_cols) := 0]
  
  required_input_cols <- c(
    "tax_regime",
    "ials21_sumven_cur_SAL",
    "ials21_sumsc_p_cur_SAL",
    "ials21_sumsc_m_cur_SAL",
    "ials21_sumsc_sm_cur_SAL",
    "ials21_sumsc_n_cur_SAL",
    "ials21_sumsc_h_cur_SAL"
  )
  
  missing_input_cols <- setdiff(required_input_cols, names(dt_scn))
  
  if (length(missing_input_cols) > 0) {
    stop(
      paste0(
        "Missing input column(s) in dt1: ",
        paste(missing_input_cols, collapse = ", ")
      )
    )
  }
  
  dt_scn[tax_regime == "ials21",
         c(
           "wage_base_prog",
           "pit_wage_flat",
           "pit_ials21_sal",
           "pit_wage_prog_standalone",
           "pitax_flat",
           "pitax"
         ) := {
           
           deductions <-
             (ials21_sumsc_p_cur_SAL  / 27000) * per_ex_art33_par1 +
             (ials21_sumsc_m_cur_SAL  / 31500) * per_ex_inc_art33_par2 +
             (ials21_sumsc_sm_cur_SAL / 19800) * ex_spouse_art34_par2 +
             (ials21_sumsc_n_cur_SAL  /  9000) * ex_dep_art35_par1 +
             (ials21_sumsc_h_cur_SAL  / 19800) * ex_dep_dis_art35_par2 +
             (ials21_sumven_cur_SAL           * ins_prem_art36_par6)
           
           wage_base <- pmax(
             ials21_sumven_cur_SAL -
               deductions -
               tax_credit -
               personal_allowance,
             0
           )
           
           wage_flat <- wage_base * rate1
           
           wage_prog <- calc_progressive_tax(
             taxable = wage_base,
             rate1 = rate1,
             rate2 = rate2,
             rate3 = rate3,
             rate4 = rate4,
             tbrk1 = tbrk1,
             tbrk2 = tbrk2,
             tbrk3 = tbrk3
           )
           
           list(
             wage_base,
             wage_flat,
             wage_flat,
             wage_prog,
             wage_flat,
             wage_flat
           )
         }]
  
  dt_scn[]
}

# XII. Variables to grow -----------------------------------------------------

vars_to_grow <- c(
  "ials21_sumven_cur_SAL",
  "ials21_sumsc_p_cur_SAL",
  "ials21_sumsc_m_cur_SAL",
  "ials21_sumsc_sm_cur_SAL",
  "ials21_sumsc_n_cur_SAL",
  "ials21_sumsc_h_cur_SAL",
  "ials21_sumsc_tot_cur_SAL",
  "ials21_sumded1_cur_SAL",
  "ials21_sumimp_cur_SAL",
  "ials21_sumded2_cur_SAL",
  "total_income"
)

# XIII. Business as usual ----------------------------------------------------

PIT_BU_list1_all <- list()

dt_scn_BU <- copy(dt)

for (i in seq_along(scenarios)) {
  
  s <- scenarios[i]
  year_i <- forecast_horizon[i]
  
  gf_values <- get_growth_factor_row(
    scenario = s,
    growth_factors = growth_factors,
    scenarios = scenarios,
    vars_to_grow = vars_to_grow
  )
  
  dt_scn_BU <- apply_growth_only(
    dt_scn = dt_scn_BU,
    gf_values = gf_values,
    vars_to_grow = vars_to_grow
  )
  
  params_year_BU <- get_params_for_year(
    params_dt = pit_simulation_parameters_raw,
    year = year_i
  )
  
  dt_scn_BU <- tax_calc_wage_fun(
    dt_scn = dt_scn_BU,
    params_dt = params_year_BU
  )
  
  dt_scn_BU[, weight := weights_pit[[s]]]
  dt_scn_BU[, year := year_i]
  dt_scn_BU[, scenarios := s]
  
  PIT_BU_list1_all[[s]] <- copy(dt_scn_BU)
}

# XIV. Simulation ------------------------------------------------------------

start_index <- match(SimulationYear, scenario_years)

if (is.na(start_index)) {
  stop("SimulationYear is not inside scenario_years.")
}

PIT_SIM_list1_all <- list()

if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    PIT_SIM_list1_all[[s_early]] <- copy(PIT_BU_list1_all[[s_early]])
  }
}

if (start_index == 1) {
  dt_scn_SIM <- copy(dt)
} else {
  prev_scenario <- scenarios[start_index - 1]
  dt_scn_SIM <- copy(PIT_BU_list1_all[[prev_scenario]])
}

for (i in seq(from = start_index, to = length(scenarios))) {
  
  s <- scenarios[i]
  year_i <- forecast_horizon[i]
  
  gf_values <- get_growth_factor_row(
    scenario = s,
    growth_factors = growth_factors,
    scenarios = scenarios,
    vars_to_grow = vars_to_grow
  )
  
  dt_scn_SIM <- apply_growth_only(
    dt_scn = dt_scn_SIM,
    gf_values = gf_values,
    vars_to_grow = vars_to_grow
  )
  
  params_year_SIM <- get_params_for_year(
    params_dt = pit_simulation_parameters_updated,
    year = year_i
  )
  
  dt_scn_SIM <- tax_calc_wage_fun(
    dt_scn = dt_scn_SIM,
    params_dt = params_year_SIM
  )
  
  dt_scn_SIM[, weight := weights_pit[[s]]]
  dt_scn_SIM[, year := year_i]
  dt_scn_SIM[, scenarios := s]
  
  PIT_SIM_list1_all[[s]] <- copy(dt_scn_SIM)
}

# XV. Reduce internal lists to needed columns --------------------------------

keep_wage_cols <- c(
  "cod_fiscal",
  "tax_regime",
  "year",
  "scenarios",
  "ials21_sumven_cur_SAL",
  "wage_base_prog",
  "pit_wage_flat",
  "pit_ials21_sal",
  "pit_wage_prog_standalone",
  "pitax_flat",
  "pitax",
  "weight"
)

PIT_BU_list1_all <- lapply(PIT_BU_list1_all, function(x) {
  
  x <- as.data.table(x)
  
  x <- add_missing_numeric_cols(
    dt = x,
    cols = c(
      "ials21_sumven_cur_SAL",
      "wage_base_prog",
      "pit_wage_flat",
      "pit_ials21_sal",
      "pit_wage_prog_standalone",
      "pitax_flat",
      "pitax",
      "weight"
    )
  )
  
  x[, intersect(keep_wage_cols, names(x)), with = FALSE]
})

PIT_SIM_list1_all <- lapply(PIT_SIM_list1_all, function(x) {
  
  x <- as.data.table(x)
  
  x <- add_missing_numeric_cols(
    dt = x,
    cols = c(
      "ials21_sumven_cur_SAL",
      "wage_base_prog",
      "pit_wage_flat",
      "pit_ials21_sal",
      "pit_wage_prog_standalone",
      "pitax_flat",
      "pitax",
      "weight"
    )
  )
  
  x[, intersect(keep_wage_cols, names(x)), with = FALSE]
})

# XVI. Old-style selected objects for downstream scripts ---------------------

make_wage_compat <- function(x) {
  
  x <- as.data.table(x)
  
  x <- add_missing_numeric_cols(
    dt = x,
    cols = c(
      "ials21_sumven_cur_SAL",
      "wage_base_prog",
      "pit_wage_flat",
      "pit_ials21_sal",
      "pit_wage_prog_standalone",
      "pitax_flat",
      "pitax",
      "weight"
    )
  )
  
  if (!"cod_fiscal" %in% names(x)) {
    x[, cod_fiscal := NA_character_]
  }
  
  if (!"tax_regime" %in% names(x)) {
    x[, tax_regime := NA_character_]
  }
  
  x[, pit_ials21_sal := fifelse(
    pit_ials21_sal == 0 & pit_wage_flat != 0,
    pit_wage_flat,
    pit_ials21_sal
  )]
  
  x[, .(
    cod_fiscal = cod_fiscal,
    tax_regime = tax_regime,
    gross_income = ials21_sumven_cur_SAL,
    wages_inc = ials21_sumven_cur_SAL,
    investment_inc = 0,
    business_inc = 0,
    wages_pit = pit_wage_flat,
    pit_ials21_sal = pit_ials21_sal,
    investment_pit = 0,
    business_pit = 0,
    pitax = pit_wage_flat,
    weight = weight
  )]
}

PIT_BU_list1 <- get_scenario_df(
  year       = SimulationYear,
  horizons   = forecast_horizon,
  scenarios  = scenarios,
  table_list = PIT_BU_list1_all
) %>%
  as.data.table() %>%
  make_wage_compat()

PIT_SIM_list1 <- get_scenario_df(
  year       = SimulationYear,
  horizons   = forecast_horizon,
  scenarios  = scenarios,
  table_list = PIT_SIM_list1_all
) %>%
  as.data.table() %>%
  make_wage_compat()

# XVII. Old summary objects --------------------------------------------------

summary_BU1 <- summarize_block_list(
  PIT_list = PIT_BU_list1_all,
  suffix = "_bu",
  value_cols = c(
    "pitax_flat",
    "pit_wage_flat",
    "pit_ials21_sal",
    "wage_base_prog"
  )
)

summary_SIM1 <- summarize_block_list(
  PIT_list = PIT_SIM_list1_all,
  suffix = "_sim",
  value_cols = c(
    "pitax_flat",
    "pit_wage_flat",
    "pit_ials21_sal",
    "wage_base_prog"
  )
)

merged_PIT_BU_SIM1_raw <- merge(
  summary_BU1,
  summary_SIM1,
  by = "scenarios",
  all = TRUE
)

merged_PIT_BU_SIM1_raw <- as.data.table(merged_PIT_BU_SIM1_raw)

if ("pitax_flat_bu" %in% names(merged_PIT_BU_SIM1_raw)) {
  merged_PIT_BU_SIM1_raw[, pitax_bu := pitax_flat_bu]
}

if ("pitax_flat_sim" %in% names(merged_PIT_BU_SIM1_raw)) {
  merged_PIT_BU_SIM1_raw[, pitax_sim := pitax_flat_sim]
}

# GUI summary uses RAW values and divides internally

pit_summary_df1 <- build_pit_summary(
  merged_dt = merged_PIT_BU_SIM1_raw,
  forecast_horizon = forecast_horizon,
  macro_dt = MACRO_FISCAL_INDICATORS
)

# This is the object later scripts use.
# It MUST contain year and scenarios.
# Values are divided by 1e06.

merged_PIT_BU_SIM1 <- format_merged_compat(
  merged_dt = merged_PIT_BU_SIM1_raw,
  forecast_horizon = forecast_horizon
)

message("Script 1 completed: wage objects created with year-specific parameters.")