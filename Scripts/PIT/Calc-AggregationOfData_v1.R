# 04_pit_final_combine.R ----------------------------------------------------
# Final combined PIT progression script
# Fast data.table version
# Keeps old objects from Scripts 1, 2, and 3 unchanged
#
# Final outputs:
#   merged_PIT_BU_SIM_final
#   pit_summary_df_final
#   merged_PIT_BU_SIM
#   pit_summary_df
#   PIT_BU_dt
#   PIT_SIM_dt
#   PIT_BU_selected
#   PIT_SIM_selected

library(data.table)
library(dplyr)
library(tidyr)

setDTthreads(threads = 8)

# 0. Clean old function definitions -----------------------------------------

rm(list = intersect(
  c(
    "prepare_block",
    "prepare_block_fast",
    "combine_three_blocks_fast",
    "combine_pit_one_scenario",
    "combine_pit_list",
    "summarize_combined_pit",
    "make_combined_microdata",
    "calc_decile_percentile_fun"
  ),
  ls()
))

# I. Required helper checks --------------------------------------------------

required_helpers <- c(
  "get_param_fun",
  "get_params_for_year",
  "calc_progressive_tax",
  "get_scenario_df",
  "add_missing_numeric_cols"
)

missing_helpers <- required_helpers[!vapply(required_helpers, exists, logical(1))]

if (length(missing_helpers) > 0) {
  stop(
    paste0(
      "Missing helper function(s). Please run Script 1 first: ",
      paste(missing_helpers, collapse = ", ")
    )
  )
}

# II. Forecast horizon and scenarios ----------------------------------------

if (!exists("forecast_horizon")) {
  
  if (!exists("dt1")) {
    stop("forecast_horizon does not exist and dt1 is not available to recreate it.")
  }
  
  base_year <- unique(dt1$Year)[1]
  end_year <- base_year + 4
  forecast_horizon <- seq(base_year, end_year)
}

if (!exists("scenario_years")) {
  scenario_years <- forecast_horizon
}

if (!exists("scenarios")) {
  scenarios <- c("t0", "t1", "t2", "t3", "t4")
}

if (length(scenarios) != length(forecast_horizon)) {
  stop("Length of scenarios must equal length of forecast_horizon.")
}

# III. Check required input objects -----------------------------------------

required_objects <- c(
  "PIT_BU_list1_all",
  "PIT_BU_list2_all",
  "PIT_BU_list3_all",
  "PIT_SIM_list1_all",
  "PIT_SIM_list2_all",
  "PIT_SIM_list3_all",
  "pit_simulation_parameters_raw",
  "pit_simulation_parameters_updated",
  "MACRO_FISCAL_INDICATORS",
  "SimulationYear"
)

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "Missing required object(s): ",
      paste(missing_objects, collapse = ", ")
    )
  )
}

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

# IV. CET-18 component names -------------------------------------------------

cet18_wage_component_cols <- c(
  "wages",
  "wages_abroad"
)

cet18_inv_component_cols <- c(
  "capital_gains",
  "dividends",
  "interest",
  "royality",
  "other_investment_income",
  "leasing_operations_abroad",
  "capital_gains_abroad",
  "interest_abroad",
  "royalty_abroad",
  "other_income_abroad",
  "dividends_abroad"
)

cet18_component_cols <- c(
  cet18_wage_component_cols,
  cet18_inv_component_cols
)

# V. Helper: aggregate one PIT block by taxpayer ----------------------------

prepare_block_fast <- function(dt, needed_cols, prefix) {
  
  dt <- as.data.table(dt)
  
  if (!"cod_fiscal" %in% names(dt)) {
    stop("cod_fiscal is missing in ", prefix, " block.")
  }
  
  dt[, cod_fiscal := as.character(cod_fiscal)]
  
  for (cc in needed_cols) {
    if (!cc %in% names(dt)) {
      dt[, (cc) := 0]
    }
  }
  
  keep_cols <- c("cod_fiscal", needed_cols)
  dt <- dt[, ..keep_cols]
  
  for (cc in needed_cols) {
    dt[, (cc) := suppressWarnings(as.numeric(get(cc)))]
    dt[is.na(get(cc)), (cc) := 0]
  }
  
  ids <- unique(dt$cod_fiscal)
  
  id_map <- data.table(
    cod_fiscal = ids,
    tp_id = seq_along(ids)
  )
  
  dt[, tp_id := match(cod_fiscal, id_map$cod_fiscal)]
  
  sum_cols <- setdiff(needed_cols, "weight")
  
  out_sum <- dt[
    ,
    lapply(.SD, sum, na.rm = TRUE),
    by = tp_id,
    .SDcols = sum_cols
  ]
  
  out_weight <- dt[
    ,
    .(
      weight = max(weight, na.rm = TRUE)
    ),
    by = tp_id
  ]
  
  out <- out_sum[out_weight, on = "tp_id"]
  
  out[is.infinite(weight) | is.na(weight), weight := 0]
  
  out <- id_map[out, on = "tp_id"]
  
  old_names <- setdiff(names(out), c("cod_fiscal", "tp_id"))
  new_names <- paste0(old_names, "_", prefix)
  
  setnames(out, old_names, new_names)
  
  out[]
}

# VI. Helper: combine three prepared blocks ---------------------------------

combine_three_blocks_fast <- function(wage_dt, inv_dt, bus_dt) {
  
  wage_dt <- as.data.table(wage_dt)
  inv_dt  <- as.data.table(inv_dt)
  bus_dt  <- as.data.table(bus_dt)
  
  all_ids <- unique(c(
    wage_dt$cod_fiscal,
    inv_dt$cod_fiscal,
    bus_dt$cod_fiscal
  ))
  
  out <- data.table(
    cod_fiscal = as.character(all_ids)
  )
  
  add_block <- function(out, block_dt) {
    
    block_dt <- as.data.table(block_dt)
    
    block_cols <- setdiff(names(block_dt), c("cod_fiscal", "tp_id"))
    
    idx <- match(out$cod_fiscal, block_dt$cod_fiscal)
    
    for (cc in block_cols) {
      vals <- rep(0, nrow(out))
      hit <- !is.na(idx)
      vals[hit] <- block_dt[[cc]][idx[hit]]
      out[, (cc) := vals]
    }
    
    out
  }
  
  out <- add_block(out, wage_dt)
  out <- add_block(out, inv_dt)
  out <- add_block(out, bus_dt)
  
  out[]
}

# VII. Combine one scenario --------------------------------------------------

combine_pit_one_scenario <- function(wage_dt,
                                     inv_dt,
                                     bus_dt,
                                     params_dt,
                                     scenario_name,
                                     year_i) {
  
  params_dt <- as.data.table(params_dt)
  
  rate1 <- get_param_fun(params_dt, "rate1")
  rate2 <- get_param_fun(params_dt, "rate2")
  rate3 <- get_param_fun(params_dt, "rate3")
  rate4 <- get_param_fun(params_dt, "rate4")
  
  tbrk1 <- get_param_fun(params_dt, "tbrk1")
  tbrk2 <- get_param_fun(params_dt, "tbrk2")
  tbrk3 <- get_param_fun(params_dt, "tbrk3")
  
  toggle_progression_wages <- get_param_fun(params_dt, "toggle_progression_wages")
  toggle_progression_all   <- get_param_fun(params_dt, "toggle_progression_all")
  
  wage_needed <- c(
    "ials21_sumven_cur_SAL",
    "wage_base_prog",
    "pit_wage_flat",
    "pit_wage_prog_standalone",
    "weight"
  )
  
  inv_needed <- c(
    "inv_base_input",
    "inv_base_prog",
    "pit_inv_flat",
    "pit_inv_prog_standalone",
    "weight"
  )
  
  bus_needed <- c(
    "bus_base_input",
    "bus_base_prog",
    "pit_bus_flat",
    "pit_bus_prog_standalone",
    "pit_cet18",
    cet18_component_cols,
    "weight"
  )
  
  wage_dt_prepared <- prepare_block_fast(
    dt = wage_dt,
    needed_cols = wage_needed,
    prefix = "wage"
  )
  
  inv_dt_prepared <- prepare_block_fast(
    dt = inv_dt,
    needed_cols = inv_needed,
    prefix = "inv"
  )
  
  bus_dt_prepared <- prepare_block_fast(
    dt = bus_dt,
    needed_cols = bus_needed,
    prefix = "bus"
  )
  
  out <- combine_three_blocks_fast(
    wage_dt = wage_dt_prepared,
    inv_dt  = inv_dt_prepared,
    bus_dt  = bus_dt_prepared
  )
  
  out <- add_missing_numeric_cols(
    dt = out,
    cols = c(
      "weight_wage",
      "weight_inv",
      "weight_bus",
      "ials21_sumven_cur_SAL_wage",
      "inv_base_input_inv",
      "bus_base_input_bus",
      "wage_base_prog_wage",
      "inv_base_prog_inv",
      "bus_base_prog_bus",
      "pit_wage_flat_wage",
      "pit_inv_flat_inv",
      "pit_bus_flat_bus",
      "pit_wage_prog_standalone_wage",
      "pit_inv_prog_standalone_inv",
      "pit_bus_prog_standalone_bus",
      "pit_cet18_bus",
      paste0(cet18_component_cols, "_bus")
    )
  )
  
  out[, weight := pmax(
    weight_wage,
    weight_inv,
    weight_bus,
    na.rm = TRUE
  )]
  
  out[is.infinite(weight) | is.na(weight), weight := 0]
  
  # VIII. Bases --------------------------------------------------------------
  
  out[, wage_base_prog := pmax(wage_base_prog_wage, 0)]
  out[, inv_base_prog  := pmax(inv_base_prog_inv, 0)]
  out[, bus_base_prog  := pmax(bus_base_prog_bus, 0)]
  
  out[, wage_gross_income := pmax(ials21_sumven_cur_SAL_wage, 0)]
  out[, inv_gross_income  := pmax(inv_base_input_inv, 0)]
  out[, bus_gross_income  := pmax(bus_base_input_bus, 0)]
  
  out[, total_gross_income :=
        wage_gross_income +
        inv_gross_income +
        bus_gross_income]
  
  out[, total_prog_base :=
        wage_base_prog +
        inv_base_prog +
        bus_base_prog]
  
  # IX. CET-18 reclassification ---------------------------------------------
  
  out[, cet18_wage_pit := rowSums(.SD, na.rm = TRUE),
      .SDcols = paste0(cet18_wage_component_cols, "_bus")]
  
  out[, cet18_inv_pit := rowSums(.SD, na.rm = TRUE),
      .SDcols = paste0(cet18_inv_component_cols, "_bus")]
  
  out[, cet18_total_components :=
        cet18_wage_pit +
        cet18_inv_pit]
  
  out[, cet18_total_pit := fifelse(
    pit_cet18_bus > 0,
    pit_cet18_bus,
    cet18_total_components
  )]
  
  # X. Flat PIT by income block ---------------------------------------------
  # Note:
  # pit_bus_flat_bus already includes pit_cet18.
  # Therefore, subtract CET-18 total from business before reallocating
  # CET-18 components to wage and investment categories.
  
  out[, pit_wage_flat :=
        pit_wage_flat_wage +
        cet18_wage_pit]
  
  out[, pit_inv_flat :=
        pit_inv_flat_inv +
        cet18_inv_pit]
  
  out[, pit_bus_flat :=
        pmax(pit_bus_flat_bus - cet18_total_pit, 0)]
  
  out[, pit_flat_total :=
        pit_wage_flat +
        pit_inv_flat +
        pit_bus_flat]
  
  # XI. Wage-only progression -----------------------------------------------
  # Progressive tax is calculated only on the wage_base_prog from Script 1.
  # CET-18 wage components are kept as flat wage PIT because their base is not
  # part of wage_base_prog in the current structure.
  
  out[, pit_wage_prog_standalone := pit_wage_prog_standalone_wage]
  
  out[, pit_wage_only_reform :=
        pit_wage_prog_standalone +
        cet18_wage_pit +
        pit_inv_flat +
        pit_bus_flat]
  
  # XII. Full progression ----------------------------------------------------
  
  out[, pit_total_prog := calc_progressive_tax(
    taxable = total_prog_base,
    rate1 = rate1,
    rate2 = rate2,
    rate3 = rate3,
    rate4 = rate4,
    tbrk1 = tbrk1,
    tbrk2 = tbrk2,
    tbrk3 = tbrk3
  )]
  
  # XIII. Final PIT selection ------------------------------------------------
  
  if (toggle_progression_all == 1) {
    
    out[, pitax := pit_total_prog]
    out[, reform_type := "Full progression: wages + investment + business"]
    
  } else if (toggle_progression_wages == 1) {
    
    out[, pitax := pit_wage_only_reform]
    out[, reform_type := "Wage-only progression"]
    
  } else {
    
    out[, pitax := pit_flat_total]
    out[, reform_type := "Current law / flat-final system"]
  }
  
  out[, scenarios := scenario_name]
  out[, year := year_i]
  
  out[]
}

# XIV. Combine all scenarios with year-specific parameters -------------------

combine_pit_list <- function(PIT_list1,
                             PIT_list2,
                             PIT_list3,
                             params_dt,
                             forecast_horizon,
                             scenarios) {
  
  out_list <- vector("list", length(scenarios))
  names(out_list) <- scenarios
  
  for (i in seq_along(scenarios)) {
    
    s <- scenarios[i]
    year_i <- forecast_horizon[i]
    
    params_year <- get_params_for_year(
      params_dt = params_dt,
      year = year_i
    )
    
    out_list[[s]] <- combine_pit_one_scenario(
      wage_dt = PIT_list1[[s]],
      inv_dt  = PIT_list2[[s]],
      bus_dt  = PIT_list3[[s]],
      params_dt = params_year,
      scenario_name = s,
      year_i = year_i
    )
  }
  
  out_list
}

# XV. Create combined BU and SIM lists ---------------------------------------

PIT_BU_combined_list <- combine_pit_list(
  PIT_list1 = PIT_BU_list1_all,
  PIT_list2 = PIT_BU_list2_all,
  PIT_list3 = PIT_BU_list3_all,
  params_dt = pit_simulation_parameters_raw,
  forecast_horizon = forecast_horizon,
  scenarios = scenarios
)

PIT_SIM_combined_list <- combine_pit_list(
  PIT_list1 = PIT_SIM_list1_all,
  PIT_list2 = PIT_SIM_list2_all,
  PIT_list3 = PIT_SIM_list3_all,
  params_dt = pit_simulation_parameters_updated,
  forecast_horizon = forecast_horizon,
  scenarios = scenarios
)

# XVI. Weighted aggregation --------------------------------------------------

summarize_combined_pit <- function(PIT_list, suffix) {
  
  summary_list <- vector("list", length(PIT_list))
  names(summary_list) <- names(PIT_list)
  
  for (scenario_name in names(PIT_list)) {
    
    dt <- as.data.table(PIT_list[[scenario_name]])
    
    required_cols <- c(
      "pitax",
      "weight",
      "pit_flat_total",
      "pit_wage_only_reform",
      "pit_total_prog",
      "pit_wage_flat",
      "pit_inv_flat",
      "pit_bus_flat",
      "cet18_wage_pit",
      "cet18_inv_pit",
      "cet18_total_pit",
      "wage_gross_income",
      "inv_gross_income",
      "bus_gross_income",
      "total_gross_income",
      "wage_base_prog",
      "inv_base_prog",
      "bus_base_prog",
      "total_prog_base"
    )
    
    missing_cols <- setdiff(required_cols, names(dt))
    
    if (length(missing_cols) > 0) {
      stop(
        paste0(
          "Missing columns in scenario ", scenario_name, ": ",
          paste(missing_cols, collapse = ", ")
        )
      )
    }
    
    year_i <- unique(dt$year)
    
    if (length(year_i) != 1L) {
      year_i <- NA
    }
    
    summary_list[[scenario_name]] <- dt[
      ,
      .(
        scenarios = scenario_name,
        year = as.character(year_i),
        
        pitax = sum(pitax * weight, na.rm = TRUE),
        
        pit_flat_total = sum(pit_flat_total * weight, na.rm = TRUE),
        pit_wage_only_reform = sum(pit_wage_only_reform * weight, na.rm = TRUE),
        pit_total_prog = sum(pit_total_prog * weight, na.rm = TRUE),
        
        pit_wage_flat = sum(pit_wage_flat * weight, na.rm = TRUE),
        pit_inv_flat = sum(pit_inv_flat * weight, na.rm = TRUE),
        pit_bus_flat = sum(pit_bus_flat * weight, na.rm = TRUE),
        
        cet18_wage_pit = sum(cet18_wage_pit * weight, na.rm = TRUE),
        cet18_inv_pit = sum(cet18_inv_pit * weight, na.rm = TRUE),
        cet18_total_pit = sum(cet18_total_pit * weight, na.rm = TRUE),
        
        wage_gross_income = sum(wage_gross_income * weight, na.rm = TRUE),
        inv_gross_income = sum(inv_gross_income * weight, na.rm = TRUE),
        bus_gross_income = sum(bus_gross_income * weight, na.rm = TRUE),
        total_gross_income = sum(total_gross_income * weight, na.rm = TRUE),
        
        wage_base_prog = sum(wage_base_prog * weight, na.rm = TRUE),
        inv_base_prog = sum(inv_base_prog * weight, na.rm = TRUE),
        bus_base_prog = sum(bus_base_prog * weight, na.rm = TRUE),
        total_prog_base = sum(total_prog_base * weight, na.rm = TRUE)
      )
    ]
  }
  
  result <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
  
  old_names <- setdiff(names(result), c("scenarios", "year"))
  new_names <- paste0(old_names, suffix)
  
  setnames(result, old_names, new_names)
  
  as.data.table(result)
}

summary_BU_combined <- summarize_combined_pit(
  PIT_list = PIT_BU_combined_list,
  suffix = "_bu"
)

summary_SIM_combined <- summarize_combined_pit(
  PIT_list = PIT_SIM_combined_list,
  suffix = "_sim"
)

# XVII. Merge BU and SIM combined summaries ---------------------------------

merged_PIT_BU_SIM_combined <- merge(
  summary_BU_combined,
  summary_SIM_combined,
  by = c("year", "scenarios"),
  all = TRUE
)

setcolorder(
  merged_PIT_BU_SIM_combined,
  c("year", "scenarios", setdiff(names(merged_PIT_BU_SIM_combined), c("year", "scenarios")))
)

numeric_columns <- names(merged_PIT_BU_SIM_combined)[
  sapply(merged_PIT_BU_SIM_combined, is.numeric)
]

numeric_columns <- setdiff(numeric_columns, "year")

merged_PIT_BU_SIM_combined[
  ,
  (numeric_columns) := lapply(.SD, function(x) x / 1e06),
  .SDcols = numeric_columns
]

merged_PIT_BU_SIM_final <- copy(merged_PIT_BU_SIM_combined)

merged_PIT_BU_SIM <- copy(merged_PIT_BU_SIM_final)

# XVIII. GUI summary table for final combined result ------------------------

pit_summary_df <- merged_PIT_BU_SIM_final[
  ,
  .(
    year,
    `Current law (LCU Mil)` = round(pitax_bu, 1),
    `Simulation (LCU Mil)` = round(pitax_sim, 1),
    `Fiscal impact (LCU Mil)` = round(pitax_sim - pitax_bu, 1)
  )
]

MACRO_FISCAL_INDICATORS <- as.data.table(MACRO_FISCAL_INDICATORS)
MACRO_FISCAL_INDICATORS[, Year := as.character(Year)]

pit_summary_df <- merge(
  pit_summary_df,
  MACRO_FISCAL_INDICATORS[, .(Year, Nominal_GDP)],
  by.x = "year",
  by.y = "Year",
  all.x = TRUE
)

pit_summary_df[
  ,
  `Current law (Pct of GDP)` :=
    round(`Current law (LCU Mil)` / Nominal_GDP * 100, 2)
]

pit_summary_df[
  ,
  `Simulation (Pct of GDP)` :=
    round(`Simulation (LCU Mil)` / Nominal_GDP * 100, 2)
]

pit_summary_df[
  ,
  `Fiscal impact (Pct of GDP)` :=
    round(`Fiscal impact (LCU Mil)` / Nominal_GDP * 100, 2)
]

pit_summary_df[, Nominal_GDP := NULL]

setcolorder(
  pit_summary_df,
  c(
    "year",
    "Current law (LCU Mil)",
    "Simulation (LCU Mil)",
    "Fiscal impact (LCU Mil)",
    "Current law (Pct of GDP)",
    "Simulation (Pct of GDP)",
    "Fiscal impact (Pct of GDP)"
  )
)

pit_summary_df_final <- copy(pit_summary_df)

# XIX. Extract selected simulation year combined microdata ------------------

PIT_BU_selected_combined <- get_scenario_df(
  year = SimulationYear,
  horizons = forecast_horizon,
  scenarios = scenarios,
  table_list = PIT_BU_combined_list
) %>%
  as.data.table()

PIT_SIM_selected_combined <- get_scenario_df(
  year = SimulationYear,
  horizons = forecast_horizon,
  scenarios = scenarios,
  table_list = PIT_SIM_combined_list
) %>%
  as.data.table()

# XX. Prepare microdata for percentiles / deciles ---------------------------

make_combined_microdata <- function(dt) {
  
  dt <- as.data.table(dt)
  
  needed_cols <- c(
    "cod_fiscal",
    "total_gross_income",
    "wage_gross_income",
    "inv_gross_income",
    "bus_gross_income",
    "total_prog_base",
    "wage_base_prog",
    "inv_base_prog",
    "bus_base_prog",
    "pit_wage_flat",
    "pit_inv_flat",
    "pit_bus_flat",
    "cet18_wage_pit",
    "cet18_inv_pit",
    "cet18_total_pit",
    "pitax",
    "weight",
    "pit_flat_total",
    "pit_wage_only_reform",
    "pit_total_prog"
  )
  
  dt <- add_missing_numeric_cols(
    dt = dt,
    cols = setdiff(needed_cols, "cod_fiscal")
  )
  
  if (!"cod_fiscal" %in% names(dt)) {
    dt[, cod_fiscal := NA_character_]
  }
  
  if (!"reform_type" %in% names(dt)) {
    dt[, reform_type := NA_character_]
  }
  
  out <- dt[
    ,
    .(
      cod_fiscal = cod_fiscal,
      tax_regime = "combined_pit",
      
      gross_income = total_gross_income,
      
      wages_inc = wage_gross_income,
      investment_inc = inv_gross_income,
      business_inc = bus_gross_income,
      
      total_prog_base = total_prog_base,
      wage_base_prog = wage_base_prog,
      inv_base_prog = inv_base_prog,
      bus_base_prog = bus_base_prog,
      
      wages_pit = pit_wage_flat,
      investment_pit = pit_inv_flat,
      business_pit = pit_bus_flat,
      
      cet18_wage_pit = cet18_wage_pit,
      cet18_inv_pit = cet18_inv_pit,
      cet18_total_pit = cet18_total_pit,
      
      pitax = pitax,
      weight = weight,
      
      pit_flat_total = pit_flat_total,
      pit_wage_only_reform = pit_wage_only_reform,
      pit_total_prog = pit_total_prog,
      reform_type = reform_type
    )
  ]
  
  out[]
}

PIT_BU_dt <- make_combined_microdata(PIT_BU_selected_combined)
PIT_SIM_dt <- make_combined_microdata(PIT_SIM_selected_combined)

PIT_BU_dt <- PIT_BU_dt[gross_income >= 10]
PIT_SIM_dt <- PIT_SIM_dt[gross_income >= 10]

# XXI. Helper: add decile and centile groups --------------------------------

calc_decile_percentile_fun <- function(DT,
                                       inc_col = "gross_income",
                                       w_col  = "weight") {
  
  setDT(DT)
  DT <- copy(DT)
  
  DT <- DT[get(inc_col) > 0 & !is.na(get(inc_col))]
  
  DT[, row_id__tmp := .I]
  
  setorderv(DT, inc_col)
  
  DT[, w_cumsum__tmp := cumsum(replace(get(w_col), is.na(get(w_col)), 0))]
  
  total_w <- DT[.N, w_cumsum__tmp]
  
  if (is.na(total_w) || total_w <= 0) {
    
    DT[, decile_group := NA_integer_]
    DT[, centile_group := NA_integer_]
    DT[, c("row_id__tmp", "w_cumsum__tmp") := NULL]
    
    return(DT[])
  }
  
  decile_breaks  <- total_w * 0:10 / 10
  centile_breaks <- total_w * 0:100 / 100
  
  DT[
    ,
    decile_group := pmin(
      findInterval(
        w_cumsum__tmp,
        decile_breaks,
        rightmost.closed = TRUE
      ),
      10
    )
  ]
  
  DT[
    ,
    centile_group := pmin(
      findInterval(
        w_cumsum__tmp,
        centile_breaks,
        rightmost.closed = TRUE
      ),
      100
    )
  ]
  
  setorderv(DT, "row_id__tmp")
  
  DT[, c("row_id__tmp", "w_cumsum__tmp") := NULL]
  
  DT[]
}

# XXII. Apply percentiles / deciles -----------------------------------------

PIT_BU_selected <- calc_decile_percentile_fun(PIT_BU_dt)

PIT_SIM_selected <- calc_decile_percentile_fun(PIT_SIM_dt)

# XXIII. Preserve old block objects -----------------------------------------
# Do NOT remove:
# PIT_BU_list1, PIT_BU_list2, PIT_BU_list3
# PIT_SIM_list1, PIT_SIM_list2, PIT_SIM_list3
# pit_summary_df1, pit_summary_df2, pit_summary_df3
# merged_PIT_BU_SIM1, merged_PIT_BU_SIM2, merged_PIT_BU_SIM3

simulation_year <- SimulationYear

# XXIV. Print final outputs -------------------------------------------------

print(merged_PIT_BU_SIM)
print(pit_summary_df)

message("Final combined PIT progression script completed with year-specific parameters and CET-18 reclassification.")


# Calc-Distribution-Effects.R ----------------------------------------------
# Distribution tables
# Corrected version:
#   - uses fixed BU centile/decile groups for BU and SIM
#   - compares the same taxpayers across BU and SIM
#   - uses weighted sums
#   - gross_income now uses total_gross_income before tax_credit/personal_allowance
#     while total_prog_base remains the taxable/progressive base

library(data.table)
library(dplyr)

# I. Checks -----------------------------------------------------------------

if (!exists("PIT_BU_selected")) {
  stop("Missing object: PIT_BU_selected. Please run final combined PIT script first.")
}

if (!exists("PIT_SIM_selected")) {
  stop("Missing object: PIT_SIM_selected. Please run final combined PIT script first.")
}

PIT_BU_selected <- as.data.table(PIT_BU_selected)
PIT_SIM_selected <- as.data.table(PIT_SIM_selected)

required_cols_distribution <- c(
  "cod_fiscal",
  "weight",
  "gross_income",
  "pitax",
  "centile_group",
  "decile_group"
)

missing_bu_cols <- setdiff(required_cols_distribution, names(PIT_BU_selected))
missing_sim_cols <- setdiff(
  c("cod_fiscal", "weight", "gross_income", "pitax"),
  names(PIT_SIM_selected)
)

if (length(missing_bu_cols) > 0) {
  stop(
    paste0(
      "Missing column(s) in PIT_BU_selected: ",
      paste(missing_bu_cols, collapse = ", ")
    )
  )
}

if (length(missing_sim_cols) > 0) {
  stop(
    paste0(
      "Missing column(s) in PIT_SIM_selected: ",
      paste(missing_sim_cols, collapse = ", ")
    )
  )
}

# II. Prepare fixed BU groups ------------------------------------------------

PIT_BU_selected[, cod_fiscal := as.character(cod_fiscal)]
PIT_SIM_selected[, cod_fiscal := as.character(cod_fiscal)]

bu_groups <- PIT_BU_selected[
  ,
  .(
    cod_fiscal,
    decile_group_fixed = decile_group,
    centile_group_fixed = centile_group
  )
]

# Attach BU groups to BU and SIM
PIT_BU_dist <- merge(
  PIT_BU_selected,
  bu_groups,
  by = "cod_fiscal",
  all.x = TRUE
)

PIT_SIM_dist <- merge(
  PIT_SIM_selected[
    ,
    .(
      cod_fiscal,
      weight,
      gross_income,
      pitax
    )
  ],
  bu_groups,
  by = "cod_fiscal",
  all.x = TRUE
)

# Keep only taxpayers with BU group
PIT_BU_dist <- PIT_BU_dist[!is.na(decile_group_fixed)]
PIT_SIM_dist <- PIT_SIM_dist[!is.na(decile_group_fixed)]

# III. Centile groups --------------------------------------------------------

# 1. BU ---------------------------------------------------------------------

pit_centile_distribution_bu <- PIT_BU_dist[
  ,
  .(
    sum_calc_pitax = sum(pitax * weight, na.rm = TRUE),
    sum_total_gross_income = sum(gross_income * weight, na.rm = TRUE),
    taxpayers = .N
  ),
  by = .(centile_group = centile_group_fixed)
]

pit_centile_distribution_bu[
  ,
  etr := fifelse(
    sum_total_gross_income > 0,
    sum_calc_pitax / sum_total_gross_income,
    NA_real_
  )
]

setorder(pit_centile_distribution_bu, centile_group)

# 2. SIM --------------------------------------------------------------------

pit_centile_distribution_sim <- PIT_SIM_dist[
  ,
  .(
    sum_calc_pitax = sum(pitax * weight, na.rm = TRUE),
    sum_total_gross_income = sum(gross_income * weight, na.rm = TRUE),
    taxpayers = .N
  ),
  by = .(centile_group = centile_group_fixed)
]

pit_centile_distribution_sim[
  ,
  etr := fifelse(
    sum_total_gross_income > 0,
    sum_calc_pitax / sum_total_gross_income,
    NA_real_
  )
]

setorder(pit_centile_distribution_sim, centile_group)

# 3. Merge BU and SIM --------------------------------------------------------

pit_centile_distribution_bu_sim <- merge(
  pit_centile_distribution_bu,
  pit_centile_distribution_sim,
  by = "centile_group",
  suffixes = c("_bu", "_sim"),
  all = TRUE
)

setorder(pit_centile_distribution_bu_sim, centile_group)

pit_centile_distribution_bu_sim[
  ,
  `:=`(
    diff_pit = sum_calc_pitax_sim - sum_calc_pitax_bu,
    diff_gross_income = sum_total_gross_income_sim - sum_total_gross_income_bu,
    diff_etr = etr_sim - etr_bu,
    etr_sim_same_bu_denominator =
      fifelse(
        sum_total_gross_income_bu > 0,
        sum_calc_pitax_sim / sum_total_gross_income_bu,
        NA_real_
      )
  )
]

pit_centile_distribution_bu_sim[
  ,
  diff_etr_same_bu_denominator := etr_sim_same_bu_denominator - etr_bu
]

# IV. Decile groups ----------------------------------------------------------

# 1. BU ---------------------------------------------------------------------

pit_decile_distribution_bu <- PIT_BU_dist[
  ,
  .(
    sum_calc_pitax = sum(pitax * weight, na.rm = TRUE),
    mean_calc_pitax = weighted.mean(pitax, weight, na.rm = TRUE),
    sum_total_gross_income = sum(gross_income * weight, na.rm = TRUE),
    taxpayers = .N
  ),
  by = .(decile_group = decile_group_fixed)
]

pit_decile_distribution_bu[
  ,
  etr := fifelse(
    sum_total_gross_income > 0,
    sum_calc_pitax / sum_total_gross_income,
    NA_real_
  )
]

setorder(pit_decile_distribution_bu, decile_group)

# 2. SIM --------------------------------------------------------------------

pit_decile_distribution_sim <- PIT_SIM_dist[
  ,
  .(
    sum_calc_pitax = sum(pitax * weight, na.rm = TRUE),
    mean_calc_pitax = weighted.mean(pitax, weight, na.rm = TRUE),
    sum_total_gross_income = sum(gross_income * weight, na.rm = TRUE),
    taxpayers = .N
  ),
  by = .(decile_group = decile_group_fixed)
]

pit_decile_distribution_sim[
  ,
  etr := fifelse(
    sum_total_gross_income > 0,
    sum_calc_pitax / sum_total_gross_income,
    NA_real_
  )
]

setorder(pit_decile_distribution_sim, decile_group)

# 3. Merge BU and SIM --------------------------------------------------------

pit_decile_distribution_bu_sim_raw <- merge(
  pit_decile_distribution_bu,
  pit_decile_distribution_sim,
  by = "decile_group",
  suffixes = c("_bu", "_sim"),
  all = TRUE
)

setorder(pit_decile_distribution_bu_sim_raw, decile_group)

pit_decile_distribution_bu_sim_raw[
  ,
  `:=`(
    diff_pit = sum_calc_pitax_sim - sum_calc_pitax_bu,
    diff_gross_income = sum_total_gross_income_sim - sum_total_gross_income_bu,
    diff_etr = etr_sim - etr_bu,
    etr_sim_same_bu_denominator =
      fifelse(
        sum_total_gross_income_bu > 0,
        sum_calc_pitax_sim / sum_total_gross_income_bu,
        NA_real_
      )
  )
]

pit_decile_distribution_bu_sim_raw[
  ,
  diff_etr_same_bu_denominator := etr_sim_same_bu_denominator - etr_bu
]

# V. Presentation table ------------------------------------------------------

pit_decile_distribution_bu_sim <- copy(pit_decile_distribution_bu_sim_raw)

pit_decile_distribution_bu_sim[
  ,
  decile_group := as.character(decile_group)
]

setnames(
  pit_decile_distribution_bu_sim,
  old = c(
    "decile_group",
    "sum_calc_pitax_bu",
    "mean_calc_pitax_bu",
    "sum_total_gross_income_bu",
    "etr_bu",
    "sum_calc_pitax_sim",
    "mean_calc_pitax_sim",
    "sum_total_gross_income_sim",
    "etr_sim",
    "diff_pit",
    "diff_gross_income",
    "diff_etr",
    "etr_sim_same_bu_denominator",
    "diff_etr_same_bu_denominator"
  ),
  new = c(
    "Decile groups",
    "Total PIT liability (business as usual)",
    "Average PIT liability (business as usual)",
    "Total gross income (business as usual)",
    "ETR (business as usual)",
    "Total PIT liability (simulation)",
    "Average PIT liability (simulation)",
    "Total gross income (simulation)",
    "ETR (simulation)",
    "Difference in PIT liability",
    "Difference in gross income",
    "Difference in ETR",
    "ETR simulation with BU denominator",
    "Difference in ETR with BU denominator"
  ),
  skip_absent = TRUE
)

pit_decile_distribution_bu_sim <- pit_decile_distribution_bu_sim %>%
  mutate(across(
    .cols = where(is.numeric) &
      !starts_with("Average PIT liability") &
      !starts_with("ETR") &
      !starts_with("Difference in ETR"),
    .fns = ~ round(. / 1e06, 1)
  )) %>%
  mutate(across(
    .cols = where(is.numeric) &
      starts_with("Average PIT liability"),
    .fns = ~ round(. / 1000, 1)
  )) %>%
  mutate(across(
    .cols = where(is.numeric) &
      (
        starts_with("ETR") |
          starts_with("Difference in ETR")
      ),
    .fns = ~ round(. * 100, 2)
  ))

# VI. PIT distribution table by income breaks --------------------------------
# Uses gross_income based on original gross/base values before tax credits.

breaks <- c(
  -Inf,
  0,
  1e-09,
  500000.0,
  1000000.0,
  1500000.0,
  2000000.0,
  3000000.0,
  4000000.0,
  5000000.0,
  10000000.0,
  9e+99
)

labels <- c(
  "<0",
  "=0",
  "0-0.5 m",
  "0.5-1m",
  "1-1.5m",
  "1.5-2m",
  "2-3m",
  "3-4m",
  "4-5m",
  "5-10m",
  ">10m"
)

# 1. BU ---------------------------------------------------------------------

combined_dt_bins_bu <- PIT_BU_dist %>%
  dplyr::select(cod_fiscal, weight, gross_income, pitax) %>%
  mutate(
    weighted_gross_income = weight * gross_income,
    weighted_pitax = weight * pitax,
    bin_group = cut(
      gross_income,
      breaks = breaks,
      labels = labels,
      right = FALSE
    )
  ) %>%
  as.data.table()

pit_result_bins_bu <- combined_dt_bins_bu[
  ,
  .(
    sum_calc_pitax = sum(weighted_pitax, na.rm = TRUE)
  ),
  by = .(bin_group)
]

all_bu <- combined_dt_bins_bu[
  ,
  .(
    bin_group = "ALL",
    sum_calc_pitax = sum(weighted_pitax, na.rm = TRUE)
  )
]

pit_result_bins_bu <- rbind(
  pit_result_bins_bu,
  all_bu,
  fill = TRUE
)

pit_result_bins_bu_sub <- pit_result_bins_bu %>%
  filter(!as.character(bin_group) %in% c("ALL", "0", "=0"))

pit_result_bins_bu_sub <- as.data.table(pit_result_bins_bu_sub)

pit_result_bins_bu_sub[
  ,
  bin_group := factor(
    bin_group,
    levels = labels
  )
]

setorder(pit_result_bins_bu_sub, bin_group)

pit_result_bins_bu_sub[
  ,
  sum_calc_pitax := round(sum_calc_pitax / 1e06, 1)
]

# 2. SIM --------------------------------------------------------------------

combined_dt_bins_sim <- PIT_SIM_dist %>%
  dplyr::select(cod_fiscal, weight, gross_income, pitax) %>%
  mutate(
    weighted_gross_income = weight * gross_income,
    weighted_pitax = weight * pitax,
    bin_group = cut(
      gross_income,
      breaks = breaks,
      labels = labels,
      right = FALSE
    )
  ) %>%
  as.data.table()

pit_result_bins_sim <- combined_dt_bins_sim[
  ,
  .(
    sum_calc_pitax = sum(weighted_pitax, na.rm = TRUE)
  ),
  by = .(bin_group)
]

all_sim <- combined_dt_bins_sim[
  ,
  .(
    bin_group = "ALL",
    sum_calc_pitax = sum(weighted_pitax, na.rm = TRUE)
  )
]

pit_result_bins_sim <- rbind(
  pit_result_bins_sim,
  all_sim,
  fill = TRUE
)

pit_result_bins_sim_sub <- pit_result_bins_sim %>%
  filter(!as.character(bin_group) %in% c("ALL", "0", "=0"))

pit_result_bins_sim_sub <- as.data.table(pit_result_bins_sim_sub)

pit_result_bins_sim_sub[
  ,
  bin_group := factor(
    bin_group,
    levels = labels
  )
]

setorder(pit_result_bins_sim_sub, bin_group)

pit_result_bins_sim_sub[
  ,
  sum_calc_pitax := round(sum_calc_pitax / 1e06, 1)
]

message("Distribution tables completed for selected simulation year only using fixed BU groups.")

###

# Data prep Distribution Dashboard ------------------------------------------
# Uses selected simulation year only:
#   PIT_BU_selected
#   PIT_SIM_selected

library(data.table)
library(dplyr)
library(tidyr)

# I. Checks -----------------------------------------------------------------

if (!exists("PIT_BU_selected")) {
  stop("Missing object: PIT_BU_selected. Please run final combined PIT script first.")
}

if (!exists("PIT_SIM_selected")) {
  stop("Missing object: PIT_SIM_selected. Please run final combined PIT script first.")
}

PIT_BU_selected <- as.data.table(PIT_BU_selected)
PIT_SIM_selected <- as.data.table(PIT_SIM_selected)

required_cols_structure <- c(
  "decile_group",
  "wages_inc",
  "investment_inc",
  "business_inc",
  "wages_pit",
  "investment_pit",
  "business_pit"
)

missing_bu_cols <- setdiff(required_cols_structure, names(PIT_BU_selected))
missing_sim_cols <- setdiff(required_cols_structure, names(PIT_SIM_selected))

if (length(missing_bu_cols) > 0) {
  stop(
    paste0(
      "Missing column(s) in PIT_BU_selected: ",
      paste(missing_bu_cols, collapse = ", ")
    )
  )
}

if (length(missing_sim_cols) > 0) {
  stop(
    paste0(
      "Missing column(s) in PIT_SIM_selected: ",
      paste(missing_sim_cols, collapse = ", ")
    )
  )
}

# II. Gross income structure -------------------------------------------------
# 1. BU ---------------------------------------------------------------------

PIT_BU_selected_dec <- PIT_BU_selected %>%
  dplyr::select(
    decile_group,
    wages_inc,
    investment_inc,
    business_inc
  )

PIT_BU_selected_dec_agg <- PIT_BU_selected_dec %>%
  group_by(decile_group) %>%
  summarise(
    wages_inc = sum(wages_inc, na.rm = TRUE),
    investment_inc = sum(investment_inc, na.rm = TRUE),
    business_inc = sum(business_inc, na.rm = TRUE),
    .groups = "drop"
  )

colnames(PIT_BU_selected_dec_agg)[-1] <- paste0(
  "bu_",
  colnames(PIT_BU_selected_dec_agg)[-1]
)

# 2. SIM --------------------------------------------------------------------

PIT_SIM_selected_dec <- PIT_SIM_selected %>%
  dplyr::select(
    decile_group,
    wages_inc,
    investment_inc,
    business_inc
  )

PIT_SIM_selected_dec_agg <- PIT_SIM_selected_dec %>%
  group_by(decile_group) %>%
  summarise(
    wages_inc = sum(wages_inc, na.rm = TRUE),
    investment_inc = sum(investment_inc, na.rm = TRUE),
    business_inc = sum(business_inc, na.rm = TRUE),
    .groups = "drop"
  )

colnames(PIT_SIM_selected_dec_agg)[-1] <- paste0(
  "sim_",
  colnames(PIT_SIM_selected_dec_agg)[-1]
)

# 3. Merge and reshape -------------------------------------------------------

merged_df <- left_join(
  PIT_BU_selected_dec_agg,
  PIT_SIM_selected_dec_agg,
  by = "decile_group"
)

long_df <- merged_df %>%
  pivot_longer(
    cols = -decile_group,
    names_to = c("scenario", "income_type"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  filter(scenario == "sim") %>%
  arrange(decile_group, income_type, scenario)

# 4. Pie chart input ---------------------------------------------------------

structure_gross_inc <- long_df %>%
  group_by(income_type) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# III. PIT revenue structure -------------------------------------------------
# 1. BU ---------------------------------------------------------------------

PIT_BU_selected_pit_dec <- PIT_BU_selected %>%
  dplyr::select(
    decile_group,
    wages_pit,
    investment_pit,
    business_pit
  )

PIT_BU_selected_pit_dec_agg <- PIT_BU_selected_pit_dec %>%
  group_by(decile_group) %>%
  summarise(
    wages_pit = sum(wages_pit, na.rm = TRUE),
    investment_pit = sum(investment_pit, na.rm = TRUE),
    business_pit = sum(business_pit, na.rm = TRUE),
    .groups = "drop"
  )

colnames(PIT_BU_selected_pit_dec_agg)[-1] <- paste0(
  "bu_",
  colnames(PIT_BU_selected_pit_dec_agg)[-1]
)

# 2. SIM --------------------------------------------------------------------

PIT_SIM_selected_pit_dec <- PIT_SIM_selected %>%
  dplyr::select(
    decile_group,
    wages_pit,
    investment_pit,
    business_pit
  )

PIT_SIM_selected_pit_dec_agg <- PIT_SIM_selected_pit_dec %>%
  group_by(decile_group) %>%
  summarise(
    wages_pit = sum(wages_pit, na.rm = TRUE),
    investment_pit = sum(investment_pit, na.rm = TRUE),
    business_pit = sum(business_pit, na.rm = TRUE),
    .groups = "drop"
  )

colnames(PIT_SIM_selected_pit_dec_agg)[-1] <- paste0(
  "sim_",
  colnames(PIT_SIM_selected_pit_dec_agg)[-1]
)

# 3. Merge and reshape -------------------------------------------------------

merged_df_pit <- left_join(
  PIT_BU_selected_pit_dec_agg,
  PIT_SIM_selected_pit_dec_agg,
  by = "decile_group"
)

long_df_pit <- merged_df_pit %>%
  pivot_longer(
    cols = -decile_group,
    names_to = c("scenario", "income_type"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  filter(scenario == "sim") %>%
  arrange(decile_group, income_type, scenario)

# 4. Pie chart input ---------------------------------------------------------

structure_pit <- long_df_pit %>%
  group_by(income_type) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

message("Distribution dashboard data prepared for selected simulation year.")


#####


# Calc-Redistribution-Effects.R ---------------------------------------------
# Re-distribution tables
# Uses selected simulation year only:
#   PIT_BU_selected
#   PIT_SIM_selected

library(data.table)
library(dplyr)
library(ineq)
library(IC2)

# I. Checks -----------------------------------------------------------------

if (!exists("PIT_BU_selected")) {
  stop("Missing object: PIT_BU_selected. Please run final combined PIT script first.")
}

if (!exists("PIT_SIM_selected")) {
  stop("Missing object: PIT_SIM_selected. Please run final combined PIT script first.")
}

PIT_BU_selected <- as.data.table(PIT_BU_selected)
PIT_SIM_selected <- as.data.table(PIT_SIM_selected)

required_cols_re <- c(
  "cod_fiscal",
  "gross_income",
  "pitax"
)

missing_bu_cols <- setdiff(required_cols_re, names(PIT_BU_selected))
missing_sim_cols <- setdiff(required_cols_re, names(PIT_SIM_selected))

if (length(missing_bu_cols) > 0) {
  stop(
    paste0(
      "Missing column(s) in PIT_BU_selected: ",
      paste(missing_bu_cols, collapse = ", ")
    )
  )
}

if (length(missing_sim_cols) > 0) {
  stop(
    paste0(
      "Missing column(s) in PIT_SIM_selected: ",
      paste(missing_sim_cols, collapse = ", ")
    )
  )
}

# II. Helper function --------------------------------------------------------

calc_re_indicators <- function(dt, scenario_label) {
  
  dt <- as.data.table(dt)
  
  dt <- dt[
    !is.na(gross_income) &
      !is.na(pitax) &
      gross_income > 0
  ]
  
  if (nrow(dt) == 0) {
    stop(paste0("No positive observations available for: ", scenario_label))
  }
  
  # Top 1 percent share of PIT ----------------------------------------------
  
  dt_top <- copy(dt)
  
  dt_top <- dt_top %>%
    mutate(percentile = ntile(gross_income, 100)) %>%
    filter(percentile == 100)
  
  top1_pit <- sum(dt_top$pitax, na.rm = TRUE)
  total_pit <- sum(dt$pitax, na.rm = TRUE)
  
  share_top1 <- ifelse(
    total_pit > 0,
    top1_pit / total_pit,
    NA_real_
  )
  
  # Gini coefficient for pre-tax income -------------------------------------
  
  gini_income_gross <- round(
    ineq::ineq(dt$gross_income, type = "Gini", na.rm = TRUE),
    4
  )
  
  # Kakwani index ------------------------------------------------------------
  # Concentration coefficient of PIT with respect to gross income
  # minus Gini coefficient of gross income.
  
  sconc_result <- IC2::calcSConc(dt$pitax, dt$gross_income)
  
  kakwani_index <- round(
    sconc_result$ineq$index - gini_income_gross,
    4
  )
  
  kakwani_index <- unname(kakwani_index)
  
  # Effective tax rate -------------------------------------------------------
  
  etr <- ifelse(
    sum(dt$gross_income, na.rm = TRUE) > 0,
    sum(dt$pitax, na.rm = TRUE) / sum(dt$gross_income, na.rm = TRUE),
    NA_real_
  )
  
  # Output table -------------------------------------------------------------
  
  indicator_table <- data.table(
    Indicator = c(
      "Gini coefficient for pre-tax income",
      "Effective tax rate",
      "Kakwani Index",
      "Top 1% taxpayers' share of total PIT"
    ),
    Name = c(
      paste0("gini_income_gross_", scenario_label),
      paste0("etr_", scenario_label),
      paste0("kakwani_index_", scenario_label),
      paste0("share_top1_", scenario_label)
    ),
    Simulation = c(
      round(gini_income_gross, 4),
      round(etr, 4),
      round(kakwani_index, 4),
      round(share_top1, 4)
    )
  )
  
  list(
    data = dt,
    indicator_table = indicator_table,
    gini_income_gross = gini_income_gross,
    etr = etr,
    kakwani_index = kakwani_index,
    share_top1 = share_top1
  )
}

# III. BU indicators ---------------------------------------------------------

bu_results <- calc_re_indicators(
  dt = PIT_BU_selected,
  scenario_label = "bu"
)

PIT_BU_simulation_year_df <- bu_results$data
indicator_table_bu <- bu_results$indicator_table

gini_income_gross_bu <- bu_results$gini_income_gross
etr_bu <- bu_results$etr
kakwani_index_BU <- bu_results$kakwani_index
share_top1_bu <- bu_results$share_top1

# IV. SIM indicators ---------------------------------------------------------

sim_results <- calc_re_indicators(
  dt = PIT_SIM_selected,
  scenario_label = "sim"
)

PIT_SIM_simulation_year_df <- sim_results$data
indicator_table_SIM <- sim_results$indicator_table

gini_income_gross_sim <- sim_results$gini_income_gross
etr_SIM <- sim_results$etr
kakwani_index_SIM <- sim_results$kakwani_index
share_top1_sim <- sim_results$share_top1

# V. Merge final redistribution table ---------------------------------------

re_effects_final <- merge(
  indicator_table_bu,
  indicator_table_SIM[, .(Indicator, Simulation)],
  by = "Indicator",
  suffixes = c("_bu", "_sim")
) %>%
  dplyr::select(-Name) %>%
  dplyr::rename(
    "Business as usual" = "Simulation_bu",
    "Simulation" = "Simulation_sim"
  ) %>%
  dplyr::mutate(
    `Business as usual` = round(`Business as usual`, 4),
    Simulation = round(Simulation, 4),
    `Percentage Difference (%)` = dplyr::if_else(
      !is.na(`Business as usual`) & `Business as usual` != 0,
      round((Simulation - `Business as usual`) / `Business as usual` * 100, 1),
      NA_real_
    )
  ) %>%
  as.data.table()

# VI. Optional memory cleanup ------------------------------------------------
# Do not remove objects that may not exist. This avoids errors in Shiny/future.

objects_to_remove <- c(
  "PIT_BU_list",
  "PIT_SIM_list",
  "extracted_dist_tables_bu",
  "extracted_dist_tables_sim",
  "extracted_tables_bu",
  "extracted_tables_sim",
  
  "PIT_BU_combined_list",
  "PIT_SIM_combined_list",
  "PIT_BU_selected_combined",
  "PIT_SIM_selected_combined",
  "PIT_BU_list2_all",
  "PIT_SIM_list2_all",
  "PIT_BU_list1_all",
  "PIT_SIM_list1_all",
  "subset2_dt",
  "subset1_dt",
  "dt_scn_BU",
  "dt_scn_SIM",
  "PIT_BU_list3_all",
  "PIT_SIM_list3_all",
  "subset3_dt",
  "subset_cet18",
  "PIT_BU_dist",
  "PIT_BU_selected",
  "PIT_SIM_selected",
  "bu_results",
  "sim_results",
  "PIT_BU_simulation_year_df",
  "PIT_SIM_simulation_year_df",
  "PIT_BU_dt",
  "PIT_SIM_dt",
  "cet18_ids",
  "dt",
  "combined_dt_bins_bu",
  "combined_dt_bins_sim",
  "PIT_SIM_dist",
  "PIT_BU_list2",
  "PIT_SIM_list2"
)

rm(list = intersect(objects_to_remove, ls()))

gc(TRUE)

message("Redistribution effects completed for selected simulation year.")
gc(TRUE)

message("Redistribution effects completed for selected simulation year.")