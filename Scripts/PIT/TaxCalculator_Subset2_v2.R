# TaxCalculator_Subset2_v1.R ------------------------------------------------
# Investment / final withholding income block
# Compatible with multi-year PIT policy parameter tables
# Requires Script 1 to be run first because helper functions are defined there.

setDTthreads(threads = 8)

# I. Check required helper functions ----------------------------------------

required_helpers <- c(
  "get_params_for_year",
  "get_param_fun",
  "calc_progressive_tax",
  "get_growth_factor_row",
  "apply_growth_only",
  "get_scenario_df",
  "add_missing_numeric_cols",
  "summarize_block_list",
  "format_merged_compat",
  "build_pit_summary"
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

# II. Input data -------------------------------------------------------------

dt <- copy(dt2)
dt <- as.data.table(dt)

weights_pit <- weights_pit2

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

# II.1 Prepared income/distribution columns ----------------------------------
# These columns are prepared in the master data-preparation script.
# They are carried through Script 2 unchanged, except where they are also
# included in vars_to_grow and corresponding growth factors exist.

prepared_income_cols_script2 <- c(
  "total_wage_income",
  "total_investment_income",
  "total_business_income",
  "total_income",
  "wage_base_prog_input",
  "inv_base_prog",
  "bus_base_prog",
  "total_prog_base_wage_business",
  "total_prog_base_wage_business_investment",
  "decile_group",
  "centile_group"
)

dt <- add_missing_numeric_cols(
  dt = dt,
  cols = prepared_income_cols_script2
)

# III. Investment tax function ----------------------------------------------

tax_calc_investment_fun <- function(dt_scn, params_dt) {
  
  dt_scn <- as.data.table(dt_scn)
  params_dt <- as.data.table(params_dt)
  
  rate1 <- get_param_fun(params_dt, "rate1")
  rate2 <- get_param_fun(params_dt, "rate2")
  rate3 <- get_param_fun(params_dt, "rate3")
  rate4 <- get_param_fun(params_dt, "rate4")
  
  tbrk1 <- get_param_fun(params_dt, "tbrk1")
  tbrk2 <- get_param_fun(params_dt, "tbrk2")
  tbrk3 <- get_param_fun(params_dt, "tbrk3")
  
  tax_credit <- get_param_fun(params_dt, "tax_credit")
  
  rate_indiv_art69       <- get_param_fun(params_dt, "rate_indiv_art69")
  rate_int_art89         <- get_param_fun(params_dt, "rate_int_art89")
  rate_adv_art90_par2    <- get_param_fun(params_dt, "rate_adv_art90_par2")
  rate_indiv_art90_1par3 <- get_param_fun(params_dt, "rate_indiv_art90_1par3")
  rate_roy_art90_1par31  <- get_param_fun(params_dt, "rate_roy_art90_1par31")
  rate_don_art90_1par31  <- get_param_fun(params_dt, "rate_don_art90_1par31")
  rate_div_art90_1par31  <- get_param_fun(params_dt, "rate_div_art90_1par31")
  rate_win_art90_1par33  <- get_param_fun(params_dt, "rate_win_art90_1par33")
  rate_nat_art90_1par35  <- get_param_fun(params_dt, "rate_nat_art90_1par35")
  rate_comm_art90_1par36 <- get_param_fun(params_dt, "rate_comm_art90_1par36")
  # rate_exmp             <- get_param_fun(params_dt, "rate_exmp")
  
  input_cols <- c(
    "ials21_sumven_cur_FOL_WH",
    # "ials21_sumven_cur_PLS_WH",
    "ials21_sumven_cur_PL_WH",
    "ials21_sumven_cur_ROY_WH",
    "ials21_sumven_cur_DONPF_WH",
    "ials21_sumven_cur_DON_P_WH",
    # "ials21_sumven_cur_RCSA_WH",
    "ials21_sumven_cur_DOBBA_WH",
    "ials21_sumven_cur_DOB_WH",
    "ials21_sumven_cur_VMS_WH",
    "ials21_sumimp_cur_DIVA_WH",
    # "ials21_sumven_cur_DON_WH",
    "ials21_sumven_cur_LIV_WH",
    "ials21_sumven_cur_NOR_WH",
    "ials21_sumven_cur_CSM_WH",
    "ials21_sumven_cur_AGRAC_WH",
    "ials21_sumven_cur_SER_WH",
    "inv_base_prog"
  )
  
  dt_scn <- add_missing_numeric_cols(dt_scn, input_cols)
  
  if (!"tax_regime" %in% names(dt_scn)) {
    stop("Missing input column in dt2: tax_regime")
  }
  
  # Preserve already grown/prepared inv_base_prog before resetting outputs.
  dt_scn[, inv_base_input := inv_base_prog]
  
  reset_cols <- c(
    "pit_ials21_fol",
    "pit_ials21_pls_exmp",
    "pit_ials21_pl",
    "pit_ials21_roy",
    "pit_ials21_donpf",
    "pit_ials21_don_p",
    "pit_ials21_rcsa",
    "pit_ials21_dobba",
    "pit_ials21_dob",
    "pit_ials21_vms",
    "pit_ials21_div",
    "pit_ials21_don",
    "pit_ials21_liv",
    "pit_ials21_nor",
    "pit_ials21_csm",
    "pit_ials21_agrac",
    "pit_ials21_ser",
    "inv_base_prog",
    "pit_inv_flat",
    "pit_inv_prog_standalone",
    "pitax_flat",
    "pitax"
  )
  
  dt_scn <- add_missing_numeric_cols(dt_scn, reset_cols)
  dt_scn[, (reset_cols) := 0]
  
  dt_scn[tax_regime == "ials21",
         c(
           "pit_ials21_fol",
           "pit_ials21_pls_exmp",
           "pit_ials21_pl",
           "pit_ials21_roy",
           "pit_ials21_donpf",
           "pit_ials21_don_p",
           "pit_ials21_rcsa",
           "pit_ials21_dobba",
           "pit_ials21_dob",
           "pit_ials21_vms",
           "pit_ials21_div",
           "pit_ials21_don",
           "pit_ials21_liv",
           "pit_ials21_nor",
           "pit_ials21_csm",
           "pit_ials21_agrac",
           "pit_ials21_ser",
           "inv_base_prog",
           "pit_inv_flat",
           "pit_inv_prog_standalone",
           "pitax_flat",
           "pitax"
         ) := {
           
           fol_wh_calc <- pmax(
             ials21_sumven_cur_FOL_WH - tax_credit,
             0
           ) * rate_indiv_art90_1par3
           
           pls_exmp_calc <- 0
           # pls_exmp_calc <- ials21_sumven_cur_PLS_WH * rate_exmp
           
           pl_wh_calc <- pmax(
             ials21_sumven_cur_PL_WH - tax_credit,
             0
           ) * rate_adv_art90_par2
           
           roy_wh_calc <- pmax(
             ials21_sumven_cur_ROY_WH - tax_credit,
             0
           ) * rate_roy_art90_1par31
           
           donpf_wh_calc <- pmax(
             ials21_sumven_cur_DONPF_WH - tax_credit,
             0
           ) * rate_don_art90_1par31
           
           don_p_wh_calc <- pmax(
             ials21_sumven_cur_DON_P_WH - tax_credit,
             0
           ) * rate_don_art90_1par31
           
           rcsa_wh_calc <- 0
           # rcsa_wh_calc <- ials21_sumven_cur_RCSA_WH * rate_exmp
           
           dobba_wh_calc <- pmax(
             ials21_sumven_cur_DOBBA_WH - tax_credit,
             0
           ) * rate_int_art89
           
           dob_wh_calc <- pmax(
             ials21_sumven_cur_DOB_WH - tax_credit,
             0
           ) * rate_int_art89
           
           vms_wh_calc <- pmax(
             ials21_sumven_cur_VMS_WH - tax_credit,
             0
           ) * rate_int_art89
           
           div_wh_calc <- pmax(
             ials21_sumimp_cur_DIVA_WH - tax_credit,
             0
           ) * rate_div_art90_1par31
           
           don_wh_calc <- 0
           # don_wh_calc <- ials21_sumven_cur_DON_WH * rate_exmp
           
           liv_wh_calc <- pmax(
             ials21_sumven_cur_LIV_WH - tax_credit,
             0
           ) * rate_nat_art90_1par35
           
           nor_wh_calc <- pmax(
             ials21_sumven_cur_NOR_WH - tax_credit,
             0
           ) * rate_win_art90_1par33
           
           csm_wh_calc <- pmax(
             ials21_sumven_cur_CSM_WH - tax_credit,
             0
           ) * rate_comm_art90_1par36
           
           agrac_wh_calc <- pmax(
             ials21_sumven_cur_AGRAC_WH - tax_credit,
             0
           ) * rate_indiv_art69
           
           ser_wh_calc <- pmax(
             ials21_sumven_cur_SER_WH - tax_credit,
             0
           ) * rate_indiv_art90_1par3
           
           inv_flat <-
             fol_wh_calc +
             pls_exmp_calc +
             pl_wh_calc +
             roy_wh_calc +
             donpf_wh_calc +
             don_p_wh_calc +
             rcsa_wh_calc +
             dobba_wh_calc +
             dob_wh_calc +
             vms_wh_calc +
             div_wh_calc +
             don_wh_calc +
             liv_wh_calc +
             nor_wh_calc +
             csm_wh_calc +
             agrac_wh_calc +
             ser_wh_calc
           
           inv_base <- pmax(
             inv_base_input -
               tax_credit,
             0
           )
           
           inv_prog <- calc_progressive_tax(
             taxable = inv_base,
             rate1 = rate1,
             rate2 = rate2,
             rate3 = rate3,
             rate4 = rate4,
             tbrk1 = tbrk1,
             tbrk2 = tbrk2,
             tbrk3 = tbrk3
           )
           
           list(
             fol_wh_calc,
             pls_exmp_calc,
             pl_wh_calc,
             roy_wh_calc,
             donpf_wh_calc,
             don_p_wh_calc,
             rcsa_wh_calc,
             dobba_wh_calc,
             dob_wh_calc,
             vms_wh_calc,
             div_wh_calc,
             don_wh_calc,
             liv_wh_calc,
             nor_wh_calc,
             csm_wh_calc,
             agrac_wh_calc,
             ser_wh_calc,
             inv_base,
             inv_flat,
             inv_prog,
             inv_flat,
             inv_flat
           )
         }]
  
  dt_scn[]
}

# IV. Variables to grow ------------------------------------------------------

vars_to_grow_base <- c(
  "ials21_sumven_cur_FOL_WH",
  "ials21_sumven_cur_DIVA_WH",
  "ials21_sumven_cur_PL_WH",
  "ials21_sumven_cur_ROY_WH",
  "ials21_sumven_cur_DONPF_WH",
  # "ials21_sumven_cur_RCSA_WH",
  "ials21_sumven_cur_DOBBA_WH",
  "ials21_sumven_cur_VMS_WH",
  # "ials21_sumven_cur_PLS_WH",
  # "ials21_sumven_cur_DON_WH",
  "ials21_sumven_cur_LIV_WH",
  "ials21_sumven_cur_NOR_WH",
  "ials21_sumven_cur_DOB_WH",
  "ials21_sumven_cur_CSM_WH",
  "ials21_sumven_cur_DON_P_WH",
  "ials21_sumven_cur_AGRAC_WH",
  "ials21_sumven_cur_SER_WH",
  "ials21_sumimp_cur_DIVA_WH",
  "total_income",
  "inv_base_prog"
)

# Only add prepared variables to growth if the growth_factors table contains
# their columns. This avoids changing the existing growth-factor requirement.

prepared_vars_available_for_growth <- intersect(
  c(
    "total_wage_income",
    "total_investment_income",
    "total_business_income",
    "wage_base_prog_input",
    "bus_base_prog",
    "total_prog_base_wage_business",
    "total_prog_base_wage_business_investment"
  ),
  names(as.data.table(growth_factors))
)

vars_to_grow <- unique(c(
  vars_to_grow_base,
  prepared_vars_available_for_growth
))

# V. Business as usual -------------------------------------------------------

PIT_BU_list2_all <- list()

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
  
  dt_scn_BU <- tax_calc_investment_fun(
    dt_scn = dt_scn_BU,
    params_dt = params_year_BU
  )
  
  dt_scn_BU[, weight := weights_pit[[s]]]
  dt_scn_BU[, year := year_i]
  dt_scn_BU[, scenarios := s]
  
  PIT_BU_list2_all[[s]] <- copy(dt_scn_BU)
}

# VI. Simulation -------------------------------------------------------------

start_index <- match(SimulationYear, scenario_years)

if (is.na(start_index)) {
  stop("SimulationYear is not inside scenario_years.")
}

PIT_SIM_list2_all <- list()

if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    PIT_SIM_list2_all[[s_early]] <- copy(PIT_BU_list2_all[[s_early]])
  }
}

if (start_index == 1) {
  dt_scn_SIM <- copy(dt)
} else {
  prev_scenario <- scenarios[start_index - 1]
  dt_scn_SIM <- copy(PIT_BU_list2_all[[prev_scenario]])
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
  
  dt_scn_SIM <- tax_calc_investment_fun(
    dt_scn = dt_scn_SIM,
    params_dt = params_year_SIM
  )
  
  dt_scn_SIM[, weight := weights_pit[[s]]]
  dt_scn_SIM[, year := year_i]
  dt_scn_SIM[, scenarios := s]
  
  PIT_SIM_list2_all[[s]] <- copy(dt_scn_SIM)
}

# VII. Reduce internal lists to needed columns -------------------------------

keep_inv_cols <- c(
  "cod_fiscal",
  "tax_regime",
  "year",
  "scenarios",
  "total_wage_income",
  "total_investment_income",
  "total_business_income",
  "total_income",
  "wage_base_prog_input",
  "inv_base_input",
  "inv_base_prog",
  "bus_base_prog",
  "total_prog_base_wage_business",
  "total_prog_base_wage_business_investment",
  "decile_group",
  "centile_group",
  "pit_inv_flat",
  "pit_inv_prog_standalone",
  "pitax_flat",
  "pitax",
  "weight"
)

PIT_BU_list2_all <- lapply(PIT_BU_list2_all, function(x) {
  
  x <- as.data.table(x)
  
  x <- add_missing_numeric_cols(
    dt = x,
    cols = c(
      "total_wage_income",
      "total_investment_income",
      "total_business_income",
      "total_income",
      "wage_base_prog_input",
      "inv_base_input",
      "inv_base_prog",
      "bus_base_prog",
      "total_prog_base_wage_business",
      "total_prog_base_wage_business_investment",
      "decile_group",
      "centile_group",
      "pit_inv_flat",
      "pit_inv_prog_standalone",
      "pitax_flat",
      "pitax",
      "weight"
    )
  )
  
  x[, intersect(keep_inv_cols, names(x)), with = FALSE]
})

PIT_SIM_list2_all <- lapply(PIT_SIM_list2_all, function(x) {
  
  x <- as.data.table(x)
  
  x <- add_missing_numeric_cols(
    dt = x,
    cols = c(
      "total_wage_income",
      "total_investment_income",
      "total_business_income",
      "total_income",
      "wage_base_prog_input",
      "inv_base_input",
      "inv_base_prog",
      "bus_base_prog",
      "total_prog_base_wage_business",
      "total_prog_base_wage_business_investment",
      "decile_group",
      "centile_group",
      "pit_inv_flat",
      "pit_inv_prog_standalone",
      "pitax_flat",
      "pitax",
      "weight"
    )
  )
  
  x[, intersect(keep_inv_cols, names(x)), with = FALSE]
})

# VIII. Old-style selected objects for downstream scripts --------------------

make_inv_compat <- function(x) {
  
  x <- as.data.table(x)
  
  x <- add_missing_numeric_cols(
    dt = x,
    cols = c(
      "total_wage_income",
      "total_investment_income",
      "total_business_income",
      "total_income",
      "wage_base_prog_input",
      "inv_base_input",
      "inv_base_prog",
      "bus_base_prog",
      "total_prog_base_wage_business",
      "total_prog_base_wage_business_investment",
      "decile_group",
      "centile_group",
      "pit_inv_flat",
      "pit_inv_prog_standalone",
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
  
  x[, .(
    cod_fiscal = cod_fiscal,
    tax_regime = tax_regime,
    
    total_wage_income = total_wage_income,
    total_investment_income = total_investment_income,
    total_business_income = total_business_income,
    total_income = total_income,
    
    wage_base_prog_input = wage_base_prog_input,
    inv_base_input = inv_base_input,
    inv_base_prog = inv_base_prog,
    bus_base_prog = bus_base_prog,
    total_prog_base_wage_business = total_prog_base_wage_business,
    total_prog_base_wage_business_investment =
      total_prog_base_wage_business_investment,
    
    decile_group = decile_group,
    centile_group = centile_group,
    
    gross_income = inv_base_input,
    wages_inc = 0,
    investment_inc = inv_base_input,
    business_inc = 0,
    wages_pit = 0,
    investment_pit = pit_inv_flat,
    business_pit = 0,
    pitax = pit_inv_flat,
    weight = weight
  )]
}

PIT_BU_list2 <- get_scenario_df(
  year       = SimulationYear,
  horizons   = forecast_horizon,
  scenarios  = scenarios,
  table_list = PIT_BU_list2_all
) %>%
  as.data.table() %>%
  make_inv_compat()

PIT_SIM_list2 <- get_scenario_df(
  year       = SimulationYear,
  horizons   = forecast_horizon,
  scenarios  = scenarios,
  table_list = PIT_SIM_list2_all
) %>%
  as.data.table() %>%
  make_inv_compat()

# IX. Old summary objects ----------------------------------------------------

summary_BU2 <- summarize_block_list(
  PIT_list = PIT_BU_list2_all,
  suffix = "_bu",
  value_cols = c(
    "pitax_flat",
    "pit_inv_flat",
    "inv_base_input",
    "inv_base_prog",
    "total_investment_income",
    "total_income"
  )
)

summary_SIM2 <- summarize_block_list(
  PIT_list = PIT_SIM_list2_all,
  suffix = "_sim",
  value_cols = c(
    "pitax_flat",
    "pit_inv_flat",
    "inv_base_input",
    "inv_base_prog",
    "total_investment_income",
    "total_income"
  )
)

merged_PIT_BU_SIM2_raw <- merge(
  summary_BU2,
  summary_SIM2,
  by = "scenarios",
  all = TRUE
)

merged_PIT_BU_SIM2_raw <- as.data.table(merged_PIT_BU_SIM2_raw)

if ("pitax_flat_bu" %in% names(merged_PIT_BU_SIM2_raw)) {
  merged_PIT_BU_SIM2_raw[, pitax_bu := pitax_flat_bu]
}

if ("pitax_flat_sim" %in% names(merged_PIT_BU_SIM2_raw)) {
  merged_PIT_BU_SIM2_raw[, pitax_sim := pitax_flat_sim]
}

# GUI summary uses RAW values and divides internally

pit_summary_df2 <- build_pit_summary(
  merged_dt = merged_PIT_BU_SIM2_raw,
  forecast_horizon = forecast_horizon,
  macro_dt = MACRO_FISCAL_INDICATORS
)

# This is the object later scripts use.
# It MUST contain year and scenarios.
# Values are divided by 1e06.

merged_PIT_BU_SIM2 <- format_merged_compat(
  merged_dt = merged_PIT_BU_SIM2_raw,
  forecast_horizon = forecast_horizon
)

# X. Checks for prepared columns ---------------------------------------------

check_script2_prepared_cols <- data.table(
  column = prepared_income_cols_script2,
  exists_BU_t0 =
    prepared_income_cols_script2 %in% names(PIT_BU_list2_all[[scenarios[1]]]),
  exists_SIM_t0 =
    prepared_income_cols_script2 %in% names(PIT_SIM_list2_all[[scenarios[1]]])
)

print(check_script2_prepared_cols[exists_BU_t0 == FALSE | exists_SIM_t0 == FALSE])

check_script2_income_summary <- rbind(
  PIT_BU_list2_all[[scenarios[1]]][
    ,
    .(
      source = "BU",
      scenario = scenarios[1],
      total_income = sum(total_income * weight, na.rm = TRUE),
      total_investment_income = sum(total_investment_income * weight, na.rm = TRUE),
      inv_base_input = sum(inv_base_input * weight, na.rm = TRUE),
      inv_base_prog = sum(inv_base_prog * weight, na.rm = TRUE),
      pit_inv_flat = sum(pit_inv_flat * weight, na.rm = TRUE)
    )
  ],
  PIT_SIM_list2_all[[scenarios[1]]][
    ,
    .(
      source = "SIM",
      scenario = scenarios[1],
      total_income = sum(total_income * weight, na.rm = TRUE),
      total_investment_income = sum(total_investment_income * weight, na.rm = TRUE),
      inv_base_input = sum(inv_base_input * weight, na.rm = TRUE),
      inv_base_prog = sum(inv_base_prog * weight, na.rm = TRUE),
      pit_inv_flat = sum(pit_inv_flat * weight, na.rm = TRUE)
    )
  ],
  fill = TRUE
)

print(check_script2_income_summary)

gc()
message("Script 2 completed: investment objects created with year-specific parameters.")