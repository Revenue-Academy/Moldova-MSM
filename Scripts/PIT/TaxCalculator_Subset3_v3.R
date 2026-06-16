# TaxCalculator_Subset3_v1.R ------------------------------------------------
# Business income block
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

# Local helper only for this script -----------------------------------------

if (!exists("add_missing_character_cols")) {
  
  add_missing_character_cols <- function(dt, cols, default_value = "") {
    
    dt <- as.data.table(dt)
    
    for (cc in cols) {
      if (!cc %in% names(dt)) {
        dt[, (cc) := default_value]
      }
    }
    
    dt
  }
}

# II. Input data -------------------------------------------------------------

dt <- copy(dt3)
dt <- as.data.table(dt)

weights_pit <- weights_pit3

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
# They are carried through Script 3 unchanged, except where they are also
# included in vars_to_grow and corresponding growth factors exist.

prepared_income_cols_script3 <- c(
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
  cols = prepared_income_cols_script3
)

# III. Variables to grow -----------------------------------------------------

vars_to_grow_base <- c(
  "ai_17_r1c2", "ai_17_r2c2", "ai_17_r3c2", "ai_17_r4c2",
  "ai_17_r5c2", "ai_17_r6c2", "ai_17_r7c2", "ai_17_r8c2",
  "ai_17_r9c2", "ai_17_Sumadecontrol",
  
  "ven12_r010", "ven12_r0101", "ven12_r0102", "ven12_r020",
  "ven12_r030", "ven12_r040", "ven12_r050", "ven12_r060",
  "ven12_r070", "ven12_r0701", "ven12_r080", "ven12_r0901",
  "ven12_r0902", "ven12_r090", "ven12_r100", "ven12_r110",
  "ven12_r120", "ven12_r130", "ven12_r140", "ven12_r150",
  "ven12_totald3c6", "ven12_totald3c7", "ven12_totald3c9",
  "ven12_totald3c10", "ven12_totald3c11", "ven12_totald4c3",
  "ven12_sumac", "ven12_row_idt", "ven12_sumavensc", "ven12_sumafacil",
  "ven12_procfac_6d",
  
  "daj17_r010", "daj17_r020", "daj17_r030", "daj17_r050",
  "daj17_r060", "daj17_c6", "daj17_c7", "daj17_c9",
  "daj17_c10", "daj17_c11", "daj17_r090", "daj17_r130",
  "daj17_r140", "daj17_control",
  
  "dass19_r010", "dass19_r0101", "dass19_r01011",
  "dass19_r01012", "dass19_r0102", "dass19_r020",
  "dass19_r030", "dass19_r040", "dass19_r050",
  "dass19_r060", "dass19_r070", "dass19_r080",
  "dass19_r090", "dass19_r100", "dass19_r110",
  "dass19_r130", "dass19_r140", "dass19_r150",
  "dass19_r160", "dass19_r170", "dass19_r180",
  "dass19_d2c6", "dass19_d2c7", "dass19_d2c8",
  "dass19_d2c9", "dass19_d2c10", "dass19_d2c11",
  "dass19_d2c12",
  
  "taxi18_sumac_cur", "taxi18_tot_col9",
  "taxi18_t1c7tot_cur", "taxi18_t1c8tot_cur", "taxi18_t1c9tot_cur",
  "taxi18_t1c7_cur", "taxi18_t1c8_cur", "taxi18_t1c9_cur",
  
  "unif21_t1r010", "unif21_t1r0101", "unif21_t1r0102",
  "unif21_t1r020", "unif21_t1r030", "unif21_t1r040",
  "unif21_t1r050", "unif21_t1r060", "unif21_t1r070",
  "unif21_t1r0701", "unif21_t1r080", "unif21_t1r090",
  "unif21_t1r0901", "unif21_t1r0902", "unif21_t1r100",
  "unif21_t1r120", "unif21_t1r130", "unif21_a1t1r120c4",
  "unif21_a1t1totc4", "unif21_a2t1totc6", "unif21_a2t1totc7",
  "unif21_a2t1totc8", "unif21_a2t1totc9", "unif21_a2t1totc10",
  
  "cet18_c1c3", "cet18_c31c3", "cet18_c5c3",
  "cet18_d1", "cet18_d2", "cet18_d3", "cet18_d4",
  "cet18_d5", "cet18_d6", "cet18_d7", "cet18_de",
  "cet18_e1", "cet18_e2", "cet18_e3", "cet18_e4",
  "cet18_f1", "cet18_f2", "cet18_f3", "cet18_f4", "cet18_f5",
  
  "total_income",
  "bus_base_prog"
)

# Only add prepared variables to growth if the growth_factors table contains
# their columns. This avoids changing the existing growth-factor requirement.

prepared_vars_available_for_growth <- intersect(
  c(
    "total_wage_income",
    "total_investment_income",
    "total_business_income",
    "wage_base_prog_input",
    "inv_base_prog",
    "total_prog_base_wage_business",
    "total_prog_base_wage_business_investment"
  ),
  names(as.data.table(growth_factors))
)

vars_to_grow <- unique(c(
  vars_to_grow_base,
  prepared_vars_available_for_growth
))

# New CET-18 input columns used for split calculations -----------------------

cet18_split_input_cols <- c(
  "cet18_h1c3",
  "cet18_c2c3",
  "cet18_c311c3",
  "cet18_c321c3",
  "cet18_c322c3",
  "cet18_c4c3",
  "cet18_h2c3",
  "cet18_h3c3",
  "cet18_h4c3",
  "cet18_h5c3",
  "cet18_h6c3",
  "cet18_h7c3"
)

business_numeric_input_cols <- unique(c(
  vars_to_grow,
  cet18_split_input_cols,
  "ven12_tp_category",
  "unif21_a1t1r120c3"
))

business_character_input_cols <- c(
  "ven12_exemption_idt"
)

# New CET-18 output columns --------------------------------------------------

cet18_component_cols <- c(
  "wages",
  "wages_abroad",
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

# IV. Business tax function --------------------------------------------------

tax_calc_business_fun <- function(dt_scn, params_dt) {
  
  dt_scn <- as.data.table(dt_scn)
  params_dt <- as.data.table(params_dt)
  
  dt_scn <- add_missing_numeric_cols(dt_scn, business_numeric_input_cols)
  dt_scn <- add_missing_character_cols(dt_scn, business_character_input_cols)
  
  if (!"tax_regime" %in% names(dt_scn)) {
    stop("Missing input column in dt3: tax_regime")
  }
  
  # Preserve business base before tax_credit and personal_allowance.
  dt_scn[, bus_base_input := bus_base_prog]
  
  rate1 <- get_param_fun(params_dt, "rate1")
  rate2 <- get_param_fun(params_dt, "rate2")
  rate3 <- get_param_fun(params_dt, "rate3")
  rate4 <- get_param_fun(params_dt, "rate4")
  
  tbrk1 <- get_param_fun(params_dt, "tbrk1")
  tbrk2 <- get_param_fun(params_dt, "tbrk2")
  tbrk3 <- get_param_fun(params_dt, "tbrk3")
  
  tax_credit <- get_param_fun(params_dt, "tax_credit")
  personal_allowance <- get_param_fun(params_dt, "personal_allowance")
  other_deduction <- get_param_fun(params_dt, "other_deduction")
  
  ai_17_rate_nat_per_art69_11 <- get_param_fun(params_dt, "ai_17_rate_nat_per_art69_11")
  ai_17_fix_nat_per_art69_11  <- get_param_fun(params_dt, "ai_17_fix_nat_per_art69_11")
  
  daj17_rate_indiv_art15a  <- get_param_fun(params_dt, "daj17_rate_indiv_art15a")
  dass19_rate_indiv_art15a <- get_param_fun(params_dt, "dass19_rate_indiv_art15a")
  
  ven12_rate_indiv_art15a <- get_param_fun(params_dt, "ven12_rate_indiv_art15a")
  ven12_rate_legal_art15b <- get_param_fun(params_dt, "ven12_rate_legal_art15b")
  ven12_rate_farm_art15c  <- get_param_fun(params_dt, "ven12_rate_farm_art15c")
  
  unif21_rate_indiv_art15a <- get_param_fun(params_dt, "unif21_rate_indiv_art15a")
  unif21_rate_farm_art15c  <- get_param_fun(params_dt, "unif21_rate_farm_art15c")
  
  taxi18_fixed_inc_tax <- get_param_fun(params_dt, "taxi18_fixed_inc_tax")
  
  contr_soc_mand      <- get_param_fun(params_dt, "contr_soc_mand")
  prem_health_mand    <- get_param_fun(params_dt, "prem_health_mand")
  ins_prem_art36_par6 <- get_param_fun(params_dt, "ins_prem_art36_par6")
  ded_cash_m_art69_11 <- get_param_fun(params_dt, "ded_cash_m_art69_11")
  
  rate_don_lim_art36   <- get_param_fun(params_dt, "rate_don_lim_art36")
  rate_undoc_exp_art24 <- get_param_fun(params_dt, "rate_undoc_exp_art24")
  
  per_ex_art33_par1     <- get_param_fun(params_dt, "per_ex_art33_par1")
  per_ex_inc_art33_par2 <- get_param_fun(params_dt, "per_ex_inc_art33_par2")
  ex_spouse_art34_par2  <- get_param_fun(params_dt, "ex_spouse_art34_par2")
  ex_dep_art35_par1     <- get_param_fun(params_dt, "ex_dep_art35_par1")
  ex_dep_dis_art35_par2 <- get_param_fun(params_dt, "ex_dep_dis_art35_par2")
  
  ven12_private_edu_exempt     <- get_param_fun(params_dt, "ven12_private_edu_exempt")
  ven12_payroll_growth_allow   <- get_param_fun(params_dt, "ven12_payroll_growth_allow")
  ven12_pension_fund_incentive <- get_param_fun(params_dt, "ven12_pension_fund_incentive")
  ven12_fez_admin_exempt       <- get_param_fun(params_dt, "ven12_fez_admin_exempt")
  ven12_cadastral_exempt       <- get_param_fun(params_dt, "ven12_cadastral_exempt")
  ven12_fez_export_50pct       <- get_param_fun(params_dt, "ven12_fez_export_50pct")
  ven12_fez_5y_holiday_invest  <- get_param_fun(params_dt, "ven12_fez_5y_holiday_invest")
  ven12_fez_domestic_75pct     <- get_param_fun(params_dt, "ven12_fez_domestic_75pct")
  
  # New rates required by revised CET-18 calculation -------------------------
  
  rate_adv_art90_par2        <- get_param_fun(params_dt, "rate_adv_art90_par2")
  rate_div_art90_1par31     <- get_param_fun(params_dt, "rate_div_art90_1par31")
  rate_roy_art90_1par31     <- get_param_fun(params_dt, "rate_roy_art90_1par31")
  rate_indiv_art71_90_par31 <- get_param_fun(params_dt, "rate_indiv_art71_90_par31")
  
  bus_pit_cols <- c(
    "pit_ai_17",
    "pit_daj17",
    "pit_dass19",
    "pit_ven12",
    "pit_unif21",
    "pit_taxi18",
    "pit_cet18"
  )
  
  output_cols <- c(
    bus_pit_cols,
    cet18_component_cols,
    "ssc",
    "d7_calc", "e1_calc", "e2_calc", "e3_calc", "e4_calc",
    "de_calc", "f1_calc", "f2_calc", "f3_calc", "f4_calc",
    "pit_bus_flat",
    "pit_bus_prog_standalone",
    "pitax_flat",
    "pitax"
  )
  
  dt_scn <- add_missing_numeric_cols(dt_scn, output_cols)
  dt_scn[, (output_cols) := 0]
  
  # 1. Form AI-17 -----------------------------------------------------------
  
  dt_scn[tax_regime == "ai_17",
         c("r3c2_calc", "r4c2_calc", "r5c2_calc",
           "r6c2_calc", "r7c2_calc", "pit_ai_17", "r9c2_calc") := {
             
             r3 <- ai_17_r1c2 * ai_17_rate_nat_per_art69_11
             r4 <- (ai_17_r4c2 / 3000) * ai_17_fix_nat_per_art69_11
             r5 <- pmax(r4, r3)
             r6 <- ai_17_r6c2 * ded_cash_m_art69_11
             r7 <- ai_17_r7c2
             
             diff <- r5 - r6 - r7
             
             r8 <- pmax(diff,  0)
             r9 <- pmax(-diff, 0)
             
             list(r3, r4, r5, r6, r7, r8, r9)
           }]
  
  # 2. Form DAJ-17 ----------------------------------------------------------
  
  dt_scn[tax_regime == "daj17",
         c("r040_calc", "r050_calc", "r060_calc", "tot_ded",
           "r070_calc", "r080_calc", "r100_calc", "r110_calc",
           "r130_calc", "coeff_trans", "r140_calc", "pit_daj17") := {
             
             r040 <- daj17_r010 + daj17_r020 - daj17_r030
             
             r050 <- fifelse(r040 <= 0 | is.na(daj17_r050) | daj17_r050 == 0,
                             0, r040 * rate_don_lim_art36)
             
             r060 <- fifelse(r040 <= 0 | is.na(daj17_r060) | daj17_r060 == 0,
                             0, r040 * rate_undoc_exp_art24)
             
             tot <- (daj17_c6  / 27000) * per_ex_art33_par1 +
               (daj17_c7  / 31500) * per_ex_inc_art33_par2 +
               (daj17_c9  / 19800) * ex_spouse_art34_par2 +
               (daj17_c10 /  9000) * ex_dep_art35_par1 +
               (daj17_c11 / 19800) * ex_dep_dis_art35_par2
             
             r070 <- fifelse(r040 - r050 - r060 > 0,
                             pmin(tot, r040 - r050 - r060), 0)
             
             r080 <- pmax(r040 - r050 - r060 - r070, 0)
             r100 <- r080 - daj17_r090
             r110 <- fifelse(r080 < 0, abs(r080), 0)
             
             r130 <- r100 * daj17_rate_indiv_art15a
             coeff <- fifelse(is.na(daj17_r140) | r130 == 0, 0, daj17_r140 / r130)
             r140 <- r130 * coeff
             r150 <- r130 - r140
             
             list(r040, r050, r060, tot, r070, r080, r100, r110,
                  r130, coeff, r140, r150)
           }]
  
  # 3. Form DASS-19 ---------------------------------------------------------
  
  dt_scn[tax_regime == "dass19",
         c("r0101_calc", "r010_calc", "r040_calc",
           "r050_calc", "r060_calc", "tot_ded",
           "r070_calc", "r080_calc", "r100_calc", "pit_dass19") := {
             
             r0101 <- dass19_r01011 + dass19_r01012
             r010  <- r0101 - dass19_r0102
             r040  <- r010 + dass19_r020 - dass19_r030
             
             r050 <- fifelse(!is.na(dass19_r050) & dass19_r050 > 0,
                             r040 * rate_don_lim_art36, 0)
             
             r060 <- fifelse(!is.na(dass19_r060) & dass19_r060 > 0,
                             r040 * rate_undoc_exp_art24, 0)
             
             tot <- (dass19_d2c6  / 27000) * per_ex_art33_par1 +
               (dass19_d2c7  / 31500) * per_ex_inc_art33_par2 +
               (dass19_d2c8  / 19800) * ex_spouse_art34_par2 +
               (dass19_d2c10 /  9000) * ex_dep_art35_par1 +
               (dass19_d2c11 / 19800) * ex_dep_dis_art35_par2
             
             r070 <- fifelse(r040 - r050 - r060 > 0,
                             pmin(tot, r040 - r050 - r060), 0)
             
             r080 <- pmax(r040 - r050 - r060 - r070, 0)
             r100 <- r080 - dass19_r090
             r130 <- r100 * dass19_rate_indiv_art15a
             
             list(r0101, r010, r040, r050, r060, tot, r070, r080, r100, r130)
           }]
  
  # 4. Form VEN-12 ----------------------------------------------------------
  
  dt_scn[tax_regime == "ven12",
         c("r010_calc", "r020_calc", "r030_calc", "r040_calc",
           "r050_calc", "r060_calc", "tot_ded", "r0701_calc",
           "r070_calc", "r080_calc", "r0901_calc", "r0902_calc", "r090_calc",
           "r100_calc", "r120_calc",
           "te_calc_6d", "r130_calc",
           "te_calc_4d", "r140_calc",
           "pit_ven12") := {
             
             r010 <- ven12_r0101 - ven12_r0102
             r020 <- ven12_r020
             r030 <- ven12_r030
             r040 <- r010 + r020 - r030
             
             r050 <- fifelse(
               r040 <= 0 | is.na(ven12_r050) | ven12_r050 == 0,
               0,
               r040 * rate_don_lim_art36
             )
             
             r060 <- fifelse(
               is.na(ven12_r060) | ven12_r060 <= 0,
               0,
               ven12_r060 * rate_undoc_exp_art24
             )
             
             tot <- (ven12_totald3c6  / 27000) * per_ex_art33_par1 +
               (ven12_totald3c7  / 31500) * per_ex_inc_art33_par2 +
               (ven12_totald3c9  / 19800) * ex_spouse_art34_par2 +
               (ven12_totald3c10 /  9000) * ex_dep_art35_par1 +
               (ven12_totald3c11 / 19800) * ex_dep_dis_art35_par2
             
             base_before_personal_ded <- pmax(r040 - r050 - r060, 0)
             
             r0701 <- pmin(tot, base_before_personal_ded)
             
             r070 <- pmax(base_before_personal_ded - r0701, 0)
             
             r080 <- pmin(r070, ven12_r080)
             
             r0901 <- pmax(r070 - r080, 0)
             
             r0902 <- ven12_totald4c3
             
             r090 <- pmax(r0901 - r0902, 0)
             
             r100 <- fifelse(r040 < 0, abs(r040), 0)
             
             r120 <- fcase(
               ven12_tp_category == 0, r090 * ven12_rate_indiv_art15a,
               ven12_tp_category == 1, r090 * ven12_rate_indiv_art15a,
               ven12_tp_category == 2, r090 * ven12_rate_farm_art15c,
               default = r090 * ven12_rate_legal_art15b
             )
             
             te6d <- fcase(
               ven12_exemption_idt == "6a", ven12_sumafacil * ven12_fez_export_50pct,
               ven12_exemption_idt == "6r", ven12_sumafacil * ven12_fez_5y_holiday_invest,
               ven12_exemption_idt == "6b", ven12_sumafacil * ven12_fez_domestic_75pct,
               ven12_exemption_idt == "6d", ven12_sumafacil * ven12_fez_domestic_75pct,
               default = ven12_sumafacil
             )
             
             r130 <- te6d
             
             te4d <- fcase(
               ven12_exemption_idt == "4c", ven12_sumafacil * ven12_private_edu_exempt,
               ven12_exemption_idt == "4i", ven12_sumafacil * ven12_payroll_growth_allow,
               ven12_exemption_idt == "4d", ven12_sumafacil * ven12_pension_fund_incentive,
               ven12_exemption_idt == "4b", ven12_sumafacil * ven12_fez_admin_exempt,
               ven12_exemption_idt == "4g", ven12_sumafacil * ven12_cadastral_exempt,
               default = ven12_sumafacil
             )
             
             r140 <- te4d
             
             r150 <- pmax(r120 - r130, 0)
             
             list(r010, r020, r030, r040,
                  r050, r060, tot, r0701,
                  r070, r080, r0901, r0902, r090,
                  r100, r120,
                  te6d, r130,
                  te4d, r140,
                  r150)
           }]
  
  # 5. Form UNIF-21 ---------------------------------------------------------
  
  dt_scn[tax_regime == "unif21",
         c("t1r010_calc", "t1r040_calc",
           "t1r050_calc", "t1r060_calc", "tot_ded",
           "t1r0701_calc", "t1r070_calc",
           "t1r0901_calc", "t1r090_calc",
           "pit_unif21") := {
             
             r010 <- unif21_t1r0101 - unif21_t1r0102
             r040 <- unif21_t1r010 + unif21_t1r020 - unif21_t1r030
             
             r050 <- fifelse(r040 <= 0 | is.na(unif21_t1r050) | unif21_t1r050 == 0,
                             0, r040 * rate_don_lim_art36)
             
             r060 <- fifelse(r040 <= 0, 0, r040 * rate_undoc_exp_art24)
             
             tot <- (unif21_a2t1totc6  / 27000) * per_ex_art33_par1 +
               (unif21_a2t1totc7  / 31500) * per_ex_inc_art33_par2 +
               (unif21_a2t1totc8  / 19800) * ex_spouse_art34_par2 +
               (unif21_a2t1totc9  /  9000) * ex_dep_art35_par1 +
               (unif21_a2t1totc10 / 19800) * ex_dep_dis_art35_par2
             
             r0701 <- fifelse(r040 - r050 - r060 > 0,
                              pmin(tot, r040 - r050 - r060), 0)
             
             r070 <- fifelse(r040 - r050 - r060 - r0701 < 0,
                             0, r040 - r050 - r060 - r0701)
             
             r0901 <- r070 - unif21_t1r080
             r090  <- r0901 - unif21_t1r0902
             
             r120 <- fcase(
               r090 < 0, 0,
               unif21_a1t1r120c3 %in% c(0, 12, 0.12), r090 * unif21_rate_indiv_art15a,
               unif21_a1t1r120c3 == 7, r090 * unif21_rate_farm_art15c,
               default = NA_real_
             )
             
             list(r010, r040,
                  r050, r060, tot,
                  r0701, r070,
                  r0901, r090,
                  r120)
           }]
  
  # 6. Form TAXI-18 ---------------------------------------------------------
  
  dt_scn[tax_regime == "taxi18",
         c("t1c7_cur_calc", "pit_taxi18", "t1c9_cur_calc") := {
           
           c7_calc <- fifelse(
             is.na(taxi18_t1c7_cur) | taxi18_t1c7_cur == 0,
             0, (taxi18_t1c7_cur / 14700) * contr_soc_mand
           )
           
           c8_calc <- fifelse(
             is.na(taxi18_t1c8_cur) | taxi18_t1c8_cur == 0,
             0, (taxi18_t1c8_cur / 6000) * taxi18_fixed_inc_tax
           )
           
           c9_calc <- fifelse(
             is.na(taxi18_t1c9_cur) | taxi18_t1c9_cur == 0,
             0, (taxi18_t1c9_cur / 12636) * prem_health_mand
           )
           
           list(c7_calc, c8_calc, c9_calc)
         }]
  
  # 7. Form CET-18 ----------------------------------------------------------
  
  dt_scn[tax_regime == "cet18",
         c("d7_calc", "e1_calc", "ssc", "e2_calc", "e3_calc", "e4_calc",
           "de_calc", "f1_calc", "f2_calc", "f3_calc", "f4_calc",
           "wages",
           "wages_abroad",
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
           "dividends_abroad",
           "pit_cet18") := {
             
             d7 <- (cet18_d1 / 27000) * per_ex_art33_par1 +
               (cet18_d2 / 31500) * per_ex_inc_art33_par2 +
               (cet18_d4 / 19800) * ex_spouse_art34_par2 +
               (cet18_d5 /  9000) * ex_dep_art35_par1 +
               (cet18_d6 / 19800) * ex_dep_dis_art35_par2
             
             ssc <- cet18_c1c3 * ins_prem_art36_par6
             
             e1 <- fifelse(!is.na(cet18_e1) & cet18_e1 > 0,
                           ssc, 0)
             
             e2 <- fifelse(!is.na(cet18_e2) & cet18_e2 > 0,
                           (cet18_e2 / 14700) * contr_soc_mand, 0)
             
             e3 <- fifelse(is.na(cet18_e3), 0, cet18_e3)
             e4 <- e1 + e2 + (e3 * other_deduction)
             de <- d7 + e4
             
             f1 <- pmax(cet18_c5c3 - de, 0)
             
             f2 <- cet18_f2
             f3 <- pmax(f2 - f1, 0)
             f4 <- f1 + f3
             
             wages_calc <- f4 * rate1
             
             wages_abroad_calc <- fifelse(
               !is.na(cet18_h1c3) & cet18_h1c3 > 0,
               cet18_h1c3 * rate1,
               0
             )
             
             capital_gains_calc <- fifelse(
               !is.na(cet18_c2c3) & cet18_c2c3 > 0,
               cet18_c2c3 * rate_adv_art90_par2,
               0
             )
             
             dividends_calc <- fifelse(
               !is.na(cet18_c311c3) & cet18_c311c3 > 0,
               cet18_c311c3 * rate_div_art90_1par31,
               0
             )
             
             interest_calc <- fifelse(
               !is.na(cet18_c321c3) & cet18_c321c3 > 0,
               cet18_c321c3 * rate_adv_art90_par2,
               0
             )
             
             royality_calc <- fifelse(
               !is.na(cet18_c322c3) & cet18_c322c3 > 0,
               cet18_c322c3 * rate_roy_art90_1par31,
               0
             )
             
             other_investment_income_calc <- fifelse(
               !is.na(cet18_c4c3) & cet18_c4c3 > 0,
               cet18_c4c3 * rate_adv_art90_par2,
               0
             )
             
             leasing_operations_abroad_calc <- fifelse(
               !is.na(cet18_h2c3) & cet18_h2c3 > 0,
               cet18_h2c3 * rate_indiv_art71_90_par31,
               0
             )
             
             capital_gains_abroad_calc <- fifelse(
               !is.na(cet18_h3c3) & cet18_h3c3 > 0,
               cet18_h3c3 * rate_indiv_art71_90_par31,
               0
             )
             
             interest_abroad_calc <- fifelse(
               !is.na(cet18_h4c3) & cet18_h4c3 > 0,
               cet18_h4c3 * rate_indiv_art71_90_par31,
               0
             )
             
             royalty_abroad_calc <- fifelse(
               !is.na(cet18_h5c3) & cet18_h5c3 > 0,
               cet18_h5c3 * rate_indiv_art71_90_par31,
               0
             )
             
             other_income_abroad_calc <- fifelse(
               !is.na(cet18_h6c3) & cet18_h6c3 > 0,
               cet18_h6c3 * rate_indiv_art71_90_par31,
               0
             )
             
             dividends_abroad_calc <- fifelse(
               !is.na(cet18_h7c3) & cet18_h7c3 > 0,
               cet18_h7c3 * rate_div_art90_1par31,
               0
             )
             
             pit_cet18_total <- 
               wages_calc +
               wages_abroad_calc +
               capital_gains_calc +
               dividends_calc +
               interest_calc +
               royality_calc +
               other_investment_income_calc +
               leasing_operations_abroad_calc +
               capital_gains_abroad_calc +
               interest_abroad_calc +
               royalty_abroad_calc +
               other_income_abroad_calc +
               dividends_abroad_calc
             
             list(
               d7, e1, ssc, e2, e3, e4,
               de, f1, f2, f3, f4,
               wages_calc,
               wages_abroad_calc,
               capital_gains_calc,
               dividends_calc,
               interest_calc,
               royality_calc,
               other_investment_income_calc,
               leasing_operations_abroad_calc,
               capital_gains_abroad_calc,
               interest_abroad_calc,
               royalty_abroad_calc,
               other_income_abroad_calc,
               dividends_abroad_calc,
               pit_cet18_total
             )
           }]
  
  # 8. Business flat and standalone progressive -----------------------------
  
  dt_scn[, pit_bus_flat := rowSums(.SD, na.rm = TRUE),
         .SDcols = bus_pit_cols]
  
  dt_scn[, bus_base_prog := pmax(
    bus_base_prog -
      tax_credit -
      personal_allowance,
    0
  )]
  
  dt_scn[, pit_bus_prog_standalone := calc_progressive_tax(
    taxable = bus_base_prog,
    rate1 = rate1,
    rate2 = rate2,
    rate3 = rate3,
    rate4 = rate4,
    tbrk1 = tbrk1,
    tbrk2 = tbrk2,
    tbrk3 = tbrk3
  )]
  
  dt_scn[, pitax_flat := pit_bus_flat]
  dt_scn[, pitax := pit_bus_flat]
  
  dt_scn[]
}

# V. Business as usual -------------------------------------------------------

PIT_BU_list3_all <- list()

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
  
  dt_scn_BU <- tax_calc_business_fun(
    dt_scn = dt_scn_BU,
    params_dt = params_year_BU
  )
  
  dt_scn_BU[, weight := weights_pit[[s]]]
  dt_scn_BU[, year := year_i]
  dt_scn_BU[, scenarios := s]
  
  PIT_BU_list3_all[[s]] <- copy(dt_scn_BU)
}

# VI. Simulation -------------------------------------------------------------

start_index <- match(SimulationYear, scenario_years)

if (is.na(start_index)) {
  stop("SimulationYear is not inside scenario_years.")
}

PIT_SIM_list3_all <- list()

if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    PIT_SIM_list3_all[[s_early]] <- copy(PIT_BU_list3_all[[s_early]])
  }
}

if (start_index == 1) {
  dt_scn_SIM <- copy(dt)
} else {
  prev_scenario <- scenarios[start_index - 1]
  dt_scn_SIM <- copy(PIT_BU_list3_all[[prev_scenario]])
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
  
  dt_scn_SIM <- tax_calc_business_fun(
    dt_scn = dt_scn_SIM,
    params_dt = params_year_SIM
  )
  
  dt_scn_SIM[, weight := weights_pit[[s]]]
  dt_scn_SIM[, year := year_i]
  dt_scn_SIM[, scenarios := s]
  
  PIT_SIM_list3_all[[s]] <- copy(dt_scn_SIM)
}

# VII. Reduce internal lists to needed columns -------------------------------

keep_bus_cols <- c(
  "cod_fiscal",
  "tax_regime",
  "year",
  "scenarios",
  "total_wage_income",
  "total_investment_income",
  "total_business_income",
  "total_income",
  "wage_base_prog_input",
  "inv_base_prog",
  "bus_base_input",
  "ssc",
  "bus_base_prog",
  "total_prog_base_wage_business",
  "total_prog_base_wage_business_investment",
  "decile_group",
  "centile_group",
  "pit_bus_flat",
  "pit_bus_prog_standalone",
  "pitax_flat",
  "pitax",
  "weight",
  "pit_cet18",
  cet18_component_cols
)

PIT_BU_list3_all <- lapply(PIT_BU_list3_all, function(x) {
  
  x <- as.data.table(x)
  
  x <- add_missing_numeric_cols(
    dt = x,
    cols = c(
      "total_wage_income",
      "total_investment_income",
      "total_business_income",
      "total_income",
      "wage_base_prog_input",
      "inv_base_prog",
      "bus_base_input",
      "ssc",
      "bus_base_prog",
      "total_prog_base_wage_business",
      "total_prog_base_wage_business_investment",
      "decile_group",
      "centile_group",
      "pit_bus_flat",
      "pit_bus_prog_standalone",
      "pitax_flat",
      "pitax",
      "weight",
      "pit_cet18",
      cet18_component_cols
    )
  )
  
  x[, intersect(keep_bus_cols, names(x)), with = FALSE]
})

PIT_SIM_list3_all <- lapply(PIT_SIM_list3_all, function(x) {
  
  x <- as.data.table(x)
  
  x <- add_missing_numeric_cols(
    dt = x,
    cols = c(
      "total_wage_income",
      "total_investment_income",
      "total_business_income",
      "total_income",
      "wage_base_prog_input",
      "inv_base_prog",
      "bus_base_input",
      "ssc",
      "bus_base_prog",
      "total_prog_base_wage_business",
      "total_prog_base_wage_business_investment",
      "decile_group",
      "centile_group",
      "pit_bus_flat",
      "pit_bus_prog_standalone",
      "pitax_flat",
      "pitax",
      "weight",
      "pit_cet18",
      cet18_component_cols
    )
  )
  
  x[, intersect(keep_bus_cols, names(x)), with = FALSE]
})

# VIII. Old-style selected objects for downstream scripts --------------------

make_bus_compat <- function(x) {
  
  x <- as.data.table(x)
  
  x <- add_missing_numeric_cols(
    dt = x,
    cols = c(
      "total_wage_income",
      "total_investment_income",
      "total_business_income",
      "total_income",
      "wage_base_prog_input",
      "inv_base_prog",
      "bus_base_input",
      "ssc",
      "bus_base_prog",
      "total_prog_base_wage_business",
      "total_prog_base_wage_business_investment",
      "decile_group",
      "centile_group",
      "pit_bus_flat",
      "pit_bus_prog_standalone",
      "pitax_flat",
      "pitax",
      "weight",
      "pit_cet18",
      cet18_component_cols
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
    inv_base_prog = inv_base_prog,
    bus_base_input = bus_base_input,
    ssc = ssc,
    bus_base_prog = bus_base_prog,
    total_prog_base_wage_business = total_prog_base_wage_business,
    total_prog_base_wage_business_investment =
      total_prog_base_wage_business_investment,
    
    decile_group = decile_group,
    centile_group = centile_group,
    
    gross_income = bus_base_input,
    wages_inc = 0,
    investment_inc = 0,
    business_inc = bus_base_input,
    wages_pit = 0,
    investment_pit = 0,
    business_pit = pit_bus_flat,
    pitax = pit_bus_flat,
    weight = weight
  )]
}

PIT_BU_list3 <- get_scenario_df(
  year       = SimulationYear,
  horizons   = forecast_horizon,
  scenarios  = scenarios,
  table_list = PIT_BU_list3_all
) %>%
  as.data.table() %>%
  make_bus_compat()

PIT_SIM_list3 <- get_scenario_df(
  year       = SimulationYear,
  horizons   = forecast_horizon,
  scenarios  = scenarios,
  table_list = PIT_SIM_list3_all
) %>%
  as.data.table() %>%
  make_bus_compat()

# IX. Old summary objects ----------------------------------------------------

summary_BU3 <- summarize_block_list(
  PIT_list = PIT_BU_list3_all,
  suffix = "_bu",
  value_cols = c(
    "pitax_flat",
    "pit_bus_flat",
    "bus_base_input",
    "ssc",
    "bus_base_prog",
    "total_business_income",
    "total_income",
    "pit_cet18",
    cet18_component_cols
  )
)

summary_SIM3 <- summarize_block_list(
  PIT_list = PIT_SIM_list3_all,
  suffix = "_sim",
  value_cols = c(
    "pitax_flat",
    "pit_bus_flat",
    "bus_base_input",
    "ssc",
    "bus_base_prog",
    "total_business_income",
    "total_income",
    "pit_cet18",
    cet18_component_cols
  )
)

merged_PIT_BU_SIM3_raw <- merge(
  summary_BU3,
  summary_SIM3,
  by = "scenarios",
  all = TRUE
)

merged_PIT_BU_SIM3_raw <- as.data.table(merged_PIT_BU_SIM3_raw)

if ("pitax_flat_bu" %in% names(merged_PIT_BU_SIM3_raw)) {
  merged_PIT_BU_SIM3_raw[, pitax_bu := pitax_flat_bu]
}

if ("pitax_flat_sim" %in% names(merged_PIT_BU_SIM3_raw)) {
  merged_PIT_BU_SIM3_raw[, pitax_sim := pitax_flat_sim]
}

# GUI summary uses RAW values and divides internally

pit_summary_df3 <- build_pit_summary(
  merged_dt = merged_PIT_BU_SIM3_raw,
  forecast_horizon = forecast_horizon,
  macro_dt = MACRO_FISCAL_INDICATORS
)

# This is the object later scripts use.
# It MUST contain year and scenarios.
# Values are divided by 1e06.

merged_PIT_BU_SIM3 <- format_merged_compat(
  merged_dt = merged_PIT_BU_SIM3_raw,
  forecast_horizon = forecast_horizon
)

# X. Checks for prepared columns ---------------------------------------------

check_script3_prepared_cols <- data.table(
  column = prepared_income_cols_script3,
  exists_BU_t0 =
    prepared_income_cols_script3 %in% names(PIT_BU_list3_all[[scenarios[1]]]),
  exists_SIM_t0 =
    prepared_income_cols_script3 %in% names(PIT_SIM_list3_all[[scenarios[1]]])
)

print(check_script3_prepared_cols[exists_BU_t0 == FALSE | exists_SIM_t0 == FALSE])

check_script3_income_summary <- rbind(
  PIT_BU_list3_all[[scenarios[1]]][
    ,
    .(
      source = "BU",
      scenario = scenarios[1],
      total_income = sum(total_income * weight, na.rm = TRUE),
      total_business_income = sum(total_business_income * weight, na.rm = TRUE),
      bus_base_input = sum(bus_base_input * weight, na.rm = TRUE),
      ssc = sum(ssc * weight, na.rm = TRUE),
      bus_base_prog = sum(bus_base_prog * weight, na.rm = TRUE),
      pit_bus_flat = sum(pit_bus_flat * weight, na.rm = TRUE),
      pit_cet18 = sum(pit_cet18 * weight, na.rm = TRUE)
    )
  ],
  PIT_SIM_list3_all[[scenarios[1]]][
    ,
    .(
      source = "SIM",
      scenario = scenarios[1],
      total_income = sum(total_income * weight, na.rm = TRUE),
      total_business_income = sum(total_business_income * weight, na.rm = TRUE),
      bus_base_input = sum(bus_base_input * weight, na.rm = TRUE),
      ssc = sum(ssc * weight, na.rm = TRUE),
      bus_base_prog = sum(bus_base_prog * weight, na.rm = TRUE),
      pit_bus_flat = sum(pit_bus_flat * weight, na.rm = TRUE),
      pit_cet18 = sum(pit_cet18 * weight, na.rm = TRUE)
    )
  ],
  fill = TRUE
)

print(check_script3_income_summary)

message("Script 3 completed: business objects created with year-specific parameters, revised CET-18 split, and ssc column.")