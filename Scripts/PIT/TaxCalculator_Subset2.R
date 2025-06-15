
# pit_simulation_parameters_raw<- read_excel("PIT-Parameters.xlsx")
# pit_simulation_parameters_updated<-pit_simulation_parameters_raw
# SimulationYear<-2023


dt<-dt2
weights_pit<-weights_pit2

setDTthreads(threads = 8)

get_param_fun <- function(params_dt, param_name) {
  params_dt[Parameters == param_name, Value]
}

            base_year <- unique(dt$Year)[1]
            end_year <- base_year + 4
            
            
            simulation_year <- SimulationYear  # Year from slider
            forecast_horizon <- seq(base_year, end_year)
            scenario_years<-forecast_horizon
            
            # Define the scenarios
            scenarios <- c("t0", "t1", "t2", "t3", "t4")
            
            # Simulation parameters must be in data.table
            pit_simulation_parameters_raw <- pit_simulation_parameters_raw %>% data.table()
            pit_simulation_parameters_updated <- pit_simulation_parameters_updated %>% data.table()


# 1. Tax Calculation Function -------------------------------------------------------
#start.time <- proc.time()
tax_calc_fun <- function(dt_scn, params_dt) {
                      rate1 <- get_param_fun(params_dt, "rate1")
                      rate2 <- get_param_fun(params_dt, "rate2")
                      rate3 <- get_param_fun(params_dt, "rate3")
                      rate4 <- get_param_fun(params_dt, "rate4")
                      tbrk1 <- get_param_fun(params_dt, "tbrk1")
                      tbrk2 <- get_param_fun(params_dt, "tbrk2")
                      tbrk3 <- get_param_fun(params_dt, "tbrk3")
                      tbrk4 <- get_param_fun(params_dt, "tbrk4")
                      rate_nat_per_art69_11         <- get_param_fun(params_dt, "rate_nat_per_art69_11")
                      fix_nat_per_art69_11          <- get_param_fun(params_dt, "fix_nat_per_art69_11")
                      
                      rate_indiv_art15a             <- get_param_fun(params_dt, "rate_indiv_art15a")
                      rate_legal_art15b             <- get_param_fun(params_dt, "rate_legal_art15b")
                      rate_farm_art15c              <- get_param_fun(params_dt, "rate_farm_art15c")
                      
                      fixed_inc_tax                 <- get_param_fun(params_dt, "fixed_inc_tax")
                      
                      rate_indiv_art69              <- get_param_fun(params_dt, "rate_indiv_art69")
                      rate_indiv_art71_90_par31     <- get_param_fun(params_dt, "rate_indiv_art71_90_par31")
                      rate_int_art89                <- get_param_fun(params_dt, "rate_int_art89")
                      rate_adv_art90_par2           <- get_param_fun(params_dt, "rate_adv_art90_par2")
                      rate_indiv_art90_1par3        <- get_param_fun(params_dt, "rate_indiv_art90_1par3")
                      rate_roy_art90_1par31         <- get_param_fun(params_dt, "rate_roy_art90_1par31")
                      rate_don_art90_1par31         <- get_param_fun(params_dt, "rate_don_art90_1par31")
                      rate_div_art90_1par31         <- get_param_fun(params_dt, "rate_div_art90_1par31")
                      rate_win_art90_1par33         <- get_param_fun(params_dt, "rate_win_art90_1par33")
                      rate_nat_art90_1par35         <- get_param_fun(params_dt, "rate_nat_art90_1par35")
                      rate_comm_art90_1par36        <- get_param_fun(params_dt, "rate_comm_art90_1par36")
                      rate_exmp                     <- get_param_fun(params_dt, "rate_exmp")
                      
                      contr_soc_mand                <- get_param_fun(params_dt, "contr_soc_mand")
                      prem_health_mand              <- get_param_fun(params_dt, "prem_health_mand")
                      ins_prem_art36_par6           <- get_param_fun(params_dt, "ins_prem_art36_par6")
                      ded_cash_m_art69_11           <- get_param_fun(params_dt, "ded_cash_m_art69_11")
                      
                      rate_don_lim_art36            <- get_param_fun(params_dt, "rate_don_lim_art36")
                      rate_undoc_exp_art24          <- get_param_fun(params_dt, "rate_undoc_exp_art24")
                      
                      per_ex_art33_par1             <- get_param_fun(params_dt, "per_ex_art33_par1")
                      per_ex_inc_art33_par2         <- get_param_fun(params_dt, "per_ex_inc_art33_par2")
                      ex_spouse_art34_par2          <- get_param_fun(params_dt, "ex_spouse_art34_par2")
                      ex_dep_art35_par1             <- get_param_fun(params_dt, "ex_dep_art35_par1")
                      ex_dep_dis_art35_par2         <- get_param_fun(params_dt, "ex_dep_dis_art35_par2")
                      
                      private_edu_exempt            <- get_param_fun(params_dt, "private_edu_exempt")
                      payroll_growth_allow          <- get_param_fun(params_dt, "payroll_growth_allow")
                      pension_fund_incentive        <- get_param_fun(params_dt, "pension_fund_incentive")
                      fez_admin_exempt              <- get_param_fun(params_dt, "fez_admin_exempt")
                      cadastral_exempt              <- get_param_fun(params_dt, "cadastral_exempt")
                      
                      fez_export_50pct              <- get_param_fun(params_dt, "fez_export_50pct")
                      fez_5y_holiday_invest         <- get_param_fun(params_dt, "fez_5y_holiday_invest")
                      fez_domestic_75pct            <- get_param_fun(params_dt, "fez_domestic_75pct")
                      fez_5y_holiday_export         <- get_param_fun(params_dt, "fez_5y_holiday_export")
                      
                      toggle_progression_all         <- get_param_fun(params_dt, "toggle_progression_all")
                      toggle_progression_wages         <- get_param_fun(params_dt, "toggle_progression_wages")
  
# I. ESTIMATION TAX LIABILITY FOR INCOME FROM LABOR --------------
   # 8.Form IALS21  -----------------------------------------------------------------------

  
                      ## ------------------------------------------------------------------------------
                      ## 2  IALS-21 – non-wage items with optional progression surcharge
                      ##      •  toggle_progression_all == 0  →  flat only  (progressive part = 0)
                      ##      •  toggle_progression_all == 1  →  add progressive tax on inv_base_prog
                      ## ------------------------------------------------------------------------------
                      
                      ## ---------------------------------------------------------------------------
                      ## 2  IALS-21 – choose flat vs. progressive
                      ## ---------------------------------------------------------------------------
                      dt_scn[tax_regime == "ials21",
                             c("pit_ials21_prog",
                               # "pit_ials21_sal",
                               "pit_ials21_fol","pit_ials21_pls_exmp","pit_ials21_pl","pit_ials21_roy",
                               "pit_ials21_donpf","pit_ials21_don_p","pit_ials21_rcsa",
                               "pit_ials21_dobba","pit_ials21_dob","pit_ials21_vms",
                               "pit_ials21_div",
                               # "pit_ials21_pls",
                               "pit_ials21_don","pit_ials21_liv","pit_ials21_nor","pit_ials21_csm",
                               "pit_ials21_agrac","pit_ials21_ser"
                             ) := {
                               
                               ## ---------- 1. flat-withholding amounts --------------------------------
                               fol_wh_calc   <- ials21_sumven_cur_FOL_WH   * rate_indiv_art90_1par3
                               pls_exmp_calc <- ials21_sumven_cur_PLS_WH   * rate_exmp
                               pl_wh_calc    <- ials21_sumven_cur_PL_WH    * rate_adv_art90_par2
                               roy_wh_calc   <- ials21_sumven_cur_ROY_WH   * rate_roy_art90_1par31
                               donpf_wh_calc <- ials21_sumven_cur_DONPF_WH * rate_don_art90_1par31
                               don_p_wh_calc <- ials21_sumven_cur_DON_P_WH * rate_don_art90_1par31
                               rcsa_wh_calc  <- ials21_sumven_cur_RCSA_WH  * rate_exmp
                               dobba_wh_calc <- ials21_sumven_cur_DOBBA_WH * rate_int_art89
                               dob_wh_calc   <- ials21_sumven_cur_DOB_WH   * rate_int_art89
                               vms_wh_calc   <- ials21_sumven_cur_VMS_WH   * rate_int_art89
                               div_wh_calc   <- ials21_sumimp_cur_DIVA_WH  * rate_div_art90_1par31
                               don_wh_calc   <- ials21_sumven_cur_DON_WH   * rate_exmp
                               liv_wh_calc   <- ials21_sumven_cur_LIV_WH   * rate_nat_art90_1par35
                               nor_wh_calc   <- ials21_sumven_cur_NOR_WH   * rate_win_art90_1par33
                               csm_wh_calc   <- ials21_sumven_cur_CSM_WH   * rate_comm_art90_1par36
                               agrac_wh_calc <- ials21_sumven_cur_AGRAC_WH * rate_indiv_art69
                               ser_wh_calc   <- ials21_sumven_cur_SER_WH   * rate_indiv_art90_1par3
                               
                               ## ---------- 2. progressive surcharge (on investment base) -------------
                               taxable2 <- pmax(inv_base_prog, 0)
                               
                               bw1 <- pmax(tbrk2 - tbrk1, 0)
                               bw2 <- pmax(tbrk3 - tbrk2, 0)
                               
                               prog_calc_second <- rate_indiv_art15a * pmin(taxable2, tbrk1) +
                                 rate2                         * pmin(bw1, pmax(0, taxable2 - tbrk1)) +
                                 rate3                         * pmin(bw2, pmax(0, taxable2 - tbrk2)) +
                                 rate4                         * pmax(0, taxable2 - tbrk3)
                               
                               ## ---------- 3. pick one regime ----------------------------------------
                               if (toggle_progression_all == 0) {
                                 prog_calc <- 0                 # keep flat withholding
                               } else {
                                 ## zero-out every flat figure
                                 fol_wh_calc   <- 0;  pls_exmp_calc <- 0;  pl_wh_calc    <- 0
                                 roy_wh_calc   <- 0;  donpf_wh_calc <- 0;  don_p_wh_calc <- 0
                                 rcsa_wh_calc  <- 0;  dobba_wh_calc <- 0;  dob_wh_calc   <- 0
                                 vms_wh_calc   <- 0;  div_wh_calc   <- 0;  don_wh_calc   <- 0
                                 liv_wh_calc   <- 0;  nor_wh_calc   <- 0;  csm_wh_calc   <- 0
                                 agrac_wh_calc <- 0;  ser_wh_calc   <- 0
                                 prog_calc     <- prog_calc_second  # apply progressive
                               }
                               
                               ## ---------- 4. return in exact LHS order ------------------------------
                               list(
                                 prog_calc,
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
                                 ser_wh_calc
                               )
                             }]
                      
                      ## -----------------------------------------------------------------------------
                      ## Re-compute total PIT (now mutually exclusive, no double-counting)
                      ## -----------------------------------------------------------------------------
                      dt_scn[, pitax := rowSums(.SD, na.rm = TRUE),
                             .SDcols = c(
                               "pit_ials21_prog",
                               "pit_ials21_fol","pit_ials21_pls_exmp","pit_ials21_pl","pit_ials21_roy",
                               "pit_ials21_donpf","pit_ials21_don_p","pit_ials21_rcsa",
                               "pit_ials21_dobba","pit_ials21_dob","pit_ials21_vms",
                               "pit_ials21_div",
                               "pit_ials21_don","pit_ials21_liv","pit_ials21_nor","pit_ials21_csm",
                               "pit_ials21_agrac","pit_ials21_ser"
                             )]
                      
            
}    
# 2. Helper to Retrieve Growth Factors for Each Variable -------------------------------
                vars_to_grow <- c(
                  # "ai_17_r1c2", "ai_17_r2c2", "ai_17_r3c2", "ai_17_r4c2", "ai_17_r5c2", "ai_17_r6c2", "ai_17_r7c2", "ai_17_r8c2", "ai_17_r9c2", "ai_17_Sumadecontrol",
                  # "ven12_r010", "ven12_r0101", "ven12_r0102", "ven12_r020", "ven12_r030", "ven12_r040", "ven12_r050", "ven12_r060", "ven12_r070", "ven12_r0701",
                  # "ven12_r080", "ven12_r0901", "ven12_r0902", "ven12_r090", "ven12_r100", "ven12_r110", "ven12_r120", "ven12_r130", "ven12_r140", "ven12_r150",
                  # "ven12_totald3c6", "ven12_totald3c7", "ven12_totald3c9", "ven12_totald3c10", "ven12_totald3c11", "ven12_totald4c3", "ven12_sumac", "ven12_row_idt",
                  # "ven12_sumavensc", "ven12_sumafacil", 
                  # "ven12_procfac_6d", 
                  # "daj17_r010", "daj17_r020", 
                  # "daj17_r030", 
                  # "daj17_r050", "daj17_r060", "daj17_c6",
                  # "daj17_c7", "daj17_c9", "daj17_c10", "daj17_c11", "daj17_r090", "daj17_r130", "daj17_r140", "daj17_control", "dass19_r010", "dass19_r0101",
                  # "dass19_r01011", "dass19_r01012", "dass19_r0102", "dass19_r020", "dass19_r030", "dass19_r040", "dass19_r050", "dass19_r060", "dass19_r070",
                  # "dass19_r080", "dass19_r090", "dass19_r100", "dass19_r110", "dass19_r130", "dass19_r140", "dass19_r150", "dass19_r160", "dass19_r170", "dass19_r180",
                  # "dass19_d2c6", "dass19_d2c7", "dass19_d2c8", "dass19_d2c9", "dass19_d2c10", "dass19_d2c11", "dass19_d2c12", 
                  "ials21_sumven_cur_SAL", "ials21_sumsc_p_cur_SAL",
                  "ials21_sumsc_m_cur_SAL", "ials21_sumsc_sm_cur_SAL", "ials21_sumsc_n_cur_SAL", "ials21_sumsc_h_cur_SAL", "ials21_sumsc_tot_cur_SAL", "ials21_sumded1_cur_SAL",
                  "ials21_sumimp_cur_SAL", "ials21_sumded2_cur_SAL", "ials21_sumven_cur_FOL_WH", "ials21_sumven_cur_DIVA_WH", "ials21_sumven_cur_PL_WH", "ials21_sumven_cur_ROY_WH",
                  "ials21_sumven_cur_DONPF_WH", "ials21_sumven_cur_RCSA_WH", "ials21_sumven_cur_DOBBA_WH", "ials21_sumven_cur_VMS_WH", "ials21_sumven_cur_PLS_WH", "ials21_sumven_cur_DON_WH",
                  "ials21_sumven_cur_LIV_WH", "ials21_sumven_cur_NOR_WH", "ials21_sumven_cur_DOB_WH", "ials21_sumven_cur_CSM_WH", "ials21_sumven_cur_DON_P_WH", "ials21_sumven_cur_AGRAC_WH",
                  "ials21_sumven_cur_DOB_B_WH", "ials21_sumven_cur_SER_WH", "ials21_sumven_cur_PLT_WH", "ials21_sumimp_cur_FOL_WH", "ials21_sumimp_cur_DIVA_WH", "ials21_sumimp_cur_PL_WH",
                  "ials21_sumimp_cur_ROY_WH", "ials21_sumimp_cur_DONPF_WH", "ials21_sumimp_cur_RCSA_WH", "ials21_sumimp_cur_DOBBA_WH", "ials21_sumimp_cur_VMS_WH", "ials21_sumimp_cur_PLS_WH",
                  "ials21_sumimp_cur_DON_WH", "ials21_sumimp_cur_LIV_WH", "ials21_sumimp_cur_NOR_WH", "ials21_sumimp_cur_DOB_WH", "ials21_sumimp_cur_CSM_WH", "ials21_sumimp_cur_DON_P_WH",
                  "ials21_sumimp_cur_AGRAC_WH", "ials21_sumimp_cur_DOB_B_WH", "ials21_sumimp_cur_SER_WH", "ials21_sumimp_cur_PLT_WH","total_income",
                  "inv_base_prog","bus_base_prog"
                  # #"taxi18_tin_cds", 
                  # "taxi18_sumac_cur",
                  # "taxi18_tot_col9", 
                  # "taxi18_t1c7tot_cur", "taxi18_t1c8tot_cur", "taxi18_t1c9tot_cur", "taxi18_t1c7_cur", "taxi18_t1c8_cur", 
                  # "taxi18_t1c9_cur",
                  # "unif21_t1r010", "unif21_t1r0101", "unif21_t1r0102", "unif21_t1r020", "unif21_t1r030", "unif21_t1r040", "unif21_t1r050", "unif21_t1r060",
                  # "unif21_t1r070", "unif21_t1r0701", "unif21_t1r080", "unif21_t1r090", "unif21_t1r0901", "unif21_t1r0902", "unif21_t1r100", "unif21_t1r120",
                  # "unif21_t1r130","unif21_a1t1r120c4", "unif21_a1t1totc4", "unif21_a2t1totc6", "unif21_a2t1totc7", "unif21_a2t1totc8",
                  # "unif21_a2t1totc9", "unif21_a2t1totc10", "cet18_c1c3", "cet18_c31c3", "cet18_c5c3", "cet18_d1", "cet18_d2", "cet18_d3", "cet18_d4",
                  # "cet18_d5", "cet18_d6", "cet18_d7", "cet18_de", "cet18_e1", "cet18_e2", "cet18_e3", "cet18_e4", "cet18_f1", "cet18_f2", "cet18_f3", "cet18_f4", "cet18_f5"
                  
                              )
                
                get_growth_factor_row <- function(scenario) {
                  gf_row <- growth_factors[scenarios == scenario]
                  out <- numeric(length(vars_to_grow))
                  names(out) <- vars_to_grow
                  
                  for (v in vars_to_grow) {
                    gf_col <- sub("_adjusted", "", v)  
                    out[v] <- gf_row[[gf_col]]
                  }
                  return(out)
                }

# 3. Business as usual  ------------------------------------------------------
          
          PIT_BU_list <- list()
          
          # Start from baseline
          dt_scn_BU <- copy(dt)
          
          for (s in scenarios) {
            
            # 1) Retrieve scenario growth factors
            gf_values <- get_growth_factor_row(s)
            
            # 2) Multiply each variable by gf_values[v] * weights[[s]]
            for (v in vars_to_grow) {
              dt_scn_BU[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
            }
            
            # 3) Row-wise tax logic
            tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
            
            # 4) ADD a 'weight' column that references weights[[s]]
            dt_scn_BU[, weight := weights_pit[[s]]]
            
            # 5) Store in PIT_BU_list
            PIT_BU_list[[s]] <- copy(dt_scn_BU)
          }

# 4. Simulation --------------------------------------------------------------
          start_index <- match(SimulationYear, scenario_years) 
          
          PIT_SIM_list <- list()

          if (start_index > 1) {
            for (i in seq_len(start_index - 1)) {
              s_early <- scenarios[i]
              PIT_SIM_list[[s_early]] <- copy(PIT_BU_list[[s_early]])
            }
          }
          
          # 2) Determine the starting data for re-simulation
          if (start_index == 1) {
            # SimulationYear=2021 => start from original dt
            dt_scn_SIM <- copy(dt)
          } else {
            # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
            prev_scenario <- scenarios[start_index - 1]
            dt_scn_SIM <- copy(PIT_BU_list[[prev_scenario]])
          }
          
          # 3) Chain from scenario index = start_index .. 5
          for (i in seq(from = start_index, to = length(scenarios))) {
            s <- scenarios[i]
            
            gf_values <- get_growth_factor_row(s)
            
            # Multiply each variable by growth factor * row-weight for scenario s
            for (v in vars_to_grow) {
              dt_scn_SIM[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
            }
            
            # Run row-wise calculations with updated parameters
            tax_calc_fun(dt_scn_SIM, pit_simulation_parameters_updated)
            
            # **Add a 'weight' column** with the row-specific weights_pit for scenario s
            dt_scn_SIM[, weight := weights_pit[[s]]]
            
            # Store final data in PIT_SIM_list
            PIT_SIM_list[[s]] <- copy(dt_scn_SIM)
          }
          
          message("Block 2 (PIT_SIM_list) done, including early years from PIT_BU_list, plus 'weight' column.\n")
          message("All done!\n")
          
         # rm(dt_scn_BU, dt_scn_SIM)

      # 5. Aggregation of simulated data -----------------------------------------------------
          
          summarize_PIT_fun_dt <- function(PIT_list, suffix) {
            # 1) Loop (via lapply) over each named data.table in the list
            # 2) Sum columns matching regex ^(calc|pit)
            # 3) Collect results into one data.table
            summary_list <- lapply(names(PIT_list), function(scenario_name) {
              dt <- PIT_list[[scenario_name]]
              
              # Select columns starting with "calc" or "pit", and sum them
             
             # sums_dt <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = patterns("^(calc|pit)")]
              sums_dt <- dt[, lapply(.SD, sum, na.rm = TRUE),
                            #.SDcols = patterns("_calc$|pit")]   # <-- changed line
                            .SDcols = patterns("pit")]   # <-- changed line
              
              # Add scenario name as a column
              sums_dt[, scenarios := scenario_name]
              
              # Make 'scenarios' the first column
              setcolorder(sums_dt, c("scenarios", setdiff(names(sums_dt), "scenarios")))
              sums_dt
            })
            
            # Combine all scenario summaries into one data.table
            result_dt <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
            
            # Append 'suffix' to every column except 'scenarios'
            old_names <- setdiff(names(result_dt), "scenarios")
            new_names <- paste0(old_names, suffix)
            setnames(result_dt, old_names, new_names)
            
            # Convert to data.frame if you want the same final type as your original code
            result_df <- as.data.frame(result_dt)
            
            return(result_df)
          }
          
          
      
      # Function to sum the specified columns in the list and store the results in a data frame

          summary_SIM <- summarize_PIT_fun_dt(PIT_SIM_list, "_sim")
          summary_BU  <- summarize_PIT_fun_dt(PIT_BU_list, "_bu")
         
      
      
          merged_PIT_BU_SIM2 <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
      merged_PIT_BU_SIM2$year <- as.character(forecast_horizon)
      merged_PIT_BU_SIM2 <- merged_PIT_BU_SIM2[, c("year", names(merged_PIT_BU_SIM2)[-length(merged_PIT_BU_SIM2)])]
      
      numeric_columns <- sapply(merged_PIT_BU_SIM2, is.numeric)
      merged_PIT_BU_SIM2[, numeric_columns] <- merged_PIT_BU_SIM2[, numeric_columns] / 1e06




# Aggregated tax liability ------------------------------------------------
                      # Convert data for presentation in GUI
                      pit_summary_df <- merged_PIT_BU_SIM2 %>%
                        pivot_longer(cols = -year, 
                                     names_to = c("variable", ".value"), 
                                     names_pattern = "(.*)_(bu|sim)")
                      
                      # Calculate the difference between _sim and _bu columns
                      pit_summary_df <- pit_summary_df %>%
                        mutate(difference = sim - bu)
                      
                      
                      pit_summary_df <- pit_summary_df %>%
                        mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
                        filter(variable=='pitax')
                      
                      # Arrange the columns
                      pit_summary_df <- pit_summary_df %>%
                                  select(year, bu, sim, difference)%>%
                                  dplyr::rename(
                                    "Current law (LCU Mil)"="bu",
                                    "Simulation (LCU Mil)"="sim",
                                    "Fiscal impact (LCU Mil)"="difference",
                                  )
                      
                      
                      MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)
                      
                      pit_summary_df<-left_join(pit_summary_df,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
                        select(year,"Current law (LCU Mil)","Simulation (LCU Mil)","Fiscal impact (LCU Mil)",Nominal_GDP)%>%
                        dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU Mil)`/Nominal_GDP*100,2),
                                       `Simulation (Pct of GDP)`=round(`Simulation (LCU Mil)`/ Nominal_GDP*100,2),
                                       `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU Mil)`/ Nominal_GDP*100,2))%>%
                        dplyr::select(-c(Nominal_GDP))
                      
                      
                      pit_summary_df2 <- as.data.table(pit_summary_df)
                      
                      
                      
                      print(merged_PIT_BU_SIM2)
                      
                      
                      # Caling functions for subseting of data
                      
                      PIT_BU_list2  <- lapply(PIT_BU_list,  process_table)
                      PIT_SIM_list2 <- lapply(PIT_SIM_list, process_table)                      
                      
                     
                      
                      # end.time <- proc.time()
                      # save.time <- end.time - start.time
                      # cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")
                      PIT_BU_list2 <- get_scenario_df(
                        year       = SimulationYear,      # 2025
                        horizons   = forecast_horizon,    # 2023-2027
                        scenarios  = scenarios,           # "t0" . "t4"
                        table_list = PIT_BU_list2         # your pre-built list
                      )%>%data.table()
                      
                      PIT_SIM_list2 <- get_scenario_df(
                        year       = SimulationYear,      # 2025
                        horizons   = forecast_horizon,    # 2023-2027
                        scenarios  = scenarios,           # "t0" . "t4"
                        table_list = PIT_SIM_list2         # your pre-built list
                      )%>%data.table()
                    
                      
                      
                      rm(PIT_BU_list,PIT_SIM_list)
                      