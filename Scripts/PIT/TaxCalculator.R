
# pit_simulation_parameters_raw<- read_excel("PIT-Parameters.xlsx")
# pit_simulation_parameters_updated<-pit_simulation_parameters_raw
# SimulationYear<-2023
#dt$Year<-2023

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
start.time <- proc.time()
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
                      
  
# I. ESTIMATION TAX LIABILITY FOR INCOME FROM LABOR --------------
  # 1.Form AI17 ----------------------------------------------------
       # Description: The natural person who carries out independent activity  (retail trade (with the exception of goods subject to excise duties))
         
                      dt_scn[ tax_regime == "ai_17",
                            c("r3c2_calc", "r4c2_calc", "r5c2_calc",
                              "r6c2_calc", "r7c2_calc", "pit_ai_17", "r9c2_calc") :=
                              {
                                ## Row 3 tax due at the reduced rate
                                r3 <- ai_17_r1c2 * rate_nat_per_art69_11
                                
                                ## Row 4 amount rounded to a multiple of the fixed unit
                                r4 <- (ai_17_r4c2 / fix_nat_per_art69_11) * fix_nat_per_art69_11
                                
                                ## Row 5 ai17larger of Row 3 and Row 4
                                r5 <- pmax(r4, r3)
                                
                                ## Row 6 ai17deductible cash payments
                                r6 <- ai_17_r6c2 * ded_cash_m_art69_11
                                
                                ## Row 7 ai17other deductible amounts
                                r7 <- ai_17_r7c2
                                
                                ## Difference used for Rows 8 & 9
                                diff <- r5 - r6 - r7
                                
                                ## Row 8 ai17income-tax still to be paid (positive part) _ pit_ai_17
                                r8 <- pmax(diff, 0)
                                
                                ## Row 9 ai17income-tax paid in excess (absolute negative part)
                                r9 <- pmax(-diff, 0)
                                
                                
                                list(r3, r4, r5, r6, r7, r8, r9 )
                              } ]
                           
  # 2.Form DAJ17 ----------------------------------------------------------------------
              # Persons who carry out professional activity in the justice sector
                 
                      dt_scn[tax_regime == "daj17",
                             c("r040_calc", "r050_calc", "r060_calc", "tot_ded",
                               "r070_calc", "r080_calc", "r100_calc", "r110_calc",
                               "r130_calc", "coeff_trans", "r140_calc", "pit_daj17") := {
                                 
                                 ## ---- 1. main subtotal -
                                 r040 <- daj17_r010 + daj17_r020 - daj17_r030            # row 040
                                 
                                 ## ---- 2. donation / undocumented-expense limits 
                                 r050 <- fifelse(r040 <= 0 | is.na(daj17_r050) | daj17_r050 == 0,
                                                 0,
                                                 r040 * rate_don_lim_art36)              # 5 % cap
                                 
                                 r060 <- fifelse(r040 <= 0 | is.na(daj17_r060) | daj17_r060 == 0,
                                                 0,
                                                 r040 * rate_undoc_exp_art24)            # 0.2 % cap
                                 
                                 ## ---- 3. personal deductions (Art. 33-35) 
                                 tot  <- (daj17_c6  / 27000) * per_ex_art33_par1 +
                                   (daj17_c7  / 31500) * per_ex_inc_art33_par2 +
                                   (daj17_c9  / 19800) * ex_spouse_art34_par2 +
                                   (daj17_c10 /  9000) * ex_dep_art35_par1 +
                                   (daj17_c11 / 19800) * ex_dep_dis_art35_par2
                                 
                                 r070 <- fifelse(r040 - r050 - r060 > 0,
                                                 pmin(tot, r040 - r050 - r060),
                                                 0)
                                 
                                 ## ---- 4. taxable income & losses 
                                 r080 <- pmax(r040 - r050 - r060 - r070, 0)
                                 
                                 r100 <- r080 - daj17_r090                                 # row 100
                                 
                                 r110 <- fifelse(r080 < 0, abs(r080), 0)                   # losses
                                 
                                 ## ---- 5. income tax & credit 
                                 r130 <- r100 * rate_indiv_art15a                          # row 130
                                 
                                 coeff <- fifelse(is.na(daj17_r140) | r130 == 0,
                                                  0,
                                                  daj17_r140 / r130)
                                 
                                 r140 <- r130 * coeff                                     # row 140
                                 
                                 r150 <- r130 - r140                                      # row 150
                                 
                                 ## ---- 6. hand back values in the same order 
                                 list(r040, r050, r060, tot, r070, r080, r100, r110,
                                      r130, coeff, r140, r150)
                               }]

  # 3.Form DASS19 ----------------------------------------------------------------------
              # Persons carrying out professional activity in the field of health (individual medical offices)
                 
                      dt_scn[tax_regime == "dass19",
                             c("r0101_calc", "r010_calc", "r040_calc",
                               "r050_calc", "r060_calc", "tot_ded",
                               "r070_calc", "r080_calc", "r100_calc", "pit_dass19") := {
                                 
                                 ## ---------- Section A: subtotals
                                 r0101 <- dass19_r01011 + dass19_r01012          # row 0101
                                 r010  <- r0101 - dass19_r0102                   # row 010
                                 r040  <- r010 + dass19_r020 - dass19_r030       # row 040
                                 
                                 ## ---------- Section B: limits & deductions 
                                 r050 <- fifelse(!is.na(dass19_r050) & dass19_r050 > 0,
                                                 r040 * rate_don_lim_art36,      # 5 % limit
                                                 0)
                                 
                                 r060 <- fifelse(!is.na(dass19_r060) & dass19_r060 > 0,
                                                 r040 * rate_undoc_exp_art24,    # 0.2 % limit
                                                 0)
                                 
                                 tot  <- (dass19_d2c6  / 27000) * per_ex_art33_par1 +
                                   (dass19_d2c7  / 31500) * per_ex_inc_art33_par2 +
                                   (dass19_d2c8  / 19800) * ex_spouse_art34_par2 +
                                   (dass19_d2c10 /  9000) * ex_dep_art35_par1 +
                                   (dass19_d2c11 / 19800) * ex_dep_dis_art35_par2
                                 
                                 r070 <- fifelse(r040 - r050 - r060 > 0,
                                                 pmin(tot, r040 - r050 - r060),
                                                 0)
                                 
                                 ## ---------- Section C: taxable base & tax 
                                 r080 <- pmax(r040 - r050 - r060 - r070, 0)
                                 
                                 r100 <- r080 - dass19_r090
                                 
                                 r130 <- r100 * rate_indiv_art15a               # income tax amount
                                 
                                 ## ---------- return results 
                                 list(r0101, r010, r040, r050, r060, tot, r070, r080, r100, r130)
                               }]
                      
                      
                      
  # 4.Form VEN12 -------------------------------------------------------------------------
                     
                      dt_scn[tax_regime == "ven12",
                             c("r010_calc", "r020_calc", "r030_calc", "r040_calc",
                               "r050_calc", "r060_calc", "tot_ded", "r0701_calc",
                               "r070_calc", "r080_calc", "r0901_calc", "r0902_calc", "r090_calc",
                               "r100_calc", "r120_calc",
                               "te_calc_6d", "r130_calc",
                               "te_calc_4d", "r140_calc",
                               "pit_ven12") := {
                                 
                                 ## ---------- Main subtotals 
                                 r010 <- ven12_r0101 - ven12_r0102
                                 r020 <- ven12_r020
                                 r030 <- ven12_r030
                                 r040 <- r010 + r020 - r030
                                 
                                 ## ---------- Donation & undocumented-expense limits 
                                 r050 <- fifelse(r040 <= 0 | is.na(ven12_r050) | ven12_r050 == 0,
                                                 0,
                                                 r040 * rate_don_lim_art36)            # 5 % cap
                                 
                                 r060 <- fifelse(is.na(ven12_r060) | ven12_r060 <= 0,
                                                 0,
                                                 ven12_r060 * rate_undoc_exp_art24)    # 0.2 % cap
                                 
                                 ## ---------- Personal deductions (Art. 33-35) 
                                 tot  <- (ven12_totald3c6  / 27000) * per_ex_art33_par1  +
                                   (ven12_totald3c7  / 31500) * per_ex_inc_art33_par2 +
                                   (ven12_totald3c9  / 19800) * ex_spouse_art34_par2  +
                                   (ven12_totald3c10 /  9000) * ex_dep_art35_par1     +
                                   (ven12_totald3c11 / 19800) * ex_dep_dis_art35_par2
                                 
                                 r0701 <- fifelse(r040 - r050 - r060 > 0,
                                                  pmin(tot, r040 - r050 - r060),
                                                  0)
                                 
                                 ## ---------- Taxable income & carried-forward losses 
                                 r070 <- fifelse(r040 - r050 - r060 < 0,
                                                 0,
                                                 r040 - r050 - r060)
                                 
                                 r080 <- fifelse(r070 < ven12_r080, r070, ven12_r080)   # losses used
                                 
                                 r0901 <- r070 - r080
                                 r0902 <- ven12_totald4c3
                                 r090  <- r0901 - r0902
                                 
                                 ## ---------- Negative income (row 100) 
                                 r100 <- fifelse(r040 < 0, abs(r040), 0)
                                 
                                 ## ---------- Income-tax amount (row 120) --------------------------
                                 r120 <- fcase(
                                   ven12_tp_category == 0, r090 * rate_indiv_art15a,
                                   ven12_tp_category == 1, r090 * rate_indiv_art15a,
                                   ven12_tp_category == 2, r090 * rate_farm_art15c,
                                   default               =  r090 * rate_legal_art15b
                                 )
                                 
                                 ## ---------- ANNEX 6D exemptions 
                                 te6d <- fcase(
                                   ven12_exemption_idt == "6a", ven12_sumafacil * fez_export_50pct,
                                   ven12_exemption_idt == "6r", ven12_sumafacil * fez_5y_holiday_invest,
                                   ven12_exemption_idt == "6b", ven12_sumafacil * fez_domestic_75pct,
                                   ven12_exemption_idt == "6d", ven12_sumafacil * fez_domestic_75pct,
                                   default                     = ven12_sumafacil
                                 )
                                 r130 <- te6d
                                 
                                 ## ---------- ANNEX 4D exemptions 
                                 te4d <- fcase(
                                   ven12_exemption_idt == "4c", ven12_sumafacil * private_edu_exempt,
                                   ven12_exemption_idt == "4i", ven12_sumafacil * payroll_growth_allow,
                                   ven12_exemption_idt == "4d", ven12_sumafacil * pension_fund_incentive,
                                   ven12_exemption_idt == "4b", ven12_sumafacil * fez_admin_exempt,
                                   ven12_exemption_idt == "4g", ven12_sumafacil * cadastral_exempt,
                                   default                     = ven12_sumafacil
                                 )
                                 r140 <- te4d
                                 
                                 ## ---------- Net tax payable (row 150) 
                                 r150 <- r120 - r130
                                 
                                 ## ---------- return results in the  order 
                                 list(r010, r020, r030, r040,
                                      r050, r060, tot, r0701,
                                      r070, r080, r0901, r0902, r090,
                                      r100, r120,
                                      te6d, r130,
                                      te4d, r140,
                                      r150)
                               }]
                      

  # 5.Form UNIF21-----------------------------------------------------------------------
                  "Business income tax"
                      dt_scn[tax_regime == "unif21",
                             c("t1r010_calc", "t1r040_calc",
                               "t1r050_calc", "t1r060_calc", "tot_ded",
                               "t1r0701_calc", "t1r070_calc",
                               "t1r0901_calc", "t1r090_calc",
                               "pit_unif21") := {
                                 
                                 ## ---------- Sub-totals before adjustments ------------------------
                                 r010  <- unif21_t1r0101 - unif21_t1r0102                 # row 010
                                 r040  <- unif21_t1r010 + unif21_t1r020 - unif21_t1r030   # row 040
                                 
                                 ## ---------- Donation & undocumented-expense limits ---------------
                                 r050 <- fifelse(r040 <= 0 | is.na(unif21_t1r050) | unif21_t1r050 == 0,
                                                 0,
                                                 r040 * rate_don_lim_art36)               # 5 % cap
                                 
                                 r060 <- fifelse(r040 <= 0,
                                                 0,
                                                 r040 * rate_undoc_exp_art24)             # 0.2 % cap
                                 
                                 ## ---------- Founder exemptions (Art. 33-35) ----------------------
                                 tot  <- (unif21_a2t1totc6  / 27000) * per_ex_art33_par1  +
                                   (unif21_a2t1totc7  / 31500) * per_ex_inc_art33_par2 +
                                   (unif21_a2t1totc8  / 19800) * ex_spouse_art34_par2 +
                                   (unif21_a2t1totc9  /  9000) * ex_dep_art35_par1   +
                                   (unif21_a2t1totc10 / 19800) * ex_dep_dis_art35_par2
                                 
                                 r0701 <- fifelse(r040 - r050 - r060 > 0,
                                                  pmin(tot, r040 - r050 - r060),
                                                  0)
                                 
                                 ## ---------- Taxable income & carried-forward losses --------------
                                 r070 <- fifelse(r040 - r050 - r060 - r0701 < 0,
                                                 0,
                                                 r040 - r050 - r060 - r0701)
                                 
                                 r0901 <- r070 - unif21_t1r080            # row 0901
                                 r090  <- r0901 - unif21_t1r0902          # row 090
                                 
                                 ## ---------- Income-tax amount (row 120) --------------------------
                                 'Original ETR is much higher in ther original than statutory !!!!!'
                                 
                                 r120 <- fcase(
                                   r090 < 0,                                      0,                # force 0 if negative
                                   unif21_a1t1r120c3 %in% c(0, 12, 0.12),         r090 * rate_indiv_art15a,  # Art 15 a
                                   unif21_a1t1r120c3 == 7,                        r090 * rate_farm_art15c,   # Art 15 c
                                   default = NA_real_
                                 )
                                 
                                 ## ---------- return results in LHS order --------------------------
                                 list(r010, r040,
                                      r050, r060, tot,
                                      r0701, r070,
                                      r0901, r090,
                                      r120)
                               }]
                      
                      
                      
                      
  # 6.Form TAXI18 -------------------------------------------------------------------------
                   
                      dt_scn[tax_regime == "taxi18",
                             c("t1c7_cur_calc", "pit_taxi18", "t1c9_cur_calc") := {
                               
                               ## ---------- Mandatory social contribution 
                               c7_calc <- fifelse(
                                 is.na(taxi18_t1c7_cur) | taxi18_t1c7_cur == 0,
                                 0,
                                 (taxi18_t1c7_cur / 14700) * contr_soc_mand
                               )
                               
                               ## ---------- Fixed-income tax
                               c8_calc <- fifelse(
                                 is.na(taxi18_t1c8_cur) | taxi18_t1c8_cur == 0,
                                 0,
                                 (taxi18_t1c8_cur /  6000) * fixed_inc_tax
                               )
                               
                               ## ---------- Mandatory health premium 
                               c9_calc <- fifelse(
                                 is.na(taxi18_t1c9_cur) | taxi18_t1c9_cur == 0,
                                 0,
                                 (taxi18_t1c9_cur / 12636) * prem_health_mand   #  corrected column name
                               )
                               
                               ## ---------- return results inorder
                               list(c7_calc, c8_calc, c9_calc)
                             }]

  # 7.Form CET18 ------------------------------------------------------------

                      dt_scn[tax_regime == "cet18",
                             c("d7_calc", "e1_calc", "e2_calc", "e3_calc", "e4_calc",
                               "de_calc", "f1_calc", "f2_calc", "f3_calc", "f4_calc", "pit_cet18") := {
                                 
                                 ## ---------- D. Exemptions 
                                 d7 <- (cet18_d1 / 27000) * per_ex_art33_par1  +
                                   (cet18_d2 / 31500) * per_ex_inc_art33_par2 +
                                   (cet18_d4 / 19800) * ex_spouse_art34_par2  +
                                   (cet18_d5 /  9000) * ex_dep_art35_par1     +
                                   (cet18_d6 / 19800) * ex_dep_dis_art35_par2
                                 
                                 ## ---------- E. Deductions 
                                 e1 <- fifelse(!is.na(cet18_e1) & cet18_e1 > 0,
                                               cet18_c1c3 * ins_prem_art36_par6,
                                               0)
                                 
                                 e2 <- fifelse(!is.na(cet18_e2) & cet18_e2 > 0,
                                               (cet18_e2 / 14700) * contr_soc_mand,
                                               0)
                                 
                                 e3 <- fifelse(is.na(cet18_e3), 0, cet18_e3)
                                 
                                 e4 <- e1 + e2 + e3
                                 
                                 de <- d7 + e4                                  # total deductions & exemptions
                                 
                                 ## ---------- F. Income-tax calculation 
                                 f1 <- pmax(cet18_c5c3 - de, 0)                 # taxable base
                                 
                                 f2 <- cet18_f2                                 # tax paid/withheld
                                 
                                 f3 <- pmax(f2 - f1, 0)                         # additional tax to pay
                                 
                                 f4 <- f1 + f3                                  # total tax liability
                                 
                                 f5 <- f4 * rate_indiv_art15a                   # income tax (CET-18 rate here)
                                 
                                 ## ---------- return results in  order 
                                 list(d7, e1, e2, e3, e4,
                                      de, f1, f2, f3, f4, f5)
                               }]
                      

  # 8.Form IALS21  -----------------------------------------------------------------------

                      dt_scn[tax_regime == "ials21",
                             c("pit_ials21_sal",               # pit_ials21_sal
                               "pit_ials21_fol",     # pit_ials21_fol
                               "pit_ials21_pls_exmp",    # pit_ials21_pls_exmp
                               "pit_ials21_pl",       #  pit_ials21_pl
                               "pit_ials21_roy",     # pit_ials21_roy
                               "pit_ials21_donpf",   #  pit_ials21_donpf
                               "pit_ials21_don_p",  # pit_ials21_don_p
                               "pit_ials21_rcsa",    #  pit_ials21_rcsa
                               "pit_ials21_dobba",   #  pit_ials21_dobba
                               "pit_ials21_dob",     #  pit_ials21_dob
                               "pit_ials21_vms",     # pit_ials21_vms
                               "pit_ials21_pls",     # pit_ials21_pls
                               "pit_ials21_don",     # pit_ials21_don
                               "pit_ials21_liv",     # pit_ials21_liv
                               "pit_ials21_nor",     # pit_ials21_nor
                               "pit_ials21_csm",     # pit_ials21_csm
                               "pit_ials21_agrac",   # pit_ials21_agrac
                               "pit_ials21_ser"     # pit_ials21_ser
                               # "pit_ials21_plt2",     # pit_ials21_plt2
                               # "pit_ials21_fol2",    # pit_ials21_fol2
                               # "pit_ials21_pl2",     # pit_ials21_pls2
                               # "pit_ials21_diva2",    # pit_ials21_diva2
                               # "pit_ials21_roy2"
                               ) := {   # pit_ials21_roy2
                                 
                                 ## ---------- 1. Wages ------------------------------------------------
                                 sal_calc <- (ials21_sumven_cur_SAL -
                                                ( (ials21_sumsc_p_cur_SAL  / 27000) * per_ex_art33_par1 +
                                                    (ials21_sumsc_m_cur_SAL  / 31500) * per_ex_inc_art33_par2 +
                                                    (ials21_sumsc_sm_cur_SAL / 19800) * ex_spouse_art34_par2 +
                                                    (ials21_sumsc_n_cur_SAL  /  9000) * ex_dep_art35_par1 +
                                                    (ials21_sumsc_h_cur_SAL  / 19800) * ex_dep_dis_art35_par2 +
                                                    (ials21_sumven_cur_SAL              * ins_prem_art36_par6) )
                                 ) * rate_indiv_art15a
                                 
                                 ## ---------- 2. Final-withholding income types -----------------------
                                 fol_wh_calc     <- ials21_sumven_cur_FOL_WH   * rate_indiv_art90_1par3
                                 pls_exmp_calc   <- ials21_sumven_cur_PLS_WH   * rate_exmp
                                 pl_wh_calc      <- ials21_sumven_cur_PL_WH    * rate_adv_art90_par2 # 10-taxpayers big difference !
                                 roy_wh_calc     <- ials21_sumven_cur_ROY_WH   * rate_roy_art90_1par31
                                 donpf_wh_calc   <- ials21_sumven_cur_DONPF_WH * rate_don_art90_1par31
                                 don_p_wh_calc   <- ials21_sumven_cur_DON_P_WH * rate_don_art90_1par31   # 1 taxpayer
                                 rcsa_wh_calc    <- ials21_sumven_cur_RCSA_WH  * rate_exmp              # explicit 0
                                 dobba_wh_calc   <- ials21_sumven_cur_DOBBA_WH * rate_int_art89   # 71 taxpayers difference !
                                 dob_wh_calc     <- ials21_sumven_cur_DOB_WH   * rate_int_art89
                                 vms_wh_calc     <- ials21_sumven_cur_VMS_WH   * rate_int_art89  # 10-taxpayers difference !
                                 pls_wh_calc     <- ials21_sumven_cur_PLS_WH   * rate_exmp
                                 don_wh_calc     <- ials21_sumven_cur_DON_WH   * rate_exmp
                                 liv_wh_calc     <- ials21_sumven_cur_LIV_WH   * rate_nat_art90_1par35
                                 nor_wh_calc     <- ials21_sumven_cur_NOR_WH   * rate_win_art90_1par33
                                 csm_wh_calc     <- ials21_sumven_cur_CSM_WH   * rate_comm_art90_1par36
                                 agrac_wh_calc   <- ials21_sumven_cur_AGRAC_WH * rate_indiv_art69
                                 ser_wh_calc     <- ials21_sumven_cur_SER_WH   * rate_indiv_art90_1par3   # 2 taxpayers
                    
                                 
                                 ## ---------- return results in LHS order ---------------------------
                                 list(sal_calc,
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
                                      pls_wh_calc,
                                      don_wh_calc,
                                      liv_wh_calc,
                                      nor_wh_calc,
                                      csm_wh_calc,
                                      agrac_wh_calc,
                                      ser_wh_calc

                                      )
                               }]
                      
                      
  
  

  
  # Total PIT ------------------------------------------------------
          
             # dt_scn[, pitax :=  pit_ai_17 + pit_unif21 + pit_daj17 + pit_dass19 + pit_ven12 +
             #                    pit_taxi18 + pit_cet18 + pit_ials21_sal + pit_ials21_fol +
             #                    pit_ials21_pls_exmp + pit_ials21_pl + pit_ials21_roy +
             #                    pit_ials21_donpf + pit_ials21_don_p + pit_ials21_rcsa +
             #                    pit_ials21_dobba + pit_ials21_dob + pit_ials21_vms +
             #                    pit_ials21_pls + pit_ials21_don + pit_ials21_liv +
             #                    pit_ials21_nor + pit_ials21_csm + pit_ials21_agrac +
             #                    pit_ials21_ser
             #        
             #           ]
                      dt_scn[, pitax := rowSums(.SD, na.rm = TRUE), 
                             .SDcols = c(
                               "pit_ai_17", "pit_unif21", "pit_daj17", "pit_dass19", "pit_ven12",
                               "pit_taxi18", "pit_cet18", "pit_ials21_sal", "pit_ials21_fol",
                               "pit_ials21_pls_exmp", "pit_ials21_pl", "pit_ials21_roy",
                               "pit_ials21_donpf", "pit_ials21_don_p", "pit_ials21_rcsa",
                               "pit_ials21_dobba", "pit_ials21_dob", "pit_ials21_vms",
                               "pit_ials21_pls", "pit_ials21_don", "pit_ials21_liv",
                               "pit_ials21_nor", "pit_ials21_csm", "pit_ials21_agrac",
                               "pit_ials21_ser"
                             )]
                      
             
       
            
            
}    
# 2. Helper to Retrieve Growth Factors for Each Variable -------------------------------
                vars_to_grow <- c(
                  "ai_17_r1c2", "ai_17_r2c2", "ai_17_r3c2", "ai_17_r4c2", "ai_17_r5c2", "ai_17_r6c2", "ai_17_r7c2", "ai_17_r8c2", "ai_17_r9c2", "ai_17_Sumadecontrol",
                  "ven12_r010", "ven12_r0101", "ven12_r0102", "ven12_r020", "ven12_r030", "ven12_r040", "ven12_r050", "ven12_r060", "ven12_r070", "ven12_r0701",
                  "ven12_r080", "ven12_r0901", "ven12_r0902", "ven12_r090", "ven12_r100", "ven12_r110", "ven12_r120", "ven12_r130", "ven12_r140", "ven12_r150",
                  "ven12_totald3c6", "ven12_totald3c7", "ven12_totald3c9", "ven12_totald3c10", "ven12_totald3c11", "ven12_totald4c3", "ven12_sumac", "ven12_row_idt",
                  "ven12_sumavensc", "ven12_sumafacil", "ven12_procfac_6d", "daj17_r010", "daj17_r020", "daj17_r030", "daj17_r050", "daj17_r060", "daj17_c6",
                  "daj17_c7", "daj17_c9", "daj17_c10", "daj17_c11", "daj17_r090", "daj17_r130", "daj17_r140", "daj17_control", "dass19_r010", "dass19_r0101",
                  "dass19_r01011", "dass19_r01012", "dass19_r0102", "dass19_r020", "dass19_r030", "dass19_r040", "dass19_r050", "dass19_r060", "dass19_r070",
                  "dass19_r080", "dass19_r090", "dass19_r100", "dass19_r110", "dass19_r130", "dass19_r140", "dass19_r150", "dass19_r160", "dass19_r170", "dass19_r180",
                  "dass19_d2c6", "dass19_d2c7", "dass19_d2c8", "dass19_d2c9", "dass19_d2c10", "dass19_d2c11", "dass19_d2c12", "ials21_sumven_cur_SAL", "ials21_sumsc_p_cur_SAL",
                  "ials21_sumsc_m_cur_SAL", "ials21_sumsc_sm_cur_SAL", "ials21_sumsc_n_cur_SAL", "ials21_sumsc_h_cur_SAL", "ials21_sumsc_tot_cur_SAL", "ials21_sumded1_cur_SAL",
                  "ials21_sumimp_cur_SAL", "ials21_sumded2_cur_SAL", "ials21_sumven_cur_FOL_WH", "ials21_sumven_cur_DIVA_WH", "ials21_sumven_cur_PL_WH", "ials21_sumven_cur_ROY_WH",
                  "ials21_sumven_cur_DONPF_WH", "ials21_sumven_cur_RCSA_WH", "ials21_sumven_cur_DOBBA_WH", "ials21_sumven_cur_VMS_WH", "ials21_sumven_cur_PLS_WH", "ials21_sumven_cur_DON_WH",
                  "ials21_sumven_cur_LIV_WH", "ials21_sumven_cur_NOR_WH", "ials21_sumven_cur_DOB_WH", "ials21_sumven_cur_CSM_WH", "ials21_sumven_cur_DON_P_WH", "ials21_sumven_cur_AGRAC_WH",
                  "ials21_sumven_cur_DOB B_WH", "ials21_sumven_cur_SER_WH", "ials21_sumven_cur_PLT_WH", "ials21_sumimp_cur_FOL_WH", "ials21_sumimp_cur_DIVA_WH", "ials21_sumimp_cur_PL_WH",
                  "ials21_sumimp_cur_ROY_WH", "ials21_sumimp_cur_DONPF_WH", "ials21_sumimp_cur_RCSA_WH", "ials21_sumimp_cur_DOBBA_WH", "ials21_sumimp_cur_VMS_WH", "ials21_sumimp_cur_PLS_WH",
                  "ials21_sumimp_cur_DON_WH", "ials21_sumimp_cur_LIV_WH", "ials21_sumimp_cur_NOR_WH", "ials21_sumimp_cur_DOB_WH", "ials21_sumimp_cur_CSM_WH", "ials21_sumimp_cur_DON_P_WH",
                  "ials21_sumimp_cur_AGRAC_WH", "ials21_sumimp_cur_DOB_B_WH", "ials21_sumimp_cur_SER_WH", "ials21_sumimp_cur_PLT_WH", "taxi18_tin_cds", "taxi18_sumac_cur",
                  "taxi18_tot_col9", "taxi18_t1c7tot_cur", "taxi18_t1c8tot_cur", "taxi18_t1c9tot_cur", "taxi18_t1c7_cur", "taxi18_t1c8_cur", "taxi18_t1c9_cur",
                  "unif21_t1r010", "unif21_t1r0101", "unif21_t1r0102", "unif21_t1r020", "unif21_t1r030", "unif21_t1r040", "unif21_t1r050", "unif21_t1r060",
                  "unif21_t1r070", "unif21_t1r0701", "unif21_t1r080", "unif21_t1r090", "unif21_t1r0901", "unif21_t1r0902", "unif21_t1r100", "unif21_t1r120",
                  "unif21_t1r130","unif21_a1t1r120c4", "unif21_a1t1totc4", "unif21_a2t1totc6", "unif21_a2t1totc7", "unif21_a2t1totc8",
                  "unif21_a2t1totc9", "unif21_a2t1totc10", "cet18_c1c3", "cet18_c31c3", "cet18_c5c3", "cet18_d1", "cet18_d2", "cet18_d3", "cet18_d4",
                  "cet18_d5", "cet18_d6", "cet18_d7", "cet18_de", "cet18_e1", "cet18_e2", "cet18_e3", "cet18_e4", "cet18_f1", "cet18_f2", "cet18_f3", "cet18_f4", "cet18_f5"
                  
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
          
      
      
      merged_PIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
      merged_PIT_BU_SIM$year <- as.character(forecast_horizon)
      merged_PIT_BU_SIM <- merged_PIT_BU_SIM[, c("year", names(merged_PIT_BU_SIM)[-length(merged_PIT_BU_SIM)])]
      
      numeric_columns <- sapply(merged_PIT_BU_SIM, is.numeric)
      merged_PIT_BU_SIM[, numeric_columns] <- merged_PIT_BU_SIM[, numeric_columns] / 1e06



# # 6. Decile ------------------------------------------------------------------

#  Decile and percentile groups --------------------------------------------

      # Names -------------------------------------------------------------------
      
      gross_cols <- c(
        "ai_17_r1c2","cet18_c5c3","daj17_r010","dass19_r010","unif21_t1r010",
        "ven12_r010","ials21_sumven_cur_SAL","ials21_sumven_cur_FOL_WH",
        "ials21_sumven_cur_PLS_WH","ials21_sumven_cur_PL_WH","ials21_sumven_cur_ROY_WH",
        "ials21_sumven_cur_DONPF_WH","ials21_sumven_cur_DON_P_WH","ials21_sumven_cur_RCSA_WH",
        "ials21_sumven_cur_DOBBA_WH","ials21_sumven_cur_DOB_WH","ials21_sumven_cur_VMS_WH",
        "ials21_sumven_cur_DON_WH","ials21_sumven_cur_LIV_WH","ials21_sumven_cur_NOR_WH",
        "ials21_sumven_cur_CSM_WH","ials21_sumven_cur_AGRAC_WH","ials21_sumven_cur_SER_WH",
        "ials21_sumven_cur_PLT_WH","ials21_sumven_cur_DIVA_WH"
      )
      pit_col <- "pitax"
      
      
      calc_decile_percentile_fun <- function(DT,
                                             inc_col = "gross_income",
                                             w_col  = "weight") {
        
        DT <- copy(DT)                     # avoid self-ref warning
        
        DT[, row_id__tmp := .I]            # remember original order
        setorderv(DT, inc_col)             # sort by income
        
        DT[, w_cumsum__tmp := cumsum(fifelse(is.na(get(w_col)), 0, get(w_col)))]
        total_w <- DT[.N, w_cumsum__tmp]
        
        decile_breaks  <- seq(0, total_w, length.out = 11)   # 10 deciles
        centile_breaks <- seq(0, total_w, length.out = 101)  # 100 centiles
        
        DT[, decile_group  := pmin(findInterval(w_cumsum__tmp, decile_breaks,
                                                rightmost.closed = TRUE), 10)]
        DT[, centile_group := pmin(findInterval(w_cumsum__tmp, centile_breaks,
                                                rightmost.closed = TRUE), 100)]
        
        setorder(DT, row_id__tmp)          # restore original order
        DT[, c("row_id__tmp","w_cumsum__tmp") := NULL]
        DT                                # return the updated data.table
      }
      
      
      # BU ----------------------------------------------------------------------
      # PICK THE RAW DATA.TABLE FOR THAT YEAR
      ix <- match(simulation_year, forecast_horizon)
      if (is.na(ix)) stop("simulation_year not found in forecast_horizon")
      
      raw_dt <- copy(PIT_BU_list[[scenarios[ix]]])   # <-- single data.table
      
      
      summarise_to_gross_pit <- function(dt) {
        need <- c("cod_fiscal", gross_cols, pit_col)
        miss <- setdiff(need, names(dt))
        if (length(miss)) dt[, (miss) := NA_real_]
        
        dt[, (c(gross_cols, pit_col)) :=
             lapply(.SD, as.numeric), .SDcols = c(gross_cols, pit_col)]
        
        dt[, gross_income := rowSums(.SD, na.rm = TRUE), .SDcols = gross_cols]
        dt[, pit_row      := get(pit_col)]
        
        out <- dt[, c(lapply(.SD, sum, na.rm = TRUE),
                      .(gross_income = sum(gross_income, na.rm = TRUE),
                        pit_sum      = sum(pit_row,      na.rm = TRUE))),
                  by      = cod_fiscal,
                  .SDcols = gross_cols]
        
        setcolorder(out, c("cod_fiscal","gross_income","pit_sum", gross_cols))
        out[]
      }
      
      PIT_BU_selected <- summarise_to_gross_pit(raw_dt)
      PIT_BU_selected[, weight := 1]
      PIT_BU_selected<-PIT_BU_selected%>%
        filter(gross_income>0)%>%
        filter(pit_sum>0)%>%
        mutate(etr=round(pit_sum/gross_income,1))%>%
        filter(etr<=0.25)
      
      
      # SIM ---------------------------------------------------------------------
      
      # PICK THE RAW DATA.TABLE FOR THAT YEAR
      ix <- match(simulation_year, forecast_horizon)
      if (is.na(ix)) stop("simulation_year not found in forecast_horizon")
      
      raw_dt <- copy(PIT_SIM_list[[scenarios[ix]]])   # <-- single data.table
      
      
      summarise_to_gross_pit <- function(dt) {
        need <- c("cod_fiscal", gross_cols, pit_col)
        miss <- setdiff(need, names(dt))
        if (length(miss)) dt[, (miss) := NA_real_]
        
        dt[, (c(gross_cols, pit_col)) :=
             lapply(.SD, as.numeric), .SDcols = c(gross_cols, pit_col)]
        
        dt[, gross_income := rowSums(.SD, na.rm = TRUE), .SDcols = gross_cols]
        dt[, pit_row      := get(pit_col)]
        
        out <- dt[, c(lapply(.SD, sum, na.rm = TRUE),
                      .(gross_income = sum(gross_income, na.rm = TRUE),
                        pit_sum      = sum(pit_row,      na.rm = TRUE))),
                  by      = cod_fiscal,
                  .SDcols = gross_cols]
        
        setcolorder(out, c("cod_fiscal","gross_income","pit_sum", gross_cols))
        out[]
      }
      
      PIT_SIM_selected <- summarise_to_gross_pit(raw_dt)
      PIT_SIM_selected[, weight := 1]
      PIT_SIM_selected<-PIT_SIM_selected%>%
        filter(gross_income>0)%>%
        filter(pit_sum>0)%>%
        mutate(etr=round(pit_sum/gross_income,1))%>%
        filter(etr<=0.25)
      
      

      

      
      


# Aggregated tax liability ------------------------------------------------
                      # Convert data for presentation in GUI
                      pit_summary_df <- merged_PIT_BU_SIM %>%
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
                      
                      
                      pit_summary_df <- as.data.table(pit_summary_df)
                      
                      
                      
                      print(merged_PIT_BU_SIM)
                      
                      end.time <- proc.time()
                      save.time <- end.time - start.time
                      cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")


                      gc(TRUE)

                    