'Distribution tables'


# # Names -------------------------------------------------------------------
# 
# gross_cols <- c(
#   "ai_17_r1c2","cet18_c5c3","daj17_r010","dass19_r010","unif21_t1r010",
#   "ven12_r010","ials21_sumven_cur_SAL","ials21_sumven_cur_FOL_WH",
#   "ials21_sumven_cur_PLS_WH","ials21_sumven_cur_PL_WH","ials21_sumven_cur_ROY_WH",
#   "ials21_sumven_cur_DONPF_WH","ials21_sumven_cur_DON_P_WH","ials21_sumven_cur_RCSA_WH",
#   "ials21_sumven_cur_DOBBA_WH","ials21_sumven_cur_DOB_WH","ials21_sumven_cur_VMS_WH",
#   "ials21_sumven_cur_DON_WH","ials21_sumven_cur_LIV_WH","ials21_sumven_cur_NOR_WH",
#   "ials21_sumven_cur_CSM_WH","ials21_sumven_cur_AGRAC_WH","ials21_sumven_cur_SER_WH",
#   "ials21_sumven_cur_PLT_WH","ials21_sumven_cur_DIVA_WH"
# )
# pit_col <- "pitax"
# 
# 
# calc_decile_percentile_fun <- function(DT,
#                                        inc_col = "gross_income",
#                                        w_col  = "weight") {
#   
#   DT <- copy(DT)                     # avoid self-ref warning
#   
#   DT[, row_id__tmp := .I]            # remember original order
#   setorderv(DT, inc_col)             # sort by income
#   
#   DT[, w_cumsum__tmp := cumsum(fifelse(is.na(get(w_col)), 0, get(w_col)))]
#   total_w <- DT[.N, w_cumsum__tmp]
#   
#   decile_breaks  <- seq(0, total_w, length.out = 11)   # 10 deciles
#   centile_breaks <- seq(0, total_w, length.out = 101)  # 100 centiles
#   
#   DT[, decile_group  := pmin(findInterval(w_cumsum__tmp, decile_breaks,
#                                           rightmost.closed = TRUE), 10)]
#   DT[, centile_group := pmin(findInterval(w_cumsum__tmp, centile_breaks,
#                                           rightmost.closed = TRUE), 100)]
#   
#   setorder(DT, row_id__tmp)          # restore original order
#   DT[, c("row_id__tmp","w_cumsum__tmp") := NULL]
#   DT                                # return the updated data.table
# }
# 
# 
# # BU ----------------------------------------------------------------------
# # PICK THE RAW DATA.TABLE FOR THAT YEAR
# ix <- match(simulation_year, forecast_horizon)
# if (is.na(ix)) stop("simulation_year not found in forecast_horizon")
# 
# raw_dt <- copy(PIT_BU_list[[scenarios[ix]]])   # <-- single data.table
# 
# 
# summarise_to_gross_pit <- function(dt) {
#   need <- c("cod_fiscal", gross_cols, pit_col)
#   miss <- setdiff(need, names(dt))
#   if (length(miss)) dt[, (miss) := NA_real_]
#   
#   dt[, (c(gross_cols, pit_col)) :=
#        lapply(.SD, as.numeric), .SDcols = c(gross_cols, pit_col)]
#   
#   dt[, gross_income := rowSums(.SD, na.rm = TRUE), .SDcols = gross_cols]
#   dt[, pit_row      := get(pit_col)]
#   
#   out <- dt[, c(lapply(.SD, sum, na.rm = TRUE),
#                 .(gross_income = sum(gross_income, na.rm = TRUE),
#                   pit_sum      = sum(pit_row,      na.rm = TRUE))),
#             by      = cod_fiscal,
#             .SDcols = gross_cols]
#   
#   setcolorder(out, c("cod_fiscal","gross_income","pit_sum", gross_cols))
#   out[]
# }
# 
# PIT_BU_selected <- summarise_to_gross_pit(raw_dt)
# PIT_BU_selected[, weight := 1]
# PIT_BU_selected<-PIT_BU_selected%>%
#   filter(gross_income>0)%>%
#   filter(pit_sum>0)%>%
#   mutate(etr=round(pit_sum/gross_income,1))%>%
#   filter(etr<=0.25)
# 
# 
# # SIM ---------------------------------------------------------------------
# 
# # PICK THE RAW DATA.TABLE FOR THAT YEAR
# ix <- match(simulation_year, forecast_horizon)
# if (is.na(ix)) stop("simulation_year not found in forecast_horizon")
# 
# raw_dt <- copy(PIT_SIM_list[[scenarios[ix]]])   # <-- single data.table
# 
# 
# summarise_to_gross_pit <- function(dt) {
#   need <- c("cod_fiscal", gross_cols, pit_col)
#   miss <- setdiff(need, names(dt))
#   if (length(miss)) dt[, (miss) := NA_real_]
#   
#   dt[, (c(gross_cols, pit_col)) :=
#        lapply(.SD, as.numeric), .SDcols = c(gross_cols, pit_col)]
#   
#   dt[, gross_income := rowSums(.SD, na.rm = TRUE), .SDcols = gross_cols]
#   dt[, pit_row      := get(pit_col)]
#   
#   out <- dt[, c(lapply(.SD, sum, na.rm = TRUE),
#                 .(gross_income = sum(gross_income, na.rm = TRUE),
#                   pit_sum      = sum(pit_row,      na.rm = TRUE))),
#             by      = cod_fiscal,
#             .SDcols = gross_cols]
#   
#   setcolorder(out, c("cod_fiscal","gross_income","pit_sum", gross_cols))
#   out[]
# }
# 
# PIT_SIM_selected <- summarise_to_gross_pit(raw_dt)
# PIT_SIM_selected[, weight := 1]
# PIT_SIM_selected<-PIT_SIM_selected%>%
#   filter(gross_income>0)%>%
#   filter(pit_sum>0)%>%
#   mutate(etr=round(pit_sum/gross_income,1))%>%
#   filter(etr<=0.25)








###############################################################################
##  3.  ADD DECILE & CENTILE GROUPS  ------------------------------------------
###############################################################################


PIT_BU_selected <- calc_decile_percentile_fun(PIT_BU_selected)
PIT_SIM_selected <- calc_decile_percentile_fun(PIT_SIM_selected)




###############################################################################
##  4.  RESULT ---------------------------------------------------------------
###############################################################################
# View(PIT_BU_selected)
# View(PIT_SIM_selected)













# I.FUNCTIONS ---------------------------------------------------------------

        # # Function to extract columns and add scenario
        # extract_centile_rev_fun <- function(dt, scenario) {
        #   dt[, .(centile_group,pit_sum,gross_income, scenario = scenario)]
        # }
        # 
        # # Function to extract columns and add scenario
        # extract_dec_rev_fun <- function(dt, scenario) {
        #   dt[, .(decile_group,pit_sum,gross_income, scenario = scenario)]
        # }
        # 
        # # Function to extract columns and add scenario
        # extract_bins_rev_fun <- function(dt, scenario) {
        #   dt[, .(cod_fiscal,weight,gross_income,pit_sum, scenario = scenario)]
        # }
        
# II.ESTIMATIONS OF PERCENTILE AND DECILE ---------------------------------
      # 1.Centile Groups ---------------------------------------------------------
          # 1.1 BU --------------------------------------------------------------------
                       # extracted_dist_tables_bu <- mapply(extract_centile_rev_fun, PIT_BU_list, scenarios, SIMPLIFY = FALSE)
                       #  combined_dt <- rbindlist(extracted_dist_tables_bu)
                       #  
                       #  
                       #  combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
                       #  
                       #  combined_dt<-combined_dt%>%
                       #    filter(year==simulation_year)
                       #  
                       # 
                       #  
                       #  
                       #  
                       #  # # # Convert your dataset to a data.table
                       #   combined_dt <- as.data.table(combined_dt)
                       # 
                        # Perform the required operations
                        pit_centile_distribution_bu <- PIT_BU_selected[, .(
                                                                            sum_calc_pitax = sum(pit_sum, na.rm = TRUE),
                                                                            sum_total_gross_income = sum(gross_income, na.rm = TRUE)
                                                                          ), by = .(centile_group)]
      
                        # Calculate ETR
                        pit_centile_distribution_bu[, etr := sum_calc_pitax / sum_total_gross_income]
                        #pit_centile_distribution_bu[, year := forecast_horizon[match(scenario, scenarios)]]
                        setorder(pit_centile_distribution_bu, centile_group)
                  
                        
          # 1.2 SIM -------------------------------------------------------------------
                        # extracted_dist_tables_sim <- mapply(extract_centile_rev_fun, PIT_SIM_list, scenarios, SIMPLIFY = FALSE)
                        # combined_dt <- rbindlist(extracted_dist_tables_sim)
                        # 
                        # combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
                        # combined_dt<-combined_dt%>%
                        #   filter(year==simulation_year)
                        
                        
                        
                        #combined_dt <- as.data.table(combined_dt)
      
                        pit_centile_distribution_sim <- PIT_SIM_selected[, .(
                          sum_calc_pitax = sum(pit_sum, na.rm = TRUE),
                          sum_total_gross_income = sum(gross_income, na.rm = TRUE)
                        ), by = .(centile_group)]
      
                        # Calculate ETR
                        pit_centile_distribution_sim[, etr := sum_calc_pitax / sum_total_gross_income]
                        #pit_centile_distribution_sim[, year := forecast_horizon[match(scenario, scenarios)]]
                        setorder(pit_centile_distribution_sim, centile_group)
                        
                     
      
          # 1.3 MERGE BU AND SIM -----------------------------------------------------------------
                        setkey(pit_centile_distribution_bu, centile_group)
                        setkey(pit_centile_distribution_sim, centile_group)
                        
                        pit_centile_distribution_bu_sim <- merge(pit_centile_distribution_bu, pit_centile_distribution_sim, by = c("centile_group"), suffixes = c("_bu", "_sim"))
                        setorder(pit_centile_distribution_bu_sim, centile_group)
      
          # 1.3 Chart -------------------------------------------------------------------
                      # pit_centile_distribution_bu_sub<-pit_centile_distribution_bu_sim%>%
                      #                               filter(year==simulation_year)
                      # 
            
      # 2.Decile Groups ---------------------------------------------------------
          # 1.BU --------------------------------------------------------------------
                       # extracted_dist_tables_bu <- mapply(extract_dec_rev_fun, PIT_BU_list, scenarios, SIMPLIFY = FALSE)
                       #  combined_dt <- rbindlist(extracted_dist_tables_bu)
                       # 
                       #  combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
                       #  
                       #  combined_dt<-combined_dt%>%
                       #    filter(year==simulation_year)
                   
                        
                        pit_decile_distribution_bu <- PIT_BU_selected[, .(
                                        sum_calc_pitax = sum(pit_sum, na.rm = TRUE),
                                        mean_calc_pitax = mean(pit_sum, na.rm = TRUE),
                                        sum_total_gross_income = sum(gross_income, na.rm = TRUE)
                        ), by = .(decile_group)]
                        
                     
                       # pit_decile_distribution_bu[, year := forecast_horizon[match(scenario, scenarios)]]
                        
                        
                        setorder(pit_decile_distribution_bu, decile_group )
                        
          # 2.SIM -------------------------------------------------------------------
                        # extracted_dist_tables_sim <- mapply(extract_dec_rev_fun, PIT_SIM_list, scenarios, SIMPLIFY = FALSE)
                        # combined_dt <- rbindlist(extracted_dist_tables_sim)
                        # 
                        # combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
                        # 
                        # combined_dt<-combined_dt%>%
                        #   filter(year==simulation_year)
                        # 
                        
                        
                        
                        #setorder(combined_dt, decile_group)
                        
                        pit_decile_distribution_sim <- PIT_SIM_selected[, .(
                                                      sum_calc_pitax = sum(pit_sum, na.rm = TRUE),
                                                      mean_calc_pitax = mean(pit_sum, na.rm = TRUE),
                                                      sum_total_gross_income = sum(gross_income, na.rm = TRUE)
                                                    ), by = .(decile_group)]
                        
                        # Calculate ETR
                        pit_decile_distribution_sim[, etr := sum_calc_pitax / sum_total_gross_income]
                        
                        #pit_decile_distribution_sim[, year := forecast_horizon[match(scenario, scenarios)]]
                        
                        
                        setorder(pit_decile_distribution_sim, decile_group )
                        
                        
          # 3.MERGE BU AND SIM -----------------------------------------------------------------
                        setkey(pit_decile_distribution_bu, decile_group)
                        setkey(pit_decile_distribution_sim, decile_group)
                        pit_decile_distribution_bu_sim_raw <- merge(pit_decile_distribution_bu, pit_decile_distribution_sim, by = c("decile_group"), suffixes = c("_bu", "_sim"))
                        
                        pit_decile_distribution_bu_sim<-pit_decile_distribution_bu_sim_raw
      
                        #pit_decile_distribution_bu_sim$year<-as.character(pit_decile_distribution_bu_sim$year)
                        pit_decile_distribution_bu_sim$decile_group<-as.character(pit_decile_distribution_bu_sim$decile_group)
                        
                        pit_decile_distribution_bu_sim<-setnames(pit_decile_distribution_bu_sim,
                                 old = c('decile_group','sum_calc_pitax_bu', 'mean_calc_pitax_bu', 'sum_total_gross_income_bu','sum_calc_pitax_sim', 'mean_calc_pitax_sim', 'sum_total_gross_income_sim'),
                                 new = c( 'Decile groups', 'Total PIT liability (business as usual)', 'Average PIT liability (business as usual)', 'Total gross income (business as usual)',
                                         'Total PIT liability (simulation)', 'Average PIT liability (simulation)', 'Total gross income (simulation)'
                                 ))
      
      
                        
      
                       
                        # pit_decile_distribution_bu_sim <- pit_decile_distribution_bu_sim %>%
                        #   mutate_if(is.numeric, ~ round(. / 1e06, 1))
      
                        
                        pit_decile_distribution_bu_sim <- pit_decile_distribution_bu_sim %>%
                          mutate(across(
                            .cols = where(is.numeric) & !starts_with("Average PIT liability (business as usual)") & 
                              !starts_with("Average PIT liability (simulation)"),
                            .fns = ~ round(. / 1e06, 1)
                          )) %>%
                          mutate(across(
                            .cols = where(is.numeric) & (starts_with("Average PIT liability (business as usual)") | 
                                                           starts_with("Average PIT liability (simulation)")),
                            .fns = ~ round(. / 1000, 1)
                          ))
                        
                        
                        #setorder(pit_decile_distribution_bu_sim, year)
                        # 
                        # pit_decile_distribution_bu_sim<-pit_decile_distribution_bu_sim%>%
                        #   filter(year==SimulationYear)
                        # 
                        # pit_decile_distribution_bu_sim$scenario<-NULL
                        # pit_decile_distribution_bu_sim$year<-NULL
                        # 
      # 3. Chart -------------------------------------------------------------------
                      # pit_decile_distribution_bu_sub<-pit_decile_distribution_bu_sim%>%
                      #                               filter(year==simulation_year)
                    
      # II.PIT Distribution Table Income Breaks ( NEW-TEST)-----------------------------------------------------------
            # 1.BU ----------------------------------------------------------------------
            
            # Define the breakpoints and labels
            breaks <- c( -Inf, 0,1e-09,500000.0,1000000.0,1500000.0,2000000.0,3000000.0,4000000.0,5000000.0,10000000.0,9e+99)
            labels <- c("<0","=0","0-0.5 m","0.5-1m","1-1.5m","1.5-2m","2-3m","3-4m","4-5m","5-10m",">10m")
            
            
            # Apply the transformations across all scenarios in PIT_BU_list
            combined_dt_bins_fun <- rbindlist(lapply(names(PIT_BU_selected), function(scenario) {
              # Extract the data frame for each scenario
              #data <- PIT_BU_list[[scenario]] %>%
              data <- PIT_BU_selected%>%
                select(cod_fiscal, weight, gross_income, pit_sum) %>%
                mutate(
                  weight_g = weight * gross_income,
                  bin_group = cut(gross_income, breaks = breaks, labels = labels, right = FALSE)
                ) %>%
                # Add the scenario identifier to the data frame
                #mutate(scenario = scenario)
              
              # Convert to data.table for efficient operations
              as.data.table(data)
            }))
            
            # Calculate the sum of calc_pit for each bin_group and scenario
            pit_result_bins <- combined_dt_bins_fun[, .(sum_calc_pitax = sum(pit_sum)), by = .(bin_group)]
            
            # Calculate the sum for the "ALL" category for each scenario
            all_scenarios <- combined_dt_bins_fun[, .(bin_group = "ALL", sum_calc_pitax = sum(pit_sum))]
            
            # Combine the results with the "ALL" category
            pit_result_bins_bu <- rbind(pit_result_bins, all_scenarios, fill = TRUE)
            
            # Add the year column using the forecast_horizon vector
            #pit_result_bins_bu[, year := forecast_horizon[match(scenario, scenarios)]]
            
            
            # Chart -------------------------------------------------------------------
            
            pit_result_bins_bu_sub <- pit_result_bins_bu %>%
              #filter(year == SimulationYear) %>%
              filter(bin_group != "ALL" & bin_group != "0")
            
            # 2.SIM -------------------------------------------------------------------
            
            combined_dt_bins_fun <- rbindlist(lapply(names(PIT_SIM_selected), function(scenario) {
              # Extract the data frame for each scenario
              #data <- PIT_SIM_selected[[scenario]] %>%
              data <- PIT_SIM_selected %>%
                select(cod_fiscal, weight, gross_income, pit_sum) %>%
                mutate(
                  weight_g = weight * gross_income,
                  bin_group = cut(gross_income, breaks = breaks, labels = labels, right = FALSE)
                ) %>%
                # Add the scenario identifier to the data frame
               # mutate(scenario = scenario)
              
              # Convert to data.table for efficient operations
              as.data.table(data)
            }))
            
            # Calculate the sum of pit_sum for each bin_group and scenario
            pit_result_bins <- combined_dt_bins_fun[, .(sum_calc_pitax = sum(pit_sum)), by = .(bin_group)]
            
            # Calculate the sum for the "ALL" category for each scenario
            all_scenarios <- combined_dt_bins_fun[, .(bin_group = "ALL", sum_calc_pitax = sum(pit_sum))]
            
            # Combine the results with the "ALL" category
            pit_result_bins_sim <- rbind(pit_result_bins, all_scenarios, fill = TRUE)
            
            # Add the year column using the forecast_horizon vector
           # pit_result_bins_sim[, year := forecast_horizon[match(scenario, scenarios)]]
            
            
            
            # Chart -------------------------------------------------------------------
            
            pit_result_bins_sim_sub <- pit_result_bins_sim %>%
              #filter(year == SimulationYear) %>%
              filter(bin_group != "ALL" & bin_group != "0")
            # %>%
            #   select(-c(scenario,year))
            # 
            
            # Reorder the bin_group factor
            pit_result_bins_sim_sub[, bin_group := factor(bin_group, levels =  c("<0","=0","0-0.5 m","0.5-1m","1-1.5m","1.5-2m","2-3m","3-4m","4-5m","5-10m",">10m"))]
            
            # Order the data.table by the new factor levels
            setorder(pit_result_bins_sim_sub, bin_group)
            # test ovde
            pit_result_bins_sim_sub$sum_calc_pitax<-pit_result_bins_sim_sub$sum_calc_pitax/1e06
            pit_result_bins_sim_sub$sum_calc_pitax<-round(pit_result_bins_sim_sub$sum_calc_pitax,1)
            
            
            
            
         
            
    # Removing of objects        
            
    #rm(PIT_BU_list,PIT_SIM_list,selected_gross_desc_tbl,selected_gross_nace_tbl)
   # gc(TRUE)
           
            
    