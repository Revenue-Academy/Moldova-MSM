'Distribution tables'

# II.ESTIMATIONS OF PERCENTILE AND DECILE ---------------------------------
      # 1.Centile Groups ---------------------------------------------------------
          # 1.1 BU --------------------------------------------------------------------
                       
                        pit_centile_distribution_bu <- PIT_BU_selected[, .(
                                                                            sum_calc_pitax = sum(pitax, na.rm = TRUE),
                                                                            sum_total_gross_income = sum(gross_income, na.rm = TRUE)
                                                                          ), by = .(centile_group)]
      
                        # Calculate ETR
                        pit_centile_distribution_bu[, etr := sum_calc_pitax / sum_total_gross_income]
                        #pit_centile_distribution_bu[, year := forecast_horizon[match(scenario, scenarios)]]
                        setorder(pit_centile_distribution_bu, centile_group)
                  
                        
          # 1.2 SIM -------------------------------------------------------------------
                       
                        pit_centile_distribution_sim <- PIT_SIM_selected[, .(
                          sum_calc_pitax = sum(pitax, na.rm = TRUE),
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

            
      # 2.Decile Groups ---------------------------------------------------------
          # 1.BU --------------------------------------------------------------------

                        pit_decile_distribution_bu <- PIT_BU_selected[, .(
                                        sum_calc_pitax = sum(pitax, na.rm = TRUE),
                                        mean_calc_pitax = mean(pitax, na.rm = TRUE),
                                        sum_total_gross_income = sum(gross_income, na.rm = TRUE)
                        ), by = .(decile_group)]
                        
                     
                       # pit_decile_distribution_bu[, year := forecast_horizon[match(scenario, scenarios)]]
                        
                        
                        setorder(pit_decile_distribution_bu, decile_group )
                        
          # 2.SIM -------------------------------------------------------------------
                      
                        pit_decile_distribution_sim <- PIT_SIM_selected[, .(
                                                      sum_calc_pitax = sum(pitax, na.rm = TRUE),
                                                      mean_calc_pitax = mean(pitax, na.rm = TRUE),
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
                select(cod_fiscal, weight, gross_income, pitax) %>%
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
            pit_result_bins <- combined_dt_bins_fun[, .(sum_calc_pitax = sum(pitax)), by = .(bin_group)]
            
            # Calculate the sum for the "ALL" category for each scenario
            all_scenarios <- combined_dt_bins_fun[, .(bin_group = "ALL", sum_calc_pitax = sum(pitax))]
            
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
                select(cod_fiscal, weight, gross_income, pitax) %>%
                mutate(
                  weight_g = weight * gross_income,
                  bin_group = cut(gross_income, breaks = breaks, labels = labels, right = FALSE)
                ) %>%
                # Add the scenario identifier to the data frame
               # mutate(scenario = scenario)
              
              # Convert to data.table for efficient operations
              as.data.table(data)
            }))
            
            # Calculate the sum of pitax for each bin_group and scenario
            pit_result_bins <- combined_dt_bins_fun[, .(sum_calc_pitax = sum(pitax)), by = .(bin_group)]
            
            # Calculate the sum for the "ALL" category for each scenario
            all_scenarios <- combined_dt_bins_fun[, .(bin_group = "ALL", sum_calc_pitax = sum(pitax))]
            
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
            
