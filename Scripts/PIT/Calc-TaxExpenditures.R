" Data Preparation for Tax Expenditures "

# I. Data preparation --------------------------------------------------------
      # 1.Summarize data and prepare table for GUI with TE -------------------------------------------------------
      # Extract by type of TE
      patterns <- c('year',
                    "pit_tax_agr", 
                    "pit_tax_donation",
                    "pit_tax_CopyrightIncomeArtisticPhotography_l",
                    "pit_tax_CopyrightIncomeMusicBallet_l", 
                    "pit_tax_CopyrightIncomePaintingsSculptural_l",
                    "pit_tax_CopyrightIncomeTranslationsLectures_l", 
                    "pit_tax_CopyrightIncomeSuccessor_l",
                    "pit_tax_WorkIncome_l", 
                    "pit_other_income_l", 
                    "pit_tax_IndustrialPropertyRights_c",
                    "pit_tax_Lease_c", 
                    "pit_LeaseBusiness_c", 
                    "pit_SolidWaste_c", 
                    "pit_GamesofChanceSpecific_c",
                    "pit_GamesofChanceBettingShop_c", 
                    "pit_Insurance_c", 
                    "pitax",
                    "pit_w",
                    "pit_c"
                    )
      
      
      # Create a select statement with all the patterns
      selected_columns <- select(merged_PIT_BU_SIM, starts_with(patterns))
      
      
      # Reshape the data to long format
      te_summary_df <- selected_columns %>%
        pivot_longer(cols = -year, 
                     names_to = c("variable", ".value"), 
                     names_pattern = "(.*)_(bu|sim)")
      
      # Calculate the TE as difference between _sim and _bu columns
      te_summary_df <- te_summary_df %>%
        mutate(difference = sim - bu)
      
      te_summary_df <- te_summary_df %>%
        mutate(across(c(bu, sim, difference), ~ round(., 1)))
      
      # Arrange the columns
      te_summary_df <- te_summary_df %>%
        select(year, variable, bu, sim, difference)%>%
        dplyr::rename("tax incentive"="variable",
                      #"tax_incentive"="variable",
                      "current law"="bu",
                      "benchmark"="sim",
                      "tax expenditure"="difference",
        )
      
      
      # Adding name from raw table
     
      
      te_summary_df<-left_join(te_summary_df,pit_simulation_parameters_raw,by=c(`tax incentive`="LongName"))%>%
        select( year,`tax incentive`,`current law`,benchmark,`tax expenditure`,AdditionalInfo)%>%
        dplyr::rename("legal reference"="AdditionalInfo")
      
      
      
      # Table for GUI
      te_summary_df <- as.data.table(te_summary_df)
      
      
      # 2.Tax expenditures by years ------------------------------------------------------------------------
      

      te_agg<-te_summary_df%>%
        dplyr::select(year, `tax incentive`, `tax expenditure`)%>%
        dplyr:: filter(`tax incentive` %in% c("pitax"))
        #dplyr:: filter(`tax incentive` %in% c("pitax",'pit_diplomatic_consular'))
     
      
       
      te_agg <- te_agg %>%
        mutate(`tax incentive` = recode(`tax incentive`, 
                                        pitax = "Tax_Expenditures"
        ))
      
      # 3.Tax expenditures by  type of income (Capital and Labor) ---------------------------------------------------
      
      te_labor_capital<-te_summary_df%>%
        dplyr::select(year, `tax incentive`, `tax expenditure`)%>%
        #dplyr:: filter(`tax incentive` %in% c("calc_labor", "calc_capital"))
        dplyr:: filter(`tax incentive` %in% c("pit_w", "pit_c"))
      
      
      # Replace 'calc_labor' with 'labor' and 'calc_capital' with 'capital'
      te_labor_capital <- te_labor_capital %>%
        mutate(`tax incentive` = recode(`tax incentive`, 
                                        pit_w = "labor", 
                                        pit_c = "capital"))
      
      
      # 4.Tax expenditures by NACE sections -----------------------------------------------------------
      
      # Function to extract columns and add scenario identifier
      extract_te_nace_fun <- function(dt, scenario) {
        dt[, .(pitax, nace_section, scenario = scenario)]
      }
      
      # Extract and process PIT_BU_list
      extracted_tables_bu <- mapply(extract_te_nace_fun, PIT_BU_list, scenarios, SIMPLIFY = FALSE)
      combined_data_bu <- rbindlist(extracted_tables_bu)
      result_bu <- combined_data_bu[, .(total_calc_pitax_bu = sum(pitax)), by = .(scenario, nace_section)]
      
      # Extract and process PIT_SIM_list
      
      extracted_tables_sim <- mapply(extract_te_nace_fun, PIT_SIM_list, scenarios, SIMPLIFY = FALSE)
      combined_data_sim <- rbindlist(extracted_tables_sim)
      result_sim <- combined_data_sim[, .(total_calc_pitax_sim = sum(pitax)), by = .(scenario, nace_section)]
      
      # Add year column to both results
      result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
      result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
      
      # Combine both results into one data frame
      nace_pit_summary <- merge(result_bu, result_sim, by = c("scenario", "year","nace_section"), all = TRUE)
      
      
      nace_pit_summary<-nace_pit_summary%>%
        dplyr::mutate(tax_expenditures=total_calc_pitax_sim-total_calc_pitax_bu)
      
      
      nace_pit_summary_te<-nace_pit_summary%>%
        select(year,nace_section,tax_expenditures)
      
      
      # Remove rows with NA in nace_section for the plot
      nace_pit_summary_te <- nace_pit_summary_te[!is.na(nace_section)]
      #nace_pit_summary_te[is.na(nace_section), nace_section := "Other"]
      
      # Left join
      
      # Convert NACE in data.table format 
      df_nace_names<-df_nace_names%>%
        data.table()
      
      # Perform the left join
      nace_pit_summary_te <- left_join(nace_pit_summary_te, df_nace_names, by = c("nace_section" = "section"))
      
      
      
      # 5.Tax Expenditures by Decile Groups---------------------------------------------
      
      # Function to extract columns and add scenario identifier
      extract_te_decile_fun <- function(dt, scenario) {
        #dt[, .(pitax, bin, scenario = scenario)]
        dt[, .(pitax, decile_group, scenario = scenario)]
      }
      
      
      # Extract and process PIT_BU_list
      extracted_tables_bu <- mapply(extract_te_decile_fun, PIT_BU_list, scenarios, SIMPLIFY = FALSE)
      combined_data_bu <- rbindlist(extracted_tables_bu)
      result_bu <- combined_data_bu[, .(total_calc_pitax_bu = sum(pitax)), by = .(scenario, decile_group)]
      
      # Extract and process PIT_SIM_list
      extracted_tables_sim <- mapply(extract_te_decile_fun, PIT_SIM_list, scenarios, SIMPLIFY = FALSE)
      combined_data_sim <- rbindlist(extracted_tables_sim)
      result_sim <- combined_data_sim[, .(total_calc_pitax_sim = sum(pitax)), by = .(scenario, decile_group)]
      
      # Add year column to both results
      result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
      result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
      
      # Combine both results into one data frame
      decile_pit_summary <- merge(result_bu, result_sim, by = c("scenario", "year","decile_group"), all = TRUE)
      
      
      decile_pit_summary<-decile_pit_summary%>%
        dplyr::mutate(tax_expenditures=total_calc_pitax_sim-total_calc_pitax_bu)
      
      
      
      decile_pit_summary<-decile_pit_summary%>%
        select(year,decile_group,tax_expenditures)
      
      #decile_pit_summary$bin<-decile_pit_summary$bin+1
      
      