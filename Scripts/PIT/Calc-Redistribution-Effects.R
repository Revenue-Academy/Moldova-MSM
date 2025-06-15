'Re-Distribution tables'

#PIT_BU_selected

# # # 1. Functions for calculation -----------------------------------------------
# extract_filtered_re_df_fun <- function(PIT_BU_gross_list, forecast_horizon, simulation_year,
#                                        filter_positive = FALSE) {
#   # Validate simulation_year: check if it is in the forecast horizon vector.
#   if (!simulation_year %in% forecast_horizon) {
#     stop("The specified simulation year is not in the forecast horizons.")
#   }
#   
#   # Find the index of the dataset that corresponds to simulation_year.
#   index <- which(forecast_horizon == simulation_year)
#   
#   # Extract the specific data.table for the simulation year.
#   PIT_BU_simulation_year_df <- PIT_BU_gross_list[[index]]
#   
#   # Define the columns to keep.
#   columns_to_keep <- c("cod_fiscal",
#                        "gross_income",
#                        # "total_taxbase",
#                        # "total_net",
#                        "pitax")
#   
#   # Check for missing columns and issue a warning if any are not found.
#   missing_columns <- setdiff(columns_to_keep, colnames(PIT_BU_simulation_year_df))
#   if (length(missing_columns) > 0) {
#     warning("The following columns are missing in the data frame: ",
#             paste(missing_columns, collapse = ", "))
#   }
#   
#   # Filter the data.table to keep only the specified columns.
#   PIT_BU_simulation_year_df <- PIT_BU_simulation_year_df[, ..columns_to_keep, with = FALSE]
#   
#   # Optionally, filter rows where all numeric columns are greater than 0.
#   if (filter_positive) {
#     # Build a logical condition: for each column that is numeric, check that it is > 0.
#     condition <- Reduce("&", lapply(columns_to_keep, function(col) {
#       # If the column is numeric, return the boolean vector for > 0,
#       # otherwise, return TRUE for all rows.
#       if (is.numeric(PIT_BU_simulation_year_df[[col]])) {
#         PIT_BU_simulation_year_df[[col]] > 0
#       } else {
#         rep(TRUE, nrow(PIT_BU_simulation_year_df))
#       }
#     }))
#     PIT_BU_simulation_year_df <- PIT_BU_simulation_year_df[condition]
#   }
#   
#   return(PIT_BU_simulation_year_df)
# }
# 


      # 1.BU ----------------------------------------------------------------------

       # PIT_BU_simulation_year_df <- extract_filtered_re_df_fun(PIT_BU_gross_list, forecast_horizon, simulation_year)
        
        PIT_BU_simulation_year_df<-PIT_BU_selected%>%
          filter(pitax>0)
        
        # Top 1
        
        result <- PIT_BU_simulation_year_df %>%
          # Create 100 percentile groups based on gross_income
          mutate(percentile = ntile(gross_income, 100)) %>%
          # Keep only the group with the highest gross_income
          filter(percentile == 100) %>%
          # Sum the PIT for observations in this percentile group
          summarise(total_pitax = sum(pitax, na.rm = TRUE))
        
        
        share_top1_bu<-result$total_pitax/sum(PIT_BU_simulation_year_df$pitax)
        
        
        
        

      # Gini gross income
      gini_income_gross_bu <- round(ineq(PIT_BU_simulation_year_df$gross_income, type = "Gini", na.rm = TRUE), 4)
      
      # # Gini net income
      # gini_income_net <- round(ineq(PIT_BU_simulation_year_df$total_net, type = "Gini", na.rm = TRUE), 4)
      # 
      # # Gini tax base
      # gini_income_gross_tb <- round(ineq(PIT_BU_simulation_year_df$total_taxbase, type = "Gini", na.rm = TRUE), 4)
      
      
      # Calculate the Kakwani index
      ineq<-calcSConc(PIT_BU_simulation_year_df$pitax, PIT_BU_simulation_year_df$gross_income)
      kakwani_index_BU <- round(ineq$ineq$index - gini_income_gross_bu, 4)
      kakwani_index_BU <- unname(kakwani_index_BU)
      
      
      
      etr_bu <- sum(PIT_BU_simulation_year_df$pitax) / sum(PIT_BU_simulation_year_df$gross_income)
      
      # calcAtkinson(PIT_BU_simulation_year_df$gross_income, epsilon = 1)
      
      
      
      # Calculate all indicators and store in a table
      indicator_table_bu <- data.frame(
        Indicator = c(
          "Gini coefficient for pre-tax income",
          #"Gini coefficient for after-tax income",
          #"Concentration coefficient for after-tax income w.r.t. pre-tax income",
          #"Concentration coefficient for tax w.r.t. pre-tax income",
          #"Atkinson Index for pre-tax income",
          #"Atkinson Index for after-tax income",
          #"Coefficient of squared variation (I2)",
          #"Mean logarithmic deviation (I0)",
          "Effective tax rate",
          "Kakwani Index"
        ),
        Name = c(
          "gini_income_gross_bu",
          # "gini_gx_t",
          # "concentration_cx_t",
          # "concentration_ct",
          # "atkinson_index_pre_tax_income",
          # "atkinson_index_after_tax_income",
          # "I2",
          # "I0",
          "etr_bu",
          "kakwani_index_BU"
          
        ),
        Simulation = c(
          round(gini_income_gross_bu,4),
          round(etr_bu,4),
          round(kakwani_index_BU,4)
        )
      )%>%data.table()
      
      
      
      # 2.SIM -------------------------------------------------------------------
      
      
      #PIT_SIM_simulation_year_df <- extract_filtered_re_df_fun(PIT_SIM_gross_list, forecast_horizon, simulation_year)
      
      
      PIT_SIM_simulation_year_df<-PIT_SIM_selected%>%
        filter(pitax>0)
      
      
      # TOP 1
      
      result <- PIT_SIM_simulation_year_df %>%
        # Create 100 percentile groups based on gross_income
        mutate(percentile = ntile(gross_income, 100)) %>%
        # Keep only the group with the highest gross_income
        filter(percentile == 100) %>%
        # Sum the PIT for observations in this percentile group
        summarise(total_pitax = sum(pitax, na.rm = TRUE))
      
      
      share_top1_sim<-result$total_pitax/sum(PIT_SIM_simulation_year_df$pitax)
      
      
      
      
      # Gini gross income
      gini_income_gross_sim <- round(ineq(PIT_SIM_simulation_year_df$gross_income, type = "Gini", na.rm = TRUE), 4)
      
      # # Gini net income
      # gini_income_net <- round(ineq(PIT_SIM_simulation_year_df$total_net, type = "Gini", na.rm = TRUE), 4)
      # 
      # # Gini tax base
      # gini_income_gross_tb <- round(ineq(PIT_SIM_simulation_year_df$total_taxbase, type = "Gini", na.rm = TRUE), 4)
      
      
      # Calculate the Kakwani index
      ineq<-calcSConc(PIT_SIM_simulation_year_df$pitax, PIT_SIM_simulation_year_df$gross_income)
      kakwani_index_SIM <- round(ineq$ineq$index - gini_income_gross_sim, 4)
      kakwani_index_SIM <- unname(kakwani_index_SIM)
      
      
      
      etr_SIM <- sum(PIT_SIM_simulation_year_df$pitax) / sum(PIT_SIM_simulation_year_df$gross_income)
      
      # calcAtkinson(PIT_SIM_simulation_year_df$gross_income, epsilon = 1)
      
      
      
      # Calculate all indicators and store in a table
      indicator_table_SIM <- data.frame(
        Indicator = c(
          "Gini coefficient for pre-tax income",
          #"Gini coefficient for after-tax income",
          #"Concentration coefficient for after-tax income w.r.t. pre-tax income",
          #"Concentration coefficient for tax w.r.t. pre-tax income",
          #"Atkinson Index for pre-tax income",
          #"Atkinson Index for after-tax income",
          #"Coefficient of squared variation (I2)",
          #"Mean logarithmic deviation (I0)",
          "Effective tax rate",
          "Kakwani Index"
        ),
        Name = c(
          "gini_income_gross_sim",
          # "gini_gx_t",
          # "concentration_cx_t",
          # "concentration_ct",
          # "atkinson_index_pre_tax_income",
          # "atkinson_index_after_tax_income",
          # "I2",
          # "I0",
          "etr_SIM",
          "kakwani_index_SIM"
          
        ),
        Simulation = c(
          round(gini_income_gross_sim,4),
          round(etr_SIM,4),
          round(kakwani_index_SIM,4)
        )
      )%>%data.table()
      
      
      
      # 3.Merging table ---------------------------------------------------------
      
      
      
      
      
      
      re_effects_final <- merge(indicator_table_bu, indicator_table_SIM[, c("Indicator",  "Simulation")], 
                                by = c("Indicator"), 
                                suffixes = c("_bu", "_sim"))%>%
        select(-c("Name"))%>%
        rename("Business as usual"="Simulation_bu",
               "Simulation"="Simulation_sim",
        )
      
      
      
      
      
      #re_effects_final$`Business as usual`<-round(re_effects_final$`Business as usual`,4)
      re_effects_final$Simulation<-round(re_effects_final$Simulation,4)
      # 
      # re_effects_final$Name<-NULL
      
      
      re_effects_final <- re_effects_final %>%
        dplyr::mutate(`Percentage Difference (%)` = round((`Simulation` - `Business as usual`) / `Business as usual` * 100, 1))
      
      

      rm(PIT_BU_list,PIT_SIM_list,extracted_dist_tables_bu,extracted_dist_tables_sim,extracted_dist_tables_bu,
         extracted_tables_bu,extracted_tables_sim
         )
      
      
      gc(TRUE)
      
      