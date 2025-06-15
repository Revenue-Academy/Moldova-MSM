" Data prep Distribution Dashboard "

# I.Gross income ----------------------------------------------------------
    # 1. Structure of gross income by decile groups ----------------------------------------------------------------------
              # BU ----------------------------------------------------------------------
              
              PIT_BU_selected_dec<-PIT_BU_selected%>%
                select(decile_group,wages_inc,investment_inc,business_inc)
              
              PIT_BU_selected_dec_agg<-PIT_BU_selected_dec%>%
                group_by(decile_group)%>%
                summarise(wages_inc=sum(wages_inc,na.rm = TRUE),
                          investment_inc=sum(investment_inc,na.rm = TRUE),
                          business_inc=sum(business_inc,na.rm = TRUE)
                )
              # Rename all columns except the first one by adding "_bu" prefix
              colnames(PIT_BU_selected_dec_agg)[-1] <- paste0("bu_", colnames(PIT_BU_selected_dec_agg)[-1])
              
              
              # SIM ---------------------------------------------------------------------
              
              PIT_SIM_selected_dec<-PIT_BU_selected%>%
                select(decile_group,wages_inc,investment_inc,business_inc)
              
              PIT_SIM_selected_dec_agg<-PIT_BU_selected_dec%>%
                group_by(decile_group)%>%
                summarise(wages_inc=sum(wages_inc,na.rm = TRUE),
                          investment_inc=sum(investment_inc,na.rm = TRUE),
                          business_inc=sum(business_inc,na.rm = TRUE)
                )
              
              
              # Rename all columns except the first one by adding "_bu" prefix
              colnames(PIT_SIM_selected_dec_agg)[-1] <- paste0("sim_", colnames(PIT_SIM_selected_dec_agg)[-1])
              
              
              
              # Step 1: Merge by decile_group
              merged_df <- left_join(PIT_BU_selected_dec_agg, PIT_SIM_selected_dec_agg, by = "decile_group")
              
              # Step 2: Pivot longer
              long_df <- merged_df %>%
                pivot_longer(
                  cols = -decile_group,
                  names_to = c("scenario", "income_type"),
                  names_sep = "_",
                  values_to = "value"
                )
              
              # Optional: Arrange neatly
              long_df <- long_df %>%
                filter(scenario=="sim")%>%
                arrange(decile_group, income_type, scenario)
    
    
    
    # 2. Pie chart ----------------------------------------------------------------------
    
    
              structure_gross_inc<-long_df%>%
               # dplyr::filter(scenario=="bu")%>%
                group_by(income_type)%>%
                summarise(value=sum(value,na.rm = TRUE))
                
   
# II.PIT revenues ---------------------------------------------------------

        
              # 1. Structure of gross income by decile groups ----------------------------------------------------------------------
              # BU ----------------------------------------------------------------------
              
              PIT_BU_selected_dec<-PIT_BU_selected%>%
                select(decile_group,wages_pit,investment_pit,business_pit)
              
              PIT_BU_selected_dec_agg<-PIT_BU_selected_dec%>%
                group_by(decile_group)%>%
                summarise(wages_pit=sum(wages_pit,na.rm = TRUE),
                          investment_pit=sum(investment_pit,na.rm = TRUE),
                          business_pit=sum(business_pit,na.rm = TRUE)
                )
              # Rename all columns except the first one by adding "_bu" prefix
              colnames(PIT_BU_selected_dec_agg)[-1] <- paste0("bu_", colnames(PIT_BU_selected_dec_agg)[-1])
              
              
              # SIM ---------------------------------------------------------------------
              
              PIT_SIM_selected_dec<-PIT_BU_selected%>%
                select(decile_group,wages_pit,investment_pit,business_pit)
              
              PIT_SIM_selected_dec_agg<-PIT_BU_selected_dec%>%
                group_by(decile_group)%>%
                summarise(wages_pit=sum(wages_pit,na.rm = TRUE),
                          investment_pit=sum(investment_pit,na.rm = TRUE),
                          business_pit=sum(business_pit,na.rm = TRUE)
                )
              
              
              # Rename all columns except the first one by adding "_bu" prefix
              colnames(PIT_SIM_selected_dec_agg)[-1] <- paste0("sim_", colnames(PIT_SIM_selected_dec_agg)[-1])
              
              
              
              # Step 1: Merge by decile_group
              merged_df <- left_join(PIT_BU_selected_dec_agg, PIT_SIM_selected_dec_agg, by = "decile_group")
              
              # Step 2: Pivot longer
              long_df_pit <- merged_df %>%
                pivot_longer(
                  cols = -decile_group,
                  names_to = c("scenario", "income_type"),
                  names_sep = "_",
                  values_to = "value"
                )
              
              # Optional: Arrange neatly
              long_df_pit <- long_df_pit %>%
                filter(scenario=="sim")%>%
                arrange(decile_group, income_type, scenario)
                
              
              
              # 2. Pie chart ----------------------------------------------------------------------
              
              
              structure_pit<-long_df_pit%>%
               # dplyr::filter(scenario=="bu")%>%
                group_by(income_type)%>%
                summarise(value=sum(value,na.rm = TRUE))
              
              
              