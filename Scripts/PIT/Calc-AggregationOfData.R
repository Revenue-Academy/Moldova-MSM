# I.Aggregation of revenues -----------------------------------------------


# 1. Combine all 3 datasets into one
combined_df <- bind_rows(pit_summary_df1, pit_summary_df2, pit_summary_df3)

# 2. Group by year and sum all numeric columns
pit_summary_df <- combined_df %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")%>%
  data.table()


pit_summary_df$`Current law (LCU Mil)`<-round(pit_summary_df$`Current law (LCU Mil)`,1)
pit_summary_df$`Simulation (LCU Mil)`<-round(pit_summary_df$`Simulation (LCU Mil)`,1)
pit_summary_df$`Fiscal impact (LCU Mil)`<-round(pit_summary_df$`Fiscal impact (LCU Mil)`,1)
pit_summary_df$`Fiscal impact (Pct of GDP)`<-round(pit_summary_df$`Fiscal impact (Pct of GDP)`,2)



rm(combined_df)



# II. Summary Tables ---------------------------------------------------------------------

# 1. Combine all 3 datasets into one
combined_df <- bind_rows(merged_PIT_BU_SIM1, merged_PIT_BU_SIM2, merged_PIT_BU_SIM3)

# 2. Group by year and sum all numeric columns
merged_PIT_BU_SIM <- combined_df %>%
  group_by(year,scenarios) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")%>%
  data.table()


# III. Preparation for data for calculations of percentiles and deciles of gross income--------


      # 1.BU ----------------------------------------------------------------------
         
                PIT_BU_dt <- rbindlist(
                  list(PIT_BU_list1, PIT_BU_list2, PIT_BU_list3),
                  use.names = TRUE, fill = TRUE
                )%>%
                filter(
                  #gross_income>=0, 
                  gross_income>=2000, 
                  pitax>=0)
                
                
                # 1.  Helper: add decile & centile groups  (unchanged)
               
                calc_decile_percentile_fun <- function(DT,
                                                       inc_col = "gross_income",
                                                       w_col  = "weight") {
                  
                  setDT(DT)                           # make sure it's a data.table
                  DT <- copy(DT)                      # avoid modifying in-place upstream
                  
                  ## keep only positive, non-missing incomes
                  DT <- DT[get(inc_col) > 0 & !is.na(get(inc_col))]
                  
                  DT[, row_id__tmp := .I]             # remember original order
                  setorderv(DT, inc_col)              # sort by income
                  
                  ## cumulative weight (treat NA weight as 0)
                  DT[, w_cumsum__tmp := cumsum(replace(get(w_col), is.na(get(w_col)), 0))]
                  total_w <- DT[.N, w_cumsum__tmp]
                  
                  ## break points
                  decile_breaks  <- total_w * 0:10   / 10    
                  centile_breaks <- total_w * 0:100  / 100   
                  
                  DT[, decile_group  := pmin(findInterval(w_cumsum__tmp, decile_breaks,
                                                          rightmost.closed = TRUE), 10)]
                  DT[, centile_group := pmin(findInterval(w_cumsum__tmp, centile_breaks,
                                                          rightmost.closed = TRUE), 100)]
                  
                  ## restore input order & drop helper cols
                  setorderv(DT, "row_id__tmp")
                  DT[, c("row_id__tmp","w_cumsum__tmp") := NULL][]
                }
                
                
                # 2.  Apply to your single table
               
                PIT_BU_selected <- calc_decile_percentile_fun(PIT_BU_dt)
                          
            
            
            #rm(PIT_BU_list)
      
      
      
      
      # 2.SIM ---------------------------------------------------------------------
      
      
      
                PIT_SIM_dt <- rbindlist(
                  list(PIT_SIM_list1, PIT_SIM_list2, PIT_SIM_list3),
                  use.names = TRUE, fill = TRUE
                )%>%
                filter(
                  #gross_income>=0,
                  gross_income>=2000, 
                  pitax>=0)
      
                
      
                PIT_SIM_selected <- calc_decile_percentile_fun(PIT_SIM_dt)
      
      

rm(PIT_BU_dt,PIT_BU_list1,PIT_BU_list2,PIT_BU_list3,
     PIT_SIM_list1,PIT_SIM_list2,PIT_SIM_list3,
     pit_summary_df1,pit_summary_df2,pit_summary_df3,
    dt_scn_BU,dt_scn_SIM
   )


      