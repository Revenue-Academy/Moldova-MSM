" Data prep Distribution Dashboard "

# Group 1 ( IALS21, CET18, TAXI18)
    # Employment income
    # Capital gains
    # Investment income

# Group 2 (VEN12, UNIF21,DAJI17,DASS19,AI17)
    # Business income 


# I. Labor-capital -----------------------------------------------------

                  columns_gross_income <- c( 
                    "ai_17_r1c2", "cet18_c5c3", "daj17_r010", "dass19_r010", "unif21_t1r010",
                    "ven12_r010", "ials21_sumven_cur_SAL", "ials21_sumven_cur_FOL_WH", "ials21_sumven_cur_PLS_WH",
                    "ials21_sumven_cur_PL_WH", "ials21_sumven_cur_ROY_WH", "ials21_sumven_cur_DONPF_WH", "ials21_sumven_cur_DON_P_WH",
                    "ials21_sumven_cur_RCSA_WH", "ials21_sumven_cur_DOBBA_WH", "ials21_sumven_cur_DOB_WH", "ials21_sumven_cur_VMS_WH",
                    "ials21_sumven_cur_DON_WH", "ials21_sumven_cur_LIV_WH", "ials21_sumven_cur_NOR_WH", "ials21_sumven_cur_CSM_WH",
                    "ials21_sumven_cur_AGRAC_WH", "ials21_sumven_cur_SER_WH", "ials21_sumven_cur_PLT_WH", "ials21_sumven_cur_DIVA_WH","decile_group"
                    
                                              )

                  
                  # Select columns 
                  #selected_gross_desc_tbl <- select(PIT_BU_gross_list$t0, all_of(columns_gross_income))
                  selected_gross_desc_tbl<-PIT_BU_selected
                  
                  
                  # Summarize the data
                  labor_capital <- selected_gross_desc_tbl %>%
                                  #select(decile_group,total_gross_l, total_gross_c) %>%
                                  dplyr::group_by(decile_group) %>%
                                  # dplyr::summarise(labor = sum(total_gross_l, na.rm = TRUE),
                                  #                  capital = sum(total_gross_c, na.rm = TRUE))
                    dplyr::summarise(# Calculate labor income
                      labor = sum(cet18_c5c3+
                                    ials21_sumven_cur_SAL + ials21_sumven_cur_FOL_WH + ials21_sumven_cur_PLS_WH +
                                    ials21_sumven_cur_PL_WH + ials21_sumven_cur_ROY_WH + ials21_sumven_cur_DONPF_WH + ials21_sumven_cur_DON_P_WH +
                                    ials21_sumven_cur_RCSA_WH + ials21_sumven_cur_DOBBA_WH + ials21_sumven_cur_DOB_WH + ials21_sumven_cur_VMS_WH +
                                    ials21_sumven_cur_DON_WH + ials21_sumven_cur_LIV_WH + ials21_sumven_cur_NOR_WH + ials21_sumven_cur_CSM_WH +
                                    ials21_sumven_cur_AGRAC_WH + ials21_sumven_cur_SER_WH + ials21_sumven_cur_PLT_WH + ials21_sumven_cur_DIVA_WH
                      ),
                      
                      # Calculate capital income
                      capital = sum(
                                       ven12_r010+unif21_t1r010+daj17_r010+dass19_r010+ai_17_r1c2
                                      )
                                  )
                    
              
                  # Reshape the data into long format
                  long_labor_capital <- labor_capital %>%
                    gather(key = "gross_income", value = "value", labor, capital)
                  
                  # Reverse the order of the factors for 'gross_income'
                  long_labor_capital$gross_income <- factor(long_labor_capital$gross_income, levels = c("labor", "capital"))
                  
                  

# II. Type of Income Aggregation of data ---------------------------------------------------------------------

types_labor_capital_tbl<-selected_gross_desc_tbl%>%
                      dplyr::mutate(
                                    labor_wages=ials21_sumven_cur_SAL+cet18_c5c3, ## <---double check for cet18_c5c3
                                    investment_income=ials21_sumven_cur_FOL_WH + ials21_sumven_cur_PLS_WH +
                                                      ials21_sumven_cur_PL_WH + ials21_sumven_cur_ROY_WH + ials21_sumven_cur_DONPF_WH + ials21_sumven_cur_DON_P_WH +
                                                      ials21_sumven_cur_RCSA_WH + ials21_sumven_cur_DOBBA_WH + ials21_sumven_cur_DOB_WH + ials21_sumven_cur_VMS_WH +
                                                      ials21_sumven_cur_DON_WH + ials21_sumven_cur_LIV_WH + ials21_sumven_cur_NOR_WH + ials21_sumven_cur_CSM_WH +
                                                      ials21_sumven_cur_AGRAC_WH + ials21_sumven_cur_SER_WH + ials21_sumven_cur_PLT_WH + ials21_sumven_cur_DIVA_WH,
                 
                                    business_income 	=  ven12_r010+unif21_t1r010+daj17_r010+dass19_r010+ai_17_r1c2)%>%
                        select(decile_group,
                               labor_wages,investment_income,business_income)
                      
      labor_capital_type <- types_labor_capital_tbl %>%
                              dplyr::group_by(decile_group) %>%
                              dplyr::summarise(labor_wages = sum(labor_wages, na.rm = TRUE),
                                               investment_income = sum(investment_income, na.rm = TRUE),
                                               business_income = sum(business_income, na.rm = TRUE)
                                       
                                               
                              )

      # Reshape the data into long format
      labor_capital_type <- labor_capital_type %>%
        gather(key = "gross_income", value = "value", labor_wages,investment_income,business_income)
      
      # Reverse the order of the factors for 'gross_income'
      labor_capital_type$gross_income <- factor(labor_capital_type$gross_income, levels = c("labor_wages","investment_income","business_income"))


# III. Treemap GROSS INCOME --------------------------------------------------------------------


long_labor_capital_type <- types_labor_capital_tbl %>%
  dplyr::select(-c(decile_group)) %>%
  dplyr::summarise(labor_wages = sum(labor_wages, na.rm = TRUE),
                   investment_income = sum(investment_income, na.rm = TRUE),
                   business_income = sum(business_income, na.rm = TRUE)
         )
                   
  



# Convert the data to long format
long_labor_capital_type <- long_labor_capital_type %>%
  gather(key = "income_type", value = "value")


long_labor_capital_type$income_type<-factor( long_labor_capital_type$income_type)



long_labor_capital_type$TypeOfIncome <- "In billion LCU"



# IV. Structure of gross income by NACE sections ---------------------------------------------------------------------

# columns_gross_nace_income <- c(
#   "nace_section",
#   "g_total_gross"
# )
# 
# 
# # Create a select statement with all the patterns
# 
# selected_gross_nace_tbl <- select(PIT_BU_list$t0, all_of(columns_gross_nace_income))
# 
# 
# gross_nace_tbl <- selected_gross_nace_tbl %>%
#   dplyr::group_by(nace_section) %>%
#   dplyr::summarise(g_total_gross = sum(g_total_gross, na.rm = TRUE)
#   )
# 
# # # Convert the data to long format
# gross_nace_tbl <- na.omit(gross_nace_tbl)
# gross_nace_tbl<-left_join(gross_nace_tbl,df_nace_names,by=c("nace_section"="section"))
# gross_nace_tbl$nace_section<-factor(gross_nace_tbl$nace_section)
# gross_nace_tbl$TypeOfIncome <- "In billion LCU"

