'Install packages and importing of data
                                          '
'Step 1. Set your local path to the  model'
rm(list = ls())

path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/Moldova-MSM" ##<---PUT YOU PATH HERE 

# I.INSTALLING REQUIRED PACKAGES AND SETTING PATH  -------------------------------------------------
          '1.Library installation'

                  list.of.packages <- c(
                                          "shiny",
                                          "shinydashboard",
                                          "shinyjs",
                                          "shinyWidgets",
                                          "DT",
                                          "ineq",
                                          "data.table",
                                          "readxl",
                                          "fontawesome",
                                          "flexdashboard",
                                          "tidyverse",
                                          "plyr",
                                          "shinycssloaders",
                                          "future",
                                          "promises",
                                          "plotly",
                                          "stringr",
                                          "reshape2",
                                          "base64enc",
                                          "parallel",
                                          "purrr",
                                          "tidyr",
                                          "RColorBrewer",
                                          "Hmisc",
                                          "openxlsx",
                                          "forcats"
                                        )


          new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
          if(length(new.packages)) install.packages(new.packages)



        install.packages("https://cran.r-project.org/src/contrib/Archive/IC2/IC2_1.0-1.tar.gz",
                            repos = NULL, type = "source", method = "wininet")


          
        library(tidyverse)
        library(readxl)
        library(reshape2)
        library(data.table)
        library(plyr)

# II.IMPORT DATA -----------------------------------------------------------------

        path2 <- paste0(path1, "/Data/PIT")
        setwd(path2)
        #getwd()
        
        
        options(scipen = 999)
        
        
        import_data <- file.path(path2, "FinalMergingVersion3-Max.RData")
        
        # Load the RData file into the global environment
        load(import_data)
        
 
        dt <-pit_dt%>%data.table()
        
        dt$dup_in_reg<-NULL
        
     

      # 1.Column names for sub-setting ---------------------------------------------

        # 1.1 Total gross income
        total_income_cols <- c(
                                "ai_17_r1c2",
                                "cet18_c5c3",
                                "daj17_r010",
                                "dass19_r010",
                                "unif21_t1r010",
                                "ven12_r010",
                                "ials21_sumven_cur_SAL",
                                "ials21_sumven_cur_FOL_WH",
                                "ials21_sumven_cur_PLS_WH",
                                "ials21_sumven_cur_PL_WH",
                                "ials21_sumven_cur_ROY_WH",
                                "ials21_sumven_cur_DONPF_WH",
                                "ials21_sumven_cur_DON_P_WH",
                                "ials21_sumven_cur_RCSA_WH",
                                "ials21_sumven_cur_DOBBA_WH",
                                "ials21_sumven_cur_DOB_WH",
                                "ials21_sumven_cur_VMS_WH",
                                "ials21_sumven_cur_DON_WH",
                                "ials21_sumven_cur_LIV_WH",
                                "ials21_sumven_cur_NOR_WH",
                                "ials21_sumven_cur_CSM_WH",
                                "ials21_sumven_cur_AGRAC_WH",
                                "ials21_sumven_cur_SER_WH",
                                "ials21_sumven_cur_PLT_WH",
                                "ials21_sumven_cur_DIVA_WH"
                              )

        # 1.2 Gross income wages
        income_wage_cols <- c(
                              "ials21_sumven_cur_SAL"
                                )

       
        # Investment
        income_investment_cols  <- c(
                                     "ials21_sumven_cur_FOL_WH",
                                     "ials21_sumven_cur_PLS_WH",
                                      "ials21_sumven_cur_PL_WH",
                                      "ials21_sumven_cur_ROY_WH",
                                      "ials21_sumven_cur_DONPF_WH",
                                      "ials21_sumven_cur_DON_P_WH",
                                      "ials21_sumven_cur_RCSA_WH",
                                      "ials21_sumven_cur_DOBBA_WH",
                                      "ials21_sumven_cur_DOB_WH",
                                      "ials21_sumven_cur_VMS_WH",
                                      "ials21_sumven_cur_DON_WH",
                                      "ials21_sumven_cur_LIV_WH",
                                      "ials21_sumven_cur_NOR_WH",
                                      "ials21_sumven_cur_CSM_WH",
                                      "ials21_sumven_cur_AGRAC_WH",
                                      "ials21_sumven_cur_SER_WH",
                                      "ials21_sumven_cur_PLT_WH",
                                      "ials21_sumven_cur_DIVA_WH"
                            )


        # 1.3 Business
        income_business_cols  <- c(
                                  "ai_17_r1c2",
                                  "cet18_c5c3",
                                  "daj17_r010",
                                  "dass19_r010",
                                  "unif21_t1r010",
                                  "ven12_r010"
                                )


        # 1.5 Base for progression ------------------------------------------------

        
        income_investment_progression_cols  <- c(
                                                "ials21_sumven_cur_FOL_WH",
                                                "ials21_sumven_cur_PLS_WH",
                                                "ials21_sumven_cur_PL_WH",
                                                "ials21_sumven_cur_ROY_WH",
                                                "ials21_sumven_cur_DONPF_WH",
                                                "ials21_sumven_cur_DON_P_WH",
                                                "ials21_sumven_cur_RCSA_WH",
                                                "ials21_sumven_cur_DOBBA_WH",
                                                "ials21_sumven_cur_DOB_WH",
                                                "ials21_sumven_cur_VMS_WH",
                                                "ials21_sumven_cur_DON_WH",
                                                "ials21_sumven_cur_LIV_WH",
                                                "ials21_sumven_cur_NOR_WH",
                                                "ials21_sumven_cur_CSM_WH",
                                                "ials21_sumven_cur_AGRAC_WH",
                                                "ials21_sumven_cur_SER_WH",
                                                "ials21_sumven_cur_PLT_WH",
                                                "ials21_sumven_cur_DIVA_WH",
                                                "ials21_sumimp_cur_TAXI_WH"
                                              )
                    
        
        
      # 1.4 Introducing of column for gross income ----------------------------------
        
        # total gross income per taxpayer 
        dt[, total_income := rowSums(.SD, na.rm = TRUE),
               .SDcols = total_income_cols]
        
        
        # total income_investment_progression_cols
        dt[, inv_base_prog := rowSums(.SD, na.rm = TRUE),
           .SDcols = income_investment_progression_cols]
        
        
        #  Base for Business
        dt[, bus_base_prog := rowSums(.SD, na.rm = TRUE),
           .SDcols = income_business_cols]
        
        
        
        
        # Dataset is without duplicates and each taxpayer is represent only in one row 
        # length(unique(dt$cod_fiscal))
        # NROW(dt)

      # 2.Preparation of subsets --------------------------------------------------

        # Create frequency table sorted descending
        tax_table <- sort(table(dt$tax_regime), decreasing = TRUE)
        
        # Convert to data frame for pretty printing
        tax_df <- as.data.frame(tax_table)
        colnames(tax_df) <- c("Tax Regime", "Count")
        
        # 1316445 unique taxpayers
        
        #  1316445/3- 438815 optimum number of dataset
        
        

              # 2.1 Sub-setting prep ----------------------------------------------------------
                        # --- 1) rows with ials21 + SAL > 0 ------------------------------------------
                        subset1 <- dt %>% 
                          filter(tax_regime == "ials21", ials21_sumven_cur_SAL > 0)
                        
                        sum(subset1$ials21_sumimp_cur_SAL)
                        
                        # --- 2) remaining rows with ials21 ------------------------------------------
                        subset2 <- dt %>% 
                          filter(tax_regime == "ials21") %>%              # still ials21 .
                          anti_join(subset1, by = "cod_fiscal")           # . but not already in subset1
                        
                
                                # test<-subset2%>%
                        #   filter(cod_fiscal=="1142136036085")
                          
                        
        
                        # --- 3) everything else ------------------------------------------------------
                        subset3 <- dt %>% 
                          anti_join(bind_rows(subset1, subset2), by = "cod_fiscal")
                        
                        # 577847+695829+42769=1316445
      
                # 2.2. Subset 1 ----------------------------------------------------------

                        
        dt1<-subset1
        dt1 <- dt1 %>% select(where(~ !all(is.na(.))))
        

    #  Weights prep

                          
                          n <- NROW(dt1)
                          
                          weights_pit1 <- data.table(
                                                    t0 = rep(1, n),
                                                    t1 = rep(1, n),
                                                    t2 = rep(1, n),
                                                    t3 = rep(1, n),
                                                    t4 = rep(1, n)
                                                    
                                                  )
                          rm(n)
                          

    dt1$Year<-2023
    dt1[is.na(dt1)] <- 0
    

                # 2.3. Subset 2 ----------------------------------------------------------------
                
                
                    dt2<-subset2    
                    
                    
                    dt2 <- dt2 %>% select(where(~ !all(is.na(.))))
                    
                    
                    
                    
                    # 1.Weights
                    
                    
                    n <- NROW(dt2)
                    
                    weights_pit2 <- data.table(
                      t0 = rep(1, n),
                      t1 = rep(1, n),
                      t2 = rep(1, n),
                      t3 = rep(1, n),
                      t4 = rep(1, n)
                      
                    )
                    rm(n)
                    
                    
                    dt2$Year<-2023
                    dt2[is.na(dt2)] <- 0
                    
                
                # 2.4. Subset 3 ----------------------------------------------------------------
                
                    dt3<-subset3    
                    
                    
                    dt3 <- dt3 %>% select(where(~ !all(is.na(.))))
                    
                    
                    
                    
                    # 1.Weights
                    
                    
                    n <- NROW(dt3)
                    
                    weights_pit3 <- data.table(
                      t0 = rep(1, n),
                      t1 = rep(1, n),
                      t2 = rep(1, n),
                      t3 = rep(1, n),
                      t4 = rep(1, n)
                      
                    )
                    rm(n)
                    
                    
                    dt3$Year<-2023
                    dt3[is.na(dt3)] <- 0
                    
                
                    
                    
            
                      
      # 3.Import other files ------------------------------------------------------
    MACRO_FISCAL_INDICATORS<-read_excel("macro_indicators.xlsx")
    
    # 2.Growth Factors & Scenario Mapping
    
    growth_factors <- read_csv("growth_factors.csv")%>%data.table()
    
    
    
    # NACE NAMES
    df_nace_names<-structure(list(section = c("A", "B", "C", "D", "E", "F", "G", 
                                              "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", 
                                              "U", "Other"), description = c("Agriculture, forestry and fishing", 
                                                                             "Mining and quarrying", "Manufacturing", "Electricity, gas, steam and air conditioning supply", 
                                                                             "Water supply; sewerage; waste managment and remediation activities", 
                                                                             "Construction", "Wholesale and retail trade; repair of motor vehicles and motorcycles", 
                                                                             "Transporting and storage", "Accommodation and food service activities", 
                                                                             "Information and communication", "Financial and insurance activities", 
                                                                             "Real estate activities", "Professional, scientific and technical activities", 
                                                                             "Administrative and support service activities", "Public administration and defence; compulsory social security", 
                                                                             "Education", "Human health and social work activities", "Arts, entertainment and recreation", 
                                                                             "Other services activities", "Activities of households as employers; undifferentiated goods - and services - producing activities of households for own use", 
                                                                             "Activities of extraterritorial organisations and bodies", "Other"
                                              )), row.names = c(NA, -22L), class = c("tbl_df", "tbl", "data.frame"
                                              ))
    
    
    
    
# III. SAVE DATA IN R ENVIRONMENT (RDS FILE) --------------------------------------------------------
   
    rm(subset1,subset2,subset3)
    rm(pit_data)
    rm(pit_dt)
    rm(dt)
    rm(tax_df)
    
    gc(TRUE)             
    
    
   
    
  setwd(path1)
                  
                
                  
save.image(file=".RData") 
          
                  
