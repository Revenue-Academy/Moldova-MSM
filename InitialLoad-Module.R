'Install packages and importing of data
                                          '



'Step 1. Set your local path to the  model'
rm(list = ls())

path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/WB/Countries/Moldova" ##<---PATH



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
                                          "openxlsx"
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
        
       # load("pit_data.RData")
        
        
        #char_cols <- names(dt)[sapply(dt, is.character)]
        
 
          dt <- read_csv("mda_dataset.csv", col_types = cols(
                          cod_fiscal = col_character(), 
                          tax_regime = col_character(),
                          ai_17_Cod_DDF = col_character(), 
                          ven12_legal_form_cdc = col_character(), 
                          ven12_tp_category = col_character(), 
                          ven12_exemption_idt = col_character(), 
                          ven12_nace = col_character(), 
                          daj17_tp_category = col_character(), 
                          dass19_taxpayer_category = col_character(),
                          ials21_nace = col_character(),
                          unif21_legal_form_cdc = col_character(),
                          unif21_nace = col_character()
                        ))%>%data.table()
       
        
          #char_cols <- names(dt23)[sapply(dt23, is.character)]
        
        
       # write.csv(dt,"dt23.csv")
        #dt<-pit_data$pit_dt%>%data.table()
        
                          # 
            #dt_test1<-read_csv("dt.csv")%>%data.table()
            
                          # dt<-read_csv("mpin_epdd_nace_final.csv")%>%data.table()
                          
                          MACRO_FISCAL_INDICATORS<-read_excel("macro_indicators.xlsx")
                          
                          
                          # 2.Growth Factors & Scenario Mapping
                          dt$Year<-NULL
                          
                          
                          # ## ------------------  done  ------------------
                          # growth_factors<-growth_factors%>%data.table()
                          
                          # base_year   <- 2023
                          # w           <- 1.05
                          # num_cols    <- names(dt)[sapply(dt, is.numeric)]
                          # 
                          # n_periods   <- 5                                 # 
                          # 
                          # growth_factors <- data.table(
                          #   Year     = base_year + seq_len(n_periods),     # 2024 2028
                          #   Scenario = paste0("t", 0:(n_periods - 1))      # t0  t4
                          # )
                          # 
                          # # add the numeric columns and fill them with the weight
                          # growth_factors[, (num_cols) := w]
                          # setcolorder(growth_factors, c("Year", "Scenario", num_cols)) 
                          # 
                          # # set all numeric columns in the t0 row to 1 instead of 1.05
                          # growth_factors[Scenario == "t0", (num_cols) := 1]
                          # 
                          # growth_factors$unif21_a1t1r120c3
                          # 
                          # 
                          # write.csv(growth_factors,"growth_factors.csv")
                          
                          growth_factors <- read_csv("growth_factors.csv")%>%data.table()
                          
                          
                          # 3.Weights

                          
                          n <- NROW(dt)
                          
                          weights_pit <- data.table(
                                                    t0 = rep(1, n),
                                                    t1 = rep(1, n),
                                                    t2 = rep(1, n),
                                                    t3 = rep(1, n),
                                                    t4 = rep(1, n)
                                                    
                                                  )
                          rm(n)
                          
                          
                          
                  
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
    

    # rm(pit_data)
    # rm(numeric_data)
    # rm(new_dt)
    
    
    dt$Year<-2023
    dt[is.na(dt)] <- 0
    
# III. SAVE DATA IN R ENVIRONMENT (RDS FILE) --------------------------------------------------------
   
    gc(TRUE)             
                  setwd(path1)
                  
                
                  
                  save.image(file=".RData") 
          
                  
