'Install packages and importing of data
                                          '
'Step 1. Set your local path to the model directory'

#rm(list = ls())
#path1<-" "# <--------Set your path here
#path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/Moldova-MSM/Scripts/VAT-Gap/VAT"# <--------Set your path here


path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/Moldova-MSM" ##<---PUT YOU PATH HERE 

#C:/Users/wb591157/OneDrive - WBG/Documents/Models/Moldova-MSM/Scripts/VAT-Gap/VAT

'Step 2. Press CTRL+A to select all lines in this script and after that press CTRL+Enter to execute selected lines'

# I. INSTALLING LIBRARIES  -------------------------------------------------

# 
# # # Define the list of required packages
# list.of.packages <- unique(c("shinydashboard",
#                              "DT",
#                              "readxl",
#                              "openxlsx",
#                              "shinyjs",
#                              "plotly",
#                              "ggplot2",
#                              "data.table",
#                              "fontawesome",
#                              "tidyverse",
#                              "countrycode",
#                              "shiny",
#                              "kableExtra",
#                              "stringr",
#                              "reshape2",
#                              "base64enc",
#                              "maps",
#                              "sfo",
#                              "sf",
#                              "circlize",
#                              "flexdashboard",
#                              "rpivotTable",
#                              "sm",
#                              "ks",
#                              "shinyWidgets",
#                              "plyr",
#                              "shinycssloaders",
#                              "future",
#                              "promises",
#                              "parallel",
#                              "purrr",
#                              "tidyr",
#                              "RColorBrewer",
#                              "Hmisc"))
# 
# # Check for missing packages and install them
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
# if (length(new.packages)) install.packages(new.packages)
# 
# # Load all packages
# lapply(list.of.packages, library, character.only = TRUE)
# 
# 
# 
# install.packages("https://cran.r-project.org/src/contrib/Archive/IC2/IC2_1.0-1.tar.gz",
#                  repos = NULL, type = "source", method = "wininet")



# Warning manual installation of rccmisc library





# II. MODULES  -------------------------

library(tidyverse)
library(readxl)
library(countrycode)
library(maps)
library(ggplot2)
library(reshape2)
library(rccmisc) 
library(openxlsx)
library(readxl)
library(stringr)
library(data.table)
library(sfo)
library(sf)

# 1.IMPORT TAXES Module --------------------------------------------------------


    # 3.VAT Module ------------------------------------------------------------

'DATA PREPROCESSING MODULE'
path2 <- paste0(path1, "/Data/VAT/Excel")
setwd(path2)
getwd()

        # 1. DEFINE FUNCTIONS ----
        
        #  The function creates an ntile group vector:
        qgroup = function(numvec, n, na.rm=TRUE){
          qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm)  
          out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
          return(out)
        }
        
        # #  to extract only English names from SUTs
        # trim <- function (x) gsub("^\\s+|\\s+$", "", x) 
        # input_output_matrix_to_long_data <- function(matrix){
        #   
        #   matrix <- matrix %>%
        #     dplyr::filter(...2 != "NA")
        #   
        #   
        #   colnames(matrix) <- matrix[1,]
        #   
        #   data <- matrix[c(-1,-2),c(-1,-2)] %>% as.matrix() %>% melt()
        #   
        #   product_industry_name <- matrix[[2]][c(-1,-2)]
        #   product_industry_code <- matrix[[1]][c(-1,-2)]
        #   industry_code <-  matrix[2,c(-1,-2)] %>% as.character()
        #   
        #   data$Var1 <- rep(product_industry_name, time = length(industry_code))
        #   
        #   data <- data %>% 
        #     dplyr::rename(PRODUCT_INDUSTRY_NAME = Var1,
        #                   INDUSTRY_NAME = Var2)
        #   
        #   
        #   data$PRODUCT_INDUSTRY_CODE <- rep(product_industry_code, time = length(industry_code))
        #   data$INDUSTRY_CODE <- rep(industry_code, each = length(product_industry_code))
        #   
        #   data <- data %>% 
        #     dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value)
        #   
        #   
        #   # Leave the names only in English
        #   data$PRODUCT_INDUSTRY_NAME<-gsub("^.*\\/", "",  data$PRODUCT_INDUSTRY_NAME) %>% trim()
        #   data$INDUSTRY_NAME<-gsub("^.*\\/", "",  data$INDUSTRY_NAME) %>% trim()
        #   
        #   data$value <- as.numeric(as.character(data$value))
        #   
        #   return(data)
        #   
        # }


# new

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

trim <- function(x) gsub("^\\s+|\\s+$", "", x)

input_output_matrix_to_long_data <- function(file_path, sheet_name) {
  
  raw <- read_excel(file_path, sheet = sheet_name, col_names = FALSE)
  
  code_row <- which(
    as.character(raw[[1]]) == "Code" &
      as.character(raw[[2]]) == "Description"
  )[1]
  
  if (is.na(code_row)) {
    stop("Could not find the row containing 'Code' and 'Description'.")
  }
  
  industry_name_row <- code_row - 1
  
  if (industry_name_row < 1) {
    stop("Invalid structure: no row above 'Code'/'Description'.")
  }
  
  # industry names and codes from columns 3+
  industry_names <- raw[industry_name_row, -(1:2)] %>%
    unlist(use.names = FALSE) %>%
    as.character()
  
  industry_codes <- raw[code_row, -(1:2)] %>%
    unlist(use.names = FALSE) %>%
    as.character()
  
  temp_value_cols <- paste0("col_", seq_along(industry_codes))
  
  industry_lookup <- data.frame(
    temp_col = temp_value_cols,
    INDUSTRY_CODE = industry_codes,
    INDUSTRY_NAME = industry_names,
    stringsAsFactors = FALSE
  ) %>%
    filter(!(is.na(INDUSTRY_CODE) & is.na(INDUSTRY_NAME))) %>%
    filter(!(trim(dplyr::coalesce(INDUSTRY_CODE, "")) == "" &
               trim(dplyr::coalesce(INDUSTRY_NAME, "")) == ""))
  
  data_wide <- raw[(code_row + 1):nrow(raw), ] %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  colnames(data_wide) <- c("PRODUCT_INDUSTRY_CODE", "PRODUCT_INDUSTRY_NAME", temp_value_cols)
  
  data_wide <- data_wide[!(is.na(data_wide$PRODUCT_INDUSTRY_CODE) & is.na(data_wide$PRODUCT_INDUSTRY_NAME)), ]
  
  data_wide <- data_wide %>%
    select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, all_of(industry_lookup$temp_col))
  
  # force all value columns to character before pivot
  data_wide <- data_wide %>%
    mutate(across(all_of(industry_lookup$temp_col), as.character))
  
  data_long <- data_wide %>%
    pivot_longer(
      cols = all_of(industry_lookup$temp_col),
      names_to = "temp_col",
      values_to = "value"
    ) %>%
    left_join(industry_lookup, by = "temp_col") %>%
    mutate(
      PRODUCT_INDUSTRY_CODE = as.character(PRODUCT_INDUSTRY_CODE),
      PRODUCT_INDUSTRY_NAME = trim(gsub("^.*\\/", "", as.character(PRODUCT_INDUSTRY_NAME))),
      INDUSTRY_CODE = as.character(INDUSTRY_CODE),
      INDUSTRY_NAME = trim(gsub("^.*\\/", "", as.character(INDUSTRY_NAME))),
      value = trim(as.character(value)),
      value = na_if(value, ""),
      value = suppressWarnings(as.numeric(value))
    ) %>%
    select(
      PRODUCT_INDUSTRY_NAME,
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_NAME,
      INDUSTRY_CODE,
      value
    )
  
  return(data_long)
}

        
        # 2. RAW DATA IMPORT AND PREPROCESS  ----- 
        
                # Initialize empty lists to store the tables
                CPA_TAXABLE_PROPORTIONS_BU_list <- list()
                CPA_TAXABLE_PROPORTIONS_SIM_list <- list()
                
                ' 
                                    In this section data are imported from five files:
                                    
                                    VAT_Model_v9.16a2.xlsx
                                    TaxableProportions-4a.xlsx
                                    MACRO_FISCAL_INDICATORS.xlsx
                                    Data4_hbs2020.xlsx  <---HBS DATA
                                    NACE_SUT_table.xlsx
                                    '
                
                # Name of the version of model
                version_vat_model<-c("VAT_Model_SUT_v5_v1.xlsx")
                
                # Taxable proportions
                taxable_proportions_raw <- read_excel("VAT_Parameters.xlsx")
        
        # 2.1 SUTs ------------------------------------
        
                ### do ovde !!! od ovde da se prodolzi !!!!
                
                # ima problem so citanjeto !! moze da e vidi da se smeni funkcijata !!!!!!!
                
                
               
                
                
                    # SUPPLY_raw <- read_excel(version_vat_model, sheet = "Supply", col_names = F)[c(-1,-2),] %>%
                    #   input_output_matrix_to_long_data()
                
                
                
                SUPPLY_raw <- input_output_matrix_to_long_data(
                  file_path = version_vat_model,
                  sheet_name = "Supply"
                )
                    
                
                
                    "Each value from Use_Purchaser are imported here"
                    # USE_PURCHASER_raw <- read_excel(version_vat_model, sheet = "Use_Purchaser", col_names = F)[c(-1,-2,-3,-4),] %>%
                    #   input_output_matrix_to_long_data()
                    
                    
                    USE_PURCHASER_raw <- input_output_matrix_to_long_data(
                      file_path = version_vat_model,
                      sheet_name = "Use_Purchaser"
                    )
                    
                    # 
                    # USE_VAT_raw <- read_excel(version_vat_model, sheet = "Use_VAT", col_names = F)[c(-1,-2,-3,-4),] %>%
                    #   input_output_matrix_to_long_data()
                    
                    USE_VAT_raw <- input_output_matrix_to_long_data(
                      file_path = version_vat_model,
                      sheet_name = "Use_VAT"
                    )
                    
                    
                    
                    
                    # USE_BASIC_raw <- read_excel(version_vat_model, sheet = "Use_Basic", col_names = F)[c(-1,-2,-3,-4),] %>%
                    #   input_output_matrix_to_long_data()
        
                    # Waerning !
                    
                    USE_BASIC_raw <- input_output_matrix_to_long_data(
                      file_path = version_vat_model,
                      sheet_name = "Use_Purchaser"
                    )
                    
                    
        
        # 2.2 COICOP table ------------------------------------------------------------
        
                
                base_year_VAT<-2024 # <-This is the same year as the year from which the data originates.
                
                max_time_horizon<-base_year_VAT+5
                
                time_horizon<-seq(base_year_VAT,max_time_horizon)
        
        
        # 2.3 MACRO-FISCAL INDICATORS ---------------------------------------------
                MacroFiscalData<-read_excel("macro_data.xlsx")
                
                
        MACRO_FISCAL_INDICATORS <-  MacroFiscalData%>%
          dplyr::select(Year,nominal_gdp)%>%
          dplyr::rename("Nominal_GDP"="nominal_gdp")
        
        
        MACRO_FISCAL_INDICATORS$Year<-as.numeric(MACRO_FISCAL_INDICATORS$Year)
        
        
        FinalConsumption <- MacroFiscalData%>%
          dplyr::select(Year)
        
        
        # 2.4 INSERT TAXABLE PROPORTIONS SIMULATION PARAMETERS ---------------------------------------------

        
        'OVDE DA SE VIDI DALI TREBA DA SE INTERVENIRA ???? '
        
            taxable_proportion_bu <- taxable_proportions_raw %>%
              dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(ProportionExempted), Current_Policy_Exempt, ProportionExempted),
                            Simulated_Policy_Reduced_Rate = ifelse(is.na(PreferentialVATRate_1), Current_Policy_Reduced_Rate, PreferentialVATRate_1),
                            Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
            
            
            CPA_TAXABLE_PROPORTIONS_BU<-taxable_proportions_raw
            growfactors_vat <-read_csv("growfactors_vat.csv")
            
            
            
            # Create an empty data.table with the specified structure
            forecast_combined_agg_tbl_wide <- data.table(
              year = numeric(),
              `Current Law (In EUR thousand)` = numeric(),
              `Simulation (In EUR thousand)` = numeric(),
              `Fiscal Impact (In EUR thousand)` = numeric(),
              `Current Law (Pct of GDP)` = numeric(),
              `Simulation (Pct of GDP)` = numeric(),
              `Fiscal Impact (Pct of GDP)` = numeric()
            )




    
        
        # 3.Weights ----------------------------------------------------------------------
        
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
        
    
             
          
# III. SAVING DATA IN R ENVIROMENT (RDS FILE)--------------------------------

setwd(path1)
getwd()

gc(TRUE)


#save.image(file=".RData") 


