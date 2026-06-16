'Install packages and importing of data
                                          '
'Step 1. Set your local path to the  model'
rm(list = ls())

#path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/Moldova-MSM" ##<---PUT YOU PATH HERE 

# I. Automatic project path detection ---------------------------------------

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable() &&
    nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  
  path1 <- dirname(normalizePath(rstudioapi::getActiveDocumentContext()$path, winslash = "/"))
  
} else {
  
  path1 <- normalizePath(getwd(), winslash = "/")
}

path1

# I.INSTALLING REQUIRED PACKAGES AND SETTING PATH  -------------------------------------------------
'1.Library installation'

#           list.of.packages <- c(
#                                   "shiny",
#                                   "shinydashboard",
#                                   "shinyjs",
#                                   "shinyWidgets",
#                                   "DT",
#                                   "ineq",
#                                   "data.table",
#                                   "readxl",
#                                   "fontawesome",
#                                   "flexdashboard",
#                                   "tidyverse",
#                                   "plyr",
#                                   "shinycssloaders",
#                                   "future",
#                                   "promises",
#                                   "plotly",
#                                   "stringr",
#                                   "reshape2",
#                                   "base64enc",
#                                   "parallel",
#                                   "purrr",
#                                   "tidyr",
#                                   "RColorBrewer",
#                                   "Hmisc",
#                                   "openxlsx",
#                                   "forcats"
#                                 )
# 
# 
#   new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#   if(length(new.packages)) install.packages(new.packages)
# 
# 
# 
# install.packages("https://cran.r-project.org/src/contrib/Archive/IC2/IC2_1.0-1.tar.gz",
#                     repos = NULL, type = "source", method = "wininet")



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


#import_data <- file.path(path2, "FinalMergingVersion3-Max.RData")
import_data <- file.path(path2, "pit_model_workspace_09052026.RData")

# Load the RData file into the global environment
load(import_data)


dt <-pit_dt%>%data.table()

#dt$dup_in_reg<-NULL

dt$ials21_sumven_cur_PLS_WH<-NULL
dt$ials21_sumven_cur_RCSA_WH<-NULL
dt$ials21_sumven_cur_DON_WH<-NULL

#   ials21_sumven_cur_PLS_WH
# ials21_sumven_cur_RCSA_WH
# ials21_sumven_cur_DON_WH

dt <- unique(dt, by = "cod_fiscal")

# Dataset is without duplicates and each taxpayer is represent only in one row 
# length(unique(dt$cod_fiscal))
# NROW(dt)


# 1.Column names for sub-setting ---------------------------------------------

# 1.1 Total gross income
total_income_cols <- c(
  "ai_17_r1c2",
  "cet18_c5c3",
  "cet18_h7c3",
  "daj17_r010",
  "dass19_r010",
  "unif21_t1r010",
  "ven12_r010",
  "ials21_sumven_cur_SAL",
  "ials21_sumven_cur_FOL_WH",
  # "ials21_sumven_cur_PLS_WH",
  "ials21_sumven_cur_PL_WH",
  "ials21_sumven_cur_ROY_WH",
  "ials21_sumven_cur_DONPF_WH",
  "ials21_sumven_cur_DON_P_WH",
  #"ials21_sumven_cur_RCSA_WH",
  "ials21_sumven_cur_DOBBA_WH",
  "ials21_sumven_cur_DOB_WH",
  "ials21_sumven_cur_VMS_WH",
  #"ials21_sumven_cur_DON_WH",
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
  "ials21_sumven_cur_SAL",
  "cet18_c1c3",
  "cet18_h1c3"
)


# Investment
income_investment_cols  <- c(
  "ials21_sumven_cur_FOL_WH",
  #"ials21_sumven_cur_PLS_WH",
  "ials21_sumven_cur_PL_WH",
  "ials21_sumven_cur_ROY_WH",
  "ials21_sumven_cur_DONPF_WH",
  "ials21_sumven_cur_DON_P_WH",
  #"ials21_sumven_cur_RCSA_WH",
  "ials21_sumven_cur_DOBBA_WH",
  "ials21_sumven_cur_DOB_WH",
  "ials21_sumven_cur_VMS_WH",
  #"ials21_sumven_cur_DON_WH",
  "ials21_sumven_cur_LIV_WH",
  "ials21_sumven_cur_NOR_WH",
  "ials21_sumven_cur_CSM_WH",
  "ials21_sumven_cur_AGRAC_WH",
  "ials21_sumven_cur_SER_WH",
  "ials21_sumven_cur_PLT_WH",
  "ials21_sumven_cur_DIVA_WH",
  "cet18_c2c3",
  "cet18_c311c3",
  "cet18_c321c3",
  "cet18_c322c3",
  "cet18_c4c3",
  "cet18_h2c3",
  "cet18_h3c3",
  "cet18_h4c3",
  "cet18_h5c3",
  "cet18_h6c3",
  "cet18_h7c3"
  
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
  #"ials21_sumven_cur_PLS_WH",
  "ials21_sumven_cur_PL_WH",
  "ials21_sumven_cur_ROY_WH",
  "ials21_sumven_cur_DONPF_WH",
  "ials21_sumven_cur_DON_P_WH",
  #"ials21_sumven_cur_RCSA_WH",
  "ials21_sumven_cur_DOBBA_WH",
  "ials21_sumven_cur_DOB_WH",
  "ials21_sumven_cur_VMS_WH",
  #"ials21_sumven_cur_DON_WH",
  "ials21_sumven_cur_LIV_WH",
  "ials21_sumven_cur_NOR_WH",
  "ials21_sumven_cur_CSM_WH",
  "ials21_sumven_cur_AGRAC_WH",
  "ials21_sumven_cur_SER_WH",
  "ials21_sumven_cur_PLT_WH",
  "ials21_sumven_cur_DIVA_WH",
  "ials21_sumimp_cur_TAXI_WH",
  "cet18_c2c3",
  "cet18_c311c3",
  "cet18_c321c3",
  "cet18_c322c3",
  "cet18_c4c3",
  "cet18_h2c3",
  "cet18_h3c3",
  "cet18_h4c3",
  "cet18_h5c3",
  "cet18_h6c3",
  "cet18_h7c3"
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






# 2.Preparation of subsets --------------------------------------------------

# Create frequency table sorted descending
tax_table <- sort(table(dt$tax_regime), decreasing = TRUE)

# Convert to data frame for pretty printing
tax_df <- as.data.frame(tax_table)
colnames(tax_df) <- c("Tax Regime", "Count")




# 2.1 Sub-setting prep -------------------------------------------------------

# Priority regimes:
# 1. Business / special regimes
# 2. CET-18
# 3. IALS21 with salary
# 4. Remaining IALS21


# --- 0) priority group: business / special regimes -------------------------

business_regimes <- c(
                        "ai_17",
                        "ven12",
                        "daj17",
                        "dass19",
                        "taxi18",
                        "unif21"
                      )

subset_business <- dt %>%
  filter(tax_regime %in% business_regimes)

business_ids <- unique(subset_business$cod_fiscal)


# --- 1) second priority group: CET-18, excluding business taxpayers ---------

subset_cet18 <- dt %>%
  filter(
    tax_regime == "cet18",
    !cod_fiscal %in% business_ids
  )

cet18_ids <- unique(subset_cet18$cod_fiscal)


# --- 2) IALS21 with salary, excluding business and CET-18 taxpayers ---------

priority_ids <- unique(c(business_ids, cet18_ids))

subset1 <- dt %>%
  filter(
    tax_regime == "ials21",
    ials21_sumven_cur_SAL > 0,
    !cod_fiscal %in% priority_ids
  )

sum(subset1$ials21_sumimp_cur_SAL, na.rm = TRUE)

subset1_ids <- unique(subset1$cod_fiscal)


# --- 3) remaining IALS21, excluding all higher-priority taxpayers -----------

assigned_ids <- unique(c(priority_ids, subset1_ids))

subset2 <- dt %>%
  filter(
    tax_regime == "ials21",
    !cod_fiscal %in% assigned_ids
  )


# --- 4) everything else -----------------------------------------------------

assigned_ids <- unique(c(
  business_ids,
  cet18_ids,
  subset1_ids,
  unique(subset2$cod_fiscal)
))

subset3 <- dt %>%
  filter(!cod_fiscal %in% c(subset1_ids, unique(subset2$cod_fiscal)))


# Count number of rows by tax regime in each subset --------------------------

subset1_dt <- as.data.table(subset1)
subset2_dt <- as.data.table(subset2)
subset3_dt <- as.data.table(subset3)

regime_count_all_subsets <- rbindlist(
  list(
    subset1_dt[, .(n_rows = .N), by = tax_regime][, subset := "subset1_wages"],
    subset2_dt[, .(n_rows = .N), by = tax_regime][, subset := "subset2_investment"],
    subset3_dt[, .(n_rows = .N), by = tax_regime][, subset := "subset3_business"]
  ),
  use.names = TRUE,
  fill = TRUE
)

setcolorder(regime_count_all_subsets, c("subset", "tax_regime", "n_rows"))
setorder(regime_count_all_subsets, subset, tax_regime)

regime_count_all_subsets





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

# # testing
# growth_factors <- read_csv("Data/PIT/growth_factors.csv")

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
rm(subset1_dt,subset2_dt,subset3_dt)

gc(TRUE)             




setwd(path1)



save.image(file=".RData") 


