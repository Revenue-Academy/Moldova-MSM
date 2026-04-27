
dt

# For deciles
dt[, decile := cut(total_income,
                   breaks = quantile(total_income, probs = seq(0, 1, 0.1), na.rm = TRUE),
                   include.lowest = TRUE, labels = FALSE)]

# For centiles
dt[, centile := cut(total_income,
                    breaks = quantile(total_income, probs = seq(0, 1, 0.01), na.rm = TRUE),
                    include.lowest = TRUE, labels = FALSE)]


dt[, centile := ceiling(100 * frank(total_income, ties.method = "average") / .N)]


### HEAT MAP

# ===============================================================
# 0.  PACKAGES ---------------------------------------------------
# ===============================================================
library(data.table)   # fast aggregation / reshape
library(ggplot2)      # plotting
library(viridis)      # colour-blind-friendly palette
library(scales)       # percent_format()

# ===============================================================
# 1.  ENSURE dt IS A MODIFIABLE data.table -----------------------
# ===============================================================
dt <- as.data.table(copy(dt))   # keeps name "dt", breaks any locks

# ===============================================================
# 2.  VARIABLE LIST ---------------------------------------------
# ===============================================================
vars <- c(
  "ai_17_r1c2", "cet18_c5c3", "daj17_r010", "dass19_r010",
  "unif21_t1r010", "ven12_r010",
  "ials21_sumven_cur_SAL",  "ials21_sumven_cur_FOL_WH",
  "ials21_sumven_cur_PLS_WH","ials21_sumven_cur_PL_WH",
  "ials21_sumven_cur_ROY_WH","ials21_sumven_cur_DONPF_WH",
  "ials21_sumven_cur_DON_P_WH","ials21_sumven_cur_RCSA_WH",
  "ials21_sumven_cur_DOBBA_WH","ials21_sumven_cur_DOB_WH",
  "ials21_sumven_cur_VMS_WH","ials21_sumven_cur_DON_WH",
  "ials21_sumven_cur_LIV_WH","ials21_sumven_cur_NOR_WH",
  "ials21_sumven_cur_CSM_WH","ials21_sumven_cur_AGRAC_WH",
  "ials21_sumven_cur_SER_WH","ials21_sumven_cur_PLT_WH",
  "ials21_sumven_cur_DIVA_WH"
)

# ===============================================================
# 3.  SLICE TO NEEDED COLUMNS ------------------------------------
# ===============================================================
dt <- dt[ , c("decile", vars), with = FALSE ]

# ===============================================================
# 4.  AGGREGATE TOTALS BY DECILE ---------------------------------
# ===============================================================
agg_dt <- dt[ , lapply(.SD, sum, na.rm = TRUE),
              by      = decile,
              .SDcols = vars]

# ===============================================================
# 5.  LONG FORMAT & SAFE SHARE CALC ------------------------------
#      • If a decile’s grand total is 0 → every share = 0
#      • No NA values produced
# ===============================================================
heat_dt <- melt(
  agg_dt,
  id.vars        = "decile",
  variable.name  = "Variable",
  value.name     = "Total",
  variable.factor = FALSE
)

heat_dt[ , Share := {
  col_sum <- sum(Total, na.rm = TRUE)
  if (col_sum == 0) 0 else Total / col_sum
}, by = decile]

# Order factors for plotting
heat_dt[ , decile   := factor(decile, levels = 1:10) ]
heat_dt[ , Variable := factor(Variable, levels = rev(vars)) ]

# ===============================================================
# 6.  PLOT — COMPOSITION HEAT-MAP (Viridis "D") ------------------
# ===============================================================
ggplot(heat_dt, aes(decile, Variable, fill = Share)) +
  geom_tile(colour = "white") +
  scale_fill_viridis_c(
    option    = "D",          # no yellow; light → dark teal/navy
    direction = 1,
    limits    = c(0, 1),
    labels    = percent_format(accuracy = 1),
    name      = "Share of\ndecile total",
    na.value  = "lightgrey"   # if any NA sneak in, show light grey
  ) +
  labs(
    title = "Income Composition by Decile",
    x     = "Decile groups",
    y     = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid   = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.ticks.y = element_blank()
  )

