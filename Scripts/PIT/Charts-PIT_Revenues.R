library(plotly)

Revenue_Charts <- function(merged_PIT_BU_SIM, forecast_horizon) {
  
  # Chart 1. Comparison of PIT Revenues -----------------------------------------------------------------
  PIT_RevenuesTotal_plt <- plot_ly(
                                    merged_PIT_BU_SIM,
                                    x = ~year,
                                    y = ~pitax_bu*1e03,
                                    name = "Baseline",
                                    type = 'scatter',
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid")
                                    ) %>%
                                        add_trace(
                                          x = ~year,
                                          y = ~pitax_sim*1e03,
                                          name = 'Simulation',
                                          line = list(width = 4, dash = "dot")
                                      ) %>%
                              layout(
                                title = paste("Total PIT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
                                              xaxis = list(title = '', tickformat = 'd'),
                                              yaxis = list(title = ' ', rangemode = 'tozero'),
                                              annotations = list(
                                                x = -0.02,
                                                y = -0.1,
                                                text = "Source: WB staff estimation",
                                                showarrow = FALSE,
                                                xref = 'paper',
                                                yref = 'paper',
                                                align = 'left'
                                )
                              )
  
 
  
  
  # Chart 2. Comparison of PIT Revenues from Capital  ------------------------- 
  # PIT_RevenuesCapital_plt <- plot_ly(
  #                                   merged_PIT_BU_SIM,
  #                                   x = ~year,
  #                                   y = ~pit_c_bu*1e06,
  #                                   name = "Baseline",
  #                                   type = 'scatter',
  #                                   mode = 'lines',
  #                                   line = list(width = 4, dash = "solid")
  #                                 ) %>%
  #                                   add_trace(
  #                                     x = ~year,
  #                                     y = ~pit_c_sim*1e06,
  #                                     name = 'Simulation',
  #                                     line = list(width = 4, dash = "dot")
  #                                   ) %>%
  #                                   layout(
  #                                     title = paste("PIT Revenues Capital,", min(forecast_horizon), "-", max(forecast_horizon)),
  #                                     xaxis = list(title = '', tickformat = 'd'),
  #                                     yaxis = list(title = ' ', rangemode = 'tozero'),
  #                                     annotations = list(
  #                                       x = -0.02,
  #                                       y = -0.1,
  #                                       text = "Source: WB staff estimation",
  #                                       showarrow = FALSE,
  #                                       xref = 'paper',
  #                                       yref = 'paper',
  #                                       align = 'left'
  #                                     )
  #                                   )
  
  PIT_RevenuesCapital_plt=PIT_RevenuesTotal_plt

  # Chart 3. Comparison of PIT Revenues from Labor  ------------------------- 
  
  # PIT_RevenuesLabor_plt <- plot_ly(
  #   merged_PIT_BU_SIM,
  #   x = ~year,
  #   y = ~(pit_w_bu*1e06),
  #   name = "Baseline",
  #   type = 'scatter',
  #   mode = 'lines',
  #   line = list(width = 4, dash = "solid")
  # ) %>%
  #   add_trace(
  #     x = ~year,
  #     y = ~(pit_w_sim*1e06),
  #     name = 'Simulation',
  #     line = list(width = 4, dash = "dot")
  #   ) %>%
  #   layout(
  #     title = paste("PIT Revenues Labor,", min(forecast_horizon), "-", max(forecast_horizon)),
  #     xaxis = list(title = '', tickformat = 'd'),
  #     yaxis = list(title = ' ', rangemode = 'tozero'),
  #     annotations = list(
  #       x = -0.02,
  #       y = -0.1,
  #       text = "Source: WB staff estimation",
  #       showarrow = FALSE,
  #       xref = 'paper',
  #       yref = 'paper',
  #       align = 'left'
  #     )
  #   )
  
  PIT_RevenuesLabor_plt=PIT_RevenuesTotal_plt
  
  
  

  
  # Chart 4. Comparison of PIT Revenues from Wages  ------------------------- 
  
  # PIT_RevenuesWages_plt <- plot_ly(
  #   merged_PIT_BU_SIM,
  #   x = ~year,
  #   y = ~pit_w_bu*1e06,
  #   name = "Baseline",
  #   type = 'scatter',
  #   mode = 'lines',
  #   line = list(width = 4, dash = "solid")
  # ) %>%
  #   add_trace(
  #     x = ~year,
  #     y = ~pit_w_sim*1e06,
  #     name = 'Simulation',
  #     line = list(width = 4, dash = "dot")
  #   ) %>%
  #   layout(
  #     title = paste("PIT Revenues Wages,", min(forecast_horizon), "-", max(forecast_horizon)),
  #     xaxis = list(title = '', tickformat = 'd'),
  #     yaxis = list(title = ' ', rangemode = 'tozero'),
  #     annotations = list(
  #       x = -0.02,
  #       y = -0.1,
  #       text = "Source: WB staff estimation",
  #       showarrow = FALSE,
  #       xref = 'paper',
  #       yref = 'paper',
  #       align = 'left'
  #     )
  #   )
  
  
  # df_barchart<-merged_PIT_BU_SIM%>%
  #   select(year,pit_w_sim,pit_c_sim)%>%
  #   filter(SimulationYear<=year)
  # 
  # 
  # # library(tidyr)
  # # library(dplyr)
  # 
  # # Assuming df_barchart is your data frame
  # df_long <- df_barchart %>%
  #   pivot_longer(
  #     cols = c(pit_w_sim, pit_c_sim),
  #     names_to = "source of income",
  #     values_to = "value"
  #   ) %>%
  #   mutate(`source of income` = recode(`source of income`, 
  #                                      pit_w_sim = "PIT from labor", 
  #                                      pit_c_sim = "PIT from capital"))
  # 
  # 
  # # Define custom colors for the tax incentive categories
  # custom_colors <- c("PIT from labor" = '#ff7f0e', "PIT from capital" = '#1f77b4')
  # 
  # # Create a stacked bar chart with Plotly and custom colors
  # PIT_RevenuesCombo_plt <- plot_ly(df_long,
  #                                  x = ~year,
  #                                  y = ~round(value*1e06, 1),  # Rounding values in the y-axis
  #                                  color = ~`source of income`,  # Corrected column reference with backticks
  #                                  colors = custom_colors,
  #                                  text = ~round(value, 1),  # Rounded values for the hover text
  #                                  hoverinfo = 'text',
  #                                  type = 'bar',
  #                                  barmode = 'stack',
  #                                  textposition = 'inside',  # Position text inside the bars
  #                                  insidetextfont = list(color = 'white')  # Ensure text is readable inside bars
  # ) %>%
  #   layout(
  #     title = paste("Structure of PIT revenues by Type of Income,", min(forecast_horizon), "-", max(forecast_horizon)),
  #     xaxis = list(title = " "),
  #     yaxis = list(title = " "),
  #     barmode = 'stack',
  #     bargap = 0.6,  # Adjust this value to control the bar width
  #     annotations = list(
  #       x = -0.02,
  #       y = -0.1,
  #       text = "Source: WB staff estimation",
  #       showarrow = FALSE,
  #       xref = 'paper',
  #       yref = 'paper',
  #       align = 'left'
  #     )
  #   )
  # 
  
 
  PIT_RevenuesCombo_plt=PIT_RevenuesTotal_plt

  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    PIT_RevenuesTotal_plt = PIT_RevenuesTotal_plt,
    PIT_RevenuesCapital_plt=PIT_RevenuesCapital_plt,
    PIT_RevenuesLabor_plt=PIT_RevenuesLabor_plt,
    PIT_RevenuesCombo_plt=PIT_RevenuesCombo_plt,
    
    
    # Tables
    merged_PIT_BU_SIM = merged_PIT_BU_SIM
  )
}
