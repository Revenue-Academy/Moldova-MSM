" Distribution Dashboard "
# I.Function for Dashboard ------------------------------------------------------------------
Distribution_Charts <- function(merged_PIT_BU_SIM,pit_centile_distribution_bu_sim,pit_decile_distribution_bu_sim_raw,
                                pit_result_bins_bu_sub,pit_result_bins_sim_sub,simulation_year) {

  # Chart 1. Centile Groups -----------------------------------------------------------------
       
  dist_centile_groups_plt <- plot_ly(pit_centile_distribution_bu_sim, x = ~centile_group, y = ~etr_bu_chart, name = "Baseline", type = 'scatter', mode = 'lines',
                 line = list(width = 4,dash = "solid"))
            dist_centile_groups_plt <- dist_centile_groups_plt %>% add_trace(y = ~etr_sim_chart, name = "Simulation", line = list(width = 4,dash = "dash"))%>%
                                    layout(
                                      title = paste("Effective Tax Rate by Percentile Groups,", SimulationYear),
                                      xaxis = list(title = 'Percentile'),
                                      yaxis = list(title = ' '),
                                      #legend = list(x = 0.01, y = 0.99),
                                      annotations = list(
                                        list(
                                          x = -0.02,
                                          y = -0.1,
                                          text = "Source: WB staff estimation",
                                          showarrow = FALSE,
                                          xref = 'paper',
                                          yref = 'paper',
                                          align = 'left'
                                        )
                                      )
                                    )
 
            
            dist_centile_groups_plt <- dist_centile_groups_plt %>%
              config(displayModeBar = FALSE)

  
  # Chart 2. Decile Groups-----------------------------------------------------------------
  

            # Define custom colors
            custom_colors <- c('#1f77b4', '#ff7f0e')
            
            # Adapt the Plotly chart
            dist_decile_groups_plt <- plot_ly(
                                              #pit_decile_distribution_bu_sub, 
                                               pit_decile_distribution_bu_sim_raw,
                                              x = ~decile_group, 
                                              y = ~sum_calc_pitax_bu/1e06, 
                                              name = 'Baseline',
                                              marker = list(color = custom_colors[1]),
                                              hoverinfo = 'text+y', 
                                              type = 'bar', 
                                              barmode = 'group') %>%
                                      add_trace(y = ~sum_calc_pitax_sim/1e06, 
                                                name = 'Simulation', 
                                                marker = list(color = custom_colors[2]),
                                                hoverinfo = 'text+y') %>%
                                      layout(title = paste("Tax Revenue by Decile Groups in LCU MIL,", simulation_year),
                                             xaxis = list(title = "Decile", tickmode = 'linear'), 
                                             yaxis = list(title = " "),
                                             annotations = list(
                                               list(
                                                 x = -0.02,
                                                 y = -0.1,
                                                 text = "Source: WB staff estimation",
                                                 showarrow = FALSE,
                                                 xref = 'paper',
                                                 yref = 'paper',
                                                 align = 'left'
                                               )
                                             ))       
            
            dist_decile_groups_plt <- dist_decile_groups_plt %>%
              config(displayModeBar = FALSE)
            

  # Chart 3. Tax Revenue by Bin Groups-BU -------------------------------------------------------------------------

            # pit_bins_bu_sub_plt <- plot_ly(
            #                             pit_result_bins_bu_sub,
            #                             labels = ~bin_group,
            #                             values = ~sum_calc_pitax,
            #                             type = 'pie',
            #                             hole = 0.6,
            #                             textinfo = 'label+percent',
            #                             insidetextorientation = 'radial',
            #                             rotation = 150
            #                           ) %>%
            #                             layout(
            #                               title = paste("Contribution to Tax Revenues by Income Groups (Baseline),", simulation_year),
            #                               showlegend = FALSE,  # Turn off the legend
            #                               margin = list(l = 20, r = 20, t = 50, b = 20),
            #                               annotations = list(
            #                                 x = 0.13,
            #                                 y = 0.0,
            #                                 text = "Source: WB staff estimation",
            #                                 showarrow = FALSE,
            #                                 xref = 'paper',
            #                                 yref = 'paper',
            #                                 xanchor = 'center',
            #                                 yanchor = 'top',
            #                                 font = list(size = 12)
            #                               )
            #                             )
            
            PIT_RevenuesTotal_plt <- plot_ly(
              merged_PIT_BU_SIM,
              x = ~year,
              y = ~pitax_bu*1e06,
              name = "Baseline",
              type = 'scatter',
              mode = 'lines',
              line = list(width = 4, dash = "solid")
            ) %>%
              add_trace(
                x = ~year,
                y = ~pitax_sim*1e06,
                name = 'Simulation',
                line = list(width = 4, dash = "dot")
              ) %>%
              layout(
                title = paste("Total PIT Revenues in LCU ,", min(forecast_horizon), "-", max(forecast_horizon)),
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
            
            
            PIT_RevenuesTotal_plt <- PIT_RevenuesTotal_plt %>%
              config(displayModeBar = FALSE)
            
           
  
  # Chart 4. Tax Revenue by Bin Groups-SIM ---------------------------------------------------------------

            pit_bins_sim_sub_plt <- plot_ly(
                                            pit_result_bins_sim_sub, 
                                            labels = ~bin_group, 
                                            values = ~sum_calc_pitax, 
                                            type = 'pie', 
                                            hole = 0.6,  
                                            textinfo = 'label+percent',
                                            insidetextorientation = 'radial',
                                            rotation = 150
                                          ) %>%
                                            layout(
                                              title = paste("Contribution to Tax Revenues by Income Groups (Simulation),", simulation_year),
                                              showlegend = FALSE,  # Turn off the legend
                                              margin = list(l = 20, r = 20, t = 50, b = 20),
                                              annotations = list(
                                                x = 0.13,
                                                y = 0.0,
                                                text = "Source: WB staff estimation",
                                                showarrow = FALSE,
                                                xref = 'paper',
                                                yref = 'paper',
                                                xanchor = 'center',
                                                yanchor = 'top',
                                                font = list(size = 12)
                                              )
                                            )
            
            pit_bins_sim_sub_plt <- pit_bins_sim_sub_plt %>%
              config(displayModeBar = FALSE)
            
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    dist_centile_groups_plt=dist_centile_groups_plt,
    dist_decile_groups_plt=dist_decile_groups_plt,
    #pit_bins_bu_sub_plt=pit_bins_bu_sub_plt,
    PIT_RevenuesTotal_plt=PIT_RevenuesTotal_plt,
    pit_bins_sim_sub_plt=pit_bins_sim_sub_plt
    
    
  )
}
