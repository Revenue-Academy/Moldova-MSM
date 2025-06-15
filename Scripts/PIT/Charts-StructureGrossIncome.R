" Strucuture of Gross Income "

# Define custom colors
#custom_colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
# Infoboxes colors: # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

# I.Function for Dashboard ------------------------------------------------------------------

#Structure_GrossIncome_Charts <- function(te_agg,te_labor_capital,nace_pit_summary_te,decile_pit_summary, forecast_horizon) {
Structure_GrossIncome_Charts <- function(structure_gross_inc,structure_pit,long_df, SimulationYear) {

# I.Chart labor-capital -------------------------------------------------------------------------

  structure_pit_inc_pie_plt <- plot_ly(
                                      structure_pit,
                                      labels = ~income_type,
                                      values = ~value,
                                      type = 'pie',
                                      hole = 0.6,
                                      textinfo = 'label+percent',
                                      insidetextorientation = 'radial'
                                    ) %>%
                                      layout(
                                        title = paste("Structure of PIT by categories,", SimulationYear),
                                        showlegend = TRUE,  # Enable the legend
                                        legend = list(
                                          orientation = 'v',  # Vertical orientation (default)
                                          x = 1.05,  # Move the legend slightly to the right of the chart
                                          y = 0.5,  # Center the legend vertically
                                          xanchor = 'left',
                                          yanchor = 'middle',
                                          font = list(size = 10)  # Adjust font size
                                        ),
                                        startangle = 90,  # Rotate the pie chart by 90 degrees clockwise
                                        margin = list(l = 20, r = 20, t = 50, b = 50),  # Leave space for the annotation
                                        annotations = list(
                                          list(
                                            x = 1.0,  # Keep the annotation centered below the chart
                                            y = 0.0,  # Move the annotation further down to ensure labels don't overlap
                                            text = "Source: WB staff estimation",
                                            showarrow = FALSE,
                                            xref = 'paper',
                                            yref = 'paper',
                                            xanchor = 'center',
                                            yanchor = 'top',
                                            font = list(size = 12)
                                          )
                                        )
                                      )
  
  
  
                    
# II. Chart Type of Income --------------------------------

  order_levels <- long_df_pit %>% 
                  group_by(income_type) %>% 
                  summarise(total = sum(value), .groups = "drop") %>% 
                  arrange(desc(total)) %>%          # largest total first  → bottom
                  pull(income_type)
                
                
                
                long_df_plot <- long_df_pit %>% 
                  mutate(income_type = factor(income_type, levels = order_levels)) %>% 
                  arrange(income_type)            
                
                
                color_mapping <- c("#1f77b4", "orange", "red")
                
    gross_pit_dec_plt <- plot_ly(
                  long_df_plot,
                  x      = ~decile_group,
                  y      = ~value,
                  color  = ~income_type,
                  colors = color_mapping,
                  type   = "bar",
                  barmode = "stack",
                  textposition   = "inside",
                  insidetextfont = list(color = "white")
                ) %>% 
                  layout(
                    #title = "Total PIT by Decile Group and Type of Income",
                    title = paste("Total PIT by Decile Group and Type of Income,", SimulationYear),
                    xaxis = list(title = "", tickvals = long_df_plot$decile_group,
                                 ticktext = long_df_plot$decile_group),
                    yaxis = list(title = ""),
                    barmode = "stack",
                    annotations = list(
                      x = -0.02, y = -0.1, text = "Source: WB staff estimation",
                      showarrow = FALSE, xref = "paper", yref = "paper", align = "left"
                    )
                  )

# III. Chart Treemap GROSS INCOME -------------------------------------------------------
    
                    
                    structure_gross_inc_pie_plt <- plot_ly(
                                  structure_gross_inc,
                                  labels = ~income_type,
                                  values = ~value,
                                  type = 'pie',
                                  hole = 0.6,
                                  textinfo = 'label+percent',
                                  insidetextorientation = 'radial'
                                ) %>%
                                  layout(
                                    title = paste("Structure of gross income by categories,", SimulationYear),
                                    showlegend = TRUE,  # Enable the legend
                                    legend = list(
                                      orientation = 'v',  # Vertical orientation (default)
                                      x = 1.05,  # Move the legend slightly to the right of the chart
                                      y = 0.5,  # Center the legend vertically
                                      xanchor = 'left',
                                      yanchor = 'middle',
                                      font = list(size = 10)  # Adjust font size
                                    ),
                                    startangle = 90,  # Rotate the pie chart by 90 degrees clockwise
                                    margin = list(l = 20, r = 20, t = 50, b = 50),  # Leave space for the annotation
                                    annotations = list(
                                      list(
                                        x = 1.0,  # Keep the annotation centered below the chart
                                        y = 0.0,  # Move the annotation further down to ensure labels don't overlap
                                        text = "Source: WB staff estimation",
                                        showarrow = FALSE,
                                        xref = 'paper',
                                        yref = 'paper',
                                        xanchor = 'center',
                                        yanchor = 'top',
                                        font = list(size = 12)
                                      )
                                    )
                                  )
                                                  
                   # treemap_labor_capital_type_plt
                    
                    

# IV. Structure of gross income by decile group and type-------------------------------------------

                    order_levels <- long_df %>% 
                      group_by(income_type) %>% 
                      summarise(total = sum(value), .groups = "drop") %>% 
                      arrange(desc(total)) %>%          # largest total first  → bottom
                      pull(income_type)
                    
            
                    
                    long_df_plot <- long_df %>% 
                      mutate(income_type = factor(income_type, levels = order_levels)) %>% 
                      arrange(income_type)              # <--- **crucial for Plotly**
                    
         
                    color_mapping <- c("#1f77b4", "orange", "red")
                    
                    gross_inc_dec_plt <- plot_ly(
                                          long_df_plot,
                                          x      = ~decile_group,
                                          y      = ~value,
                                          color  = ~income_type,
                                          colors = color_mapping,
                                          type   = "bar",
                                          barmode = "stack",
                                          textposition   = "inside",
                                          insidetextfont = list(color = "white")
                                        ) %>% 
                                          layout(
                                            #title = "Total Gross Income by Decile Group and Type of Income",
                                            title = paste("Total Gross Income by Decile Group and Type of Income,", SimulationYear),
                                            xaxis = list(title = "", tickvals = long_df_plot$decile_group,
                                                         ticktext = long_df_plot$decile_group),
                                            yaxis = list(title = ""),
                                            barmode = "stack",
                                            annotations = list(
                                              x = -0.02, y = -0.1, text = "Source: WB staff estimation",
                                              showarrow = FALSE, xref = "paper", yref = "paper", align = "left"
                                            )
                                          )
                                        
                    # treemap_labor_capital_type_plt= gross_inc_dec_plt
                    # treemap_nace_type_plt=gross_inc_dec_plt
                    
# Export Charts -----------------------------------------------------------
                    list(
                      # Charts
                      structure_gross_inc_pie_plt=structure_gross_inc_pie_plt,
                      gross_inc_dec_plt=gross_inc_dec_plt,
                      structure_pit_inc_pie_plt=structure_pit_inc_pie_plt,
                      gross_pit_dec_plt=gross_pit_dec_plt

                    )
}      