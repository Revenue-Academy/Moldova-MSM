" Strucuture of Gross Income "

# Define custom colors
#custom_colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
# Infoboxes colors: # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

# I.Function for Dashboard ------------------------------------------------------------------

#Structure_GrossIncome_Charts <- function(te_agg,te_labor_capital,nace_pit_summary_te,decile_pit_summary, forecast_horizon) {
Structure_GrossIncome_Charts <- function(long_labor_capital,labor_capital_type,long_labor_capital_type, forecast_horizon) {

# I.Chart labor-capital -------------------------------------------------------------------------

  
                    # Define custom colors for the gross income categories
                    custom_colors <- c("labor" = '#1f77b4', "capital" = '#ff7f0e')
                    
                    # Create a stacked bar chart with Plotly and custom colors
                    labor_capital_plt <- plot_ly(long_labor_capital, 
                                                        x = ~decile_group, 
                                                        y = ~value, 
                                                        color = ~gross_income, 
                                                        colors = custom_colors,
                                                        type = 'bar', 
                                                        barmode = 'stack',
                                                        textposition = 'inside',  # Position text inside the bars
                                                        insidetextfont = list(color = 'white')  # Ensure text is readable inside bars
                                                        ) %>%
                                                          layout(title = "Total Gross Income by Decile Group and Type of Income",
                                                                 xaxis = list(
                                                                   title = " ",
                                                                   tickvals = long_labor_capital$decile_group,
                                                                   ticktext = long_labor_capital$decile_group
                                                                 ),
                                                                 yaxis = list(title = " "),
                                                                 barmode = 'stack',
                                                                 annotations = list(
                                                                   x = -0.02,
                                                                   y = -0.1,
                                                                   text = "Source: WB staff estimation",
                                                                   showarrow = FALSE,
                                                                   xref = 'paper',
                                                                   yref = 'paper',
                                                                   align = 'left'
                                                                 ))
                    

# II. Chart Type of Income --------------------------------

                    #color_mapping <- c('#1f77b4', "cyan", "brown", "purple", "orange", "red", "chartreuse", "darkturquoise", "forestgreen")
                    color_mapping <- c('#1f77b4', "cyan", "brown", "forestgreen","purple", "orange",  "chartreuse", "darkturquoise","red" )
                    
                    # Create a stacked bar chart with Plotly and custom colors
                    labor_capital_type_plt <- plot_ly(labor_capital_type, 
                                                 x = ~decile_group, 
                                                 y = ~value, 
                                                 color = ~gross_income, 
                                                 colors = color_mapping,
                                                 type = 'bar', 
                                                 barmode = 'stack',
                                                 textposition = 'inside',  # Position text inside the bars
                                                 insidetextfont = list(color = 'white')  # Ensure text is readable inside bars
                                                ) %>%
                                                  layout(title = "Total Gross Income by Decile Group and Type of Income",
                                                         xaxis = list(
                                                           title = " ",
                                                           tickvals = long_labor_capital$decile_group,
                                                           ticktext = long_labor_capital$decile_group
                                                         ),
                                                         yaxis = list(title = " "),
                                                         barmode = 'stack',
                                                         annotations = list(
                                                           x = -0.02,
                                                           y = -0.1,
                                                           text = "Source: WB staff estimation",
                                                           showarrow = FALSE,
                                                           xref = 'paper',
                                                           yref = 'paper',
                                                           align = 'left'
                                                         ))

# III. Chart Treemap GROSS INCOME -------------------------------------------------------
    
                    
                    treemap_labor_capital_type_plt <- plot_ly(data = long_labor_capital_type, 
                                                        type = "treemap", 
                                                        values = ~round(value/1e09,1), 
                                                        labels = ~income_type,
                                                        parents = ~TypeOfIncome,  
                                                        name = " ",
                                                        text = ~TypeOfIncome,
                                                        textinfo = "label+value+percent parent")%>%
                                                    layout(
                              
                                                      title = list(
                                                        text = "Structure of Gross income by Type of Income",
                                                        font = list(size = 14)  # Set the font size here
                                                      ),
                                                      annotations = list(
                                                        x = 0.05,
                                                        y = -0.01,
                                                        text = "Source: WB staff estimation",
                                                        showarrow = FALSE,
                                                        xref = 'paper',
                                                        yref = 'paper',
                                                        xanchor = 'center',
                                                        yanchor = 'top',
                                                        font = list(size = 10)
                                                      ))
                                                  
                   # treemap_labor_capital_type_plt
                    
                    

# IV. Structure of gross income by NACE sections-------------------------------------------

                    treemap_labor_capital_type_plt=treemap_labor_capital_type_plt
                    treemap_nace_type_plt=labor_capital_type_plt
                    # treemap_nace_type_plt <- plot_ly(
                    #                             data = gross_nace_tbl, 
                    #                             type = "treemap", 
                    #                             values = ~round(g_total_gross / 1e09, 1), 
                    #                             labels = ~nace_section,
                    #                             parents = ~TypeOfIncome,  
                    #                             name = " ",
                    #                             text = ~TypeOfIncome,
                    #                             hovertext = ~description,
                    #                             textinfo = "label+value+percent parent",
                    #                             hoverinfo = "label+value+percent parent+text"
                    #                           ) %>%
                    #                             layout(
                    #                               title = list(
                    #                                 text = "Structure of Gross income by NACE sections",
                    #                                 font = list(size = 14)  # Set the font size here
                    #                               ),
                    #                               annotations = list(
                    #                                 x = 0.05,
                    #                                 y = -0.01,
                    #                                 text = "Source: WB staff estimation",
                    #                                 showarrow = FALSE,
                    #                                 xref = 'paper',
                    #                                 yref = 'paper',
                    #                                 xanchor = 'center',
                    #                                 yanchor = 'top',
                    #                                 font = list(size = 10)
                    #                               )
                    #                             )

                    
# Export Charts -----------------------------------------------------------
                    list(
                      # Charts
                      labor_capital_plt=labor_capital_plt,
                      labor_capital_type_plt=labor_capital_type_plt,
                      treemap_labor_capital_type_plt=treemap_labor_capital_type_plt,
                      treemap_nace_type_plt=treemap_nace_type_plt

                    )
}      