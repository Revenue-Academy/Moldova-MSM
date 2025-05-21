" Tax Expenditures Dashboard "

# I.Function for Dashboard ------------------------------------------------------------------

Tax_Expenditures_Charts <- function(te_agg,te_labor_capital,nace_pit_summary_te,decile_pit_summary, forecast_horizon) {
  # Check if forecast_horizon is provided and is valid
  if (missing(forecast_horizon) || length(forecast_horizon) != 2) {
    stop("Please provide a valid 'forecast_horizon' with minimum and maximum values.")
  }
  # Chart 1. Total Tax Expenditures -----------------------------------------------------------------
       
          te_agg_plt <- plot_ly(
                                te_agg,
                                x = ~year,
                                y = ~`tax expenditure`,
                                name = "Baseline",
                                type = 'scatter',
                                mode = 'lines+markers+text',
                                text = ~`tax expenditure`,
                                textposition = 'top middle',
                                hoverinfo = 'text+y',
                                line = list(width = 4, dash = "solid")
                              ) %>%
                                layout(
                                  title = paste("Total Tax Expenditures,", min(forecast_horizon), "-", max(forecast_horizon)),
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
  
  # Chart 2. Tax Expenditures by Type of Income-----------------------------------------------------------------
          # Define custom colors for the tax incentive categories
          custom_colors <- c("labor" = '#ff7f0e', "capital" = '#1f77b4')
          
          # Create a stacked bar chart with Plotly and custom colors
          te_stacked_bar_chart_plt <- plot_ly(te_labor_capital, 
                                              x = ~year, 
                                              y = ~`tax expenditure`, 
                                              color = ~`tax incentive`, 
                                              colors = custom_colors,
                                              text = ~`tax expenditure`, 
                                              hoverinfo = 'text', 
                                              type = 'bar', 
                                              barmode = 'stack',
                                              textposition = 'inside',  # Position text inside the bars
                                              insidetextfont = list(color = 'white')  # Ensure text is readable inside bars
          ) %>%
            layout(#title = "Tax Expenditures by Type of Income",
                   
                   title = paste("Tax Expenditures by Type of Income,", min(forecast_horizon), "-", max(forecast_horizon)),
                   
                   xaxis = list(title = " "),
                   yaxis = list(title = " "),
                   barmode = 'stack',
                   bargap = 0.6,  # Adjust this value to control the bar width
                   annotations = list(
                     x = -0.02,
                     y = -0.1,
                     text = "Source: WB staff estimation",
                     showarrow = FALSE,
                     xref = 'paper',
                     yref = 'paper',
                     align = 'left'
                   ))
          
          

  # Chart 3. Tax Expenditures by NACE Section -------------------------------------------------------------------------
  
            # Get unique years from the data
            unique_years <- unique(nace_pit_summary_te$year)
            
            # Generate a color palette with as many colors as there are unique years
            colors <- brewer.pal(length(unique_years), "Dark2")
            
            # Create a named vector for colors, mapping each year to a color
            custom_colors <- setNames(colors, unique_years)
            
            
            # Create a grouped bar chart with Plotly and custom colors
            te_nace_plt <- plot_ly(nace_pit_summary_te, 
                                   x = ~nace_section, 
                                   y = ~tax_expenditures, 
                                   color = ~as.factor(year), 
                                   colors = custom_colors,
                                   text = ~description, # Add the description text for hover info
                                   hoverinfo = 'text+y', # Specify hover info to include text and y value
                                   type = 'bar', 
                                   barmode = 'group') %>%
              layout(#title = "Tax Expenditures by NACE Section",
                     title = paste("Tax Expenditures by NACE Section,", min(forecast_horizon), "-", max(forecast_horizon)),
                     xaxis = list(title = "NACE Section"),
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
            
  
  # Chart 4. Tax Expenditures by Decile Groups---------------------------------------------------------------
          # Get unique years from the data
          unique_years <- unique(decile_pit_summary$year)
          
          # Generate a color palette with as many colors as there are unique years
          colors <- brewer.pal(length(unique_years), "Dark2")
          
          # Create a named vector for colors, mapping each year to a color
          custom_colors <- setNames(colors, unique_years)
          
          
          # Create a grouped bar chart with Plotly and custom colors
          te_decile_groups_plt <- plot_ly(decile_pit_summary, 
                                          x = ~decile_group, 
                                          y = ~tax_expenditures, 
                                          color = ~as.factor(year), 
                                          colors = custom_colors,
                                          hoverinfo = 'text+y', # Specify hover info to include text and y value
                                          type = 'bar', 
                                          barmode = 'group') %>%
            layout(#title = "Tax Expenditures by Decile Groups",
                   title = paste("Tax Expenditures by Decile Groups,", min(forecast_horizon), "-", max(forecast_horizon)),
                   
                   xaxis = list(title = "Decile", tickmode = 'linear'), # Show all values on the x-axis
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
          
        
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    te_agg_plt=te_agg_plt,
    te_stacked_bar_chart_plt=te_stacked_bar_chart_plt,
    te_nace_plt=te_nace_plt,
    te_decile_groups_plt=te_decile_groups_plt
  )
}

        
     