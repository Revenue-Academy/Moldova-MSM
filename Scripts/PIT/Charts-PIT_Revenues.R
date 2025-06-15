library(plotly)

Revenue_Charts <- function(merged_PIT_BU_SIM, forecast_horizon) {
  
  # Chart 1. Comparison of PIT Revenues -----------------------------------------------------------------
  PIT_RevenuesTotal_plt <- plot_ly(
                                    merged_PIT_BU_SIM,
                                    x = ~year,
                                    y = ~pitax_bu,
                                    name = "Baseline",
                                    type = 'scatter',
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid")
                                    ) %>%
                                        add_trace(
                                          x = ~year,
                                          y = ~pitax_sim,
                                          name = 'Simulation',
                                          line = list(width = 4, dash = "dot")
                                      ) %>%
                              layout(
                                title = paste("Total PIT Revenues in LCU MIL,", min(forecast_horizon), "-", max(forecast_horizon)),
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
    merged_PIT_BU_SIM_select<-merged_PIT_BU_SIM%>%
    dplyr::select(year,ends_with("_sim"))
  
  merged_PIT_BU_SIM_wide<-melt(merged_PIT_BU_SIM_select)%>%
    dplyr::filter(variable!="pitax_sim")
  
  
  merged_PIT_BU_SIM_wide$year<-as.factor(merged_PIT_BU_SIM_wide$year)
  
  
  # Define unique colors
  colors <- c( '#ff7f0e','#1f77b4','#e377c2', '#7f7f7f', '#f4cce8',  '#5fd35f', '#57a9e2',
               '#ffb574', '#17becf', '#9467bd', '#e77c7c', '#c6aedc', '#bcbd22',
               '#d62728','#bc8b81', '#2ca02c', '#b2b2b2', '#e2e362', 
               '#5fe0ed', '#8c564b', '#103d5d', '#a74e00', '#c0504d', '#ff6666', '#8b8b8b')
  
  # Create plotly bar chart
  StructureRevenues_plt <- plot_ly(merged_PIT_BU_SIM_wide, 
                                   x = ~year, 
                                   y = ~value, 
                                   type = 'bar', 
                                   color = ~variable,
                                   colors = colors) %>%
    layout(
      title = paste("Structure of Revenues by Type of Income (in LCU MIL),", min(forecast_horizon), "-", max(forecast_horizon)),
      xaxis = list(title = " ", tickmode = 'linear'),
      yaxis = list(title = " "),
      barmode = 'stack',  # Changed from 'group' to 'stack' for stacked bars
      bargap = 0.7,
      legend = list(
        orientation = 'h',
        x = 0, 
        y = -0.2
      ),
      annotations = list(
        list(
          x = -0.03,
          y = -0.2,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = 'paper',
          yref = 'paper',
          align = 'left'
        )
      )
    )
  
  
  
  # Chart 3. Comparison of PIT Revenues from Labor  ------------------------- 
  

  
  merged_PIT_BU_SIM_select<-merged_PIT_BU_SIM%>%
    dplyr::select(year,ends_with("_sim"))
  
  
  merged_PIT_BU_SIM_select<-merged_PIT_BU_SIM_select%>%
    mutate(wages_income=pit_ials21_sal_sim,
           investment_income=pit_ials21_fol_sim + pit_ials21_pls_exmp_sim + pit_ials21_pl_sim + pit_ials21_roy_sim + pit_ials21_donpf_sim + pit_ials21_don_p_sim + pit_ials21_rcsa_sim + pit_ials21_dobba_sim + pit_ials21_dob_sim + pit_ials21_vms_sim + pit_ials21_div_sim+
             # pit_ials21_pls_sim +
             pit_ials21_don_sim + pit_ials21_liv_sim + pit_ials21_nor_sim + pit_ials21_csm_sim + pit_ials21_agrac_sim + pit_ials21_ser_sim+pit_cet18_sim,
           business_income=pit_ai_17_sim + pit_daj17_sim + pit_dass19_sim + pit_ven12_sim + pit_unif21_sim + pit_taxi18_sim + pit_cet18_sim)%>%
    dplyr::select(year,wages_income,investment_income,business_income)
  
  
  merged_PIT_BU_SIM_wide<-melt(merged_PIT_BU_SIM_select)
  #dplyr::filter(variable!="pitax_sim")
  #dplyr::select(wages_income,investment_income,business_income)
  
  
  merged_PIT_BU_SIM_wide$year<-as.factor(merged_PIT_BU_SIM_wide$year)
  
  
  
  # type of income
  
  colors <- c( '#ff7f0e','#1f77b4','#2ca02c')
  
  # Create plotly bar chart
  TypeOfRevenues_plt <- plot_ly(merged_PIT_BU_SIM_wide, 
                                x = ~year, 
                                y = ~value, 
                                type = 'bar', 
                                color = ~variable,
                                colors = colors
  ) %>%
    layout(
      title = paste("Structure of PIT Revenues by Type of Income (in LCU MIL),", min(forecast_horizon), "-", max(forecast_horizon)),
      xaxis = list(title = " ", tickmode = 'linear'),
      yaxis = list(title = " "),
      barmode = 'stack',  # Changed from 'group' to 'stack' for stacked bars
      bargap = 0.7,
      legend = list(
        orientation = 'h',
        x = 0, 
        y = -0.2
      ),
      annotations = list(
        list(
          x = -0.03,
          y = -0.2,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = 'paper',
          yref = 'paper',
          align = 'left'
        )
      )
    )

 
  

  
  # Chart 4. Comparison of PIT Revenues from Wages  ------------------------- 
  
  
  merged_PIT_BU_SIM_select<-merged_PIT_BU_SIM%>%
    dplyr::select(year,ends_with("_sim"))
  
  merged_PIT_BU_SIM_wide<-melt(merged_PIT_BU_SIM_select)%>%
    dplyr::filter(variable=="pit_ials21_sal_sim")
  
  
  merged_PIT_BU_SIM_wide$year<-as.factor(merged_PIT_BU_SIM_wide$year)
  
  
  # Create plotly bar chart
  WagesRevenues_plt <- plot_ly(merged_PIT_BU_SIM_wide, 
                                   x = ~year, 
                                   y = ~value, 
                                   type = 'bar'
                                   #color = ~variable
                                  ) %>%
    layout(
      title = paste("PIT Revenues from Wages (in LCU MIL),", min(forecast_horizon), "-", max(forecast_horizon)),
      xaxis = list(title = " ", tickmode = 'linear'),
      yaxis = list(title = " "),
      barmode = 'stack',  # Changed from 'group' to 'stack' for stacked bars
      bargap = 0.7,
      legend = list(
        orientation = 'h',
        x = 0, 
        y = -0.2
      ),
      annotations = list(
        list(
          x = -0.03,
          y = -0.2,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = 'paper',
          yref = 'paper',
          align = 'left'
        )
      )
    )


  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    PIT_RevenuesTotal_plt = PIT_RevenuesTotal_plt,
    StructureRevenues_plt=StructureRevenues_plt,
    TypeOfRevenues_plt=TypeOfRevenues_plt,
    WagesRevenues_plt=WagesRevenues_plt,
    
    
    # Tables
    merged_PIT_BU_SIM = merged_PIT_BU_SIM
  )
}
