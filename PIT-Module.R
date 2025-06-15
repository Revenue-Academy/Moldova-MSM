library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ineq)
library(IC2)
library(data.table)
library(readxl)
library(fontawesome)
library(flexdashboard)
library(tidyverse)
library(plyr)
library(shinycssloaders) 
library(future)
library(promises)
library(plotly)
library(stringr)
library(reshape2)
library(base64enc)
library(parallel)
library(purrr)
library(tidyr)
library(RColorBrewer) 
library(Hmisc)
library(openxlsx)
library(forcats)


options(scipen = 999)


# I. UI --------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",  # Align image and text
      uiOutput("headerImage"),  # Output slot for the image in the header
      tags$span("PIT Module", style = "flex-grow: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("file-excel")),
      menuItem("Simulation Parameters", icon = icon("list-alt"),
               menuSubItem("Policy Parameters", tabName = "PolicyParameters", icon = icon("edit"))
      ),
      menuItem("Results", icon = icon("magnifying-glass-chart"),
               menuSubItem("Main Results", tabName = "MainResultsSimulation", icon = icon("gauge")),
               menuSubItem("Redistribution Effects", tabName = "MainRedistributionEffects", icon = icon("square-poll-vertical")),
               menuSubItem("Distribution Effects", tabName = "MainDistributionTables", icon = icon("chart-column")),
               menuSubItem("Tax Contribution", tabName = "MainResultBins", icon = icon("chart-pie"))
              # menuSubItem("Tax Expenditures", tabName = "MainResultsTE", icon = icon("wallet"))
      ),
      menuItem("Visualizations", tabName = "CustomsDuties-charts", icon = icon("chart-simple"),
               menuSubItem("Dashboards", tabName = "PIT_Revenues", icon = icon("chart-column"))
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "input",
              fluidRow(
                column(6,
                       h4("Data Input"),
                       selectInput("inputType", "Data Source",
                                   choices = c("Manual", "Excel File"),
                                   selected = "Excel File"),
                       conditionalPanel(
                         condition = "input.inputType == 'Excel File'",
                         fileInput("fileInput", "Upload Excel File", accept = c(".xlsx")),
                         checkboxInput("hasHeader", "Header", TRUE)
                       ),
                       actionButton("importExcel", "Import Excel Data")
                )
              )
      ),
      tabItem(tabName = "PolicyParameters",
             # h4("Setting simulations parameters"),
              fluidRow(
                column(3,
                       sliderInput("SimulationYear", "Setting Simulation Year",
                                   min = 2023, max = 2027, step = 1, value = 2023, width = "100%", round = 0, sep = ""),
                       uiOutput("PolicyParameter"),
                       uiOutput("Descriptions_Select"),
                       uiOutput("LongNameSelect"),
                       actionButton("addValuesValue", "Add to Table", style = "float: left;"),
                       actionButton("clearValuesTable", "Clear Table", style = "float: left;")
                ),
                column(3,
                       numericInput("default_Year", "Initial year ", value = 0, min = 0, step = 0.01),
                       numericInput("default_Value", "Value", value = 0, min = 0, step = 0.01)

                ),
                column(3,
                       switchInput("toggleSimulationRates", "Toggle Tax Expenditures", value = FALSE, onLabel = "On", offLabel = "Off"),
                       numericInput("sim_PIT_Rates", "Tax Benchmark", value = 0, min = 0, step = 1)
                )
              ),
              #h4("Selected simulations parameters"),
             div(h4("Selected Simulations Parameters"), style = "text-align: center;"),
              fluidRow(
                column(12,
                       DTOutput("pit_simulation_parameters_updated"),
                       actionButton("calc_Customs_Sim_Button", "Run Simulation", style = "float: right;"),
                       actionButton("savepit_simulation_parameters_updated", "Save Data", style = "float: right;")
                )
              )
              
      ),
      tabItem(
        tabName = "MainResultsSimulation",
        fluidRow(
          column(12,
                 DTOutput("PIT_SUMMARY_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "MainResultsTE",
        fluidRow(
          column(12,
                 DTOutput("TE_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "MainRedistributionEffects",
        fluidRow(
          column(12,
                 DTOutput("RE_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "MainDistributionTables",
        fluidRow(
          column(12,
                 DTOutput("DIST_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "MainResultBins",
        fluidRow(
          column(12,
                 DTOutput("BIN_TABLES")
          )
        )
      ),
      tabItem(
        tabName = "PIT_Revenues",
        fluidRow(
          column(6,
                 selectInput("chartSelectPIT_Revenues", "Select Chart",
                             choices = c(
                               "Structure_Charts",
                               "Revenue_Charts",
                               "Distribution_Charts"
                               #"Tax_Expenditures_Charts"
                             ),
                             selected = "Structure_Charts")
          )
        ),
        fluidRow(
          infoBoxOutput("infoBox1", width = 6),
          infoBoxOutput("infoBox2", width = 6)
        ),
        fluidRow(
          column(12,
                 uiOutput("additionalCharts")
          )
        )
      )
    )
  )
)

# II. Server ---------------------------------------------------------------------
server <- function(input, output, session) {

  # Render the image dynamically using Base64 encoding
  output$headerImage <- renderUI({
    img_data <- base64enc::dataURI(file = "img/WB_pic.png", mime = "image/png")
    tags$img(src = img_data, height = "40px", style = "float:left; margin-right:20px;")
  })
  
  # Input from slider -------------------------------------------------------
  observeEvent(input$SimulationYear, {
    assign("SimulationYear", input$SimulationYear, envir = .GlobalEnv)
    cat("Simulation year updated:", input$SimulationYear, "\n")
  })
  
  observeEvent(input$PersonalAllowance, {
    assign("PersonalAllowance", input$PersonalAllowance, envir = .GlobalEnv)
    cat("Personal Allowance updated:", input$PersonalAllowance, "\n")
  })
  
  observeEvent(input$HighestBaseSSC_Employment, {
    assign("HighestBaseSSC_Employment", input$HighestBaseSSC_Employment, envir = .GlobalEnv)
    cat("Highest Base SSC Employment updated:", input$HighestBaseSSC_Employment, "\n")
  })
  
  observeEvent(input$SSC_rate, {
    assign("SSC_rate", input$SSC_rate, envir = .GlobalEnv)
    cat("SSC_rate updated:", input$SSC_rate, "\n")
  })
  
  observeEvent(input$tax_regime, {
    assign("tax_regime", input$tax_regime, envir = .GlobalEnv)
    cat("tax_regime updated:", input$tax_regime, "\n")
  })
  
  shinyjs::disable("default_Year")
  
  
  # Reactive data frame to store Excel data
  excelData <- reactiveVal(NULL)
  
  observeEvent(input$importExcel, {
    req(input$fileInput)
    inFile <- input$fileInput
    if (!is.null(inFile)) {
      data <- read_excel(inFile$datapath, col_names = input$hasHeader)
      if (!all(c("PolicyParameter", "Descriptions", "LongName", "Value", "Year") %in% colnames(data))) {
        showModal(modalDialog(
          title = "Error",
          "The Excel file must contain the columns: 'PolicyParameter', 'Descriptions', 'LongName', 'Value', 'Year'",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      data <- data %>%
        mutate(
          Value = as.numeric(gsub("[^0-9.]", "", Value)),
          Year = as.numeric(gsub("[^0-9.]", "", Year))
        )
      excelData(data)
      assign("pit_simulation_parameters_raw", excelData(), envir = .GlobalEnv)
      cat("Excel data imported successfully\n")
    }
  })
  
  pit_simulation_parameters_updated <- reactiveVal(data.table(
    PolicyParameter = character(),
    Descriptions = character(),
    LongName = character(),
    Value = numeric(),
    Year = numeric()
  ))
  
  output$PolicyParameter <- renderUI({
    if (!is.null(excelData())) {
      selectInput("PolicyParameter", "Policy Parameter Selection", choices = unique(excelData()$PolicyParameter))
    } else {
      selectInput("PolicyParameter", "Policy Parameter Selection", choices = NULL)
    }
  })
  
  output$Descriptions_Select <- renderUI({
    req(input$PolicyParameter)
    PolicyParameter <- input$PolicyParameter
    if (!is.null(PolicyParameter) && !is.null(excelData())) {
      selectInput("Descriptions_Select", "Description of parameter", choices = unique(excelData()[excelData()$PolicyParameter == PolicyParameter,]$Descriptions))
    } else {
      selectInput("Descriptions_Select", "Description of parameter", choices = NULL)
    }
  })
  
  output$LongNameSelect <- renderUI({
    req(input$Descriptions_Select)
    Descriptions <- input$Descriptions_Select
    if (!is.null(Descriptions) && !is.null(excelData())) {
      selectInput("LongNameSelect", "Selected variable", choices = unique(excelData()[excelData()$Descriptions == Descriptions,]$LongName))
    } else {
      selectInput("LongNameSelect", "Selected variable", choices = NULL)
    }
  })
  
  observeEvent(input$LongNameSelect, {
    selected_class <- input$LongNameSelect
    cat("Selected LongName: ", selected_class, "\n")
    if (!is.null(selected_class) && !is.null(excelData())) {
      selected_row <- excelData() %>% filter(LongName == selected_class)
      cat("Selected row:\n")
      print(selected_row)
      if (nrow(selected_row) == 1) {
        updateNumericInput(session, "default_Value", value = selected_row$Value)
        updateNumericInput(session, "default_Year", value = selected_row$Year)
        cat("Numeric inputs updated with selected row values\n")
      } else {
        cat("No matching row found or multiple rows returned\n")
      }
    }
  })

  observeEvent(input$addValuesValue, {
    req(input$LongNameSelect)
    newEntry <- data.table(
      PolicyParameter = input$PolicyParameter,
      Descriptions = input$Descriptions_Select,
      LongName = input$LongNameSelect,
      Value = if (input$toggleSimulationRates) input$sim_PIT_Rates else input$default_Value,
      Year = if (input$toggleSimulationRates) input$SimulationYear else input$default_Year
    )
    pit_simulation_parameters_updated(rbind(pit_simulation_parameters_updated(), newEntry))
    cat("New entry added to pit_simulation_parameters_updated:\n")
    print(newEntry)
  })
  
  observeEvent(input$clearValuesTable, {
    pit_simulation_parameters_updated(data.table(
      PolicyParameter = character(),
      Descriptions = character(),
      LongName = character(),
      Value = numeric(),
      Year = numeric()
    ))
    cat("pit_simulation_parameters_updated table cleared\n")
  })
  
  observeEvent(input$savepit_simulation_parameters_updated, {
    assign("ValueTableUpdate", pit_simulation_parameters_updated(), envir = .GlobalEnv)
    cat("PIT simulation parameters saved to GlobalEnv as ValueTableUpdate\n")
    
    pit_simulation_parameters_updated_copy <- get("pit_simulation_parameters_raw", envir = .GlobalEnv)
    
    if (input$toggleSimulationRates) {
      pit_simulation_parameters_updated_copy$Value[pit_simulation_parameters_updated_copy$PolicyParameter == "Deductions"] <- input$sim_PIT_Rates
      pit_simulation_parameters_updated_copy$Year[pit_simulation_parameters_updated_copy$PolicyParameter == "Deductions"] <- input$SimulationYear
      cat("Simulation rates updated in pit_simulation_parameters_updated_copy\n")
    }
    
    pitRateData <- get("ValueTableUpdate", envir = .GlobalEnv)
    if (nrow(pitRateData) > 0) {
      for (i in 1:nrow(pitRateData)) {
        row <- pitRateData[i, ]
        pit_simulation_parameters_updated_copy[pit_simulation_parameters_updated_copy$PolicyParameter == row$PolicyParameter & pit_simulation_parameters_updated_copy$Descriptions == row$Descriptions & pit_simulation_parameters_updated_copy$LongName == row$LongName, 
                                               c("Value", "Year")] <- list(row$Value, row$Year)
      }
    }
    
    assign("pit_simulation_parameters_updated", pit_simulation_parameters_updated_copy, envir = .GlobalEnv)
    cat("pit_simulation_parameters_updated assigned to GlobalEnv\n")
  })
  
  observe({
    toggleState("sim_PIT_Rates", input$toggleSimulationRates)
    
    if (!is.null(input$toggleSimulationRates) && length(input$toggleSimulationRates) > 0 && input$toggleSimulationRates) {
      assign("Value", input$sim_PIT_Rates, envir = .GlobalEnv)
      assign("Year", input$SimulationYear, envir = .GlobalEnv)
      cat("Simulation rates assigned to GlobalEnv\n")
    } else {
      if (exists("Value", envir = .GlobalEnv)) {
        rm("Value", envir = .GlobalEnv)
      }
      if (exists("Year", envir = .GlobalEnv)) {
        rm("Year", envir = .GlobalEnv)
      }
      if (exists("Regime", envir = .GlobalEnv)) {
        rm("Regime", envir = .GlobalEnv)
      }
      cat("Simulation rates removed from GlobalEnv\n")
    }
  })
  
  output$pit_simulation_parameters_updated <- renderDT({
    datatable(pit_simulation_parameters_updated(), options = list(dom = 't', paging = FALSE), editable = TRUE)
  })
  
  reactive_simulation_results <- reactiveVal()
  
  observeEvent(input$calc_Customs_Sim_Button, {
    if (nrow(pit_simulation_parameters_updated()) == 0 && is.null(excelData())) {
      showModal(modalDialog(
        title = "Error",
        "No parameters have been added to the table or imported from the Excel file. Please select parameters, add them to the table or import from Excel before running the simulation.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    showModal(modalDialog(
      title = "Running Simulation...",
      "Please wait while the simulation is running...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    future({
      #source("Scripts/PIT/TaxCalculator.R")
      source("Scripts/PIT/TaxCalculator_Subset1.R")
      source("Scripts/PIT/TaxCalculator_Subset2.R")
      source("Scripts/PIT/TaxCalculator_Subset3.R")
      source("Scripts/PIT/Calc-AggregationOfData.R")
      source("Scripts/PIT/Calc-Distribution-Effects.R")
      source("Scripts/PIT/Calc-Structure.R")
     # source("Scripts/PIT/Calc-TaxExpenditures.R")
      #source("Scripts/PIT/Calc-Distribution-Effects.R")
      source("Scripts/PIT/Calc-Redistribution-Effects.R")
      list(
        pit_summary_df = get("pit_summary_df", envir = .GlobalEnv),
        #te_summary_df = get("te_summary_df", envir = .GlobalEnv),
        re_effects_final = get("re_effects_final", envir = .GlobalEnv),
        pit_decile_distribution_bu_sim = get("pit_decile_distribution_bu_sim", envir = .GlobalEnv),
        pit_result_bins_sim_sub = get("pit_result_bins_sim_sub", envir = .GlobalEnv)
      )
    }) %...>% (function(results) {
      removeModal()
      showModal(modalDialog(
        title = "Success",
        "Simulation is done!",
        easyClose = TRUE,
        footer = NULL
      ))
      
      reactive_simulation_results(results)
      updateCharts()
    }) %...!% (function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Error during calculation:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
 
  output$PIT_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$pit_summary_df,
      caption = tags$caption(paste("PIT Projections,", min(forecast_horizon), "-", max(forecast_horizon)), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',  
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'PIT_Projections',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'csvHtml5',
            text = 'CSV',
            filename = 'PIT_Projections',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'print',
            text = 'Print',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          )
        ),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
 
  # output$TE_TABLES <- renderDT({
  #   req(input$toggleSimulationRates)  # Ensure the table is only rendered when toggleSimulationRates is TRUE
  #   req(reactive_simulation_results())  # Ensure simulation results exist
  #   
  #   te_summary_selected <- reactive_simulation_results()$te_summary_df %>%
  #     select(year, `legal reference`, `current law`, benchmark, `tax expenditure`) %>%
  #     filter(`legal reference` != "" & !is.na(`legal reference`))
  #   
  #   datatable(
  #     te_summary_selected,
  #     caption = tags$caption(
  #       paste("Tax Expenditures in LCU MIL,", min(forecast_horizon), "-", max(forecast_horizon)),
  #       class = "table-caption-bold"
  #     ),
  #     options = list(
  #       pageLength = 15,
  #       dom = 'Blfrtip',  
  #       buttons = list(
  #         list(
  #           extend = 'copyHtml5',
  #           text = 'Copy',
  #           filename = 'TE_Projections',
  #           exportOptions = list(
  #             format = list(
  #               body = JS("function(data, row, column, node) {
  #                          return $('<div>').html(data).text();  // Strip HTML tags
  #                        }")
  #             )
  #           )
  #         ),
  #         list(
  #           extend = 'csvHtml5',
  #           text = 'CSV',
  #           filename = 'TE_Projections',
  #           exportOptions = list(
  #             format = list(
  #               body = JS("function(data, row, column, node) {
  #                          return $('<div>').html(data).text();  // Strip HTML tags
  #                        }")
  #             )
  #           )
  #         ),
  #         list(
  #           extend = 'print',
  #           text = 'Print',
  #           exportOptions = list(
  #             format = list(
  #               body = JS("function(data, row, column, node) {
  #                          return $('<div>').html(data).text();  // Strip HTML tags
  #                        }")
  #             )
  #           )
  #         )
  #       ),
  #       autoWidth = TRUE,
  #       escape = FALSE,
  #       lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
  #     ),
  #     rownames = FALSE
  #   )
  # })
  # 
  
  output$RE_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$re_effects_final,
      caption = tags$caption(paste("Redistributive Effects,", simulation_year), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',  
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'RE_effects',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'csvHtml5',
            text = 'CSV',
            filename = 'RE_effects',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'print',
            text = 'Print',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          )
        ),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$DIST_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$pit_decile_distribution_bu_sim,
      caption = tags$caption(paste("Distribution Tables LCU,", SimulationYear), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',  
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'DistTable',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'csvHtml5',
            text = 'CSV',
            filename = 'DistTable',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'print',
            text = 'Print',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          )
        ),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  output$BIN_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$pit_result_bins_sim_sub,
      caption = tags$caption(paste("Structure of PIT liability by income groups, ", simulation_year), class = "table-caption-bold"),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',  
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'BinTables',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'csvHtml5',
            text = 'CSV',
            filename = 'BinTables',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          ),
          list(
            extend = 'print',
            text = 'Print',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();  // Strip HTML tags
                         }")
              )
            )
          )
        ),
        autoWidth = TRUE,
        escape = FALSE,
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      ),
      rownames = FALSE
    )
  })
  
  
  updateCharts <- function() {
    cat("Updating charts after simulation\n")
    chart_type <- isolate(input$chartSelectPIT_Revenues)
    cat("Selected chart type:", chart_type, "\n")
    
    if (exists("merged_PIT_BU_SIM", envir = .GlobalEnv) && exists("forecast_horizon", envir = .GlobalEnv)) {
      merged_PIT_BU_SIM <- get("merged_PIT_BU_SIM", envir = .GlobalEnv)
      forecast_horizon <- get("forecast_horizon", envir = .GlobalEnv)
      
      if (chart_type == "Revenue_Charts") {
        cat("Preparing Revenue_Charts charts\n")
        source("Scripts/PIT/Charts-PIT_Revenues.R")
        charts <- Revenue_Charts(merged_PIT_BU_SIM, range(forecast_horizon))


        output$infoBox1 <- renderInfoBox({
          infobox_pitax_bu <- merged_PIT_BU_SIM %>%
            dplyr::select(year, pitax_bu) %>%
            dplyr::filter(year == SimulationYear) %>%
            dplyr::select(-c(year))
          
          infoBox(
            title = paste("(Business as usual)", SimulationYear),
            #value = paste(round(infobox_pitax_bu$pitax_bu[1] / 1e03, 1), "(in BIL LCU)"),
            value = paste(round(infobox_pitax_bu$pitax_bu[1], 1), "(in MIL LCU)"), 
            icon = icon("coins"), 
            color = "orange"
          )
        })
        
        
        output$infoBox2 <- renderInfoBox({
          infobox_pitax_sim <- merged_PIT_BU_SIM %>%
            dplyr::select(year, pitax_sim) %>%
            dplyr::filter(year == SimulationYear) %>%
            dplyr::select(-c(year))
          
          infoBox(
            title = paste("Simulation PIT revenues", SimulationYear),
            #value = paste(round(infobox_pitax_sim$pitax_sim[1] / 1e03, 1), "(in BIL LCU)"),
            value = paste(round(infobox_pitax_sim$pitax_sim[1] , 1), "(in MIL LCU)"), 
            icon = icon("chart-line"), 
            color = "light-blue"
          )
        })
        
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("PIT_RevenuesTotal_plt", height = "400px")),
              column(6, plotlyOutput("WagesRevenues_plt", height = "400px"))
              
            ),
            fluidRow(
              column(6, plotlyOutput("TypeOfRevenues_plt", height = "400px")),
              column(6, plotlyOutput("StructureRevenues_plt", height = "400px"))
            )
          )
        })
        
        output$PIT_RevenuesTotal_plt <- renderPlotly({ charts$PIT_RevenuesTotal_plt })
        output$WagesRevenues_plt <- renderPlotly({ charts$WagesRevenues_plt })
        output$TypeOfRevenues_plt <- renderPlotly({ charts$TypeOfRevenues_plt })
        output$StructureRevenues_plt <- renderPlotly({ charts$StructureRevenues_plt })
        
      } else if (chart_type == "Structure_Charts") {
        cat("Preparing Structure_Charts charts\n")
        source("Scripts/PIT/Charts-StructureGrossIncome.R")
        #Charts_structure <- Structure_GrossIncome_Charts(long_labor_capital, labor_capital_type, long_labor_capital_type, gross_nace_tbl)
        Charts_structure <- Structure_GrossIncome_Charts(structure_gross_inc,structure_pit,long_df, SimulationYear)

        output$infoBox1 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            title = " ",   # Remove title
            icon = icon("chart-area"),
            color = "orange"
          )
        })
        
        output$infoBox2 <- renderInfoBox({
          cat("Rendering infoBox1\n")
          infoBox(
            title = " ",   # Remove title
            value = NULL,  # Remove value text
            icon = icon("industry"),
            color = "light-blue"
          )
        })
        
      
        output$chartOutputPIT <- renderPlotly({ Charts_structure$labor_capital_plt })
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("structure_gross_inc_pie_plt", height = "400px")),
              column(6, plotlyOutput("gross_inc_dec_plt", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("structure_pit_inc_pie_plt", height = "400px")),
              column(6, plotlyOutput("gross_pit_dec_plt", height = "400px"))
            )
          )
        })
        
        output$structure_gross_inc_pie_plt <- renderPlotly({ Charts_structure$structure_gross_inc_pie_plt })
        output$gross_inc_dec_plt <- renderPlotly({ Charts_structure$gross_inc_dec_plt })
        output$structure_pit_inc_pie_plt <- renderPlotly({ Charts_structure$structure_pit_inc_pie_plt })
        output$gross_pit_dec_plt <- renderPlotly({ Charts_structure$gross_pit_dec_plt })
        
      } else if (chart_type == "Distribution_Charts") {
        cat("Preparing Distribution_Charts charts\n")
        source("Scripts/PIT/Charts-Distribution.R")
        #source(paste0(path1, "/Scripts/PIT/Charts-Distribution.R"))
        charts_dist <- Distribution_Charts(pit_centile_distribution_bu_sim, pit_decile_distribution_bu_sim_raw,
                                           pit_result_bins_bu_sub, pit_result_bins_sim_sub, simulation_year)
        
        output$infoBox1 <- renderInfoBox({
          #infoBox("Baseline Average Tax Rate", value=round(etr_bu*100,2), icon = icon("percent"), color = "orange")
          #infoBox("Top 1% taxpayers' share of total PIT (Business as usual)", value=round(share_top1_bu*100,2), icon = icon("percent"), color = "orange")
          infoBox("Average Tax Rate (Business as usual)", value=round(re_effects_final$`Business as usual`[1],2), icon = icon("percent"), color = "orange")
        })
        
        output$infoBox2 <- renderInfoBox({
          #infoBox("Simulation Average Tax Rate", value=round(etr_SIM*100,2), icon = icon("percent"), color = "light-blue")
          #infoBox("Top 1% taxpayers' share of total PIT (Simulation)", value=round(share_top1_sim*100,2), icon = icon("percent"), color = "light-blue")
          infoBox("Average Tax Rate (Simulation)", value=round(re_effects_final$Simulation[1],2), icon = icon("percent"), color = "light-blue")
        })
        
        output$chartOutputPIT <- renderPlotly({ charts_dist$dist_centile_groups_plt })
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("labor_capital_plt", height = "400px")),
              column(6, plotlyOutput("labor_capital_type_plt", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("pit_bins_bu_sub_plt", height = "400px")),
              column(6, plotlyOutput("treemap_nace_type_plt", height = "400px"))
            )
          )
        })
        
        output$labor_capital_plt <- renderPlotly({ charts_dist$dist_centile_groups_plt })
        output$labor_capital_type_plt <- renderPlotly({ charts_dist$dist_decile_groups_plt })
        output$pit_bins_bu_sub_plt <- renderPlotly({ charts_dist$pit_bins_bu_sub_plt })
        output$treemap_nace_type_plt <- renderPlotly({ charts_dist$pit_bins_sim_sub_plt })
        
      } else if (chart_type == "Tax_Expenditures_Charts") {
        cat("Preparing Tax_Expenditures_Charts charts\n")
        source("Scripts/PIT/Charts-TaxExpenditures.R")
        #source(paste0(path1, "/Scripts/PIT/Charts-TaxExpenditures.R"))
        charts_te <- Tax_Expenditures_Charts(te_agg, te_labor_capital, nace_pit_summary_te, decile_pit_summary, range(forecast_horizon))
        
        # Conditionally render infoBox1
        output$infoBox1 <- renderInfoBox({
          req(input$toggleSimulationRates)  # Ensure the infoBox is only rendered when toggleSimulationRates is TRUE
          
          cat("Rendering infoBox1\n")
          selected_data <- te_agg %>%
            filter(simulation_year == year) %>%
            select(year, `tax expenditure`)
          
         # selected_value <- round(selected_data$`tax expenditure`[1]/1e03, 2)  # Convert to billions and round to zero decimal places
          title_text <- " "
          
          infoBox(
            title_text,
            #paste0(selected_value, " (in BIL LCU)"),
            icon = icon("hand-holding-usd"),
            color = "orange"
          )
        })
        
       
        
        output$infoBox2 <- renderInfoBox({
          req(input$toggleSimulationRates)
          
          title_text <- " "
          selected_value <- "N/A"  # if you don't have a computed value
          
          infoBox(
            title = title_text,
            #value = paste0(selected_value, " (%)"),
            icon = icon("chart-pie"),
            color = "light-blue"
          )
        })
        
        
        
        # Conditionally render the charts
        output$chartOutputPIT <- renderPlotly({
          req(input$toggleSimulationRates)  # Ensure the chart is only rendered when toggleSimulationRates is TRUE
          charts_te$te_agg_plt
        })
        
        output$additionalCharts <- renderUI({
          req(input$toggleSimulationRates)  # Ensure the additional charts are only rendered when toggleSimulationRates is TRUE
          tagList(
            fluidRow(
              column(6, plotlyOutput("labor_capital_plt", height = "400px")),
              column(6, plotlyOutput("labor_capital_type_plt", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("pit_bins_bu_sub_plt", height = "400px")),
              column(6, plotlyOutput("treemap_nace_type_plt", height = "400px"))
            )
          )
        })
        
        output$labor_capital_plt <- renderPlotly({ charts_te$te_agg_plt })
        output$labor_capital_type_plt <- renderPlotly({ charts_te$te_stacked_bar_chart_plt })
        output$pit_bins_bu_sub_plt <- renderPlotly({ charts_te$te_nace_plt })
        output$treemap_nace_type_plt <- renderPlotly({ charts_te$te_decile_groups_plt })
      }
      
    } else {
      cat("Error: merged_PIT_BU_SIM or forecast_horizon not found in the global environment\n")
    }
  }
  
  observeEvent(input$chartSelectPIT_Revenues, {
    updateCharts()
  })
}

shinyApp(ui = ui, server = server)
