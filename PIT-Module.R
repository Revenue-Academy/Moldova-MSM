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
gc()
options(scipen = 999)

# I. UI ----------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      uiOutput("headerImage"),
      tags$span(
        "PIT Module",
        style = "flex-grow: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("file-excel")),
      
      menuItem(
        "Simulation Parameters",
        icon = icon("list-alt"),
        menuSubItem("Policy Parameters", tabName = "PolicyParameters", icon = icon("edit"))
      ),
      
      menuItem(
        "Results",
        icon = icon("magnifying-glass-chart"),
        menuSubItem("Main Results", tabName = "MainResultsSimulation", icon = icon("gauge")),
        menuSubItem("Redistribution Effects", tabName = "MainRedistributionEffects", icon = icon("square-poll-vertical")),
        menuSubItem("Distribution Effects", tabName = "MainDistributionTables", icon = icon("chart-column")),
        menuSubItem("Tax Contribution", tabName = "MainResultBins", icon = icon("chart-pie"))
      ),
      
      menuItem(
        "Visualizations",
        tabName = "CustomsDuties-charts",
        icon = icon("chart-simple"),
        menuSubItem("Dashboards", tabName = "PIT_Revenues", icon = icon("chart-column"))
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tabItems(
      
      tabItem(
        tabName = "input",
        fluidRow(
          column(
            6,
            h4("Data Input"),
            selectInput(
              "inputType",
              "Data Source",
              choices = c("Manual", "Excel File"),
              selected = "Excel File"
            ),
            conditionalPanel(
              condition = "input.inputType == 'Excel File'",
              fileInput("fileInput", "Upload Excel File", accept = c(".xlsx")),
              checkboxInput("hasHeader", "Header", TRUE)
            ),
            actionButton("importExcel", "Import Excel Data")
          )
        )
      ),
      
      tabItem(
        tabName = "PolicyParameters",
        
        fluidRow(
          column(
            3,
            sliderInput(
              "SimulationYear",
              "Setting Simulation Year",
              min = 2023,
              max = 2027,
              step = 1,
              value = 2023,
              width = "100%",
              round = 0,
              sep = ""
            ),
            
            uiOutput("ParameterYearSelect"),
            uiOutput("RegimeSelect"),
            uiOutput("PolicyParameter"),
            uiOutput("Descriptions_Select"),
            uiOutput("LongNameSelect"),
            
            actionButton("addValuesValue", "Add to Table", style = "float: left;"),
            actionButton("clearValuesTable", "Clear Table", style = "float: left;")
          ),
          
          column(
            3,
            numericInput("default_Year", "Initial year ", value = 0, min = 0, step = 1),
            numericInput("default_Value", "Value", value = 0, min = 0, step = 0.01),
            switchInput(
              "applyToFutureYears",
              "Apply value to subsequent years",
              value = FALSE,
              onLabel = "On",
              offLabel = "Off"
            )
          )
        ),
        
        div(h4("Selected Simulations Parameters"), style = "text-align: center;"),
        
        fluidRow(
          column(
            12,
            DTOutput("pit_simulation_parameters_updated"),
            actionButton("calc_Customs_Sim_Button", "Run Simulation", style = "float: right;"),
            actionButton("savepit_simulation_parameters_updated", "Save Data", style = "float: right;")
          )
        )
      ),
      
      tabItem(
        tabName = "MainResultsSimulation",
        fluidRow(
          column(12, DTOutput("PIT_SUMMARY_TABLES"))
        )
      ),
      
      tabItem(
        tabName = "MainResultsTE",
        fluidRow(
          column(12, DTOutput("TE_TABLES"))
        )
      ),
      
      tabItem(
        tabName = "MainRedistributionEffects",
        fluidRow(
          column(12, DTOutput("RE_TABLES"))
        )
      ),
      
      tabItem(
        tabName = "MainDistributionTables",
        fluidRow(
          column(12, DTOutput("DIST_TABLES"))
        )
      ),
      
      tabItem(
        tabName = "MainResultBins",
        fluidRow(
          column(12, DTOutput("BIN_TABLES"))
        )
      ),
      
      tabItem(
        tabName = "PIT_Revenues",
        fluidRow(
          column(
            6,
            selectInput(
              "chartSelectPIT_Revenues",
              "Select Chart",
              choices = c(
                "Structure_Charts",
                #"Revenue_Charts"
               "Distribution_Charts"
              ),
              selected = "Structure_Charts"
            )
          )
        ),
        fluidRow(
          infoBoxOutput("infoBox1", width = 6),
          infoBoxOutput("infoBox2", width = 6)
        ),
        fluidRow(
          column(12, uiOutput("additionalCharts"))
        )
      )
    )
  )
)

# II. Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # I. Header image ----------------------------------------------------------
  
  output$headerImage <- renderUI({
    img_data <- base64enc::dataURI(file = "img/WB_pic.png", mime = "image/png")
    tags$img(
      src = img_data,
      height = "40px",
      style = "float:left; margin-right:20px;"
    )
  })
  
  # II. Simulation year ------------------------------------------------------
  
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
  
  # III. Reactive Excel data -------------------------------------------------
  
  excelData <- reactiveVal(NULL)
  
  observeEvent(input$importExcel, {
    
    req(input$fileInput)
    
    inFile <- input$fileInput
    
    if (!is.null(inFile)) {
      
      data <- read_excel(inFile$datapath, col_names = input$hasHeader)
      
      required_cols <- c(
        "Year",
        "Regime",
        "PolicyParameter",
        "Descriptions",
        "LongName",
        "Parameters",
        "Value"
      )
      
      missing_cols <- setdiff(required_cols, colnames(data))
      
      if (length(missing_cols) > 0) {
        showModal(modalDialog(
          title = "Error",
          paste0(
            "The Excel file must contain the following columns: ",
            paste(required_cols, collapse = ", "),
            ". Missing: ",
            paste(missing_cols, collapse = ", ")
          ),
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      data <- data %>%
        mutate(
          Year = as.numeric(gsub("[^0-9.]", "", as.character(Year))),
          Value = as.numeric(gsub("[^0-9.\\-]", "", as.character(Value))),
          Regime = as.character(Regime),
          PolicyParameter = as.character(PolicyParameter),
          Descriptions = as.character(Descriptions),
          LongName = as.character(LongName),
          Parameters = as.character(Parameters)
        )
      
      data <- as.data.table(data)
      
      excelData(data)
      
      assign("pit_simulation_parameters_raw", copy(data), envir = .GlobalEnv)
      assign("pit_simulation_parameters_updated", copy(data), envir = .GlobalEnv)
      
      cat("Excel data imported successfully\n")
    }
  })
  
  # IV. Reactive update table ------------------------------------------------
  
  pit_simulation_parameters_updated <- reactiveVal(data.table())
  
  # V. Parameter year dropdown ----------------------------------------------
  
  output$ParameterYearSelect <- renderUI({
    
    if (!is.null(excelData())) {
      
      available_years <- sort(unique(excelData()$Year))
      
      selectInput(
        "ParameterYearSelect",
        "Parameter year",
        choices = available_years,
        selected = ifelse(
          input$SimulationYear %in% available_years,
          input$SimulationYear,
          min(available_years, na.rm = TRUE)
        )
      )
      
    } else {
      
      selectInput(
        "ParameterYearSelect",
        "Parameter year",
        choices = NULL
      )
    }
  })
  
  # VI. Regime dropdown ------------------------------------------------------
  
  output$RegimeSelect <- renderUI({
    
    req(input$ParameterYearSelect)
    
    if (!is.null(excelData())) {
      
      dt <- excelData() %>%
        filter(Year == input$ParameterYearSelect)
      
      selectInput(
        "RegimeSelect",
        "Regime",
        choices = unique(dt$Regime)
      )
      
    } else {
      
      selectInput(
        "RegimeSelect",
        "Regime",
        choices = NULL
      )
    }
  })
  
  # VII. Policy parameter dropdown ------------------------------------------
  
  output$PolicyParameter <- renderUI({
    
    req(input$ParameterYearSelect)
    req(input$RegimeSelect)
    
    if (!is.null(excelData())) {
      
      dt <- excelData() %>%
        filter(
          Year == input$ParameterYearSelect,
          Regime == input$RegimeSelect
        )
      
      selectInput(
        "PolicyParameter",
        "Policy Parameter Selection",
        choices = unique(dt$PolicyParameter)
      )
      
    } else {
      
      selectInput(
        "PolicyParameter",
        "Policy Parameter Selection",
        choices = NULL
      )
    }
  })
  
  # VIII. Description dropdown ----------------------------------------------
  
  output$Descriptions_Select <- renderUI({
    
    req(input$ParameterYearSelect)
    req(input$RegimeSelect)
    req(input$PolicyParameter)
    
    if (!is.null(excelData())) {
      
      dt <- excelData() %>%
        filter(
          Year == input$ParameterYearSelect,
          Regime == input$RegimeSelect,
          PolicyParameter == input$PolicyParameter
        )
      
      selectInput(
        "Descriptions_Select",
        "Description of parameter",
        choices = unique(dt$Descriptions)
      )
      
    } else {
      
      selectInput(
        "Descriptions_Select",
        "Description of parameter",
        choices = NULL
      )
    }
  })
  
  # IX. LongName dropdown ----------------------------------------------------
  
  output$LongNameSelect <- renderUI({
    
    req(input$ParameterYearSelect)
    req(input$RegimeSelect)
    req(input$PolicyParameter)
    req(input$Descriptions_Select)
    
    if (!is.null(excelData())) {
      
      dt <- excelData() %>%
        filter(
          Year == input$ParameterYearSelect,
          Regime == input$RegimeSelect,
          PolicyParameter == input$PolicyParameter,
          Descriptions == input$Descriptions_Select
        )
      
      selectInput(
        "LongNameSelect",
        "Selected variable",
        choices = unique(dt$LongName)
      )
      
    } else {
      
      selectInput(
        "LongNameSelect",
        "Selected variable",
        choices = NULL
      )
    }
  })
  
  # X. Update value box based on selected year/regime/parameter --------------
  
  observeEvent(
    list(
      input$ParameterYearSelect,
      input$RegimeSelect,
      input$PolicyParameter,
      input$Descriptions_Select,
      input$LongNameSelect
    ),
    {
      req(input$ParameterYearSelect)
      req(input$RegimeSelect)
      req(input$PolicyParameter)
      req(input$Descriptions_Select)
      req(input$LongNameSelect)
      req(excelData())
      
      selected_row <- excelData() %>%
        filter(
          Year == input$ParameterYearSelect,
          Regime == input$RegimeSelect,
          PolicyParameter == input$PolicyParameter,
          Descriptions == input$Descriptions_Select,
          LongName == input$LongNameSelect
        )
      
      if (nrow(selected_row) >= 1) {
        
        selected_row <- selected_row[1, ]
        
        updateNumericInput(session, "default_Value", value = selected_row$Value)
        updateNumericInput(session, "default_Year", value = selected_row$Year)
        
        cat("Selected parameter:\n")
        print(selected_row)
        
      } else {
        
        cat("No matching row found for selected year, regime and parameter.\n")
      }
    },
    ignoreInit = TRUE
  )
  
  # XI. Add selected parameter update to table -------------------------------
  
  observeEvent(input$addValuesValue, {
    
    req(input$ParameterYearSelect)
    req(input$RegimeSelect)
    req(input$PolicyParameter)
    req(input$Descriptions_Select)
    req(input$LongNameSelect)
    req(excelData())
    
    selected_year <- as.numeric(input$ParameterYearSelect)
    selected_value <- input$default_Value
    
    all_years <- sort(unique(excelData()$Year))
    
    if (isTRUE(input$applyToFutureYears)) {
      years_to_update <- all_years[all_years >= selected_year]
    } else {
      years_to_update <- selected_year
    }
    
    new_entries_list <- lapply(years_to_update, function(yy) {
      
      row_yy <- excelData() %>%
        filter(
          Year == yy,
          Regime == input$RegimeSelect,
          PolicyParameter == input$PolicyParameter,
          Descriptions == input$Descriptions_Select,
          LongName == input$LongNameSelect
        )
      
      if (nrow(row_yy) == 0) {
        
        row_template <- excelData() %>%
          filter(
            Year == selected_year,
            Regime == input$RegimeSelect,
            PolicyParameter == input$PolicyParameter,
            Descriptions == input$Descriptions_Select,
            LongName == input$LongNameSelect
          )
        
        if (nrow(row_template) == 0) {
          return(NULL)
        }
        
        row_yy <- row_template[1, ]
        row_yy$Year <- yy
        
      } else {
        
        row_yy <- row_yy[1, ]
      }
      
      row_yy$Value <- selected_value
      
      as.data.table(row_yy)
    })
    
    new_entries <- rbindlist(new_entries_list, use.names = TRUE, fill = TRUE)
    
    if (nrow(new_entries) == 0) {
      showModal(modalDialog(
        title = "Error",
        "No matching parameter row was found.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    current_updates <- pit_simulation_parameters_updated()
    
    combined_updates <- rbindlist(
      list(current_updates, new_entries),
      use.names = TRUE,
      fill = TRUE
    )
    
    key_cols <- intersect(
      c("Year", "Regime", "PolicyParameter", "Descriptions", "LongName", "Parameters"),
      names(combined_updates)
    )
    
    combined_updates <- combined_updates[
      !duplicated(combined_updates[, ..key_cols], fromLast = TRUE)
    ]
    
    setorder(combined_updates, Year, Regime, PolicyParameter, LongName)
    
    pit_simulation_parameters_updated(combined_updates)
    
    cat("New entries added to pit_simulation_parameters_updated:\n")
    print(new_entries)
  })
  
  # XII. Clear update table --------------------------------------------------
  
  observeEvent(input$clearValuesTable, {
    
    pit_simulation_parameters_updated(data.table())
    
    cat("pit_simulation_parameters_updated table cleared\n")
  })
  
  # XIII. Save updated parameters to GlobalEnv -------------------------------
  
  observeEvent(input$savepit_simulation_parameters_updated, {
    
    req(excelData())
    
    ValueTableUpdate <- as.data.table(pit_simulation_parameters_updated())
    
    assign("ValueTableUpdate", copy(ValueTableUpdate), envir = .GlobalEnv)
    
    pit_simulation_parameters_updated_copy <- as.data.table(copy(excelData()))
    
    if (nrow(ValueTableUpdate) > 0) {
      
      for (i in seq_len(nrow(ValueTableUpdate))) {
        
        row <- ValueTableUpdate[i]
        
        if ("Parameters" %in% names(row) &&
            "Parameters" %in% names(pit_simulation_parameters_updated_copy)) {
          
          pit_simulation_parameters_updated_copy[
            Year == row$Year &
              Regime == row$Regime &
              PolicyParameter == row$PolicyParameter &
              Descriptions == row$Descriptions &
              LongName == row$LongName &
              Parameters == row$Parameters,
            Value := row$Value
          ]
          
        } else {
          
          pit_simulation_parameters_updated_copy[
            Year == row$Year &
              Regime == row$Regime &
              PolicyParameter == row$PolicyParameter &
              Descriptions == row$Descriptions &
              LongName == row$LongName,
            Value := row$Value
          ]
        }
      }
    }
    
    assign(
      "pit_simulation_parameters_updated",
      copy(pit_simulation_parameters_updated_copy),
      envir = .GlobalEnv
    )
    
    cat("pit_simulation_parameters_updated assigned to GlobalEnv with year/regime-specific updates\n")
  })
  
  # XIV. Render update table -------------------------------------------------
  
  output$pit_simulation_parameters_updated <- renderDT({
    
    datatable(
      pit_simulation_parameters_updated(),
      options = list(
        dom = 't',
        paging = FALSE,
        scrollX = TRUE
      ),
      editable = TRUE,
      rownames = FALSE
    )
  })
  
  # XV. Simulation -----------------------------------------------------------
  
  reactive_simulation_results <- reactiveVal()
  
  observeEvent(input$calc_Customs_Sim_Button, {
    
    if (is.null(excelData())) {
      showModal(modalDialog(
        title = "Error",
        "No Excel file has been imported. Please import the PIT policy parameter file first.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    if (!exists("pit_simulation_parameters_updated", envir = .GlobalEnv)) {
      assign("pit_simulation_parameters_updated", copy(excelData()), envir = .GlobalEnv)
    }
    
    showModal(modalDialog(
      title = "Running Simulation...",
      "Please wait while the simulation is running...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    future({
      
      source("Scripts/PIT/TaxCalculator_Subset1_v3.R")
      source("Scripts/PIT/TaxCalculator_Subset2_v2.R")
      source("Scripts/PIT/TaxCalculator_Subset3_v3.R")
      source("Scripts/PIT/Calc-AggregationOfData_v3.R")
      #source("Scripts/PIT/Calc-Distribution-Effects.R")
      #source("Scripts/PIT/Calc-Structure.R")
      #source("Scripts/PIT/Calc-Redistribution-Effects.R")
      
      list(
        pit_summary_df = get("pit_summary_df", envir = .GlobalEnv),
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
  
  # XVI. Main PIT summary table ---------------------------------------------
  
  output$PIT_SUMMARY_TABLES <- renderDT({
    
    req(reactive_simulation_results())
    
    datatable(
      reactive_simulation_results()$pit_summary_df,
      caption = tags$caption(
        paste("PIT Projections,", min(forecast_horizon), "-", max(forecast_horizon)),
        class = "table-caption-bold"
      ),
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
                  return $('<div>').html(data).text();
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
                  return $('<div>').html(data).text();
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
                  return $('<div>').html(data).text();
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
  
  # XVII. Redistribution effects table ---------------------------------------
  
  output$RE_TABLES <- renderDT({
    
    req(reactive_simulation_results())
    
    datatable(
      reactive_simulation_results()$re_effects_final,
      caption = tags$caption(
        paste("Redistributive Effects,", simulation_year),
        class = "table-caption-bold"
      ),
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
                  return $('<div>').html(data).text();
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
                  return $('<div>').html(data).text();
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
                  return $('<div>').html(data).text();
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
  
  # XVIII. Distribution table ------------------------------------------------
  
  output$DIST_TABLES <- renderDT({
    
    req(reactive_simulation_results())
    
    datatable(
      reactive_simulation_results()$pit_decile_distribution_bu_sim,
      caption = tags$caption(
        paste("Distribution Tables LCU,", SimulationYear),
        class = "table-caption-bold"
      ),
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
                  return $('<div>').html(data).text();
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
                  return $('<div>').html(data).text();
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
                  return $('<div>').html(data).text();
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
  
  # XIX. Income-bin table ----------------------------------------------------
  
  output$BIN_TABLES <- renderDT({
    
    req(reactive_simulation_results())
    
    datatable(
      reactive_simulation_results()$pit_result_bins_sim_sub,
      caption = tags$caption(
        paste("Structure of PIT liability by income groups, ", simulation_year),
        class = "table-caption-bold"
      ),
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
                  return $('<div>').html(data).text();
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
                  return $('<div>').html(data).text();
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
                  return $('<div>').html(data).text();
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
  
  # XX. Charts ---------------------------------------------------------------
  
  updateCharts <- function() {
    
    cat("Updating charts after simulation\n")
    
    chart_type <- isolate(input$chartSelectPIT_Revenues)
    
    cat("Selected chart type:", chart_type, "\n")
    
    if (exists("merged_PIT_BU_SIM", envir = .GlobalEnv) &&
        exists("forecast_horizon", envir = .GlobalEnv)) {
      
      merged_PIT_BU_SIM <- get("merged_PIT_BU_SIM", envir = .GlobalEnv)
      forecast_horizon <- get("forecast_horizon", envir = .GlobalEnv)
      
      if (chart_type == "Revenue_Charts") {
        
        cat("Preparing Revenue_Charts charts\n")
        
        source("Scripts/PIT/Charts-PIT_Revenues.R")
        
        charts <- Revenue_Charts(
          merged_PIT_BU_SIM,
          range(forecast_horizon)
        )
        
        output$infoBox1 <- renderInfoBox({
          
          infobox_pitax_bu <- merged_PIT_BU_SIM %>%
            dplyr::select(year, pitax_bu) %>%
            dplyr::filter(year == SimulationYear) %>%
            dplyr::select(-c(year))
          
          infoBox(
            title = paste("(Business as usual)", SimulationYear),
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
            value = paste(round(infobox_pitax_sim$pitax_sim[1], 1), "(in MIL LCU)"),
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
        
        Charts_structure <- Structure_GrossIncome_Charts(
          structure_gross_inc,
          structure_pit,
          long_df,
          SimulationYear
        )
        
        output$infoBox1 <- renderInfoBox({
          infoBox(
            title = " ",
            icon = icon("chart-area"),
            color = "orange"
          )
        })
        
        output$infoBox2 <- renderInfoBox({
          infoBox(
            title = " ",
            value = NULL,
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
        
        charts_dist <- Distribution_Charts(merged_PIT_BU_SIM,
                                            pit_centile_distribution_bu_sim,
                                            pit_decile_distribution_bu_sim_raw,
                                            pit_result_bins_bu_sub,
                                            pit_result_bins_sim_sub,
                                            simulation_year
        )
        
        output$infoBox1 <- renderInfoBox({
          infoBox(
            "Average Tax Rate (Business as usual)",
            value = round(re_effects_final$`Business as usual`[1]*100, 2),
            icon = icon("percent"),
            color = "orange"
          )
        })
        
        output$infoBox2 <- renderInfoBox({
          infoBox(
            "Average Tax Rate (Simulation)",
            value = round(re_effects_final$Simulation[1]*100, 2),
            icon = icon("percent"),
            color = "light-blue"
          )
        })
        
        output$chartOutputPIT <- renderPlotly({ charts_dist$dist_centile_groups_plt })
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("labor_capital_plt", height = "400px")),
              column(6, plotlyOutput("labor_capital_type_plt", height = "400px"))
            ),
            fluidRow(
              #column(6, plotlyOutput("pit_bins_bu_sub_plt", height = "400px")),
              column(6, plotlyOutput("PIT_RevenuesTotal_plt", height = "400px")),
              column(6, plotlyOutput("treemap_nace_type_plt", height = "400px"))
            )
          )
        })
        
        output$labor_capital_plt <- renderPlotly({ charts_dist$dist_centile_groups_plt })
        output$labor_capital_type_plt <- renderPlotly({ charts_dist$dist_decile_groups_plt })
        #output$pit_bins_bu_sub_plt <- renderPlotly({ charts_dist$pit_bins_bu_sub_plt })
        output$PIT_RevenuesTotal_plt <- renderPlotly({ charts_dist$PIT_RevenuesTotal_plt })
        output$treemap_nace_type_plt <- renderPlotly({ charts_dist$pit_bins_sim_sub_plt })
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