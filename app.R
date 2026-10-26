knitr::opts_chunk$set(echo = TRUE)

# clones repository to computer running app
system("git clone  https://github.com/oystersdukebc/summer-2026-visualization-tool.git")

# pulls in repository and sets it as the working directory
setwd("summer-2026-visualization-tool")
system("git pull origin main")  # downloads most recent changes from main branch

# sets "Cleaned" folder as the working directory
setwd("Cleaned")

# cleans up R environment
rm(list = ls())

# load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(readxl)

# load data
load_accel <- function(file, farm, type) {
  accel_data <- read_excel(file)
  accel_data %>%
    mutate(
      DateTime = ymd_hms(DateTime),
      across(
        c(X_Acc_G, Y_Acc_G, Z_Acc_G, Sum_Acc_G),
        as.numeric
      ),
      Farm = farm,
      Type = type,
      Dataset = paste(farm, type),
      Month = month(DateTime),
      Day = day(DateTime)
    )
}

acc_summer <- bind_rows(
  load_accel("DUML_G_Floating_2026.xlsx", "DUML", "Floating"),
  load_accel("DUML_G_Seapa_2026.xlsx", "DUML", "Seapa"),
  load_accel("PIL_G_Floating_2026.xlsx", "PIL", "Floating"),
  load_accel("PIL_G_Seapa_2026.xlsx", "PIL", "Seapa"),
  load_accel("MOF_G_Floating_2026.xlsx", "MOF", "Floating")
) %>%
  filter(
    !is.na(DateTime),
    Month %in% 5:9
  ) %>%
  arrange(Farm, Type, DateTime)

# Choices
farm_order <- c("PIL", "DUML", "MOF")
type_order <- c("Floating", "Seapa")

variable_choices <- c(
  "X Acceleration" = "X_Acc_G",
  "Y Acceleration" = "Y_Acc_G",
  "Z Acceleration" = "Z_Acc_G",
  "Sum Acceleration" = "Sum_Acc_G"
)

dataset_colors <- c(
  "DUML Floating" = "#1f77b4",
  "DUML Seapa" = "#cb181d",
  "PIL Floating" = "#238b45",
  "PIL Seapa" = "#9467bd",
  "MOF Floating" = "#ff7f0e"
)

# UI
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    /* Uniform sidebar text */
    .well {
      font-family: Arial, sans-serif;
      font-size: 14px;
    }

    .well .control-label,
    .well h4 {
      font-size: 15px;
      font-weight: 600;
      line-height: 1.4;
      margin-bottom: 10px;
    }

    .well .checkbox label,
    .well .radio label,
    .well .form-control,
    .well .selectize-input,
    .well .selectize-dropdown,
    .well .help-block,
    #stats_variable {
      font-size: 14px;
      line-height: 1.4;
    }

    /* Fit the statistics table within the sidebar */
    #summary_stats table {
      width: 100%;
      table-layout: fixed;
      font-size: 13px;
    }

    #summary_stats th,
    #summary_stats td {
      padding: 6px 4px;
      white-space: normal;
      overflow-wrap: anywhere;
      vertical-align: middle;
    }

    /* Thicker legend samples only */
    #acc_plot .legend .js-line {
      stroke-width: 4px !important;
    }
  "))
  ),
  
  titlePanel("Acceleration Data Visualizer (ADV)"),
  
  tabsetPanel(
    id = "month_tabs",
    type = "tabs",
    tabPanel("Full Summer", value = "full"),
    tabPanel("May", value = "5"),
    tabPanel("June", value = "6"),
    tabPanel("July", value = "7"),
    tabPanel("August", value = "8"),
    tabPanel("September", value = "9")
  ),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      
      checkboxGroupInput(
        "farm_select",
        "Select Farms:",
        choices = farm_order,
        selected = farm_order
      ),
      
      hr(),
      
      checkboxGroupInput(
        "type_select",
        "Select Bag Type:",
        choices = type_order,
        selected = type_order
      ),
      
      hr(),
      
      selectInput(
        "variable",
        "Select Variable:",
        choices = variable_choices,
        selected = "Sum_Acc_G"
      ),
      
      conditionalPanel(
        condition = "input.month_tabs != 'full'",
        
        hr(),
        
        radioButtons(
          "biweek",
          "Select Time Interval:",
          choices = c(
            "Full Month" = "full",
            "First Biweek" = "1",
            "Second Biweek" = "2"
          ),
          selected = "full"
        )
      ),
      
      hr(),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = min(as.Date(acc_summer$DateTime), na.rm = TRUE),
        end = max(as.Date(acc_summer$DateTime), na.rm = TRUE),
        min = min(as.Date(acc_summer$DateTime), na.rm = TRUE),
        max = max(as.Date(acc_summer$DateTime), na.rm = TRUE)
      ),
      
      hr(),
      
      textInput(
        "custom_title",
        "Edit Plot Title:",
        value = "2026 Acceleration Data"
      ),

      hr(),
      
      h4("Summary Statistics"),
      
      textOutput("stats_variable"),
      
      tableOutput("summary_stats"),

      width = 3
    ),
    
    mainPanel(
      plotlyOutput("acc_plot", height = "700px"),
      hr(),
      helpText(
        "Note: Table automatically updates based on time interval, farm, and bag type selections made in the left-hand panel. Manually zooming in/out on the plot will not update table."
      ),
      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # offer only bag types available at the selected farms.
  observeEvent(input$farm_select, {
    
    available_types <- acc_summer %>%
      filter(Farm %in% input$farm_select) %>%
      pull(Type) %>%
      unique()
    
    available_types <- type_order[
      type_order %in% available_types
    ]
    
    selected_types <- intersect(
      input$type_select,
      available_types
    )
    
    if (length(selected_types) == 0) {
      selected_types <- available_types
    }
    
    updateCheckboxGroupInput(
      session,
      "type_select",
      choices = available_types,
      selected = selected_types
    )
    
  }, ignoreNULL = FALSE)
  
  # reset to the full month when opening a different tab
  observeEvent(input$month_tabs, {
    updateRadioButtons(
      session,
      "biweek",
      selected = "full"
    )
  })
  
  # display name of the selected variable.
  variable_label <- reactive({
    req(input$variable)
    
    names(variable_choices)[
      match(input$variable, unname(variable_choices))
    ]
  })
  
  # Update the date fields when a month or biweek is selected.
  observeEvent(
    list(input$month_tabs, input$biweek),
    {
      req(input$month_tabs)
      
      summer_start <- as.Date("2026-05-01")
      summer_end <- as.Date("2026-09-30")
      
      if (input$month_tabs == "full") {
        
        start_date <- min(
          as.Date(acc_summer$DateTime),
          na.rm = TRUE
        )
        
        end_date <- max(
          as.Date(acc_summer$DateTime),
          na.rm = TRUE
        )
        
      } else {
        
        start_date <- as.Date(
          sprintf("2026-%02d-01", as.integer(input$month_tabs))
        )
        
        end_date <- ceiling_date(
          start_date,
          unit = "month"
        ) - days(1)
        
        if (identical(input$biweek, "1")) {
          end_date <- start_date + days(14)
        }
        
        if (identical(input$biweek, "2")) {
          start_date <- start_date + days(15)
        }
      }
      
      updateDateRangeInput(
        session,
        "date_range",
        start = start_date,
        end = end_date,
        min = summer_start,
        max = summer_end
      )
    },
    ignoreInit = FALSE
  )
  
  # shared data for the plot and statistics table
  filtered_data <- reactive({
    req(input$month_tabs, input$variable, input$date_range)
    
    df <- acc_summer %>%
      filter(
        as.Date(DateTime) >= input$date_range[1],
        as.Date(DateTime) <= input$date_range[2],
        Farm %in% input$farm_select,
        Type %in% input$type_select
      )
    
    if (input$month_tabs != "full") {
      
      df <- df %>%
        filter(Month == as.numeric(input$month_tabs))
      
      if (identical(input$biweek, "1")) {
        df <- df %>% filter(Day <= 15)
      }
      
      if (identical(input$biweek, "2")) {
        df <- df %>% filter(Day >= 16)
      }
    }
    
    df
  })
  
# summary statistics
  output$stats_variable <- renderText({
    paste0(variable_label(), " (G)")
  })
  
  output$summary_stats <- renderTable({
    
    df <- filtered_data()
    
    df %>%
      mutate(
        Value = .data[[input$variable]],
        Farm = factor(Farm, levels = farm_order),
        Type = factor(Type, levels = type_order)
      ) %>%
      group_by(Farm, Type) %>%
      summarise(
        Mean = {
          values <- Value[is.finite(Value)]
          if (length(values) > 0) mean(values) else NA_real_
        },
        Max = {
          values <- Value[is.finite(Value)]
          if (length(values) > 0) max(values) else NA_real_
        },
        Min = {
          values <- Value[is.finite(Value)]
          if (length(values) > 0) min(values) else NA_real_
        },
        .groups = "drop"
      ) %>%
      mutate(Range = Max - Min) %>%
      arrange(Farm, Type) %>%
      rename(`Bag Type` = Type)
    
  },
  digits = 3,
  striped = TRUE,
  bordered = TRUE,
  spacing = "xs",
  rownames = FALSE,
  na = "—"
  )
  
  plot_title <- reactive({
    input$custom_title
  })
  
# plot
  output$acc_plot <- renderPlotly({
    
    plot_data <- filtered_data()
    
    p <- plot_ly(
      type = "scatter",
      mode = "lines"
    )
    
    for (farm in farm_order) {
      for (data_type in type_order) {
        
        trace_data <- plot_data %>%
          filter(Farm == farm, Type == data_type) %>%
          arrange(DateTime)
        
        if (nrow(trace_data) == 0) {
          next
        }
        
        dataset_name <- paste(farm, data_type)
        
        # treat nonfinite values as gaps in the plot
        y_values <- trace_data[[input$variable]]
        y_values[!is.finite(y_values)] <- NA_real_
        
        p <- add_lines(
          p,
          x = trace_data$DateTime,
          y = y_values,
          name = data_type,
          legendgroup = farm,
          legendgrouptitle = list(text = farm),
          text = rep(dataset_name, nrow(trace_data)),
          hovertemplate = paste0(
            "%{text}<br>",
            "%{x|%b %d, %Y %H:%M}<br>",
            variable_label(),
            ": %{y:.3f} G",
            "<extra></extra>"
          ),
          line = list(
            color = unname(dataset_colors[dataset_name]),
            width = 2
          ),
          connectgaps = FALSE
        )
      }
    }
    
    p %>%
      layout(
        title = list(text = plot_title()),
        
        xaxis = list(
          title = "Date & Time",
          tickformat = "%b %d\n%H:%M"
        ),
        
        yaxis = list(
          title = paste0(variable_label(), " (G)"),
          autorange = TRUE
        ),
        
        legend = list(
          orientation = "v",
          traceorder = "grouped",
          groupclick = "toggleitem",
          x = 1.02,
          y = 1,
          xanchor = "left",
          yanchor = "top",
          tracegroupgap = 15
        ),
        
        margin = list(r = 140)
      )
  })
}

# Run app
shinyApp(ui, server)