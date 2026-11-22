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
library(shiny) # builds interactive app
library(tidyverse)
library(lubridate) # data and times
library(plotly) # interactive graphs
library(readxl) # imports Excel files
library(shinyWidgets) # additional capabilities for app

# time interval function
add_time_columns <- function(df){
  df %>%
    mutate(
      Year = year(DateTime),
      Month = month(DateTime),
      Day = day(DateTime),
      OverlayTime = update(DateTime, year = 2000)
    )
}
# overlay time changes every date to year 2000 without changing month, day, or time.
# this allows different years to be plotted on top of each other.

# another time interval function
filter_time <- function(df, month_val, biweek_val){
  if(month_val != "full"){
    df <- df %>% filter(Month == as.numeric(month_val))
  }
  
  if(biweek_val == "1"){
    df <- df %>% filter(Day >= 1 & Day <= 15)
  }
  
  if(biweek_val == "2"){
    df <- df %>% filter(Day >= 16)
  }
  
  df
}

# load data functions
load_do <- function(file, farm){
  read_excel(file) %>%
    mutate(
      DateTime = ymd_hms(DateTime),
      Year = year(DateTime),
      DO = as.numeric(DomgL),
      Temp_C = as.numeric(Temp_C),
      Farm = farm)  %>%
    add_time_columns() %>%
    filter(DO >= 0 | is.na(DO)) %>% # this line could be removed after proper data cleaning :)
    select(DateTime, OverlayTime, Year, Month, Day, Farm, DO, Temp_C) # only columns needed for app
}

load_ph <- function(file, farm){
  read_excel(file) %>%
    mutate(
      DateTime = ymd_hms(DateTime),
      Year = year(DateTime),
      pH = as.numeric(pH),
      Farm = farm) %>%
    add_time_columns() %>%
    select(DateTime, OverlayTime, Year, Month, Day, Farm, pH)
}

load_sal <- function(file, farm){
  read_excel(file) %>%
    mutate(
      DateTime = ymd_hms(DateTime),
      Salinity = as.numeric(Sal_ppt), # also renames column
      Farm = farm) %>%
    add_time_columns() %>%
    select(DateTime, OverlayTime, Year, Month, Day, Farm, Salinity)
}

# Precipitation measurements are available only for 2026.
load_precip <- function(file, farm){
  read_excel(file) %>%
    mutate(
      DateTime = ymd_hms(DateTime),
      Precip_in = as.numeric(Precip_in),
      Farm = farm
    ) %>%
    add_time_columns() %>%
    filter(Year == 2026) %>%
    arrange(DateTime) %>%
    select(DateTime, OverlayTime, Year, Month, Day, Farm, Precip_in)
}

moyst_precip_26 <- load_precip("Moyst_Precip_2026.xlsx", "MOF")
pil_precip_26   <- load_precip("PIL_Precip_2026.xlsx", "PIL")
duml_precip_26  <- load_precip("DUML_Precip_2026.xlsx", "DUML")

cmast_do   <- load_do("CMAST_DO_Cleaned.xlsx","CMAST")
cmast_ph   <- load_ph("CMAST_pH_Cleaned.xlsx","CMAST")
cmast_sal  <- load_sal("CMAST_Con_Cleaned.xlsx","CMAST")

stump_do   <- load_do("StumpSound_DO_Cleaned.xlsx","Stump Sound")
stump_ph   <- load_ph("StumpSound_pH_Cleaned.xlsx","Stump Sound")
stump_sal  <- load_sal("StumpSound_Con_Cleaned.xlsx","Stump Sound")

ward_do    <- load_do("WardCreek_DO_Cleaned.xlsx","Ward Creek")
ward_ph    <- load_ph("WardCreek_pH_Cleaned.xlsx","Ward Creek")
ward_sal   <- load_sal("WardCreek_Con_Cleaned.xlsx","Ward Creek")

duml_do    <- load_do("DUML_DO_Cleaned.xlsx","DUML")
duml_ph    <- load_ph("DUML_pH_Cleaned.xlsx","DUML")
duml_sal   <- load_sal("DUML_Con_Cleaned.xlsx","DUML")

nelson_do  <- load_do("Nelson_DO_Cleaned.xlsx","Nelson Bay")
nelson_ph  <- load_ph("Nelson_pH_Cleaned.xlsx","Nelson Bay")
nelson_sal <- load_sal("Nelson_Con_Cleaned.xlsx","Nelson Bay")

duml_do_26 <- read_excel("DUML_DO_2026.xlsx") %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    DO = as.numeric(DomgL),
    DO_Adj = as.numeric(DomgL_Adj),
    Temp_C = as.numeric(Temp_C),
    Farm = "DUML"
  ) %>%
  add_time_columns() %>%
  filter(DO >= 0 | is.na(DO)) %>%
  filter(DO_Adj >= 0 | is.na(DO_Adj)) %>%
  select(DateTime, OverlayTime, Year, Month, Day,
         Farm, DO, DO_Adj, Temp_C)

duml_ph_26  <- load_ph("DUML_pH_2026.xlsx", "DUML")
duml_sal_26 <- load_sal("DUML_Con_2026.xlsx", "DUML")

pil_do_26 <- read_excel("PIL_DO_2026.xlsx") %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    DO = as.numeric(DomgL),
    DO_Adj = as.numeric(DomgL_Adj),
    Temp_C = as.numeric(Temp_C),
    Farm = "PIL"
  ) %>%
  add_time_columns() %>%
  filter(DO >= 0 | is.na(DO)) %>%
  select(DateTime, OverlayTime, Year, Month, Day,
         Farm, DO, DO_Adj, Temp_C)

pil_ph_26  <- load_ph("PIL_pH_2026.xlsx", "PIL")
pil_sal_26 <- load_sal("PIL_Con_2026.xlsx", "PIL")

moyst_do_26 <- read_excel("Moyst_DO_2026.xlsx") %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    DO = as.numeric(DomgL),
    DO_Adj = as.numeric(DomgL_Adj),
    Temp_C = as.numeric(Temp_C),
    Farm = "MOF"
  ) %>%
  add_time_columns() %>%
  filter(DO >= 0 | is.na(DO)) %>%
  select(DateTime, OverlayTime, Year, Month, Day,
         Farm, DO, DO_Adj, Temp_C)

moyst_ph_26  <- load_ph("Moyst_pH_2026.xlsx", "MOF")
moyst_sal_26 <- load_sal("Moyst_Con_2026.xlsx", "MOF")

# these load data functions for our 2024 data were written by Desa, another member of the project team.
# in an ideal world, datasets are uniform and we combine these load functions. :) 2024 files contain data from multiple farms and locations, which is why additional filtering is necessary.
load_do_24 <- function(file, farm){
  read_excel(file) %>%
    mutate(
      DateTime = ymd_hms(DateTime),
      Farm = as.character(Farm),
      Location = as.character(Location),
    ) %>%
    filter(Farm == farm, Location == "Array") %>%
    mutate(
      Year = year(DateTime),
      DO = as.numeric(DomgL),
      Temp_C = as.numeric(Temp_C)
    ) %>%
    add_time_columns() %>%
    select(DateTime, OverlayTime, Farm, Year, Month, Day, DO, Temp_C)
}

load_ph_24 <- function(file, farm){
  read_excel(file) %>%
    mutate(
      DateTime = ymd_hms(DateTime),
      Farm = as.character(Farm),
      Location = as.character(Location)
    ) %>%
    filter(Farm == farm,
           Location == "Array" | Location == "Line1B") %>%
    mutate(
      Year = year(DateTime),
      pH = as.numeric(pH)
    ) %>%
    add_time_columns() %>%
    select(DateTime, OverlayTime, Farm, Year, Month, Day, pH)
}

load_sal_24 <- function(file, farm){
  read_excel(file) %>%
    mutate(
      DateTime = ymd_hms(DateTime),
      Farm = as.character(Farm),
      Location = as.character(Location)
    ) %>%
    filter(Farm == farm, Location == "Array") %>%
    mutate(
      Year = year(DateTime),
      Salinity = as.numeric(Sal_ppt)
    ) %>%
    add_time_columns() %>%
    select(DateTime, OverlayTime, Farm, Year, Month, Day, Salinity)
}

cmast_do_24  <- load_do_24("DOsensor_2024.xlsx", "CMAST")
cmast_ph_24  <- load_ph_24("pH_2024.xlsx", "CMAST")
cmast_sal_24 <- load_sal_24("Salinity_2024.xlsx", "CMAST")

duml_do_24  <- load_do_24("DOsensor_2024.xlsx", "DUML")
duml_ph_24  <- load_ph_24("pH_2024.xlsx", "DUML")
duml_sal_24 <- load_sal_24("Salinity_2024.xlsx", "DUML")

# ysi data taken from the drive and reformatted in excel.
# mostly for QA/QC purposes
ysi_2026 <- read_excel("YSI_2026.xlsx") %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    Temp_C = as.numeric(Temp_C),
    Salinity = as.numeric(Sal_ppt),
    pH = as.numeric(pH),
    DO = as.numeric(DomgL),
    Year = year(DateTime),
    Farm = as.character(Farm),
    Farm = recode(Farm, "Moyst" = "MOF")) %>%
  add_time_columns() %>%
  select(DateTime, OverlayTime, Year, Month, Day, Farm, Temp_C, Salinity, pH, DO)

ysi_2025 <- read_excel("YSI.xlsx") %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    Temp_C = as.numeric(Temp_C),
    Salinity = as.numeric(Sal_ppt),
    pH = as.numeric(pH),
    DO = as.numeric(DomgL),
    Year = year(DateTime),
    Farm = as.character(Farm)) %>%
  add_time_columns() %>%
  select(DateTime, OverlayTime, Year, Month, Day, Farm, Temp_C, Salinity, pH, DO)

ysi_2024 <- read_excel("YSI_2024.xlsx") %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    Temp_C = as.numeric(Temp_C),
    Salinity = as.numeric(Sal_ppt),
    pH = as.numeric(pH),
    DO = as.numeric(DomgL),
    Year = year(DateTime),
    Farm = as.character(Farm),
    Location = as.character(Location)) %>%
  filter(
    Farm %in% c("DUML", "CMAST"),
    (Farm == "DUML" & Location == "Array") |
      (Farm == "CMAST" & Location == "Line 1B")
  ) %>%
  add_time_columns() %>%
  select(DateTime, OverlayTime, Year, Month, Day, Farm, Temp_C, Salinity, pH, DO)

# combine all three YSI datasets into one
ysi <- bind_rows(ysi_2026, ysi_2025, ysi_2024)

# establish list of farms, parameters, and years that are available for user selection
farm_list  <- c("CMAST", "Stump Sound", "Ward Creek", "DUML", "Nelson Bay", "PIL", "MOF")
param_list <- c(
  "Temperature",
  "Dissolved Oxygen",
  "Adjusted DO",
  "pH",
  "Salinity",
  "Precipitation"
)
year_list <- c("2024", "2025", "2026")

# identify which farms data was collected from for that year
# later, these lists will be used to restrict user selections live
farm_2024 <- c("CMAST", "DUML")
farm_2025 <- c("CMAST", "Stump Sound", "Ward Creek", "DUML", "Nelson Bay")
farm_2026 <- c("DUML", "PIL", "MOF")

# colors! I am not confident these are cb friendly -- something to revisit.
auto_colors <- c(
  "#1f77b4",
  "#cb181d",
  "#238b45",  
  "#9467bd",
  "#ff7f0e",
  "#17becf",
  "#8c564b",
  "#c51b8a",
  "#bcbd22",
  "#EE554E",
  
  "#2171b5",
  "#a50f15",
  "#006d2c",
  "#6a51a3",
  "#980043",
  "#1b9e77",
  "#984ea3",
  "#2ca02c",
  "#3182bd",
  "#de2d26",
  
  "#08519c",
  "#31a354",
  "#dd1c77",
  "#17becf",
  "#d62728",
  "#6baed6",
  "#fb6a4a",
  "#9ecae1",
  "#a1d99b",
  "#bcbddc"  
)

# There are three main components of a Shiny app: User Interface (UI), server, and shinyApp() which connects the two!
# I talk more about what these parts mean throughout the script :)

# User interface begins here!
# The UI controls the layout and appearance of the app.
# UI is built of widgets that we build out here.
# Code should be written in the vertical order that you want it to appear on the app.

ui <- fluidPage( 
  # fluidPage just means layout adjusts to screen size -- not sure what other options exist...
  
  titlePanel("Environmental Data Visualizer (EDV)"),
  
  # create clickable tabs
  # each tab has visible label and internal value (that connects to server)
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
  
  # break (for aesthetics)
  br(),
  
  # create two column layout with sidebar and main panel
  sidebarLayout(
    sidebarPanel(
      
      # conditional panels will appear live only with user selection.
      # in this case, the panel allowing you to break down the visualization into biweeks will only become available if the user selects a specific month.
      # i.e. the condition is, if the month_tabs widget from above does not equal full (for Full Summer), then the app will display this widget.
      conditionalPanel(
        condition = "input.month_tabs != 'full'",
        
        h4("Select Time Interval"),
        radioButtons("biweek", # biweek is the name of the widget used by the server. it is not seen by the user!
                     NULL,
                     choices = c("Full Month" = "full",
                                 "First Biweek" = "1", # these were established before the UI!
                                 "Second Biweek" = "2"),
                     selected = "full"), # this just means that the default selection is the full month
        
        # same as br() but faint horizontal line drawn
        hr()
      ),
      
      h4("Select Years:"),
      checkboxGroupInput("year_select",
                         NULL,
                         year_list, # pre-defined list of choices from above
                         selected = "2026"),
      
      hr(),
      
      h4("Select Farms:"),
      checkboxGroupInput("farm_select", 
                         NULL, 
                         farm_list, 
                         selected = farm_list),
      
      hr(),
      
      h4("Select Parameters:"),
      checkboxGroupInput("param_select", 
                         NULL, 
                         param_list, 
                         selected = param_list),
      
      hr(),
      
      # another type of widget -- a switch! default set to off.
      materialSwitch("show_ysi",
                     "Display YSI Data", # UI label
                     value = FALSE),
      
      hr(),
      
      h4("Edit Plot Title:"),
      textInput("custom_title", NULL, ""),
      width = 3 # makes the sidebar thinner than main panel
    ),
    
    mainPanel(
      plotlyOutput("plot", height = "700px"),
      width = 9
    )
  )
)

# Server starts here!
# The server contains instructions needed to build the app.
# It's really the behind-the-scenes making the widgets functional, connected by widget IDs (I pointed out some above)!

server <- function(input, output, session){
  # input is the values selected by the user
  # output is the objects displayed in the app
  # session allows the serve to update UI controls interactively
  # how I think about it: upon opening the app, the user is in a live session, and their selections will make that session unique!
  
  # this is a reactive programming function that will trigger an action in response to a specific event.
  # this code runs whenever selected years change.
  observeEvent(input$year_select, {
    
    years <- input$year_select # stores selected years in a shorter variable
    
    # create a list of farms that have data for at least one selected year
    available_farms <- unique(c(
      if("2024" %in% years) farm_2024, # e.g., if 2024 is selected, then only the farms in the farm_2024 list will be included
      if("2025" %in% years) farm_2025,
      if("2026" %in% years) farm_2026
    ))
    
    # this just preserves the farms already selected, as long as they are still available.
    current_selected <- input$farm_select
    new_selected <- intersect(current_selected, available_farms)
    
    # if none of the previously selected farms are available, all available farms are selected automatically.
    if(length(new_selected) == 0){
      new_selected <- available_farms
    }
    
    # now, we can update both the choices and selected values in the UI.
    # event (user selection) has been observed, and this is the response of the server to update the UI.
    updateCheckboxGroupInput(
      session,
      "farm_select",
      choices = available_farms,
      selected = new_selected
    )
  })
  
  # onto the next reactive function!
  # let's observe the input into our month_tabs widget.
  # whenever the month tab changes, the server will check whether full was selected for Full Summer.
  # if so, the biweekly setting will be reset to full.
  observeEvent(input$month_tabs, {
    if(input$month_tabs == "full"){
      
      updateRadioButtons(session, 
                         "biweek", 
                         selected = "full")
    }
  })
  
  # automatic title generation
  # To be completely honest, I used ChatGPT to vibe code this part. 
  # If you're interested in learning about what the heck the code is doing here, I would paste it back into ChatGPT and ask for it to explain. Sorry!
  auto_title <- reactive({
    
    pretty_list <- function(x){
      
      if(length(x) == 1){
        x
        
      } else if(length(x) == 2){
        paste(x, collapse = " and ")
        
      } else {
        paste0(
          paste(x[-length(x)], collapse = ", "),
          ", and ",
          x[length(x)]
        )
      }
    }
    
    params <- if(length(input$param_select) == length(param_list)){
      "All Parameters"
    } else {
      pretty_list(input$param_select)
    }
    
    available_farms <- unique(c(
      if("2024" %in% input$year_select) farm_2024,
      if("2025" %in% input$year_select) farm_2025,
      if("2026" %in% input$year_select) farm_2026
    ))
    
    farms <- if(setequal(input$farm_select, available_farms)){
      "All Farms"
    } else {
      pretty_list(input$farm_select)
    }
    
    month_label <- switch(input$month_tabs,
                          "full" = "Full Summer",
                          "5" = "May",
                          "6" = "June",
                          "7" = "July",
                          "8" = "August",
                          "9" = "September")
    
    time_label <- if(input$month_tabs == "full"){
      
      "(Full Summer)"
      
    } else {
      
      biweek_label <- switch(input$biweek,
                             "full" = "",
                             "1" = " - First Biweek",
                             "2" = " - Second Biweek")
      
      paste0("(", month_label, biweek_label, ")")
    }
    
    paste0(
      params,
      " at ",
      farms,
      " ",
      time_label
    )
  })
  
  # Here is another reactive function! 
  # It basically builds off the code above and watches several inputs at once, recalculating the title whenever one changes.
  # Reflecting back -- an interactive title might not have been the best idea. Yes, we are incorporating a lot of data, but I'm sure this component isn't helping the app to run any faster.
  observeEvent(
    list(
      input$year_select, 
      input$farm_select, 
      input$param_select, 
      input$month_tabs, 
      input$biweek
    ),
    { # updates text box with automatically generated title
      updateTextInput(
        session, 
        "custom_title", 
        value = auto_title()
      )
    }, # basically means this runs when the app first starts, not only after user selection
    ignoreInit = FALSE
  )
  
  # reactive function number ?? but this creates the interactive plot!!
  output$plot <- renderPlotly({
    
    # plot is regenerated whenever inputs change
    month_val  <- input$month_tabs
    biweek_val <- input$biweek
    years <- input$year_select
    
    p <- plot_ly()
    
    # empty vector that will eventually contain all ploted y-values
    all_y_values <- c()
    
    color_index <- 0
    trace_colors <- list()
    
    # adds one line to the graph
    add_trace_auto <- function(
    data, 
    yvar, 
    name, 
    farm_name, 
    color_key
    ){
      
      # also from ChatGPT tbh
      if(is.null(trace_colors[[color_key]])){
        color_index <<- color_index + 1
        trace_colors[[color_key]] <<- 
          auto_colors[
            (color_index - 1) %% length(auto_colors) + 1]
      }
      
      # retrieves assigned color
      trace_color <- trace_colors[[color_key]]
      
      data_filtered <- filter_time(data, month_val, biweek_val)
      
      if(nrow(data_filtered) > 0 && yvar %in% colnames(data_filtered)){
        
        all_y_values <<- c(all_y_values, data_filtered[[yvar]])
        
        farm_group <- farm_name
        
        param_label <- switch(
          yvar,
          "Temp_C" = "Temperature",
          "DO" = "DO",
          "DO_Adj" = "Adjusted DO",
          "pH" = "pH",
          "Salinity" = "Salinity",
          "Precip_in" = "Precipitation",
          yvar
        )
        
        trace_label <- if(length(years) > 1){
          paste(param_label, sub(".* ", "", color_key))
        } else {
          param_label
        }
        
        p <<- add_lines(
          p,
          data = data_filtered,
          x = ~OverlayTime,
          y = as.formula(paste0("~", yvar)),
          name = trace_label,
          legendgroup = farm_group,
          legendgrouptitle = list(text = farm_group),
          line = list(color = trace_color, width = 2)
        )
        
        # YSI
        # YSI
        if(isTRUE(input$show_ysi) && yvar != "Precip_in"){
          
          trace_year <- as.numeric(gsub(".* ", "", color_key))
          
          # Use measured YSI DO for the 2026 adjusted DO line.
          ysi_yvar <- if(yvar == "DO_Adj" && trace_year == 2026){
            "DO"
          } else {
            yvar
          }
          
          ysi_filtered <- ysi %>%
            filter(
              Farm == gsub(" 2024| 2025| 2026", "", farm_name),
              Year == trace_year
            ) %>%
            filter_time(month_val, biweek_val)
          
          if(nrow(ysi_filtered) > 0 &&
             ysi_yvar %in% colnames(ysi_filtered)){
            
            all_y_values <<- c(
              all_y_values,
              ysi_filtered[[ysi_yvar]]
            )
            
            p <<- add_markers(
              p,
              data = ysi_filtered,
              x = ~OverlayTime,
              y = as.formula(paste0("~", ysi_yvar)),
              name = paste0(name, " (YSI)"),
              marker = list(
                symbol = "circle",
                size = 6,
                color = trace_color,
                line = list(
                  color = "black",
                  width = 1.5
                )
              ),
              showlegend = FALSE
            )
          }
        }
      }
    }
    
    farms  <- input$farm_select
    params <- input$param_select
    
    show_param <- function(pn) pn %in% params
    
    if("CMAST" %in% farms){
      if("2024" %in% years){
        if(show_param("Temperature")) add_trace_auto(cmast_do_24, "Temp_C", "Temp CMAST", "CMAST", "Temp CMAST 2024")
        if(show_param("Dissolved Oxygen")) add_trace_auto(cmast_do_24, "DO", "DO CMAST", "CMAST", "DO CMAST 2024")
        if(show_param("pH")) add_trace_auto(cmast_ph_24, "pH", "pH CMAST", "CMAST", "pH CMAST 2024")
        if(show_param("Salinity")) add_trace_auto(cmast_sal_24, "Salinity", "Salinity CMAST", "CMAST", "Salinity CMAST 2024")
      }
      
      if("2025" %in% years){
        if(show_param("Temperature")) add_trace_auto(cmast_do, "Temp_C", "Temp CMAST", "CMAST", "Temp CMAST 2025")
        if(show_param("Dissolved Oxygen")) add_trace_auto(cmast_do, "DO", "DO CMAST", "CMAST", "DO CMAST 2025")
        if(show_param("pH")) add_trace_auto(cmast_ph, "pH", "pH CMAST", "CMAST", "pH CMAST 2025")
        if(show_param("Salinity")) add_trace_auto(cmast_sal, "Salinity", "Salinity CMAST", "CMAST", "Salinity CMAST 2025")
      }
    }
    
    if("2025" %in% years && "Stump Sound" %in% farms){
      if(show_param("Temperature")) add_trace_auto(stump_do, "Temp_C", "Temp Stump", "Stump Sound", "Temp Stump 2025")
      if(show_param("Dissolved Oxygen")) add_trace_auto(stump_do, "DO", "DO Stump", "Stump Sound", "DO Stump 2025")
      if(show_param("pH")) add_trace_auto(stump_ph, "pH", "pH Stump", "Stump Sound", "pH Stump 2025")
      if(show_param("Salinity")) add_trace_auto(stump_sal, "Salinity", "Salinity Stump", "Stump Sound", "Salinity Stump 2025")
    }
    
    if("2025" %in% years && "Ward Creek" %in% farms){
      if(show_param("Temperature")) add_trace_auto(ward_do, "Temp_C", "Temp Ward", "Ward Creek", "Temp Ward 2025")
      if(show_param("Dissolved Oxygen")) add_trace_auto(ward_do, "DO", "DO Ward", "Ward Creek", "DO Ward 2025")
      if(show_param("pH")) add_trace_auto(ward_ph, "pH", "pH Ward", "Ward Creek", "pH Ward 2025")
      if(show_param("Salinity")) add_trace_auto(ward_sal, "Salinity", "Salinity Ward", "Ward Creek", "Salinity Ward 2025")
    }
    
    if("DUML" %in% farms){
      if("2024" %in% years){
        if(show_param("Temperature")) add_trace_auto(duml_do_24, "Temp_C", "Temp DUML", "DUML", "Temp DUML 2024")
        if(show_param("Dissolved Oxygen")) add_trace_auto(duml_do_24, "DO", "DO DUML", "DUML", "DO DUML 2024")
        if(show_param("pH")) add_trace_auto(duml_ph_24, "pH", "pH DUML", "DUML", "pH DUML 2024")
        if(show_param("Salinity")) add_trace_auto(duml_sal_24, "Salinity", "Salinity DUML", "DUML", "Salinity DUML 2024")
      }
      
      
      if("2025" %in% years){
        if(show_param("Temperature")) add_trace_auto(duml_do, "Temp_C", "Temp DUML", "DUML", "Temp DUML 2025")
        if(show_param("Dissolved Oxygen")) add_trace_auto(duml_do, "DO", "DO DUML", "DUML", "DO DUML 2025")
        if(show_param("pH")) add_trace_auto(duml_ph, "pH", "pH DUML", "DUML", "pH DUML 2025")
        if(show_param("Salinity")) add_trace_auto(duml_sal, "Salinity", "Salinity DUML", "DUML", "Salinity DUML 2025")
      }
      
      if("2026" %in% years){
        if(show_param("Temperature")) add_trace_auto(duml_do_26, "Temp_C", "Temp DUML", "DUML", "Temp DUML 2026")
        if(show_param("Dissolved Oxygen")) add_trace_auto(duml_do_26, "DO", "DO DUML", "DUML", "DO DUML 2026")
        if(show_param("Adjusted DO"))
          add_trace_auto(
            duml_do_26,
            "DO_Adj",
            "Adjusted DO DUML",
            "DUML",
            "Adjusted DO DUML 2026"
          )
        if(show_param("pH")) add_trace_auto(duml_ph_26, "pH", "pH DUML", "DUML", "pH DUML 2026")
        if(show_param("Salinity")) add_trace_auto(duml_sal_26, "Salinity", "Salinity DUML", "DUML", "Salinity DUML 2026")
        if(show_param("Precipitation"))
          add_trace_auto(duml_precip_26, "Precip_in", "Precipitation DUML",
                         "DUML", "Precipitation DUML 2026")
      }
    }
    
    if("2025" %in% years && "Nelson Bay" %in% farms){
      if(show_param("Temperature")) add_trace_auto(nelson_do, "Temp_C", "Temp Nelson", "Nelson Bay", "Temp Nelson 2025")
      if(show_param("Dissolved Oxygen")) add_trace_auto(nelson_do, "DO", "DO Nelson", "Nelson Bay", "DO Nelson 2025")
      if(show_param("pH")) add_trace_auto(nelson_ph, "pH", "pH Nelson", "Nelson Bay", "pH Nelson 2025")
      if(show_param("Salinity")) add_trace_auto(nelson_sal, "Salinity", "Salinity Nelson", "Nelson Bay", "Salinity Nelson 2025")
    }
    
    if("PIL" %in% farms && "2026" %in% years){
      if(show_param("Temperature"))
        add_trace_auto(pil_do_26, "Temp_C", "Temp PIL",
                       "PIL", "Temp PIL 2026")
      if(show_param("Dissolved Oxygen"))
        add_trace_auto(pil_do_26, "DO", "DO PIL",
                       "PIL", "DO PIL 2026")
      if(show_param("Adjusted DO"))
        add_trace_auto(pil_do_26, "DO_Adj", "Adjusted DO PIL",
                      "PIL", "Adjusted DO PIL 2026")
      if(show_param("pH"))
        add_trace_auto(pil_ph_26, "pH", "pH PIL",
                       "PIL", "pH PIL 2026")
      if(show_param("Salinity"))
        add_trace_auto(pil_sal_26, "Salinity", "Salinity PIL",
                       "PIL", "Salinity PIL 2026")
      if(show_param("Precipitation"))
        add_trace_auto(pil_precip_26, "Precip_in", "Precipitation PIL",
                       "PIL", "Precipitation PIL 2026")
    }
    
    if("MOF" %in% farms && "2026" %in% years){
      if(show_param("Temperature")) add_trace_auto(moyst_do_26, "Temp_C", "Temp MOF", "MOF", "Temp MOF 2026")
      if(show_param("Dissolved Oxygen")) add_trace_auto(moyst_do_26, "DO", "DO MOF", "MOF", "DO MOF 2026")
      if(show_param("Adjusted DO"))
        add_trace_auto(
          moyst_do_26,
          "DO_Adj",
          "Adjusted_DO MOF",
          "MOF",
          "Adjusted_DO MOF 2026"
        )
      if(show_param("pH")) add_trace_auto(moyst_ph_26, "pH", "pH MOF", "MOF", "pH MOF 2026")
      if(show_param("Salinity")) add_trace_auto(moyst_sal_26, "Salinity", "Salinity MOF", "MOF", "Salinity MOF 2026")
      if(show_param("Precipitation"))
        add_trace_auto(moyst_precip_26, "Precip_in", "Precipitation MOF",
                       "MOF", "Precipitation MOF 2026")
    }
    
    # interactive y-axis rescale
    y_range <- NULL
    if(length(all_y_values) > 0){
      y_range <- range(all_y_values, na.rm=TRUE)
    }
    
    p %>% layout(
      title = input$custom_title,
      xaxis = list(
        title = "Date & Time",
        tickformat = "%b %d\n%H:%M",
        hoverformat = "%b %d %H:%M"
      ),
      yaxis=list(title="Value", range=y_range),
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 1,
        xanchor = "left",
        yanchor = "top",
        font = list(size = 11),
        itemsizing = "constant",
        tracegroupgap = 15
      )
    )
  })
}

shinyApp(ui, server)