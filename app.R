# app.R  --- ADS 506 Week 5 Shiny app

# -----------------------------
# Libraries
# -----------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(lubridate)
library(stringr)
library(purrr)

# -----------------------------
# Data load + wrangle
# -----------------------------

wine_raw <- readr::read_csv("AustralianWines.csv")

wine_ts <- wine_raw |>
  mutate(
    Month = yearmonth(Month, format = "%b-%y")
  ) |>
  mutate(
    yr   = year(Month),
    mon  = month(Month),
    yr   = if_else(yr >= 2000, yr - 100L, yr),
    Month = yearmonth(sprintf("%d-%02d", yr, mon))
  ) |>
  select(-yr, -mon) |>
  mutate(
    across(
      -Month,
      ~ .x |>
        as.character() |>
        str_replace_all(",", "") |>
        str_replace_all("[^0-9.]", "") |>
        na_if("") |>
        as.numeric()
    )
  ) |>
  pivot_longer(
    cols      = -Month,
    names_to  = "Varietal",
    values_to = "Sales"
  ) |>
  filter(!is.na(Sales)) |>
  as_tsibble(index = Month, key = Varietal) |>
  arrange(Varietal, Month)

# Helper values for UI controls
min_month <- min(wine_ts$Month, na.rm = TRUE)
max_month <- max(wine_ts$Month, na.rm = TRUE)

min_date <- as.Date(min_month)
max_date <- as.Date(max_month)

year_choices  <- year(min_date):year(max_date)
month_choices <- month.abb

varietal_choices <- wine_ts |>
  distinct(Varietal) |>
  arrange(Varietal) |>
  pull(Varietal)

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Australian Wine Sales Forecasting by Varietal"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput(
        "varietal",
        "Select varietal(s):",
        choices  = varietal_choices,
        selected = "Red",
        multiple = TRUE
      ),
      
      selectInput(
        "start_year",
        "Start year:",
        choices  = year_choices,
        selected = year(min_date)
      ),
      selectInput(
        "start_month",
        "Start month:",
        choices  = month_choices,
        selected = "Jan"
      ),
      
      selectInput(
        "end_year",
        "End year:",
        choices  = year_choices,
        selected = year(max_date)
      ),
      selectInput(
        "end_month",
        "End month:",
        choices  = month_choices,
        selected = "Dec"
      ),
      
      sliderInput(
        "train_end_year",
        "Training end year:",
        min   = year(min_date),
        max   = year(max_date) - 1,
        value = year(max_date) - 2,
        step  = 1
      ),
      
      numericInput(
        "h",
        "Forecast horizon (months):",
        value = 12,
        min = 1,
        max = 60
      ),
      
      checkboxInput(
        "show_decomp",
        "Show STL decomposition",
        value = FALSE
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Overview",
          plotOutput("ts_plot"),
          br(),
          conditionalPanel(
            "input.show_decomp == true",
            plotOutput("decomp_plot")
          )
        ),
        
        tabPanel(
          "Forecasts",
          plotOutput("fc_plot")
        ),
        
        tabPanel(
          "Accuracy & Specs",
          h4("Training accuracy"),
          tableOutput("train_accuracy_table"),
          br(),
          h4("Validation accuracy"),
          tableOutput("valid_accuracy_table"),
          br(),
          h4("Model specifications"),
          tableOutput("model_specs_table")
        )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------

server <- function(input, output, session) {
  
  # Date range from dropdowns
  date_range_selected <- reactive({
    start_date <- make_date(
      year  = as.integer(input$start_year),
      month = match(input$start_month, month.abb),
      day   = 1
    )
    end_date <- make_date(
      year  = as.integer(input$end_year),
      month = match(input$end_month, month.abb),
      day   = 1
    )
    
    start_date <- max(start_date, min_date)
    end_date   <- min(end_date,   max_date)
    
    list(start = start_date, end = end_date)
  })
  
  # Filtered data
  data_filtered <- reactive({
    req(input$varietal)
    
    dr <- date_range_selected()
    start_date <- dr$start
    end_date   <- dr$end
    
    df <- wine_ts |>
      filter(
        Varietal %in% input$varietal,
        as.Date(Month) >= start_date,
        as.Date(Month) <= end_date
      )
    
    validate(
      need(
        nrow(df) > 0,
        "No data in this date range. Try widening the window or changing varietal."
      )
    )
    
    df
  })
  
  # Train / validation split
  train_validation <- reactive({
    df <- data_filtered()
    
    te_year <- as.integer(input$train_end_year)
    te_year <- min(max(te_year, year(min_date)), year(max_date))
    train_end_date <- make_date(te_year, 12, 1)
    
    train <- df |>
      filter(as.Date(Month) <= train_end_date)
    
    val <- df |>
      filter(as.Date(Month) > train_end_date)
    
    list(train = train, validation = val)
  })
  
  # Models
  models <- reactive({
    tv    <- train_validation()
    train <- tv$train
    
    validate(
      need(
        nrow(train) >= 24,
        "Not enough training data to fit the models. Move the training end earlier."
      )
    )
    
    train |>
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales ~ pdq(0,1,1) + PDQ(0,1,1))
      )
  })
  
  # Forecasts
  fc_future <- reactive({
    m <- models()
    req(input$h > 0)
    forecast(m, h = input$h)
  })
  
  # Training accuracy
  train_accuracy <- reactive({
    models() |>
      accuracy() |>
      as_tibble() |>
      select(Varietal, .model, RMSE, MAE, MAPE)
  })
  
  # Validation accuracy
  valid_accuracy <- reactive({
    tv  <- train_validation()
    val <- tv$validation
    
    if (nrow(val) == 0) {
      return(
        tibble(
          Message = "No validation data in this split. Move the training end earlier."
        )
      )
    }
    
    m      <- models()
    fc_val <- forecast(m, new_data = val)
    
    accuracy(fc_val, val) |>
      as_tibble() |>
      select(Varietal, .model, RMSE, MAE, MAPE)
  })
  
  # Model specs
  model_specs <- reactive({
    m <- models()
    m_tbl <- as_tibble(m)
    
    validate(
      need(nrow(m_tbl) > 0, "No models to summarise.")
    )
    
    get_spec <- function(fit) {
      tryCatch(
        {
          out  <- capture.output(report(fit))
          line <- out[grepl("^Model:", out)][1]
          if (is.na(line)) {
            "Model summary unavailable"
          } else {
            stringr::str_trim(sub("^Model:\\s*", "", line))
          }
        },
        error = function(e) "Model failed to fit"
      )
    }
    
    specs <- list()
    
    if ("TSLM" %in% names(m_tbl)) {
      specs[[length(specs) + 1]] <-
        m_tbl |>
        transmute(
          Varietal,
          Model = "TSLM",
          Specification = "TSLM(Sales ~ trend() + season())"
        )
    }
    
    if ("ETS" %in% names(m_tbl)) {
      specs[[length(specs) + 1]] <-
        m_tbl |>
        transmute(
          Varietal,
          Model = "ETS",
          Specification = map_chr(ETS, get_spec)
        )
    }
    
    if ("ARIMA" %in% names(m_tbl)) {
      specs[[length(specs) + 1]] <-
        m_tbl |>
        transmute(
          Varietal,
          Model = "ARIMA",
          Specification = map_chr(ARIMA, get_spec)
        )
    }
    
    bind_rows(specs)
  })
  
  # -----------------------------
  # Plots
  # -----------------------------
  
  output$ts_plot <- renderPlot({
    df <- data_filtered()
    
    ggplot(df, aes(Month, Sales, colour = Varietal)) +
      geom_line() +
      labs(
        x = "Month",
        y = "Sales",
        colour = "Varietal"
      ) +
      theme_minimal()
  })
  
  output$decomp_plot <- renderPlot({
    req(input$show_decomp)
    
    df        <- data_filtered()
    first_var <- sort(unique(df$Varietal))[1]
    
    dcmp <- df |>
      filter(Varietal == first_var) |>
      model(
        STL(Sales ~ season(window = "periodic"))
      ) |>
      components()
    
    autoplot(dcmp)
  })
  
  output$fc_plot <- renderPlot({
    df <- data_filtered()
    fc <- fc_future()
    
    autoplot(fc, data = df) +
      labs(
        x = "Month",
        y = "Sales",
        colour = "Model"
      ) +
      theme_minimal() +
      facet_wrap(vars(Varietal), scales = "free_y")
  })
  
  # -----------------------------
  # Tables
  # -----------------------------
  
  output$train_accuracy_table <- renderTable({
    train_accuracy()
  }, digits = 2)
  
  output$valid_accuracy_table <- renderTable({
    valid_accuracy()
  }, digits = 2)
  
  output$model_specs_table <- renderTable({
    model_specs()
  })
}

# -----------------------------
# Run app
# -----------------------------
shinyApp(ui = ui, server = server)
