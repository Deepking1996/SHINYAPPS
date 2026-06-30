# Australian Wine Sales Forecasting Shiny Application

## Project Overview

This interactive Shiny application analyzes historical Australian wine sales and generates forecasts using multiple time series forecasting models. Users can explore sales trends across different wine varietals, compare forecasting methods, evaluate model performance, and visualize seasonal patterns.

The application was developed in **R** using the **Shiny** framework and supports interactive forecasting for business decision-making and time series analysis.

---

## Features

* Interactive selection of one or more wine varietals
* Custom date range filtering
* Adjustable training period for model development
* User-defined forecast horizon (1–60 months)
* Optional STL decomposition for seasonal trend analysis
* Comparison of multiple forecasting models:

  * TSLM (Time Series Linear Model)
  * ETS (Error, Trend, Seasonal Exponential Smoothing)
  * ARIMA (AutoRegressive Integrated Moving Average)
* Training and validation accuracy metrics
* Model specification summaries

---

## Dataset

The application uses the **AustralianWines.csv** dataset containing monthly Australian wine sales by varietal.

The dataset is automatically cleaned and transformed by:

* Converting dates into a monthly time-series format
* Removing formatting characters from numeric values
* Handling missing values
* Reshaping the dataset into a long format
* Converting the data into a **tsibble** for time series modeling

---

## Required Packages

Install the required packages before running the application.

```r
install.packages(c(
  "shiny",
  "shinythemes",
  "tidyverse",
  "tsibble",
  "fable",
  "fabletools",
  "feasts",
  "lubridate",
  "stringr",
  "purrr"
))
```

---

## Running the Application

1. Place the following files in the same directory:

```
app.R
AustralianWines.csv
```

2. Open `app.R` in RStudio.

3. Click **Run App**, or execute:

```r
shiny::runApp()
```

---

## Application Tabs

### Overview

Displays historical wine sales over time for the selected varietals.

Optional STL decomposition can be enabled to visualize:

* Trend
* Seasonal component
* Remainder (residuals)

---

### Forecasts

Generates future forecasts using:

* TSLM
* ETS
* ARIMA

Users can compare forecasting performance across selected wine varietals.

---

### Accuracy & Specifications

Displays:

* Training accuracy (RMSE, MAE, MAPE)
* Validation accuracy
* Model specifications and fitted model details

---

## Forecasting Models

### TSLM

A regression-based time series model incorporating trend and seasonal effects.

### ETS

An exponential smoothing model that automatically identifies error, trend, and seasonality components.

### ARIMA

A statistical forecasting model that captures autocorrelation and temporal dependencies within the data.

---

## Performance Metrics

The application reports:

* **RMSE** — Root Mean Squared Error
* **MAE** — Mean Absolute Error
* **MAPE** — Mean Absolute Percentage Error

These metrics help compare forecasting accuracy across models.

---

## File Structure

```
Project Folder/
│
├── app.R
├── AustralianWines.csv
└── README.md
```

---

## Technologies Used

* R
* Shiny
* tidyverse
* tsibble
* fable
* feasts
* lubridate
* ggplot2

---

## Future Improvements

Potential enhancements include:

* Additional forecasting models (Prophet, Neural Networks)
* Automatic model selection
* Downloadable forecast reports
* Interactive Plotly visualizations
* Confidence interval customization
* Forecast export to CSV or Excel

---

## Author
Gagandeep Singh

Developed as part of an Applied Data Science time series forecasting project using R Shiny.
