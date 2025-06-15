#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#library(shiny)

# Define UI for application that draws a histogram
#ui <- fluidPage(

    # Application title
    #titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
        #sidebarPanel(
            #sliderInput("bins",
                        #"Number of bins:",
                        #min = 1,
                        #max = 50,
                        #value = 30)
        #),

        # Show a plot of the generated distribution
        #mainPanel(
           #plotOutput("distPlot")
        #)
    #)
#)

# Define server logic required to draw a histogram
#server <- function(input, output) {

    #output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
             #xlab = 'Waiting time to next eruption (in mins)',
             #main = 'Histogram of waiting times')
    #})
#}

# Run the application 
#shinyApp(ui = ui, server = server)

library(shiny)
library(shinydashboard)
library(plotly)
library(forecast)
library(tidyverse)
library(lubridate)
library(DT)

# Load data
cpi_data <- read.csv("data/pakistan_cpi.csv") %>%
  mutate(
    Month = match(Month, month.abb),  # Convert abbreviated month to numeric
    Date = as.Date(paste(Year, Month, "01", sep = "-")),
    CPI = as.numeric(CPI)
  ) %>%
  arrange(Date)

# Temporary dummy forecast data if forecast_results.csv is not ready
forecast_data <- cpi_data %>%
  tail(12) %>%
  mutate(
    Forecast = CPI + rnorm(12, 0, 2),  # Add random noise
    Lo_80 = Forecast - 1,
    Hi_80 = Forecast + 1,
    Lo_95 = Forecast - 2,
    Hi_95 = Forecast + 2
  )



#forecast_data <- read.csv("data/forecast_results.csv") %>% 
  #mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Pakistan CPI Forecasting"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line")),
      menuItem("Model Analysis", tabName = "analysis", icon = icon("search")),
      menuItem("Model Info", tabName = "model", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotlyOutput("cpiPlot"), width = 12,
                    title = "Historical CPI Trends", status = "primary")
              ),
              fluidRow(
                box(plotOutput("decompositionPlot"), width = 12,
                    title = "CPI Decomposition", status = "primary")
              )
      ),
      
      # Forecast tab
      tabItem(tabName = "forecast",
              fluidRow(
                box(dateRangeInput("dateRange", "Select Date Range:",
                                   start = min(cpi_data$Date),
                                   end = max(forecast_data$Date)),
                    width = 12)
              ),
              fluidRow(
                box(plotlyOutput("forecastPlot"), width = 12,
                    title = "CPI Forecast with Confidence Intervals", status = "primary")
              ),
              fluidRow(
                box(DT::dataTableOutput("forecastTable"), width = 12,
                    title = "Forecast Values", status = "primary")
              )
      ),
      
      # Model Analysis tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(plotOutput("acfPlot"), width = 6,
                    title = "ACF of Residuals", status = "primary"),
                box(plotOutput("pacfPlot"), width = 6,
                    title = "PACF of Residuals", status = "primary")
              ),
              fluidRow(
                box(plotOutput("residualPlot"), width = 12,
                    title = "Residual Diagnostics", status = "primary")
              ),
              fluidRow(
                box(tableOutput("accuracyTable"), width = 12,
                    title = "Model Accuracy Metrics", status = "primary")
              )
      ),
      
      # Model Info tab
      tabItem(tabName = "model",
              fluidRow(
                box(h3("SARIMA Model Summary"), width = 12,
                    status = "primary",
                    verbatimTextOutput("modelSummary"))
              ),
              fluidRow(
                box(h3("Research Abstract"), width = 12,
                    status = "primary",
                    p("This study developed a Seasonal Autoregressive Integrated Moving Average (SARIMA) model to forecast inflation in Pakistan using monthly Consumer Price Index (CPI) data from 2014 to 2024. The optimal model identified was SARIMA(0,2,1)(0,0,2)[12], which demonstrated exceptional forecasting accuracy with a MAPE of 0.75%."),
                    p("The findings provide valuable insights for policymakers at the State Bank of Pakistan and other stakeholders in formulating evidence-based strategies to manage inflation."))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filter data based on date range
  filtered_data <- reactive({
    cpi_data %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  })
  
  filtered_forecast <- reactive({
    forecast_data %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  })
  
  # Historical CPI plot
  output$cpiPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Date, y = CPI)) +
      geom_line(color = "#3c8dbc") +
      labs(title = "Historical CPI of Pakistan",
           x = "Date", y = "Consumer Price Index") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Decomposition plot
  output$decompositionPlot <- renderPlot({
    ts_data <- ts(cpi_data$CPI, frequency = 12, start = c(2014, 1))
    plot(decompose(ts_data))
  })
  
  # Forecast plot
  output$forecastPlot <- renderPlotly({
    p <- ggplot() +
      geom_line(data = filtered_data(), 
                aes(x = Date, y = CPI, color = "Historical")) +
      geom_line(data = filtered_forecast(), 
                aes(x = Date, y = Forecast, color = "Forecast")) +
      geom_ribbon(data = filtered_forecast(), 
                  aes(x = Date, ymin = Lo_95, ymax = Hi_95),
                  fill = "blue", alpha = 0.2) +
      labs(title = "CPI Forecast with 95% Confidence Interval",
           x = "Date", y = "Consumer Price Index", color = "") +
      scale_color_manual(values = c("Historical" = "#3c8dbc", "Forecast" = "red")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Forecast table
  output$forecastTable <- DT::renderDataTable({
    filtered_forecast() %>% 
      select(Year, Month, Forecast, Lo_80, Hi_80, Lo_95, Hi_95) %>% 
      mutate(across(where(is.numeric), round, 2))
  })
  
  # ACF plot
  output$acfPlot <- renderPlot({
    # Replace with your actual residuals
    Acf(rnorm(100), main = "ACF of Residuals")
  })
  
  # PACF plot
  output$pacfPlot <- renderPlot({
    # Replace with your actual residuals
    Pacf(rnorm(100), main = "PACF of Residuals")
  })
  
  # Residual plot
  output$residualPlot <- renderPlot({
    # Replace with your actual residuals
    plot(rnorm(100), type = "l", main = "Residuals Over Time", ylab = "Residuals")
    abline(h = 0, col = "red")
  })
  
  # Accuracy table
  output$accuracyTable <- renderTable({
    data.frame(
      Metric = c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE"),
      Value = c(-0.0909, 1.8439, 1.0632, -0.0267, 0.7455, 0.0768)
    )
  })
  
  # Model summary
  output$modelSummary <- renderPrint({
    cat("SARIMA(0,2,1)(0,0,2)[12] Model Summary\n\n")
    cat("Coefficients:\n")
    cat("MA1: -0.8346 (p < 0.001)\n")
    cat("SMA2: 0.2550 (p < 0.001)\n\n")
    cat("Model Selection Criteria:\n")
    cat("AIC: 735.59\n")
    cat("AICc: 735.65\n")
    cat("BIC: 741.95\n\n")
    cat("Accuracy Metrics:\n")
    cat("MAPE: 0.75%\n")
    cat("RMSE: 1.84\n")
    cat("MASE: 0.077\n\n")
    cat("Residual Diagnostics:\n")
    cat("Ljung-Box test p-value: 0.012\n")
    cat("Residuals show some autocorrelation at seasonal lags")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

