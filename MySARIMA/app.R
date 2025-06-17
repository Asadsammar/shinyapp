library(shiny)
library(pacman)

pacman::p_load(magrittr, forecast, tseries, TSA, ggplot2, shiny, readxl, DT)

# Define UI
ui <- fluidPage(
  titlePanel("Inflation (CPI) Forecasting Shiny Application"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CPI Data (TXT or Excel)",
                accept = c(".txt", ".xlsx")),
      numericInput("forecast_period", "Forecast Period (months):", 12, min = 1, max = 120),
      actionButton("predict", "Generate Forecast")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots & Summary",
                 plotOutput("forecast_plot"),
                 verbatimTextOutput("model_summary"),
                 plotOutput("residual_plot")),
        tabPanel("Forecast Table",
                 DTOutput("forecast_table"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  data_reactive <- eventReactive(input$predict, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "xlsx") {
      data <- read_excel(input$file$datapath)
    } else {
      data <- read.delim2(input$file$datapath)
    }
    
    dt <- as.numeric(data[,3])
    cpidt <- ts(dt, frequency = 12, start = c(2010,1))
    return(list(raw = dt, ts = cpidt))
  })
  
  forecast_reactive <- eventReactive(input$predict, {
    cpidt <- data_reactive()$ts
    fit <- auto.arima(cpidt)
    forecast(fit, h = input$forecast_period)
  })
  
  output$forecast_plot <- renderPlot({
    req(forecast_reactive())
    
    original_data <- data_reactive()$ts
    fc <- forecast_reactive()
    
    autoplot(original_data, series = "Actual CPI") +
      autolayer(fc, series = "Forecast CPI", PI = TRUE) +
      ggtitle("Actual CPI and Forecast") +
      xlab("Year") + ylab("CPI") +
      theme_minimal() +
      scale_color_manual(values = c("Actual CPI" = "black", "Forecast CPI" = "blue"))
  })
  
  output$model_summary <- renderPrint({
    fit <- auto.arima(data_reactive()$ts)
    summary(fit)
  })
  
  output$residual_plot <- renderPlot({
    fit <- auto.arima(data_reactive()$ts)
    res <- residuals(fit)
    par(mfrow = c(2, 1))
    acf(res, main = "ACF of Residuals")
    pacf(res, main = "PACF of Residuals")
  })
  
  output$forecast_table <- renderDT({
    fc <- forecast_reactive()
    
    fc_df <- data.frame(
      Month = rep(month.abb, length.out = length(fc$mean)),
      Year = floor(time(fc$mean)),
      Forecast = round(fc$mean, 2),
      Lo80 = round(fc$lower[,1], 2),
      Hi80 = round(fc$upper[,1], 2),
      Lo95 = round(fc$lower[,2], 2),
      Hi95 = round(fc$upper[,2], 2)
    )
    
    datatable(fc_df, options = list(pageLength = 12, autoWidth = TRUE))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
