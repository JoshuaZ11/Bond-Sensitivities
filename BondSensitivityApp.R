library(shiny)
library(ggplot2)
library(writexl)

# --- Bond Functions ---
bond_price <- function(face, coupon_rate, ytm, years, freq) {
  n <- years * freq
  coupon <- face * coupon_rate / freq
  discount_rate <- ytm / freq
  times <- 1:n
  cash_flows <- rep(coupon, n)
  cash_flows[n] <- cash_flows[n] + face
  sum(cash_flows / (1 + discount_rate)^times)
}

macaulay_duration <- function(face, coupon_rate, ytm, years, freq) {
  n <- years * freq
  coupon <- face * coupon_rate / freq
  discount_rate <- ytm / freq
  times <- 1:n
  cash_flows <- rep(coupon, n)
  cash_flows[n] <- cash_flows[n] + face
  discounted_times <- times * cash_flows / (1 + discount_rate)^times
  price <- sum(cash_flows / (1 + discount_rate)^times)
  duration <- sum(discounted_times) / price
  return(duration / freq)
}

modified_duration <- function(face, coupon_rate, ytm, years, freq) {
  mac <- macaulay_duration(face, coupon_rate, ytm, years, freq)
  return(mac / (1 + ytm / freq))
}

dv01 <- function(face, coupon_rate, ytm, years, freq) {
  price1 <- bond_price(face, coupon_rate, ytm, years, freq)
  price2 <- bond_price(face, coupon_rate, ytm + 0.0001, years, freq)
  return(price2 - price1)
}

convexity <- function(face, coupon_rate, ytm, years, freq) {
  n <- years * freq
  coupon <- face * coupon_rate / freq
  discount_rate <- ytm / freq
  times <- 1:n
  cash_flows <- rep(coupon, n)
  cash_flows[n] <- cash_flows[n] + face
  price <- sum(cash_flows / (1 + discount_rate)^times)
  conv <- sum(times * (times + 1) * cash_flows / (1 + discount_rate)^(times + 2))
  return(conv / (price * freq^2))
}

# --- UI ---
ui <- fluidPage(
  titlePanel("Bond Portfolio Sensitivities + Interactive Stress Testing"),
  
  numericInput("num_bonds", "Number of Bonds:", 1, min = 1),
  actionButton("generate", "Generate Inputs"),
  
  uiOutput("bond_inputs"),
  actionButton("calc", "Calculate Sensitivities"),
  
  tableOutput("results"),
  verbatimTextOutput("portfolio_dv01"),
  plotOutput("yield_curve"),
  
  hr(),
  h4("Stress Test Tool"),
  numericInput("shock", "Yield Shock (bps):", value = 50),
  actionButton("run_stress", "Apply Stress Test"),
  tableOutput("stress_results"),
  verbatimTextOutput("portfolio_change"),
  
  hr(),
  h4("Export to Excel"),
  downloadButton("download_excel", "Download All Results (Excel)")
)

# --- Server ---
server <- function(input, output, session) {
  sens_data <- reactiveVal()
  stress_data <- reactiveVal()
  
  observeEvent(input$generate, {
    output$bond_inputs <- renderUI({
      lapply(1:input$num_bonds, function(i) {
        fluidRow(
          column(2, numericInput(paste0("face", i), paste("Bond", i, "Face Value"), 1000)),
          column(2, numericInput(paste0("coupon", i), paste("Bond", i, "Coupon Rate (%)"), 5)),
          column(2, numericInput(paste0("ytm", i), paste("Bond", i, "YTM (%)"), 4)),
          column(2, numericInput(paste0("years", i), paste("Bond", i, "Years to Maturity"), 5)),
          column(2, selectInput(paste0("freq", i), paste("Bond", i, "Coupon Frequency"),
                                choices = c("Annual" = 1, "Semiannual" = 2)))
        )
      })
    })
  })
  
  bond_data <- reactiveVal()
  
  observeEvent(input$calc, {
    results <- data.frame(
      Bond = character(), Price = numeric(),
      MacaulayDuration = numeric(), ModifiedDuration = numeric(),
      Convexity = numeric(), DV01 = numeric(),
      ConvAdj_25bps = numeric(), ConvAdj_50bps = numeric(), ConvAdj_100bps = numeric()
    )
    
    total_dv01 <- 0
    price_function <- function(y) 0
    
    bond_info <- list()
    yields <- seq(0.005, 0.10, by = 0.001)
    prices_actual <- numeric(length(yields))
    prices_duration <- numeric(length(yields))
    prices_convexity <- numeric(length(yields))
    
    for (i in 1:input$num_bonds) {
      face <- input[[paste0("face", i)]]
      coupon <- input[[paste0("coupon", i)]] / 100
      ytm <- input[[paste0("ytm", i)]] / 100
      years <- input[[paste0("years", i)]]
      freq <- as.numeric(input[[paste0("freq", i)]])
      
      price <- bond_price(face, coupon, ytm, years, freq)
      mac <- macaulay_duration(face, coupon, ytm, years, freq)
      mod <- modified_duration(face, coupon, ytm, years, freq)
      dvo1 <- dv01(face, coupon, ytm, years, freq)
      conv <- convexity(face, coupon, ytm, years, freq)
      
      adj_25 <- 0.5 * conv * (0.0025)^2 * price
      adj_50 <- 0.5 * conv * (0.005)^2 * price
      adj_100 <- 0.5 * conv * (0.01)^2 * price
      
      total_dv01 <- total_dv01 + dvo1
      
      results <- rbind(results, data.frame(
        Bond = paste("Bond", i),
        Price = round(price, 2),
        MacaulayDuration = round(mac, 4),
        ModifiedDuration = round(mod, 4),
        Convexity = round(conv, 4),
        DV01 = round(dvo1, 4),
        ConvAdj_25bps = round(adj_25, 4),
        ConvAdj_50bps = round(adj_50, 4),
        ConvAdj_100bps = round(adj_100, 4)
      ))
      
      bond_info[[i]] <- list(face = face, coupon = coupon, ytm = ytm,
                             years = years, freq = freq, price = price)
      
      for (j in seq_along(yields)) {
        y <- yields[j]
        prices_actual[j] <- prices_actual[j] + bond_price(face, coupon, y, years, freq)
        delta_y <- y - ytm
        prices_duration[j] <- prices_duration[j] + price * (1 - mod * delta_y)
        prices_convexity[j] <- prices_convexity[j] + price * (1 - mod * delta_y + 0.5 * conv * delta_y^2)
      }
    }
    
    bond_data(bond_info)
    sens_data(results)
    
    output$results <- renderTable(results)
    output$portfolio_dv01 <- renderText({
      paste("Total Portfolio DV01:", round(total_dv01, 4))
    })
    
    output$yield_curve <- renderPlot({
      df <- data.frame(
        Yield = rep(yields * 100, 3),
        Price = c(prices_actual, prices_duration, prices_convexity),
        Type = rep(c("Actual", "Duration Only", "Duration + Convexity"), each = length(yields))
      )
      ggplot(df, aes(x = Yield, y = Price, color = Type)) +
        geom_line() +
        labs(title = "Portfolio Price vs. Yield", x = "Yield (%)", y = "Portfolio Price") +
        theme_minimal()
    })
  })
  
  observeEvent(input$run_stress, {
    shock_bps <- input$shock
    shock_decimal <- shock_bps / 10000
    bonds <- bond_data()
    if (is.null(bonds)) return()
    
    results <- data.frame(
      Bond = character(), OriginalPrice = numeric(),
      StressedPrice = numeric(), ChangeDollar = numeric(), ChangePercent = numeric()
    )
    
    portfolio_original <- 0
    portfolio_stressed <- 0
    
    for (i in seq_along(bonds)) {
      b <- bonds[[i]]
      p0 <- b$price
      ytm_stress <- b$ytm + shock_decimal
      p1 <- bond_price(b$face, b$coupon, ytm_stress, b$years, b$freq)
      
      portfolio_original <- portfolio_original + p0
      portfolio_stressed <- portfolio_stressed + p1
      
      results <- rbind(results, data.frame(
        Bond = paste("Bond", i),
        OriginalPrice = round(p0, 2),
        StressedPrice = round(p1, 2),
        ChangeDollar = round(p1 - p0, 2),
        ChangePercent = round(100 * (p1 - p0) / p0, 2)
      ))
    }
    
    stress_data(results)
    output$stress_results <- renderTable(results)
    output$portfolio_change <- renderText({
      change <- portfolio_stressed - portfolio_original
      pct <- 100 * change / portfolio_original
      paste0("Portfolio Value Change: $", round(change, 2), " (", round(pct, 2), "%)")
    })
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("bond_analysis_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(list(
        "Bond Sensitivities" = sens_data(),
        "Stress Test Results" = stress_data()
      ), path = file)
    }
  )
}

# --- Run App ---
shinyApp(ui, server)
