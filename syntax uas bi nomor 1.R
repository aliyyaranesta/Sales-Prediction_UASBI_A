library(shiny)
library(tidyverse)
library(car)
library(shinydashboard)

# Load data
data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("x1", "Jumlah Pengunjung:", value = 200000),
      numericInput("x2", "Transaksi:", value = 10000),
      numericInput("x3", "Barang/Transaksi:", value = 5),
      numericInput("x4", "Rating:", value = 8.5),
      numericInput("x5", "Iklan:", value = 30000),
      actionButton("submit", "Dapatkan Estimasi")
    ),
    mainPanel(
      h2("Estimasi Penjualan"),
      verbatimTextOutput("estimate"),
      plotOutput("line_plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Membuat reactiveValues untuk menyimpan input dari user
  user_inputs <- reactiveValues()
  
  # Menangani input saat tombol diklik
  observeEvent(input$submit, {
    user_inputs$x1 <- input$x1
    user_inputs$x2 <- input$x2
    user_inputs$x3 <- input$x3
    user_inputs$x4 <- input$x4
    user_inputs$x5 <- input$x5
    
    # Menyiapkan data yang akan digunakan untuk plotting
    new_data <- data.frame(
      x1 = user_inputs$x1,
      x2 = user_inputs$x2,
      x3 = user_inputs$x3,
      x4 = user_inputs$x4,
      x5 = user_inputs$x5
    )
    
    # Membuat model
    model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
    
    # Membuat data frame untuk plotting
    bar_data <- data.frame(Month = data$Month, Actual_Sales = data$y, Predicted_Sales = predict(model, newdata = new_data))
    
    # Menggambar plot garis
    output$line_plot <- renderPlot({
      plot(1:nrow(bar_data), bar_data$Actual_Sales, type = "l", col = "blue", lwd = 2,
           xlab = "Month", ylab = "Total Sales", main = "Perbandingan Penjualan Aktual dan Prediksi",
           xaxt = "n")
      
      lines(1:nrow(bar_data), bar_data$Predicted_Sales, col = "red", lwd = 2)
      legend("topright", legend = c("Actual Sales", "Predicted Sales"), col = c("blue", "red"), lwd = 2)
      
      # Menetapkan label bulan pada sumbu x
      axis(1, at = 1:nrow(bar_data), labels = bar_data$Month)
    })
    
    # Menampilkan estimasi penjualan di teks
    output$estimate <- renderText({
      paste("Total Predicted Sales: ", sum(bar_data$Predicted_Sales))
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)

#3
#uji asumsi
# Uji normalitas
shapiro.test(model$residuals)
# Uji homoscedasticity
library(lmtest)
bptest(model)

#uji signifikansi koef
# Wald test (menggunakan library lmtest)
waldtest(model, terms = c("x1", "x2", "x3", "x4", "x5"))

