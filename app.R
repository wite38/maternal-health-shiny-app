library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(reshape2)

# Load data once at app start
clean_data <- read_csv("cleaned_maternal_data.csv")

# Make sure RiskLevel is factor with correct order
clean_data$RiskLevel <- factor(clean_data$RiskLevel, levels = c("low risk", "mid risk", "high risk"))

ui <- fluidPage(
  titlePanel("Maternal Health Risk Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("risk_filter", "Select Risk Level:",
                  choices = c("All", levels(clean_data$RiskLevel)),
                  selected = "All"),
      sliderInput("age_range", "Select Age Range:",
                  min = min(clean_data$Age, na.rm = TRUE),
                  max = max(clean_data$Age, na.rm = TRUE),
                  value = c(min(clean_data$Age, na.rm = TRUE), max(clean_data$Age, na.rm = TRUE)))
    ),
    
    mainPanel(
      plotOutput("barRisk"),
      plotOutput("ageHist"),
      plotOutput("multiBoxplots"),
      plotOutput("corrHeatmap")
    )
  )
)

server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- clean_data %>%
      filter(Age >= input$age_range[1], Age <= input$age_range[2])
    
    if (input$risk_filter != "All") {
      data <- data %>% filter(RiskLevel == input$risk_filter)
    }
    data
  })
  
  # Bar plot of Risk Levels
  output$barRisk <- renderPlot({
    if (input$risk_filter == "All") {
      ggplot(filtered_data(), aes(x = RiskLevel, fill = RiskLevel)) +
        geom_bar() +
        theme_minimal() +
        labs(title = "Count of Patients by Risk Level", x = "Risk Level", y = "Count") +
        scale_fill_brewer(palette = "Set2")
    }
  })
  
  # Age histogram
  output$ageHist <- renderPlot({
    ggplot(filtered_data(), aes(x = Age)) +
      geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = "Age Distribution", x = "Age (years)", y = "Frequency")
  })
  
  # Multi-variable boxplots by RiskLevel
  output$multiBoxplots <- renderPlot({
    plot_data <- filtered_data() %>%
      select(RiskLevel, Age, SystolicBP, DiastolicBP, BloodSugar) %>%
      pivot_longer(cols = -RiskLevel, names_to = "Variable", values_to = "Value")
    
    ggplot(plot_data, aes(x = RiskLevel, y = Value, fill = RiskLevel)) +
      geom_boxplot() +
      facet_wrap(~ Variable, scales = "free_y") +
      theme_minimal() +
      labs(title = "Boxplots of Variables by Risk Level", x = "Risk Level", y = "Value") +
      scale_fill_brewer(palette = "Pastel1") +
      theme(legend.position = "none")
  })
  
  # Correlation heatmap including RiskLevelNum
  output$corrHeatmap <- renderPlot({
    num_vars <- filtered_data() %>%
      select(Age, SystolicBP, DiastolicBP, BloodSugar, BodyTemp, HeartRate, RiskLevelNum)
    
    cor_matrix <- cor(num_vars, use = "complete.obs")
    melted_cor <- melt(cor_matrix)
    
    ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                           limits = c(-1,1), name = "Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Correlation Heatmap")
  })
}

shinyApp(ui = ui, server = server)

