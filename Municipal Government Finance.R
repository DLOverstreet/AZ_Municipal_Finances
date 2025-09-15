# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(dplyr)
library(shinyjs)
library(shinycssloaders)
library(fresh)
library(forecast)
library(zoo)
library(httr)
library(jsonlite)

# Load the data
MunicipalMetrics <- read.csv("merged_arizona_data.csv")

# Create custom theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#1f4257",
    green = "#28a745",
    red = "#dc3545",
    purple = "#6f42c1"
  ),
  adminlte_sidebar(
    width = "240px",
    dark_bg = "#1f4257",
    dark_hover_bg = "#2c5f7c",
    dark_color = "#ffffff"
  ),
  adminlte_global(
    content_bg = "#ffffff",
    box_bg = "#ffffff",
    info_box_bg = "#ffffff"
  )
)

# Custom CSS
custom_css <- "
  /* Header styling */
  .skin-blue .main-header .logo {
    font-weight: 600;
    font-size: 20px;
    background-color: #1f4257;
  }
  
  .skin-blue .main-header .logo:hover {
    background-color: #2c5f7c;
  }
  
  .skin-blue .main-header .navbar {
    background-color: #1f4257;
  }
  
  /* Sidebar styling */
  .skin-blue .main-sidebar {
    box-shadow: 2px 0 10px rgba(0,0,0,0.1);
  }
  
  .sidebar-menu > li > a {
    padding: 12px 15px;
  }
  
  /* Box styling */
  .box {
    border-radius: 8px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.05);
    border-top: 3px solid #1f4257;
    margin-bottom: 20px;
  }
  
  .box-header {
    padding: 15px;
    border-bottom: 1px solid #f4f4f4;
  }
  
  /* Value box styling */
  .small-box {
    border-radius: 8px;
    transition: transform 0.2s;
  }
  
  .small-box:hover {
    transform: translateY(-3px);
  }
  
  /* Table styling */
  .dataTables_wrapper {
    padding: 15px;
  }
  
  .dataTable thead th {
    background-color: #f8f9fa;
    border-bottom: 2px solid #dee2e6 !important;
  }
  
  .dataTable tbody tr:hover {
    background-color: #f8f9fa !important;
  }
  
  /* Input styling */
  .form-control {
    border-radius: 4px;
    border: 1px solid #ddd;
  }
  
  .selectize-input {
    border-radius: 4px;
    box-shadow: none;
  }
  
  /* Plot styling */
  .plotly {
    border-radius: 8px;
    background-color: white;
  }
  
  /* Loading spinner */
  .load-container {
    display: flex;
    justify-content: center;
    align-items: center;
    height: 200px;
  }
  
  /* Content and banner styling */
  .content-wrapper {
    padding-bottom: 50px;
  }
  
  .powered-by-banner {
    position: fixed;
    bottom: 10px;
    left: 10px;
    background-color: rgba(31, 66, 87, 0.9);
    color: white;
    padding: 5px 10px;
    border-radius: 4px;
    font-size: 12px;
    z-index: 1000;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2);
  }
  
  .data-source-info {
    padding: 15px;
    font-size: 12px;
    color: rgba(255,255,255,0.8);
    border-top: 1px solid rgba(255,255,255,0.1);
    margin-top: 15px;
    white-space: normal;
    word-wrap: break-word;
    line-height: 1.4;
    max-width: 100%;
  }
  
  /* Chat window styles */
  #chat-window {
    position: fixed;
    bottom: 20px;
    right: 20px;
    width: 350px;
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
    z-index: 1000;
    display: flex;
    flex-direction: column;
    transition: transform 0.3s ease;
  }
  
  #chat-window.minimized {
    transform: translateY(calc(100% - 40px));
  }
  
  .chat-header {
    background: #1f4257;
    color: white;
    padding: 10px 15px;
    border-radius: 8px 8px 0 0;
    cursor: pointer;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  
  .chat-header h3 {
    margin: 0;
    font-size: 16px;
  }
  
  .chat-header .minimize-btn {
    background: none;
    border: none;
    color: white;
    cursor: pointer;
    padding: 0 5px;
  }
  
  .chat-body {
    height: 300px;
    overflow-y: auto;
    padding: 10px;
    background: #f8f9fa;
  }
  
  .chat-input-area {
    padding: 10px;
    border-top: 1px solid #dee2e6;
    background: white;
    border-radius: 0 0 8px 8px;
  }
  
  .user-message {
    background: #e3f2fd;
    padding: 8px 12px;
    border-radius: 15px 15px 15px 0;
    margin: 5px 0;
    max-width: 80%;
    float: left;
    clear: both;
  }
  
  .assistant-message {
    background: #1f4257;
    color: white;
    padding: 12px 15px;
    border-radius: 15px 15px 0 15px;
    margin: 5px 0;
    max-width: 85%;
    float: right;
    clear: both;
  }
  
  .assistant-message p {
  margin: 0 0 12px 0;
  line-height: 1.5;
}
  
 .assistant-message p:last-child {
  margin-bottom: 0;
}
  
/* Style for numbered lists */
  .assistant-message p[style*=\"margin: 6px 0 6px 20px\"] {
    padding-left: 20px;
    border-left: 2px solid rgba(255, 255, 255, 0.2);
  }
  
  .message-time {
    font-size: 10px;
    color: #666;
    margin-top: 2px;
    clear: both;
  }
  
  .chat-toggle-btn {
    position: fixed;
    bottom: 20px;
    right: 20px;
    background: #1f4257;
    color: white;
    border: none;
    border-radius: 50%;
    width: 50px;
    height: 50px;
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
    z-index: 999;
    transition: transform 0.3s ease;
  }
  
  .chat-toggle-btn:hover {
    transform: scale(1.1);
  }
  
  .typing-indicator {
    padding: 5px 10px;
    background: rgba(0,0,0,0.05);
    border-radius: 10px;
    margin: 5px 0;
    display: none;
  }
  
  .suggested-questions {
    display: flex;
    flex-direction: column;
    gap: 10px;
  }
  
  .suggested-question {
    background: #f8f9fa !important;
    border: 1px solid #dee2e6 !important;
    border-radius: 8px !important;
    padding: 12px 15px !important;
    cursor: pointer;
    transition: all 0.2s ease;
    display: flex;
    align-items: center;
    gap: 10px;
    color: #1f4257 !important;
    margin-bottom: 10px;
    width: 100%;
    text-align: left;
  }
  
  .suggested-question:hover {
    background: #e9ecef !important;
    transform: translateY(-1px);
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  .suggested-question i {
    color: #1f4257;
    font-size: 0.9em;
    margin-right: 8px;
  }
"

# Helper function for formatting currency values
format_currency <- function(value) {
  if (is.na(value)) return("N/A")
  
  # Multiply by 1000 since data is in thousands
  value <- value * 1000
  
  if (abs(value) >= 1e6) {
    paste0("$", format(round(value/1e6, 2), trim = TRUE, big.mark = ","), "M")
  } else if (abs(value) >= 1e3) {
    paste0("$", format(round(value/1e3, 2), trim = TRUE, big.mark = ","), "K")
  } else {
    paste0("$", format(round(value, 2), trim = TRUE, big.mark = ","))
  }
}
# Define plot theme for consistent styling
plot_theme <- list(
  font = list(family = "Arial, sans-serif"),
  plot_bgcolor = "rgba(0, 0, 0, 0)",
  paper_bgcolor = "rgba(0, 0, 0, 0)",
  margin = list(t = 40),
  xaxis = list(
    gridcolor = "#f5f5f5",
    zerolinecolor = "#e9e9e9"
  ),
  yaxis = list(
    gridcolor = "#f5f5f5",
    zerolinecolor = "#e9e9e9"
  )
)

# UI Component
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "AZ Municipal Finance",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      
      menuItem("Overview", 
               tabName = "overview", 
               icon = icon("dashboard"),
               selected = TRUE),
      
      menuItem("Revenue Analysis", 
               tabName = "revenue", 
               icon = icon("dollar-sign")),
      
      menuItem("Expenditure Analysis", 
               tabName = "expenditure", 
               icon = icon("chart-pie")),
      
      menuItem("Growth Analysis", 
               tabName = "growth", 
               icon = icon("chart-line")),
      
      menuItem("Debt & Financial Position", 
               tabName = "debt", 
               icon = icon("coins")),
      
      menuItem("Benchmarks", 
               tabName = "benchmarks", 
               icon = icon("chart-bar")),
      
      menuItem("Forecasting", 
               tabName = "forecast", 
               icon = icon("chart-line")),
      
      menuItem("Tax Rates (2024)", 
               tabName = "taxrates", 
               icon = icon("percent")),
      
      div(
        style = "padding: 15px;",
        
        selectInput("selected_city", 
                    tags$span(
                      icon("city"), 
                      "Select City:"),
                    choices = sort(unique(MunicipalMetrics$Name)),
                    selected = "Mesa",
                    width = "100%"),
        
        selectInput("selected_year", 
                    tags$span(
                      icon("calendar"), 
                      "Select Year:"),
                    choices = sort(unique(MunicipalMetrics$Year4[MunicipalMetrics$Year4 < 2024]), decreasing = TRUE),
                    selected = 2022,
                    width = "100%"),
        
        sliderInput("pop_filter", 
                    tags$span(
                      icon("users"), 
                      "Population Range:"),
                    min = 0,
                    max = max(MunicipalMetrics$Population, na.rm = TRUE),
                    value = c(0, max(MunicipalMetrics$Population, na.rm = TRUE)),
                    step = 1000,
                    width = "100%"),
        
        selectizeInput("peer_cities", 
                       tags$span(
                         icon("building"), 
                         "Select Peer Cities:"),
                       choices = sort(unique(MunicipalMetrics$Name)),
                       selected = NULL,
                       multiple = TRUE,
                       options = list(maxItems = 10),
                       width = "100%")
      ),
      
      # Data source information at bottom of sidebar
      div(
        class = "data-source-info",
        tags$span(
          icon("database", class = "fa-fw"),
          "Data Source:",
          style = "display: block; margin-bottom: 5px;"
        ),
        "U.S. Census Bureau's Survey of
        State and Local Government Finances"
      )
    )
  ),
  
  dashboardBody(
    # Include custom CSS and JS
    tags$head(
      tags$style(custom_css),
      tags$script(HTML("
        $(document).ready(function() {
          // Add smooth scrolling
          $('a[href*=\"#\"]').on('click', function(e) {
            e.preventDefault();
            $('html, body').animate({
              scrollTop: $($(this).attr('href')).offset().top
            }, 500, 'linear');
          });
          
          // Chat window controls
          $('#chat-toggle').on('click', function() {
            $('#chat-window').toggleClass('minimized');
            $('#chat-window').css('display', 'flex');
          });
          
          $('#minimize-chat').on('click', function(e) {
            e.stopPropagation();
            $('#chat-window').addClass('minimized');
          });
          
          $('.chat-header').on('click', function() {
            $('#chat-window').removeClass('minimized');
          });
          
          // Handle Enter key in chat textarea
          $('#chat_input').keypress(function(e) {
            if(e.which == 13 && !e.shiftKey) {
              e.preventDefault();
              $('#send_message').click();
            }
          });
          
          // Scroll chat to bottom when new message arrives
          function scrollChatToBottom() {
            var chatBody = $('.chat-body');
            chatBody.scrollTop(chatBody[0].scrollHeight);
          }
          
          // Observer for chat history changes
          var observer = new MutationObserver(scrollChatToBottom);
          var chatBody = $('.chat-body')[0];
          if (chatBody) {
            observer.observe(chatBody, { childList: true, subtree: true });
          }
        });
      "))
    ),
    
    # Use shinyjs
    useShinyjs(),
    
    # Apply custom theme
    use_theme(mytheme),
    
    # Add powered by banner
    div(class = "powered-by-banner", "Powered by Alt-30", href = "https://www.alt-30.com"),
    
    # Main content
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                class = "value-boxes",
                valueBoxOutput("total_revenue_box", width = 3) %>% 
                  withSpinner(type = 8, color = "#1f4257"),
                valueBoxOutput("total_expenditure_box", width = 3) %>% 
                  withSpinner(type = 8, color = "#1f4257"),
                valueBoxOutput("population_box", width = 3) %>% 
                  withSpinner(type = 8, color = "#1f4257"),
                valueBoxOutput("operating_ratio_box", width = 3) %>% 
                  withSpinner(type = 8, color = "#1f4257")
              ),
              
              fluidRow(
                box(
                  plotlyOutput("revenue_trend") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Revenue Trend Over Time",
                  status = "primary",
                  solidHeader = TRUE
                ),
                box(
                  plotlyOutput("exp_trend") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Expenditure Trend Over Time",
                  status = "primary",
                  solidHeader = TRUE
                )
              ),
              
              fluidRow(
                box(
                  plotlyOutput("financial_overview_plot") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Key Financial Metrics Per Capita",
                  status = "primary",
                  solidHeader = TRUE
                )
              )
      ),
      
      # Revenue Analysis Tab
      tabItem(tabName = "revenue",
              fluidRow(
                box(
                  plotlyOutput("revenue_composition") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Revenue Source Distribution",
                  status = "success",
                  solidHeader = TRUE
                ),
                box(
                  plotlyOutput("revenue_per_capita_comparison") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Revenue Per Capita Comparison",
                  status = "success",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("revenue_metrics_table") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Detailed Revenue Metrics",
                  status = "success",
                  solidHeader = TRUE
                )
              )
      ),
      
      # Expenditure Analysis Tab
      tabItem(tabName = "expenditure",
              fluidRow(
                box(
                  plotlyOutput("exp_composition") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Expenditure Distribution",
                  status = "warning",
                  solidHeader = TRUE
                ),
                box(
                  plotlyOutput("exp_per_capita_comparison") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Expenditure Per Capita Comparison",
                  status = "warning",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("expenditure_metrics_table") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Detailed Expenditure Metrics",
                  status = "warning",
                  solidHeader = TRUE
                )
              )
      ),
      
      # Growth Analysis Tab
      tabItem(tabName = "growth",
              fluidRow(
                box(
                  plotlyOutput("revenue_growth_trend") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Revenue Growth Trends",
                  status = "primary",
                  solidHeader = TRUE
                ),
                box(
                  plotlyOutput("expenditure_growth_trend") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Expenditure Growth Trends",
                  status = "primary",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("population_growth_trend") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Population Growth Trend",
                  status = "primary",
                  solidHeader = TRUE
                ),
                box(
                  plotlyOutput("per_capita_growth_trend") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Per Capita Metrics Growth",
                  status = "primary",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("growth_metrics_table") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Detailed Growth Metrics",
                  status = "primary",
                  solidHeader = TRUE
                )
              )
      ),
      
      # Debt & Financial Position Tab
      tabItem(tabName = "debt",
              fluidRow(
                box(
                  plotlyOutput("debt_metrics") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Debt Metrics",
                  status = "danger",
                  solidHeader = TRUE
                ),
                box(
                  plotlyOutput("debt_trend") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Debt Trend Over Time",
                  status = "danger",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("debt_metrics_table") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Detailed Debt Metrics",
                  status = "danger",
                  solidHeader = TRUE
                )
              )
      ),
      
      # Benchmarks Tab
      tabItem(tabName = "benchmarks",
              fluidRow(
                box(
                  width = 12,
                  title = "Benchmark Settings",
                  status = "primary",
                  solidHeader = TRUE,
                  fluidRow(
                    column(6,
                           selectInput("benchmark_metric",
                                       "Select Metric:",
                                       choices = c(
                                         "Revenue Per Capita" = "Revenue_Per_Capita",
                                         "Expenditure Per Capita" = "Total_Expenditure_Per_Capita",
                                         "Operating Ratio" = "Operating_Ratio",
                                         "Debt to Revenue" = "Debt_to_Revenue",
                                         "Cash to Debt" = "Cash_to_Debt"
                                       ),
                                       selected = "Revenue_Per_Capita")
                    ),
                    column(6,
                           sliderInput("peer_population_range",
                                       "Population Range for Peers (%):",
                                       min = 10,
                                       max = 100,
                                       value = 25,
                                       step = 5)
                    )
                  )
                )
              ),
              
              fluidRow(
                valueBoxOutput("state_rank_box", width = 3) %>% 
                  withSpinner(type = 8, color = "#1f4257"),
                valueBoxOutput("percentile_box", width = 3) %>% 
                  withSpinner(type = 8, color = "#1f4257"),
                valueBoxOutput("peer_rank_box", width = 3) %>% 
                  withSpinner(type = 8, color = "#1f4257"),
                valueBoxOutput("benchmark_score_box", width = 3) %>% 
                  withSpinner(type = 8, color = "#1f4257")
              ),
              fluidRow(
                box(
                  plotlyOutput("benchmark_distribution") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Distribution Compared to All Cities",
                  status = "primary",
                  solidHeader = TRUE
                ),
                box(
                  plotlyOutput("peer_comparison") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 6,
                  title = "Comparison to Peer Cities",
                  status = "primary",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("historical_benchmark") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Historical Benchmark Position",
                  status = "primary",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("benchmark_table") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Detailed Benchmark Metrics",
                  status = "primary",
                  solidHeader = TRUE
                )
              )
      ),
      
      # Tax Rates Tab
      tabItem(tabName = "taxrates",
              fluidRow(
                box(
                  width = 12,
                  title = "Sales Tax Rate Analysis - 2024",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("selected_tax_type",
                              "Select Sales Tax Type:",
                              choices = sort(unique(MunicipalMetrics$tax_type[!is.na(MunicipalMetrics$tax_type)])),
                              selected = "Retail Sales")
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("tax_rate_comparison") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Tax Rate Comparison Across Municipalities",
                  status = "primary",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Tax Rate Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("tax_rate_distribution") %>% 
                    withSpinner(type = 8, color = "#1f4257")
                ),
                box(
                  width = 6,
                  title = "Summary Statistics",
                  status = "primary",
                  solidHeader = TRUE,
                  tableOutput("tax_summary_stats")
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("tax_rates_table") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Detailed Tax Rates",
                  status = "primary",
                  solidHeader = TRUE
                )
              )
      ),
      
      # Forecasting Tab
      tabItem(tabName = "forecast",
              fluidRow(
                box(
                  width = 12,
                  title = "Forecast Settings",
                  status = "primary",
                  solidHeader = TRUE,
                  fluidRow(
                    column(6,
                           selectInput("forecast_metric",
                                       "Select Metric to Forecast:",
                                       choices = c("Total Revenue" = "Total_Revenue",
                                                   "Total Expenditure" = "Total_Expenditure",
                                                   "Operating Ratio" = "Operating_Ratio"),
                                       selected = "Total_Revenue")
                    ),
                    column(6,
                           numericInput("forecast_years",
                                        "Number of Years to Forecast:",
                                        value = 3,
                                        min = 1,
                                        max = 5)
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  plotlyOutput("forecast_plot") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Forecast Results",
                  status = "primary",
                  solidHeader = TRUE
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("forecast_table") %>% 
                    withSpinner(type = 8, color = "#1f4257"),
                  width = 12,
                  title = "Detailed Forecast Values",
                  status = "primary",
                  solidHeader = TRUE
                )
              )
      )
    ),
    
    # Add footer
    tags$footer(
      class = "dashboard-footer",
      tags$div(
        tags$span(
          icon("database", class = "fa-fw"),
          "Data Source: U.S. Census Bureau's Survey of State and Local Government Finances"
        )
      )
    ),
    
    # Add chat window
    tags$div(
      id = "chat-window",
      class = "minimized",
      tags$div(
        class = "chat-header",
        tags$h3("Chat with Saguaro Sam"),
        tags$button(
          id = "minimize-chat",
          class = "minimize-btn",
          icon("minus")
        )
      ),
      tags$div(
        class = "chat-body",
        uiOutput("chat_history"),
        tags$div(
          class = "typing-indicator",
          "Sam is typing..."
        )
      ),
      tags$div(
        class = "chat-input-area",
        textAreaInput("chat_input", NULL, 
                      placeholder = "Ask about the financial data...",
                      rows = 2),
        actionButton("send_message", "Send", 
                     class = "btn-primary",
                     style = "width: 100%; margin-top: 5px;")
      )
    ),
    
    # Add chat toggle button
    tags$button(
      id = "chat-toggle",
      class = "chat-toggle-btn",
      icon("comments")
    )
  )
)

# Server Component
server <- function(input, output, session) {
  
  # Initialize chat messages reactive value
  chat_messages <- reactiveVal(list())
  
  # Filtered dataset with proper handling of NAs
  filtered_data <- reactive({
    withProgress(message = 'Filtering data...', {
      if (input$sidebar == "taxrates") {
        # For tax rates tab, include 2024 data
        MunicipalMetrics %>%
          filter(Population >= input$pop_filter[1],
                 Population <= input$pop_filter[2],
                 !is.na(tax_rate))
      } else {
        # For all other tabs, exclude 2024 data
        MunicipalMetrics %>%
          filter(Population >= input$pop_filter[1],
                 Population <= input$pop_filter[2],
                 Year4 < 2024,
                 !is.na(Total_Revenue))
      }
    })
  })
  
  # Selected city data
  selected_city_data <- reactive({
    withProgress(message = 'Loading city data...', {
      filtered_data() %>%
        filter(Name == input$selected_city)
    })
  })
  
  # Current year data (for financial metrics)
  current_year_data <- reactive({
    withProgress(message = 'Processing current year...', {
      selected_city_data() %>%
        filter(Year4 == input$selected_year,
               !is.na(Total_Revenue))  # Only include rows with financial data
    })
  })
  
  # Tax data (2024 only)
  tax_data <- reactive({
    MunicipalMetrics %>%
      filter(Year4 == 2024,
             !is.na(tax_rate))
  })
  
  # Dynamic year selection based on city and data availability
  observe({
    selected_city <- input$selected_city
    
    # Get available years with financial data, excluding 2024
    financial_years <- MunicipalMetrics %>%
      filter(Name == selected_city,
             !is.na(Total_Revenue),
             Year4 < 2024) %>%
      pull(Year4) %>%
      sort(decreasing = TRUE)
    
    updateSelectInput(session, "selected_year",
                      choices = financial_years,
                      selected = max(financial_years))
  })
  
  # Add loading message
  observe({
    show("loading-content")
    delay(500, hide("loading-content"))
  })
  
  # Update peer cities when main city changes
  observe({
    if (!input$selected_city %in% input$peer_cities) {
      updateSelectizeInput(session, "peer_cities",
                           selected = unique(c(input$selected_city, input$peer_cities)))
    }
  })
  
  # Peer comparison data
  peer_data <- reactive({
    withProgress(message = 'Loading peer data...', {
      filtered_data() %>%
        filter(Year4 == input$selected_year,
               Name %in% input$peer_cities,
               !is.na(Total_Revenue))
    })
  })
  
  # Function to generate context for Claude based on current view
  generate_finance_context <- reactive({
    # Get current city and year
    city_data <- current_year_data()
    historical_data <- selected_city_data()
    
    # Create comprehensive context string
    context <- paste0(
      "Context for municipal finance analysis:\n",
      "- Municipality: ", input$selected_city, "\n",
      "- Current Year: ", input$selected_year, "\n",
      "\nKey Financial Metrics (Current Year):\n",
      "- Total Revenue: ", format_currency(city_data$Total_Revenue), "\n",
      "- Total Expenditure: ", format_currency(city_data$Total_Expenditure), "\n",
      "- Population: ", format(city_data$Population, big.mark = ","), "\n",
      "- Operating Ratio: ", round(city_data$Operating_Ratio, 2), "\n",
      "\nRevenue Breakdown:\n",
      "- Property Tax: ", round(city_data$Property_Tax_Pct, 1), "%\n",
      "- Sales Tax: ", round(city_data$Sales_Tax_Pct, 1), "%\n",
      "- License Tax: ", round(city_data$License_Tax_Pct, 1), "%\n",
      "- Intergovernmental: ", round(city_data$Intergov_Revenue_Pct, 1), "%\n",
      "\nExpenditure Breakdown:\n",
      "- Police: ", round(city_data$Police_Exp_Pct, 1), "%\n",
      "- Fire: ", round(city_data$Fire_Exp_Pct, 1), "%\n",
      "- Parks: ", round(city_data$Parks_Exp_Pct, 1), "%\n",
      "- Highways: ", round(city_data$Highway_Exp_Pct, 1), "%\n",
      "\nDebt Metrics:\n",
      "- Debt to Revenue Ratio: ", round(city_data$Debt_to_Revenue, 2), "\n",
      "- Cash to Debt Ratio: ", round(city_data$Cash_to_Debt, 2), "\n",
      "- Debt Service Burden: ", round(city_data$Debt_Service_Burden, 1), "%\n",
      "\nHistorical Trends:\n"
    )
    
    # Add historical trends
    if (nrow(historical_data) > 1) {
      revenue_growth <- diff(historical_data$Total_Revenue) / head(historical_data$Total_Revenue, -1)
      exp_growth <- diff(historical_data$Total_Expenditure) / head(historical_data$Total_Expenditure, -1)
      
      context <- paste0(
        context,
        "- Average Revenue Growth: ", round(mean(revenue_growth, na.rm = TRUE) * 100, 1), "%\n",
        "- Average Expenditure Growth: ", round(mean(exp_growth, na.rm = TRUE) * 100, 1), "%\n"
      )
    }
    
    # Add peer comparison if available
    if (length(input$peer_cities) > 0) {
      peer_data <- filtered_data() %>%
        filter(Year4 == input$selected_year,
               Name %in% input$peer_cities)
      
      context <- paste0(
        context,
        "\nPeer Comparison Metrics:\n",
        "- Revenue Per Capita Rank: ", rank(-peer_data$Revenue_Per_Capita)[peer_data$Name == input$selected_city],
        " of ", length(input$peer_cities), "\n",
        "- Expenditure Per Capita Rank: ", rank(-peer_data$Total_Expenditure_Per_Capita)[peer_data$Name == input$selected_city],
        " of ", length(input$peer_cities), "\n"
      )
    }
    
    return(context)
  })
  
  # Function to call Claude API
  get_claude_response <- function(prompt) {
    tryCatch({
      # Hardcode API key
      api_key <- "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"  # Replace with your actual Claude API key
      
      # Create the messages list in Claude-3 format
      messages <- list(
        list(
          role = "user",
          content = prompt
        )
      )
      
      # Prepare the API request body
      body <- list(
        model = "claude-3-5-sonnet-20241022",
        messages = messages,
        max_tokens = 1000,
        temperature = 0.7
      )
      
      # Make the API call
      response <- POST(
        url = "https://api.anthropic.com/v1/messages",
        add_headers(
          "x-api-key" = api_key,  # Use the hardcoded key here
          "anthropic-version" = "2023-06-01",
          "Content-Type" = "application/json"
        ),
        body = toJSON(body, auto_unbox = TRUE),
        encode = "json"
      )
      
      # Handle response
      if (http_error(response)) {
        error_content <- fromJSON(rawToChar(response$content))
        error_message <- if (!is.null(error_content$error$message)) {
          error_content$error$message
        } else {
          "Unknown API error"
        }
        return(list(success = FALSE, response = paste("API Error:", error_message)))
      }
      
      # Parse successful response
      response_content <- fromJSON(rawToChar(response$content))
      if (!is.null(response_content$content) && 
          is.data.frame(response_content$content) && 
          nrow(response_content$content) > 0 &&
          "text" %in% names(response_content$content)) {
        return(list(success = TRUE, response = response_content$content$text[1]))
      } else {
        return(list(success = FALSE, response = "Error: Unexpected response format from API"))
      }
      
    }, error = function(e) {
      return(list(success = FALSE, response = paste("Error:", e$message)))
    })
  }
  
  # Add this at the start of your server function, with other reactive values
  suggestion_selected <- reactiveVal(FALSE)
  
  # Handle chat messages
  observeEvent(input$send_message, {
    req(input$chat_input)
    
    if (suggestion_selected()) {
      # Show typing indicator
      runjs("$('.typing-indicator').show();")
      
      # Disable send button while processing
      shinyjs::disable("send_message")
      
      # Get user message
      user_msg <- input$chat_input
      
      # Add user message to chat history
      current_messages <- chat_messages()
      current_messages[[length(current_messages) + 1]] <- list(
        role = "user",
        content = user_msg,
        timestamp = Sys.time()
      )
      
      chat_messages(current_messages)
      
      # Show processing message
      withProgress(message = 'Getting response from Sam...', value = 0.1, {
        
        # Generate response context
        context <- generate_finance_context()
        
        # Prepare prompt with context
        prompt <- paste0(
          "You are an expert municipal finance analyst helping users understand financial data for Arizona municipalities. ",
          "Your role is to explain financial metrics, trends, and implications in clear, accessible language while maintaining technical accuracy. ",
          "\n\nHere is the current context of the financial data:\n\n",
          context,
          "\n\nUser question: ", user_msg,
          "\n\nPlease provide a clear, concise explanation that helps the user understand the financial situation. ",
          "Focus on practical implications and comparative context when relevant. ",
          "Always cite specific numbers from the data when discussing financial metrics and trends."
        )
        
        # Get Claude's response
        result <- get_claude_response(prompt)
        
        # Add Claude's response to chat history
        current_messages[[length(current_messages) + 1]] <- list(
          role = "assistant",
          content = result$response,
          timestamp = Sys.time()
        )
        
        chat_messages(current_messages)
      })
      
      # Hide typing indicator
      runjs("$('.typing-indicator').hide();")
      
      # Clear input and re-enable send button
      updateTextAreaInput(session, "chat_input", value = "")
      shinyjs::enable("send_message")
      
      # Reset suggestion flag
      suggestion_selected(FALSE)
      
    } else if (nchar(trimws(input$chat_input)) > 0) {
      # Show typing indicator
      runjs("$('.typing-indicator').show();")
      
      # Disable send button while processing
      shinyjs::disable("send_message")
      
      # Get user message
      user_msg <- input$chat_input
      
      # Add user message to chat history
      current_messages <- chat_messages()
      current_messages[[length(current_messages) + 1]] <- list(
        role = "user",
        content = user_msg,
        timestamp = Sys.time()
      )
      
      chat_messages(current_messages)
      
      # Show processing message
      withProgress(message = 'Getting response from Sam...', value = 0.1, {
        
        # Generate response context
        context <- generate_finance_context()
        
        # Prepare prompt with context
        prompt <- paste0(
          "You are an expert municipal finance analyst helping users understand financial data for Arizona municipalities. ",
          "Your role is to explain financial metrics, trends, and implications in clear, accessible language while maintaining technical accuracy. ",
          "\n\nHere is the current context of the financial data:\n\n",
          context,
          "\n\nUser question: ", user_msg,
          "\n\nPlease provide a clear, concise explanation that helps the user understand the financial situation. ",
          "Focus on practical implications and comparative context when relevant. ",
          "Always cite specific numbers from the data when discussing financial metrics and trends."
        )
        
        # Get Claude's response
        result <- get_claude_response(prompt)
        
        # Add Claude's response to chat history
        current_messages[[length(current_messages) + 1]] <- list(
          role = "assistant",
          content = result$response,
          timestamp = Sys.time()
        )
        
        chat_messages(current_messages)
      })
      
      # Hide typing indicator
      runjs("$('.typing-indicator').hide();")
      
      # Clear input and re-enable send button
      updateTextAreaInput(session, "chat_input", value = "")
      shinyjs::enable("send_message")
    }
  })
  
  # Add observers for suggested questions
  observe({
    suggestions <- c(
      "How does the city's revenue compare to other cities?",
      "What are the main sources of revenue for the city?",
      "How has the city's debt changed over time?",
      "What's the city's operating ratio and is it healthy?",
      "Can you explain the different tax rates?",
      "What are the major expenditure categories?"
    )
    
    lapply(suggestions, function(question) {
      button_id <- paste0("suggest_", gsub("[^[:alnum:]]", "", question))
      observeEvent(input[[button_id]], {
        updateTextAreaInput(session, "chat_input", value = question)
        suggestion_selected(TRUE)
        # Trigger send button click
        runjs("document.getElementById('send_message').click();")
      })
    })
  })
  
  # Modify the chat history UI
  output$chat_history <- renderUI({
    messages <- chat_messages()
    if(length(messages) == 0) {
      return(tags$div(
        style = "text-align: left; color: #666; padding: 20px;",
        div(
          style = "text-align: center; margin-bottom: 20px;",
          icon("robot"), 
          tags$p("Hi! I'm Saguaro Sam, your municipal finance assistant.", 
                 style = "margin-top: 10px;"),
          tags$p("Here are some questions you can ask:", 
                 style = "font-size: 0.9em; margin-top: 15px;")
        ),
        div(
          class = "suggested-questions",
          lapply(c(
            "How does the city's revenue compare to other cities?",
            "What are the main sources of revenue for the city?",
            "How has the city's debt changed over time?",
            "What's the city's operating ratio and is it healthy?",
            "Can you explain the different tax rates?",
            "What are the major expenditure categories?"
          ), function(question) {
            actionButton(
              inputId = paste0("suggest_", gsub("[^[:alnum:]]", "", question)), # Create unique ID
              label = div(icon("question-circle"), question),
              class = "suggested-question",
              style = "width: 100%; text-align: left; background: none; border: none;"
            )
          })
        )
      ))
    }
    
    formatted_messages <- lapply(messages, function(msg) {
      time_str <- format(msg$timestamp, "%H:%M:%S")
      
      # Function to format text with paragraphs
      # Function to format text with paragraphs
      format_text <- function(text) {
        # First, standardize line endings and clean up number formatting
        text <- gsub("\r\n", "\n", text)
        text <- gsub("\\s+\\.", ".", text)  # Remove spaces before periods in numbers
        text <- gsub("\\$\\s+", "\\$", text)  # Remove spaces after dollar signs
        
        # Properly format numbered list items
        text <- gsub("([0-9]+)\\.\\s*\n", "\\1. ", text)  # Fix list numbers followed by newlines
        
        # Split text into paragraphs
        paragraphs <- strsplit(text, "\n\\s*\n|(?<=:)\\s*(?=[A-Z])|(?<=[.!?])\\s+(?=[A-Z])", perl = TRUE)[[1]]
        
        # Process each paragraph
        formatted_paragraphs <- lapply(paragraphs, function(p) {
          # Trim whitespace
          p <- trimws(p)
          
          if (p == "") return(NULL)
          
          # Special handling for list items
          if (grepl("^The largest expenditure categories are:", p)) {
            # Return the header as its own paragraph
            return(tags$p(p, style = "margin-bottom: 10px;"))
          } else if (grepl("^[0-9]+\\.", p)) {
            # Format list items with proper spacing and indentation
            p <- gsub("^([0-9]+\\.)", "\\1 ", p)  # Ensure space after number
            p <- gsub("\\s+", " ", p)  # Normalize spaces
            return(tags$p(p, style = "margin: 6px 0 6px 20px;"))
          } else {
            # Regular paragraph
            return(tags$p(p, style = "margin-bottom: 10px;"))
          }
        })
        
        # Remove NULL elements and return
        formatted_paragraphs <- formatted_paragraphs[!sapply(formatted_paragraphs, is.null)]
        do.call(tagList, formatted_paragraphs)
      }
      
      if(msg$role == "user") {
        div(
          class = "user-message",
          tags$p(msg$content),
          tags$span(class = "message-time", time_str)
        )
      } else {
        div(
          class = "assistant-message",
          format_text(msg$content),
          tags$span(class = "message-time", time_str)
        )
      }
    })
    
    div(
      class = "chat-messages-container",
      style = "display: flex; flex-direction: column;",
      do.call(tagList, formatted_messages)
    )
  })
  
  observe({
    suggestions <- c(
      "How does the city's revenue compare to other cities?",
      "What are the main sources of revenue for the city?",
      "How has the city's debt changed over time?",
      "What's the city's operating ratio and is it healthy?",
      "Can you explain the different tax rates?",
      "What are the major expenditure categories?"
    )
    
    lapply(suggestions, function(question) {
      button_id <- paste0("suggest_", gsub("[^[:alnum:]]", "", question))
      observeEvent(input[[button_id]], {
        updateTextAreaInput(session, "chat_input", value = question)
        suggestion_selected(TRUE)
      })
    })
  })
  
  # Add observer for city/year changes to update context
  observeEvent(c(input$selected_city, input$selected_year), {
    if (length(chat_messages()) > 0) {
      current_messages <- chat_messages()
      current_messages[[length(current_messages) + 1]] <- list(
        role = "assistant",
        content = paste0(
          "I notice you've switched to viewing data for ", input$selected_city,
          " (", input$selected_year, "). Feel free to ask me anything about this city's financial data!"
        ),
        timestamp = Sys.time()
      )
      chat_messages(current_messages)
    }
  })
  
  # Enhanced Value Boxes with animations and formatting
  output$total_revenue_box <- renderValueBox({
    data <- current_year_data()
    valueBox(
      format_currency(data$Total_Revenue),
      "Total Revenue",
      icon = icon("dollar-sign"),
      color = "green",
      href = NULL,
      width = NULL
    )
  })
  
  output$total_expenditure_box <- renderValueBox({
    data <- current_year_data()
    valueBox(
      format_currency(data$Total_Expenditure),
      "Total Expenditure",
      icon = icon("shopping-cart"),
      color = "red"
    )
  })
  
  output$population_box <- renderValueBox({
    data <- current_year_data()
    valueBox(
      format(round(data$Population, 0), 
             trim = TRUE, big.mark = ","),
      "Population",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$operating_ratio_box <- renderValueBox({
    data <- current_year_data()
    valueBox(
      format(round(data$Operating_Ratio, 2), 
             trim = TRUE),
      "Operating Ratio",
      icon = icon("balance-scale"),
      color = "purple"
    )
  })
  
  # Enhanced Trend Plots with consistent styling and proper scaling
  output$revenue_trend <- renderPlotly({
    data <- selected_city_data() %>%
      filter(!is.na(Total_Revenue),
             Year4 < 2024) %>%
      arrange(Year4)
    
    p <- plot_ly(data, x = ~Year4, 
                 y = ~(Total_Revenue * 1000), # Scale to actual dollars
                 type = 'scatter', 
                 mode = 'lines+markers',
                 line = list(color = '#28a745'),
                 marker = list(color = '#28a745')) %>%
      layout(title = paste("Revenue Trend -", input$selected_city),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Revenue ($)",
                          tickformat = "$,.0f"),
             showlegend = FALSE) %>%
      layout(plot_theme)
    
    p %>% add_trace(
      hovertemplate = paste(
        "<b>Year:</b> %{x}<br>",
        "<b>Revenue:</b> $%{y:,.2f}<br>",
        "<extra></extra>"
      )
    )
  })
  
  output$exp_trend <- renderPlotly({
    data <- selected_city_data() %>%
      filter(!is.na(Total_Expenditure),
             Year4 < 2024) %>%
      arrange(Year4)
    
    p <- plot_ly(data, x = ~Year4, 
                 y = ~(Total_Expenditure * 1000), # Scale to actual dollars
                 type = 'scatter', 
                 mode = 'lines+markers',
                 line = list(color = '#dc3545'),
                 marker = list(color = '#dc3545')) %>%
      layout(title = paste("Expenditure Trend -", input$selected_city),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Expenditure ($)",
                          tickformat = "$,.0f"),
             showlegend = FALSE) %>%
      layout(plot_theme)
    
    p %>% add_trace(
      hovertemplate = paste(
        "<b>Year:</b> %{x}<br>",
        "<b>Expenditure:</b> $%{y:,.2f}<br>",
        "<extra></extra>"
      )
    )
  })
  
  output$financial_overview_plot <- renderPlotly({
    data <- current_year_data()
    metrics <- data.frame(
      Metric = c("Revenue", "Expenditure", "Debt", "Cash"),
      Value = c(data$Revenue_Per_Capita * 1000, 
                data$Total_Expenditure_Per_Capita * 1000,
                data$Long_Term_Debt_Per_Capita * 1000,
                data$Cash_Per_Capita * 1000)  # Scale to actual dollars
    )
    
    colors <- c('#28a745', '#dc3545', '#ffc107', '#17a2b8')
    
    plot_ly(metrics, x = ~Metric, y = ~Value, type = 'bar',
            marker = list(color = colors)) %>%
      layout(title = "Per Capita Metrics",
             xaxis = list(title = "Metric"),
             yaxis = list(title = "Amount per Capita ($)",
                          tickformat = "$,.0f"),
             showlegend = FALSE) %>%
      layout(plot_theme) %>%
      add_trace(
        hovertemplate = paste(
          "<b>%{x}:</b><br>",
          "$%{y:,.2f} per capita<br>",
          "<extra></extra>"
        )
      )
  })
  
  # Enhanced Revenue Composition Plot
  output$revenue_composition <- renderPlotly({
    data <- current_year_data()
    revenue_data <- data.frame(
      Source = c("Property Tax", "Sales Tax", "License Tax", "Intergovernmental", "Other"),
      Percentage = c(data$Property_Tax_Pct/100, data$Sales_Tax_Pct/100, 
                     data$License_Tax_Pct/100, data$Intergov_Revenue_Pct/100,
                     1 - ((data$Property_Tax_Pct + data$Sales_Tax_Pct + 
                             data$License_Tax_Pct + data$Intergov_Revenue_Pct)/100))
    )
    
    colors <- c('#28a745', '#20c997', '#17a2b8', '#0dcaf0', '#6c757d')
    
    plot_ly(revenue_data, labels = ~Source, values = ~Percentage, type = 'pie',
            marker = list(colors = colors),
            textinfo = 'label+percent',
            hoverinfo = 'label+percent',
            textposition = 'outside',
            insidetextorientation = 'radial') %>%
      layout(title = paste("Revenue Composition -", input$selected_city),
             showlegend = TRUE,
             legend = list(orientation = "h", y = -0.1)) %>%
      layout(plot_theme)
  })
  
  # Revenue Per Capita Comparison
  output$revenue_per_capita_comparison <- renderPlotly({
    data <- peer_data()
    
    plot_ly(data, x = ~reorder(Name, -Revenue_Per_Capita), 
            y = ~(Revenue_Per_Capita * 1000), # Scale to actual dollars
            type = 'bar',
            marker = list(color = '#28a745')) %>%
      layout(title = "Revenue Per Capita - Peer Comparison",
             xaxis = list(title = "City", tickangle = 45),
             yaxis = list(title = "Revenue Per Capita ($)",
                          tickformat = "$,.0f"),
             showlegend = FALSE) %>%
      layout(plot_theme) %>%
      add_trace(
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Revenue: $%{y:,.2f} per capita<br>",
          "<extra></extra>"
        )
      )
  })
  
  # Enhanced Expenditure Composition Plot
  output$exp_composition <- renderPlotly({
    data <- current_year_data()
    exp_data <- data.frame(
      Category = c("Police", "Fire", "Parks", "Highways", "Libraries", "Other"),
      Percentage = c(data$Police_Exp_Pct/100, data$Fire_Exp_Pct/100,
                     data$Parks_Exp_Pct/100, data$Highway_Exp_Pct/100,
                     data$Library_Exp_Pct/100,
                     1 - ((data$Police_Exp_Pct + data$Fire_Exp_Pct +
                             data$Parks_Exp_Pct + data$Highway_Exp_Pct +
                             data$Library_Exp_Pct)/100))
    )
    
    colors <- c('#dc3545', '#e83e8c', '#6f42c1', '#6610f2', '#0d6efd', '#6c757d')
    
    plot_ly(exp_data, labels = ~Category, values = ~Percentage, type = 'pie',
            marker = list(colors = colors),
            textinfo = 'label+percent',
            hoverinfo = 'label+percent',
            textposition = 'outside',
            insidetextorientation = 'radial') %>%
      layout(title = paste("Expenditure Composition -", input$selected_city),
             showlegend = TRUE,
             legend = list(orientation = "h", y = -0.1)) %>%
      layout(plot_theme)
  })
  
  # Enhanced Expenditure Per Capita Comparison
  output$exp_per_capita_comparison <- renderPlotly({
    data <- peer_data()
    
    plot_ly(data, x = ~reorder(Name, -Total_Expenditure_Per_Capita), 
            y = ~(Total_Expenditure_Per_Capita * 1000), # Scale to actual dollars
            type = 'bar',
            marker = list(color = '#dc3545')) %>%
      layout(title = "Expenditure Per Capita - Peer Comparison",
             xaxis = list(title = "City", tickangle = 45),
             yaxis = list(title = "Expenditure Per Capita ($)",
                          tickformat = "$,.0f"),
             showlegend = FALSE) %>%
      layout(plot_theme) %>%
      add_trace(
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Expenditure: $%{y:,.2f} per capita<br>",
          "<extra></extra>"
        )
      )
  })
  
  # Calculate growth rates with proper scaling
  calculate_growth_rates <- function(data) {
    data %>%
      arrange(Year4) %>%
      mutate(
        Revenue_Growth = (Total_Revenue / lag(Total_Revenue) - 1),
        Expenditure_Growth = (Total_Expenditure / lag(Total_Expenditure) - 1),
        Population_Growth = (Population / lag(Population) - 1),
        Revenue_PC_Growth = (Revenue_Per_Capita / lag(Revenue_Per_Capita) - 1),
        Expenditure_PC_Growth = (Total_Expenditure_Per_Capita / lag(Total_Expenditure_Per_Capita) - 1),
        Debt_PC_Growth = (Long_Term_Debt_Per_Capita / lag(Long_Term_Debt_Per_Capita) - 1)
      )
  }
  
  # Growth data reactive expression
  growth_data <- reactive({
    withProgress(message = 'Calculating growth rates...', {
      selected_city_data() %>%
        filter(!is.na(Total_Revenue),
               Year4 < 2024) %>%
        calculate_growth_rates()
    })
  })
  
  # Revenue Growth Trend Plot
  output$revenue_growth_trend <- renderPlotly({
    data <- growth_data()
    
    plot_ly(data, x = ~Year4) %>%
      add_trace(y = ~Revenue_Growth, 
                name = 'Total Revenue Growth',
                type = 'scatter', 
                mode = 'lines+markers',
                line = list(color = '#28a745'),
                marker = list(color = '#28a745'),
                hovertemplate = paste(
                  "<b>Year:</b> %{x}<br>",
                  "<b>Growth Rate:</b> %{y:.1%}<br>",
                  "<extra></extra>"
                )) %>%
      add_trace(y = ~Revenue_PC_Growth,
                name = 'Per Capita Revenue Growth',
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color = '#20c997'),
                marker = list(color = '#20c997'),
                hovertemplate = paste(
                  "<b>Year:</b> %{x}<br>",
                  "<b>Growth Rate:</b> %{y:.1%}<br>",
                  "<extra></extra>"
                )) %>%
      layout(title = paste("Revenue Growth Trends -", input$selected_city),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Growth Rate",
                          tickformat = ".1%"),
             legend = list(orientation = "h", y = -0.2),
             showlegend = TRUE) %>%
      layout(plot_theme)
  })
  
  # Enhanced Expenditure Growth Trend Plot
  output$expenditure_growth_trend <- renderPlotly({
    data <- growth_data()
    
    plot_ly(data, x = ~Year4) %>%
      add_trace(y = ~Expenditure_Growth, 
                name = 'Total Expenditure Growth',
                type = 'scatter', 
                mode = 'lines+markers',
                line = list(color = '#dc3545'),
                marker = list(color = '#dc3545'),
                hovertemplate = paste(
                  "<b>Year:</b> %{x}<br>",
                  "<b>Growth Rate:</b> %{y:.1%}<br>",
                  "<extra></extra>"
                )) %>%
      add_trace(y = ~Expenditure_PC_Growth,
                name = 'Per Capita Expenditure Growth',
                type = 'scatter',
                mode = 'lines+markers',
                line = list(color = '#e83e8c'),
                marker = list(color = '#e83e8c'),
                hovertemplate = paste(
                  "<b>Year:</b> %{x}<br>",
                  "<b>Growth Rate:</b> %{y:.1%}<br>",
                  "<extra></extra>"
                )) %>%
      layout(title = paste("Expenditure Growth Trends -", input$selected_city),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Growth Rate",
                          tickformat = ".1%"),
             legend = list(orientation = "h", y = -0.2),
             showlegend = TRUE) %>%
      layout(plot_theme)
  })
  
  # Population Growth Trend Plot
  output$population_growth_trend <- renderPlotly({
    data <- growth_data()
    
    plot_ly(data, x = ~Year4, y = ~Population_Growth,
            name = 'Population Growth',
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = '#0dcaf0'),
            marker = list(color = '#0dcaf0'),
            hovertemplate = paste(
              "<b>Year:</b> %{x}<br>",
              "<b>Growth Rate:</b> %{y:.1%}<br>",
              "<extra></extra>"
            )) %>%
      layout(title = paste("Population Growth Trend -", input$selected_city),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Growth Rate",
                          tickformat = ".1%"),
             showlegend = FALSE) %>%
      layout(plot_theme)
  })
  
  # Per Capita Metrics Growth Comparison
  output$per_capita_growth_trend <- renderPlotly({
    data <- growth_data()
    
    plot_ly(data, x = ~Year4) %>%
      add_trace(y = ~Revenue_PC_Growth, 
                name = 'Revenue per Capita',
                type = 'scatter', 
                mode = 'lines+markers',
                line = list(color = '#28a745'),
                hovertemplate = paste(
                  "<b>Year:</b> %{x}<br>",
                  "<b>Growth Rate:</b> %{y:.1%}<br>",
                  "<extra></extra>"
                )) %>%
      add_trace(y = ~Expenditure_PC_Growth, 
                name = 'Expenditure per Capita',
                type = 'scatter', 
                mode = 'lines+markers',
                line = list(color = '#dc3545'),
                hovertemplate = paste(
                  "<b>Year:</b> %{x}<br>",
                  "<b>Growth Rate:</b> %{y:.1%}<br>",
                  "<extra></extra>"
                )) %>%
      add_trace(y = ~Debt_PC_Growth, 
                name = 'Debt per Capita',
                type = 'scatter', 
                mode = 'lines+markers',
                line = list(color = '#ffc107'),
                hovertemplate = paste(
                  "<b>Year:</b> %{x}<br>",
                  "<b>Growth Rate:</b> %{y:.1%}<br>",
                  "<extra></extra>"
                )) %>%
      layout(title = paste("Per Capita Metrics Growth -", input$selected_city),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Growth Rate",
                          tickformat = ".1%"),
             legend = list(orientation = "h", y = -0.2),
             showlegend = TRUE) %>%
      layout(plot_theme)
  })
  
  # Debt Metrics Plot with proper scaling
  output$debt_metrics <- renderPlotly({
    data <- current_year_data()
    metrics <- data.frame(
      Metric = c("Debt to Revenue", "Cash to Debt", "Debt Service Burden"),
      Value = c(data$Debt_to_Revenue, data$Cash_to_Debt, data$Debt_Service_Burden/100)
    )
    
    colors <- c('#ffc107', '#20c997', '#0dcaf0')
    
    plot_ly(metrics, x = ~Metric, y = ~Value, type = 'bar',
            marker = list(color = colors)) %>%
      layout(title = paste("Debt Metrics -", input$selected_city),
             xaxis = list(title = "Metric"),
             yaxis = list(title = "Ratio/Percentage",
                          tickformat = ".1%"),
             showlegend = FALSE) %>%
      layout(plot_theme) %>%
      add_trace(
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Value: %{y:.2%}<br>",
          "<extra></extra>"
        )
      )
  })
  
  # Debt Trend Plot with proper scaling
  output$debt_trend <- renderPlotly({
    data <- selected_city_data() %>%
      filter(!is.na(Long_Term_Debt_Per_Capita))
    
    plot_ly(data, x = ~Year4, 
            y = ~(Long_Term_Debt_Per_Capita * 1000), # Scale to actual dollars
            type = 'scatter', 
            mode = 'lines+markers',
            name = 'Debt Per Capita',
            line = list(color = '#ffc107'),
            marker = list(color = '#ffc107')) %>%
      layout(title = paste("Debt Trend -", input$selected_city),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Long Term Debt Per Capita ($)",
                          tickformat = "$,.0f"),
             showlegend = FALSE) %>%
      layout(plot_theme) %>%
      add_trace(
        hovertemplate = paste(
          "<b>Year:</b> %{x}<br>",
          "<b>Debt:</b> $%{y:,.2f} per capita<br>",
          "<extra></extra>"
        )
      )
  })
  
  # Benchmarks Prep
  # Calculate benchmark metrics
  benchmark_data <- reactive({
    req(input$selected_year, input$selected_city, input$benchmark_metric)
    
    # Get current city's population
    city_pop <- filtered_data() %>%
      filter(Name == input$selected_city,
             Year4 == input$selected_year) %>%
      pull(Population)
    
    # Calculate population range for peers
    pop_range <- city_pop * (1 + c(-input$peer_population_range/100, input$peer_population_range/100))
    
    # Get data for selected year and apply scaling for monetary values
    year_data <- filtered_data() %>%
      filter(Year4 == input$selected_year,
             !is.na(Total_Revenue)) %>%
      mutate(
        Revenue_Per_Capita = Revenue_Per_Capita * 1000,
        Total_Expenditure_Per_Capita = Total_Expenditure_Per_Capita * 1000
      )
    
    # Identify peer cities
    peer_data <- year_data %>%
      filter(Population >= pop_range[1],
             Population <= pop_range[2])
    
    # For metrics where higher is better (Revenue_Per_Capita, Operating_Ratio)
    higher_better <- c("Revenue_Per_Capita", "Operating_Ratio", "Cash_to_Debt")
    
    # Calculate rankings based on metric direction
    if(input$benchmark_metric %in% higher_better) {
      state_rank <- year_data %>%
        arrange(desc(!!sym(input$benchmark_metric))) %>%
        mutate(rank = row_number()) %>%
        filter(Name == input$selected_city) %>%
        pull(rank)
      
      peer_rank <- peer_data %>%
        arrange(desc(!!sym(input$benchmark_metric))) %>%
        mutate(rank = row_number()) %>%
        filter(Name == input$selected_city) %>%
        pull(rank)
      
      percentile <- ecdf(year_data[[input$benchmark_metric]])(
        year_data %>%
          filter(Name == input$selected_city) %>%
          pull(!!sym(input$benchmark_metric))
      )
    } else {
      state_rank <- year_data %>%
        arrange(!!sym(input$benchmark_metric)) %>%
        mutate(rank = row_number()) %>%
        filter(Name == input$selected_city) %>%
        pull(rank)
      
      peer_rank <- peer_data %>%
        arrange(!!sym(input$benchmark_metric)) %>%
        mutate(rank = row_number()) %>%
        filter(Name == input$selected_city) %>%
        pull(rank)
      
      percentile <- 1 - ecdf(year_data[[input$benchmark_metric]])(
        year_data %>%
          filter(Name == input$selected_city) %>%
          pull(!!sym(input$benchmark_metric))
      )
    }
    
    # Calculate statistics
    stats <- list(
      current_value = year_data %>%
        filter(Name == input$selected_city) %>%
        pull(!!sym(input$benchmark_metric)),
      
      state_mean = mean(year_data[[input$benchmark_metric]], na.rm = TRUE),
      state_median = median(year_data[[input$benchmark_metric]], na.rm = TRUE),
      state_sd = sd(year_data[[input$benchmark_metric]], na.rm = TRUE),
      
      peer_mean = mean(peer_data[[input$benchmark_metric]], na.rm = TRUE),
      peer_median = median(peer_data[[input$benchmark_metric]], na.rm = TRUE),
      peer_sd = sd(peer_data[[input$benchmark_metric]], na.rm = TRUE),
      
      percentile = percentile,
      state_rank = state_rank,
      total_cities = nrow(year_data),
      peer_rank = peer_rank,
      total_peers = nrow(peer_data)
    )
    
    # Calculate benchmark score (0-100)
    stats$benchmark_score <- if(input$benchmark_metric %in% higher_better) {
      pnorm(stats$current_value, 
            mean = stats$state_mean, 
            sd = stats$state_sd) * 100
    } else {
      (1 - pnorm(stats$current_value, 
                 mean = stats$state_mean, 
                 sd = stats$state_sd)) * 100
    }
    
    stats
  })
  
  # Render benchmark value boxes
  output$state_rank_box <- renderValueBox({
    data <- benchmark_data()
    valueBox(
      paste0("#", data$state_rank, " of ", data$total_cities),
      "State Rank",
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  output$percentile_box <- renderValueBox({
    data <- benchmark_data()
    valueBox(
      paste0(round(data$percentile * 100, 1), "th"),
      "Percentile",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$peer_rank_box <- renderValueBox({
    data <- benchmark_data()
    valueBox(
      paste0("#", data$peer_rank, " of ", data$total_peers),
      "Peer Group Rank",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$benchmark_score_box <- renderValueBox({
    data <- benchmark_data()
    valueBox(
      paste0(round(data$benchmark_score, 1)),
      "Benchmark Score",
      icon = icon("star"),
      color = if(data$benchmark_score >= 80) "green"
      else if(data$benchmark_score >= 60) "yellow"
      else "red"
    )
  })
  
  # Render benchmark distribution plot
  output$benchmark_distribution <- renderPlotly({
    req(input$selected_year, input$selected_city, input$benchmark_metric)
    
    # Get data for selected year
    data <- filtered_data() %>%
      filter(Year4 == input$selected_year,
             !is.na(Total_Revenue)) %>%
      mutate(
        Revenue_Per_Capita = Revenue_Per_Capita * 1000,
        Total_Expenditure_Per_Capita = Total_Expenditure_Per_Capita * 1000
      )
    
    # Get current city's value
    current_value <- data %>%
      filter(Name == input$selected_city) %>%
      pull(!!sym(input$benchmark_metric))
    
    # Create histogram with density curve
    p <- plot_ly() %>%
      add_histogram(
        data = data,
        x = as.formula(paste0("~", input$benchmark_metric)),
        name = "Distribution",
        marker = list(color = "rgba(31, 66, 87, 0.3)")
      ) %>%
      add_trace(
        x = c(current_value, current_value),
        y = c(0, 10),
        type = "scatter",
        mode = "lines",
        line = list(color = "#28a745", width = 2, dash = "dash"),
        name = "Your City"
      ) %>%
      layout(
        title = paste("Distribution of", gsub("_", " ", input$benchmark_metric)),
        showlegend = TRUE,
        xaxis = list(
          title = if(grepl("Per_Capita", input$benchmark_metric)) 
            "Amount per Capita ($)" 
          else 
            "Value",
          tickformat = if(grepl("Per_Capita", input$benchmark_metric)) 
            "$,.0f" 
          else 
            ".2f"
        ),
        yaxis = list(title = "Number of Cities"),
        bargap = 0.1
      ) %>%
      layout(plot_theme)
  })
  
  # Render peer comparison plot
  output$peer_comparison <- renderPlotly({
    req(input$selected_year, input$selected_city, input$benchmark_metric)
    
    # Get current city's population
    city_pop <- filtered_data() %>%
      filter(Name == input$selected_city,
             Year4 == input$selected_year) %>%
      pull(Population)
    
    # Calculate population range for peers
    pop_range <- city_pop * (1 + c(-input$peer_population_range/100, input$peer_population_range/100))
    
    # Get peer data and scale monetary values
    peer_data <- filtered_data() %>%
      filter(Year4 == input$selected_year,
             Population >= pop_range[1],
             Population <= pop_range[2],
             !is.na(Total_Revenue)) %>%
      mutate(
        Revenue_Per_Capita = Revenue_Per_Capita * 1000,
        Total_Expenditure_Per_Capita = Total_Expenditure_Per_Capita * 1000,
        is_selected = Name == input$selected_city
      )
    
    # Sort based on metric direction
    higher_better <- c("Revenue_Per_Capita", "Operating_Ratio", "Cash_to_Debt")
    if(input$benchmark_metric %in% higher_better) {
      peer_data <- peer_data %>% arrange(desc(!!sym(input$benchmark_metric)))
    } else {
      peer_data <- peer_data %>% arrange(!!sym(input$benchmark_metric))
    }
    
    # Create bar plot
    plot_ly(peer_data) %>%
      add_bars(
        x = ~Name,
        y = as.formula(paste0("~`", input$benchmark_metric, "`")),
        marker = list(
          color = ~ifelse(is_selected, "#28a745", "#1f4257")
        ),
        name = "Cities"
      ) %>%
      layout(
        title = "Peer City Comparison",
        xaxis = list(
          title = "City",
          tickangle = 45
        ),
        yaxis = list(
          title = if(grepl("Per_Capita", input$benchmark_metric)) 
            "Amount per Capita ($)" 
          else if(input$benchmark_metric == "Operating_Ratio")
            "Revenue/Expenditure Ratio"
          else 
            "Value",
          tickformat = if(grepl("Per_Capita", input$benchmark_metric)) 
            "$,.0f" 
          else 
            ".2f"
        ),
        showlegend = FALSE
      ) %>%
      layout(plot_theme)
  })
  
  # Render historical benchmark trend
  output$historical_benchmark <- renderPlotly({
    req(input$selected_city, input$benchmark_metric)
    
    # Calculate percentile for each year
    historical_percentiles <- filtered_data() %>%
      filter(!is.na(Total_Revenue)) %>%
      group_by(Year4) %>%
      mutate(
        percentile = ecdf(!!sym(input$benchmark_metric))(!!sym(input$benchmark_metric))
      ) %>%
      ungroup() %>%
      filter(Name == input$selected_city)
    
    # Create line plot
    plot_ly(historical_percentiles) %>%
      add_trace(
        x = ~Year4,
        y = ~percentile * 100,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#28a745"),
        marker = list(color = "#28a745"),
        name = "Percentile"
      ) %>%
      layout(
        title = "Historical Benchmark Position",
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "Percentile",
          tickformat = ".0f",
          range = c(0, 100)
        ),
        showlegend = FALSE
      ) %>%
      layout(plot_theme)
  })
  
  # Tax rate comparison plot
  output$tax_rate_comparison <- renderPlotly({
    req(input$selected_tax_type)
    
    data <- tax_data() %>%
      filter(tax_type == input$selected_tax_type) %>%
      arrange(desc(tax_rate))
    
    plot_ly(data, 
            x = ~reorder(Name, tax_rate), 
            y = ~tax_rate,
            type = 'bar',
            marker = list(color = '#1f4257'),
            hovertemplate = paste(
              "<b>%{x}</b><br>",
              "Tax Rate: %{y:.2f}%<br>",
              "<extra></extra>"
            )) %>%
      layout(
        title = paste("Tax Rates for", input$selected_tax_type, "(2024)"),
        xaxis = list(title = "Municipality",
                     tickangle = 45),
        yaxis = list(title = "Tax Rate (%)",
                     tickformat = ".2f"),
        showlegend = FALSE
      ) %>%
      layout(plot_theme)
  })
  
  # Tax rate distribution plot
  output$tax_rate_distribution <- renderPlotly({
    req(input$selected_tax_type)
    
    data <- tax_data() %>%
      filter(tax_type == input$selected_tax_type)
    
    plot_ly() %>%
      add_histogram(
        x = ~data$tax_rate,
        name = "Distribution",
        marker = list(color = "rgba(31, 66, 87, 0.6)")
      ) %>%
      add_trace(
        x = c(median(data$tax_rate), median(data$tax_rate)),
        y = c(0, 15),
        type = "scatter",
        mode = "lines",
        line = list(color = "#28a745", width = 2, dash = "dash"),
        name = "Median"
      ) %>%
      layout(
        title = "Distribution of Tax Rates (2024)",
        xaxis = list(title = "Tax Rate (%)",
                     tickformat = ".2f"),
        yaxis = list(title = "Number of Municipalities"),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2)
      ) %>%
      layout(plot_theme)
  })
  
  # Tax rate summary statistics
  output$tax_summary_stats <- renderTable({
    req(input$selected_tax_type)
    
    data <- tax_data() %>%
      filter(tax_type == input$selected_tax_type)
    
    tibble(
      Statistic = c("Minimum", "Maximum", "Mean", "Median", "Standard Deviation",
                    "Number of Municipalities"),
      Value = c(
        sprintf("%.2f%%", min(data$tax_rate)),
        sprintf("%.2f%%", max(data$tax_rate)),
        sprintf("%.2f%%", mean(data$tax_rate)),
        sprintf("%.2f%%", median(data$tax_rate)),
        sprintf("%.2f%%", sd(data$tax_rate)),
        length(unique(data$Name))
      )
    )
  })
  
  # Enhanced Tables with improved styling and formatting
  output$revenue_metrics_table <- DT::renderDataTable({
    withProgress(message = 'Preparing revenue metrics...', {
      current_year_data() %>%
        select(Name, Year4, 
               Revenue_Per_Capita, Own_Source_Revenue_Per_Capita,
               Property_Tax_Per_Capita, Sales_Tax_Per_Capita,
               Property_Tax_Pct, Sales_Tax_Pct, 
               License_Tax_Pct, Intergov_Revenue_Pct) %>%
        mutate(
          # Scale dollar amounts
          Revenue_Per_Capita = Revenue_Per_Capita * 1000,
          Own_Source_Revenue_Per_Capita = Own_Source_Revenue_Per_Capita * 1000,
          Property_Tax_Per_Capita = Property_Tax_Per_Capita * 1000,
          Sales_Tax_Per_Capita = Sales_Tax_Per_Capita * 1000,
          # Convert percentages to decimal form
          Property_Tax_Pct = Property_Tax_Pct / 100,
          Sales_Tax_Pct = Sales_Tax_Pct / 100,
          License_Tax_Pct = License_Tax_Pct / 100,
          Intergov_Revenue_Pct = Intergov_Revenue_Pct / 100
        ) %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            dom = 'Bfrtip',
            buttons = list(
              list(extend = 'copy', className = 'btn-primary'),
              list(extend = 'csv', className = 'btn-primary'),
              list(extend = 'excel', className = 'btn-primary')
            ),
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = '100px', targets = "_all"),
              list(className = 'dt-center', targets = '_all')
            )
          ),
          caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: #333;',
            'Revenue Metrics'
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        ) %>%
        formatCurrency(
          columns = c("Revenue_Per_Capita", "Own_Source_Revenue_Per_Capita",
                      "Property_Tax_Per_Capita", "Sales_Tax_Per_Capita"),
          currency = "$", digits = 2
        ) %>%
        formatPercentage(
          columns = c("Property_Tax_Pct", "Sales_Tax_Pct",
                      "License_Tax_Pct", "Intergov_Revenue_Pct"),
          digits = 1
        )
    })
  })
  
  output$expenditure_metrics_table <- DT::renderDataTable({
    withProgress(message = 'Preparing expenditure metrics...', {
      current_year_data() %>%
        select(Name, Year4,
               Total_Expenditure_Per_Capita,
               Police_Exp_Per_Capita, Fire_Exp_Per_Capita,
               Parks_Exp_Per_Capita, Highway_Exp_Per_Capita,
               Police_Exp_Pct, Fire_Exp_Pct,
               Parks_Exp_Pct, Highway_Exp_Pct) %>%
        mutate(
          # Scale dollar amounts
          Total_Expenditure_Per_Capita = Total_Expenditure_Per_Capita * 1000,
          Police_Exp_Per_Capita = Police_Exp_Per_Capita * 1000,
          Fire_Exp_Per_Capita = Fire_Exp_Per_Capita * 1000,
          Parks_Exp_Per_Capita = Parks_Exp_Per_Capita * 1000,
          Highway_Exp_Per_Capita = Highway_Exp_Per_Capita * 1000,
          # Convert percentages to decimal form
          Police_Exp_Pct = Police_Exp_Pct / 100,
          Fire_Exp_Pct = Fire_Exp_Pct / 100,
          Parks_Exp_Pct = Parks_Exp_Pct / 100,
          Highway_Exp_Pct = Highway_Exp_Pct / 100
        ) %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            dom = 'Bfrtip',
            buttons = list(
              list(extend = 'copy', className = 'btn-primary'),
              list(extend = 'csv', className = 'btn-primary'),
              list(extend = 'excel', className = 'btn-primary')
            ),
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = '100px', targets = "_all"),
              list(className = 'dt-center', targets = '_all')
            )
          ),
          caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: #333;',
            'Expenditure Metrics'
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        ) %>%
        formatCurrency(
          columns = c("Total_Expenditure_Per_Capita",
                      "Police_Exp_Per_Capita", "Fire_Exp_Per_Capita",
                      "Parks_Exp_Per_Capita", "Highway_Exp_Per_Capita"),
          currency = "$", digits = 2
        ) %>%
        formatPercentage(
          columns = c("Police_Exp_Pct", "Fire_Exp_Pct",
                      "Parks_Exp_Pct", "Highway_Exp_Pct"),
          digits = 1
        )
    })
  })
  
  output$debt_metrics_table <- DT::renderDataTable({
    withProgress(message = 'Preparing debt metrics...', {
      current_year_data() %>%
        select(Name, Year4,
               Long_Term_Debt_Per_Capita,
               Debt_to_Revenue, Cash_to_Debt,
               Debt_Service_Burden) %>%
        mutate(
          # Scale dollar amounts
          Long_Term_Debt_Per_Capita = Long_Term_Debt_Per_Capita * 1000,
          # Convert percentage to decimal form
          Debt_Service_Burden = Debt_Service_Burden / 100
        ) %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            dom = 'Bfrtip',
            buttons = list(
              list(extend = 'copy', className = 'btn-primary'),
              list(extend = 'csv', className = 'btn-primary'),
              list(extend = 'excel', className = 'btn-primary')
            ),
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = '100px', targets = "_all"),
              list(className = 'dt-center', targets = '_all')
            )
          ),
          caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: #333;',
            'Debt Metrics'
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        ) %>%
        formatCurrency(
          columns = c("Long_Term_Debt_Per_Capita"),
          currency = "$", digits = 2
        ) %>%
        formatPercentage(
          columns = c("Debt_Service_Burden"),
          digits = 1
        ) %>%
        formatRound(
          columns = c("Debt_to_Revenue", "Cash_to_Debt"),
          digits = 2
        )
    })
  })
  
  output$growth_metrics_table <- DT::renderDataTable({
    withProgress(message = 'Preparing growth metrics...', {
      growth_data() %>%
        select(Year4,
               Revenue_Growth, Expenditure_Growth,
               Population_Growth, Revenue_PC_Growth,
               Expenditure_PC_Growth, Debt_PC_Growth) %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            dom = 'Bfrtip',
            buttons = list(
              list(extend = 'copy', className = 'btn-primary'),
              list(extend = 'csv', className = 'btn-primary'),
              list(extend = 'excel', className = 'btn-primary')
            ),
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = '100px', targets = "_all"),
              list(className = 'dt-center', targets = '_all')
            )
          ),
          caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: #333;',
            'Growth Metrics'
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover'
        ) %>%
        formatPercentage(
          columns = c("Revenue_Growth", "Expenditure_Growth",
                      "Population_Growth", "Revenue_PC_Growth",
                      "Expenditure_PC_Growth", "Debt_PC_Growth"),
          digits = 1
        )
    })
  })
  
  output$tax_rates_table <- DT::renderDataTable({
    req(input$selected_tax_type)
    
    data <- tax_data() %>%
      filter(tax_type == input$selected_tax_type) %>%
      select(Name, tax_rate) %>%
      arrange(desc(tax_rate))
    
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn-primary'),
          list(extend = 'csv', className = 'btn-primary'),
          list(extend = 'excel', className = 'btn-primary')
        ),
        order = list(list(1, 'desc')),
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: #333;',
        paste('Tax Rates for', input$selected_tax_type, '(2024)')
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatRound('tax_rate', digits = 2)
  })
  
  output$benchmark_table <- DT::renderDataTable({
    req(input$selected_year, input$selected_city, input$benchmark_metric)
    
    # Get current city's population
    city_pop <- filtered_data() %>%
      filter(Name == input$selected_city,
             Year4 == input$selected_year) %>%
      pull(Population)
    
    # Calculate population range for peers
    pop_range <- city_pop * (1 + c(-input$peer_population_range/100, input$peer_population_range/100))
    
    # Get data for selected year
    year_data <- filtered_data() %>%
      filter(Year4 == input$selected_year,
             !is.na(Total_Revenue)) %>%
      mutate(
        Revenue_Per_Capita = Revenue_Per_Capita * 1000,
        Total_Expenditure_Per_Capita = Total_Expenditure_Per_Capita * 1000
      )
    
    # Get peer data
    peer_data <- year_data %>%
      filter(Population >= pop_range[1],
             Population <= pop_range[2])
    
    # Create comparison data frame
    comparison_data <- data.frame(
      Metric = c(
        "Your Value",
        "State Average",
        "State Median",
        "Peer Average",
        "Top Quartile",
        "Bottom Quartile"
      ),
      Value = c(
        year_data %>% 
          filter(Name == input$selected_city) %>% 
          pull(!!sym(input$benchmark_metric)),
        mean(year_data[[input$benchmark_metric]], na.rm = TRUE),
        median(year_data[[input$benchmark_metric]], na.rm = TRUE),
        mean(peer_data[[input$benchmark_metric]], na.rm = TRUE),
        quantile(year_data[[input$benchmark_metric]], 0.75, na.rm = TRUE),
        quantile(year_data[[input$benchmark_metric]], 0.25, na.rm = TRUE)
      )
    ) %>%
      mutate(
        `Difference from Your Value` = Value - Value[Metric == "Your Value"],
        `Percent Difference` = ((Value - Value[Metric == "Your Value"]) / 
                                  abs(Value[Metric == "Your Value"]))
      )
    
    DT::datatable(
      comparison_data,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn-primary'),
          list(extend = 'csv', className = 'btn-primary'),
          list(extend = 'excel', className = 'btn-primary')
        )
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: #333;',
        'Benchmark Comparison'
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatCurrency(
        columns = "Value",
        currency = if(grepl("Per_Capita", input$benchmark_metric)) "$" else "",
        digits = if(grepl("Per_Capita", input$benchmark_metric)) 0 else 3
      ) %>%
      formatCurrency(
        columns = "Difference from Your Value",
        currency = if(grepl("Per_Capita", input$benchmark_metric)) "$" else "",
        digits = if(grepl("Per_Capita", input$benchmark_metric)) 0 else 3
      ) %>%
      formatPercentage(
        columns = "Percent Difference",
        digits = 1
      )
  })
  
  # Prepare time series data for forecasting
  prepare_ts_data <- reactive({
    req(input$selected_city, input$forecast_metric)
    
    if (input$forecast_metric == "Operating_Ratio") {
      # Calculate Operating Ratio from Revenue and Expenditure
      data <- selected_city_data() %>%
        filter(!is.na(Total_Revenue), 
               !is.na(Total_Expenditure),
               Year4 < 2024) %>%
        arrange(Year4) %>%
        mutate(Operating_Ratio = Total_Revenue / Total_Expenditure) %>%
        pull(Operating_Ratio)
    } else {
      data <- selected_city_data() %>%
        filter(!is.na(!!sym(input$forecast_metric)),
               Year4 < 2024) %>%
        arrange(Year4) %>%
        pull(!!sym(input$forecast_metric))
    }
    
    # Convert to time series object
    ts(data, start = min(selected_city_data()$Year4), frequency = 1)
  })
  
  # Generate forecast
  generate_forecast <- reactive({
    req(input$forecast_years)
    
    ts_data <- prepare_ts_data()
    
    # Use ETS model for forecasting
    forecast(ets(ts_data), h = input$forecast_years)
  })
  
  # Render forecast plot
  output$forecast_plot <- renderPlotly({
    forecast_result <- generate_forecast()
    
    # Create data frame for plotting
    historical_data <- selected_city_data() %>%
      filter(!is.na(Total_Revenue)) %>%
      arrange(Year4)
    
    if (input$forecast_metric == "Operating_Ratio") {
      historical_data <- historical_data %>%
        mutate(Operating_Ratio = Total_Revenue / Total_Expenditure)
    }
    
    historical_data <- historical_data %>%
      select(Year4, !!sym(input$forecast_metric))
    
    # Prepare forecast data
    forecast_years <- seq(max(historical_data$Year4) + 1,
                          max(historical_data$Year4) + input$forecast_years)
    
    forecast_data <- data.frame(
      Year4 = forecast_years,
      Point_Forecast = forecast_result$mean,
      Lower_80 = forecast_result$lower[, 1],
      Upper_80 = forecast_result$upper[, 1],
      Lower_95 = forecast_result$lower[, 2],
      Upper_95 = forecast_result$upper[, 2]
    )
    
    # Calculate y-axis range for Operating Ratio
    if (input$forecast_metric == "Operating_Ratio") {
      all_values <- c(
        historical_data[[input$forecast_metric]],
        forecast_data$Point_Forecast,
        forecast_data$Lower_95,
        forecast_data$Upper_95
      )
      min_val <- min(all_values, na.rm = TRUE)
      max_val <- max(all_values, na.rm = TRUE)
      # Add 10% padding to the range
      range_padding <- (max_val - min_val) * 0.1
      y_range <- c(min_val - range_padding, max_val + range_padding)
    } else {
      # Scale values by 1000 for display if not Operating Ratio
      historical_data[[input$forecast_metric]] <- historical_data[[input$forecast_metric]] * 1000
      forecast_data$Point_Forecast <- forecast_data$Point_Forecast * 1000
      forecast_data$Lower_80 <- forecast_data$Lower_80 * 1000
      forecast_data$Upper_80 <- forecast_data$Upper_80 * 1000
      forecast_data$Lower_95 <- forecast_data$Lower_95 * 1000
      forecast_data$Upper_95 <- forecast_data$Upper_95 * 1000
      y_range <- NULL  # Let plotly auto-scale for revenue/expenditure
    }
    
    # Create plot
    plot_ly() %>%
      # Historical data
      add_trace(
        data = historical_data,
        x = ~Year4,
        y = as.formula(paste0("~", input$forecast_metric)),
        type = "scatter",
        mode = "lines+markers",
        name = "Historical",
        line = list(color = "#1f4257"),
        marker = list(color = "#1f4257"),
        hovertemplate = if(input$forecast_metric == "Operating_Ratio") 
          paste(
            "<b>Year:</b> %{x}<br>",
            "<b>Operating Ratio:</b> %{y:.3f}<br>",
            "<extra></extra>"
          ) else paste(
            "<b>Year:</b> %{x}<br>",
            "<b>Amount:</b> $%{y:,.2f}<br>",
            "<extra></extra>"
          )
      ) %>%
      # Forecast
      add_trace(
        data = forecast_data,
        x = ~Year4,
        y = ~Point_Forecast,
        type = "scatter",
        mode = "lines+markers",
        name = "Forecast",
        line = list(color = "#28a745", dash = "dash"),
        marker = list(color = "#28a745"),
        hovertemplate = if(input$forecast_metric == "Operating_Ratio") 
          paste(
            "<b>Year:</b> %{x}<br>",
            "<b>Operating Ratio:</b> %{y:.3f}<br>",
            "<extra></extra>"
          ) else paste(
            "<b>Year:</b> %{x}<br>",
            "<b>Amount:</b> $%{y:,.2f}<br>",
            "<extra></extra>"
          )
      ) %>%
      # 95% Confidence interval
      add_ribbons(
        data = forecast_data,
        x = ~Year4,
        ymin = ~Lower_95,
        ymax = ~Upper_95,
        name = "95% CI",
        fillcolor = "rgba(40, 167, 69, 0.1)",
        line = list(color = "transparent"),
        showlegend = TRUE
      ) %>%
      # 80% Confidence interval
      add_ribbons(
        data = forecast_data,
        x = ~Year4,
        ymin = ~Lower_80,
        ymax = ~Upper_80,
        name = "80% CI",
        fillcolor = "rgba(40, 167, 69, 0.2)",
        line = list(color = "transparent"),
        showlegend = TRUE
      ) %>%
      layout(
        title = paste("Forecast for", gsub("_", " ", input$forecast_metric)),
        xaxis = list(title = "Year"),
        yaxis = list(
          title = if(input$forecast_metric == "Operating_Ratio") 
            "Operating Ratio (Revenue/Expenditure)" 
          else 
            "Amount (Dollars)",
          tickformat = if(input$forecast_metric == "Operating_Ratio") 
            ".2f" 
          else 
            "$,.0f",
          range = y_range
        ),
        hovermode = "x unified",
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2)
      ) %>%
      layout(plot_theme)
  })
  
  # Render forecast table
  output$forecast_table <- DT::renderDataTable({
    forecast_result <- generate_forecast()
    
    # Create forecast table data
    forecast_data <- data.frame(
      Year = seq(max(selected_city_data()$Year4) + 1,
                 max(selected_city_data()$Year4) + input$forecast_years),
      `Point Forecast` = forecast_result$mean,
      `Lower 80` = forecast_result$lower[, 1],
      `Upper 80` = forecast_result$upper[, 1],
      `Lower 95` = forecast_result$lower[, 2],
      `Upper 95` = forecast_result$upper[, 2]
    )
    
    # Scale monetary values
    if (input$forecast_metric != "Operating_Ratio") {
      forecast_data[-1] <- forecast_data[-1] * 1000
    }
    
    # Format table
    DT::datatable(
      forecast_data,
      options = list(
        pageLength = input$forecast_years,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn-primary'),
          list(extend = 'csv', className = 'btn-primary'),
          list(extend = 'excel', className = 'btn-primary')
        )
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold; color: #333;',
        paste('Forecast Values for', gsub("_", " ", input$forecast_metric))
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = if(input$forecast_metric == "Operating_Ratio") 2:6 else NULL,
        digits = 3
      ) %>%
      formatCurrency(
        columns = if(input$forecast_metric != "Operating_Ratio") 2:6 else NULL,
        currency = "$",
        digits = 0
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)