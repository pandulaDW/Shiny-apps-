# runApp('C:/Users/Pandula/Desktop/Dash apps/Shiny Apps/clustering-app')
# Packages --------
library(shiny)
library(dtw)
library(dtwclust)
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(plotly)

# Script files ----------
source("r_script.R")
source('data_manip.R')
source("equity_graph.R")

# Define UI for data upload app ----
ui <- fluidPage(
  
  titlePanel('Clustering App'),
  
  sidebarLayout(
    
    sidebarPanel(
      # Input widget 
      fileInput(inputId = 'org_file',label = 'Choose CSV File',
                accept = c("text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),
      
      # Horizontal line 
      tags$hr(),
      
      # Input: Checkbox if the file has header 
      checkboxInput(inputId = 'header', label = "Header", value = TRUE),
      
      # Input : Select seperator 
      radioButtons(inputId = 'seperator', label = 'Seperator', 
                   choices = c(Comma = ',', 
                               Semicolon =  ';',
                               Tab = '\t'),
                   selected = ','),
      
      # Input : Select number of rows to display 
      radioButtons(inputId = 'display', label = 'Display',
                   choices = c(Head = 'head',
                               All = 'all'),
                   selected = 'head'),
      
      # Horizontal line 
      tags$hr(),
      
      # Choose the number of clusters
       selectInput(inputId = 'cluster_number', label = 'Enter the number of clusters',
                    choices = c('Four-Clusters' = 4, 'Five-Clusters' = 5, 'Six-Clusters' = 6,
                                'Seven-Clusters' = 7, 'Eight-Clusters' = 8), selected = 5),
      
      # Submit button 
      actionButton(inputId = 'submit_init', label = 'Submit')
      
    ),
    mainPanel(
      
      tabsetPanel(
        tabPanel(title = 'Data',
          tableOutput(outputId = 'initial_content'),
          tableOutput(outputId = 'cluster_table')),
      tabPanel(title = 'Graphs',
          plotlyOutput(outputId = 'equity_graph'),
          tags$hr(),
          selectInput(inputId = 'cluster_select', label = 'Select Clusters', 
                      choices = c('Cluster 1' = 1)),
          plotlyOutput(outputId = 'single_cluster_graph')
      )
    )
  )
 )
)

# Define server logic to read selected file ----
server <- function(input, output, session){
  
 output$initial_content <- renderTable({
   
   # Ensure that file is uploaded before proceding 
   req(input$org_file)
   
   df <- read.csv(input$org_file$datapath,
                  header = input$header, sep = input$seperator)
   
   write.csv(df, file = 'df.csv', row.names = FALSE)
   
    if (input$display == 'head'){
      return(head(df))
    } 
    else {
      return(df)
    }
 })
  
  # Running the clustering function when the button clicks
  cluster_df <- observeEvent(input$submit_init, {
    
    req(input$org_file)
    
    df <- read.csv(input$org_file$datapath,
                   header = input$header, sep = input$seperator)
    
    n = as.integer(input$cluster_number)
    clusters <- cluster_function(df, n)
    
    write.csv(clusters, file = 'final.csv')
    MtoM = create_MtoM()
    cumsum = create_cumsum(MtoM)
    table = arrange_cluster()
    
    output$cluster_table <- renderTable({ table })
    
    output$equity_graph <- renderPlotly({ main_equity_graph(cumsum) })
    
    max_cum = create_maxcum(cumsum)
    dd = create_DD(cumsum, max_cum)
    lower_band = create_lowerband(cumsum, max_cum, dd)
    
    c_names <- paste('Cluster', 1:n, sep = " ")
    c_values <- 1:n
    c_full <- setNames(c_values, nm = c_names)
    
    # Setting the label and select items
    updateSelectInput(session, inputId = "cluster_select",
                      label = "Select input label",
                      choices = c_full,
                      selected = head(c_full, 1))
    
    c_select = as.integer(input$cluster_select)
    
    output$single_cluster_graph <- renderPlotly({ 
          single_cluster_graph(MtoM, cumsum, max_cum, lower_band, dd, n = c_select)
   })
 })
}

# Create Shiny app ----
shinyApp(ui, server)




