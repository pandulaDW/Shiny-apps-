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
library(lubridate)

# Script files ----------
source("r_script.R")
source('data_manip.R')
source("equity_graph.R")

# Define UI for data upload app ----
ui <- fluidPage(
  
  titlePanel('Clustering App'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Date range
      dateRangeInput(inputId = 'select_date', label = 'Select the date range', 
                     start = (ymd(Sys.Date()) - years(1)), end = (ymd(Sys.Date()) - days(1)),
                     max = (ymd(Sys.Date()) - days(1)), format = "yyyy/mm/dd", separator = 'to'),
      
      htmlOutput("range"), 
      tags$br(),
      actionButton(inputId = 'confirm_range', label = 'Confirm data range'),
      
      tags$hr(),
      
      # Input widget 
      fileInput(inputId = 'upload',label = 'Choose CSV File',
                accept = c("text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),
      
      # Horizontal line 
      tags$hr(),
      
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
      actionButton(inputId = 'submit_init', label = 'Submit'),
      
      tags$hr(),
      
      selectInput(inputId = 'select_output', label = "Select the main clusters needed for the output",
                  choices = c(""), multiple = TRUE),
      
      selectInput(inputId = 'select_output_sub', label = "Select the sub clusters needed for the output",
                  choices = c(""), multiple = TRUE),
      tags$hr(), 
      
      actionButton(inputId = 'output_init', label = 'Proceed')
    ),
    mainPanel(
      
      tabsetPanel(id = 'main_tabset',
        tabPanel(title = 'Data', value = 'data_panel',
          tableOutput(outputId = 'initial_content'),
          tableOutput(outputId = 'cluster_table')),
      tabPanel(title = 'Graphs', value = 'graph_panel',
          plotlyOutput(outputId = 'equity_graph'),
          tags$hr(),
      fluidRow(
        column(3, 
          selectInput(inputId = 'cluster_select', label = 'Select Clusters', 
                      choices = c('Cluster 1' = 1))),
        column(3, 
          numericInput("c_drawdown", label = "Startegy drawdown limit", value = 1)), 
        column(2, tags$br(), 
          actionButton(inputId = 'single_submit', label = 'Update')),
        column(4,
          selectInput(inputId = 'update_dw', label = 'Maximum Drawdowns', 
                      choices = c()))
      ),
        plotlyOutput(outputId = 'single_cluster_graph'),
          tags$hr(),
      fluidRow(
        column(5, offset = 5,
          actionButton(inputId = 'subcluster_init', label = h4('Sub-cluster')))
      ),
          tags$hr()
      ),
      tabPanel(title = 'Sub-cluster', value = 'sub_cluster_panel',
           tableOutput(outputId = 'subcluster_table'),
           tags$hr(),
           plotlyOutput(outputId = 'sub_graph_equity'),
        fluidRow(
             column(3, 
                    selectInput(inputId = 'cluster_select_sub', label = 'Select Clusters', 
                                choices = c('Sub_cluster 1' = 1))),
             column(3, 
                    numericInput("c_drawdown_sub", label = "Startegy drawdown limit", value = 1)), 
             column(2, tags$br(), 
                    actionButton(inputId = 'single_submit_sub', label = 'Update')),
             column(4,
                    selectInput(inputId = 'update_dw_sub', label = 'Maximum Drawdowns', 
                                choices = c()))
        ),
        plotlyOutput(outputId = 'sub_graph_single')
      ),
      tabPanel(title = 'Output', value = 'output_tab_panel', 
               downloadButton(outputId = 'download_cluster', label = 'Download Cluster Data'))
    )
  )
 )
)

# Define server logic to read selected file ----
server <- function(input, output, session){
  
  output$range <- renderText({
    paste("Selected date range is ", "<b>", input$select_date[1], "</b>", " to ", "<b>", 
          input$select_date[2], "</b>")
  })
  
  gen_seq <- observeEvent(input$confirm_range, {
    start = as.character(input$select_date[1])
    end = as.character(input$select_date[2])
    
    master_seq <- seq(ymd(start),ymd(end), by = '1 week')
    master_seq <- sapply(master_seq, as.character, USE.NAMES = FALSE)
    
    data <- as.data.frame(master_seq)
    write.csv(data, file = 'data_file.csv', row.names = FALSE)
  })
  
 output$initial_content <- renderTable({
   
   start = as.character(input$select_date[1])
   end = as.character(input$select_date[2])
   master_seq <- seq(ymd(start),ymd(end), by = '1 week')
   master_seq <- sapply(master_seq, as.character, USE.NAMES = FALSE)
   
   # Ensure that file is uploaded before proceding 
   
   
   if (is.null(input$upload)){
      return ()
   } else {
       df <- read.csv(input$upload$datapath, header = TRUE)
       df[,1] =  as.Date(df[, 1], format = '%m/%d/%Y')
       
     if( all(df[, 1] == master_seq) ){
       data <- read.csv('data_file.csv')
       df[, 1] <- NULL 
       data <- cbind(data, df)
       write.csv(data, file = 'data_file.csv', row.names = FALSE)
         if (input$display == 'head'){
            return(head(data))  
         } else {
           return(data)    
         }
  } else {
       return(paste('Try again'))
     }
   }
})
   
  # Running the clustering function when the button clicks
  cluster_df <- observeEvent(input$submit_init, {
    
    req('data_file.csv')
    
    df <- read.csv('data_file.csv')
    
    n = as.integer(input$cluster_number)
    clusters <- cluster_function(df, n)
    
    write.csv(clusters, file = 'final.csv')
    final = read.csv('final.csv')
    
    MtoM = create_MtoM(df, final)
    cumsum = create_cumsum(MtoM)
    table = arrange_cluster(final)
    
    output$cluster_table <- renderTable({ table })
    
    output$equity_graph <- renderPlotly({ main_equity_graph(cumsum) })
    
    max_cum = create_maxcum(cumsum)
    dd = create_DD(cumsum, max_cum)
    lower_band = create_lowerband(cumsum, max_cum, dd)
    
    c_names <- paste('Cluster', 1:n, sep = " ")
    c_values <- 1:n
    c_full <- setNames(c_values, nm = c_names)
    
    # Update the output selectInput field 
    updateSelectInput(session, inputId = "select_output", label = "Some label", 
                      choices = c_full, selected = head(c_full, 1))
    
    # Setting the label and select items
    updateSelectInput(session, inputId = "cluster_select",
                      label = "Select input label",
                      choices = c_full,
                      selected = head(c_full, 1))
    
    # Initial run for the cluster 1
    output$single_cluster_graph <- renderPlotly({ 
      single_cluster_graph(MtoM, cumsum, max_cum, lower_band, dd, n = 1)
  })
    
    dw_cluster = apply(dd[, -1], 2, min)
    dw_cluster = paste('Cluster_', 1:n, ' = (', round(unname(dw_cluster[1:n]),2), ')')
    
    # Updating the maximum drawdowns 
    updateSelectInput(session, inputId = 'update_dw', label = 'some label',
                      choices = dw_cluster, selected = head(dw_cluster, 1))
    
    # Updating the graph reactively 
  single_graph <- observeEvent(input$single_submit, {  
    
    c_select = as.integer(input$cluster_select)
    c_drawdown = as.integer(input$c_drawdown)
    lower_band = create_lowerband(cumsum, max_cum, dd, max_dw = c_drawdown, n = c_select)
    
    output$single_cluster_graph <- renderPlotly({ 
          single_cluster_graph(MtoM, cumsum, max_cum, lower_band, dd, n = c_select)
   })
  })
  
  # Populate the subcluster tab 
  subclusters <- observeEvent(input$subcluster_init, {
    
    c_select = as.integer(input$cluster_select)
    selected = final %>% filter(clusters == c_select)
    rev = as.character(selected[['X']])
    cluster_sub = df[rev]
    cluster_sub = cbind(df[,1], cluster_sub)
    clusters <- cluster_function(cluster_sub, 4)
    
    write.csv(cluster_sub, file = 'cluster_sub.csv')
    write.csv(clusters, file = 'final_sub.csv')
    final_sub = read.csv('final_sub.csv')
    sub_table = arrange_cluster(final_sub)
    
    output$subcluster_table = renderTable({ sub_table })
    
    updateTabsetPanel(session, inputId = 'main_tabset', 
                      selected = 'sub_cluster_panel')
    
    # Creating the graphs again for the subclusters 
    MtoM_sub = create_MtoM(cluster_sub, final_sub)
    cumsum_sub = create_cumsum(MtoM_sub)
    
    output$sub_graph_equity <- renderPlotly({ sub_main_equity_graph(cumsum, cumsum_sub, c_select) })
    
    max_cum_sub = create_maxcum(cumsum_sub)
    dd_sub = create_DD(cumsum_sub, max_cum_sub)
    lower_band_sub = create_lowerband(cumsum_sub, max_cum_sub, dd_sub)
    
    # Initial run for the sub_cluster 1
    output$sub_graph_single <- renderPlotly({ 
      single_cluster_graph(MtoM_sub, cumsum_sub, max_cum_sub, lower_band_sub, dd_sub, n = 1)
    })
    
    # Updating the maximum drawdowns 
    dw_cluster_sub = apply(dd_sub[, -1], 2, min)
    dw_cluster_sub = paste('Sub_cluster_', 1:4, ' = (', round(unname(dw_cluster_sub[1:4]),2), ')')
      
    updateSelectInput(session, inputId = 'update_dw_sub', label = 'some label',
                      choices = dw_cluster_sub, selected = head(dw_cluster_sub, 1))
    
    # Filling the subcluster select input drawdown
    c_names_sub <- paste('Sub_cluster', 1:4, sep = " ")
    c_values_sub <- 1:4
    c_full_sub <- setNames(c_values_sub, nm = c_names_sub)
    
    updateSelectInput(session, inputId = "cluster_select_sub",
                      label = "Select input label",
                      choices = c_full_sub,
                      selected = head(c_full_sub, 1))
    
    # Updating the subcluster graph reactively 
    single_sub_graph <- observeEvent(input$single_submit_sub, {  
      
      c_select_sub = as.integer(input$cluster_select_sub)
      c_drawdown_sub = as.integer(input$c_drawdown_sub)
      lower_band_sub = create_lowerband(cumsum_sub, max_cum_sub, dd_sub, 
                                    max_dw = c_drawdown_sub, n = c_select_sub)
      
      output$sub_graph_single <- renderPlotly({ 
        single_cluster_graph(MtoM_sub, cumsum_sub, max_cum_sub, lower_band_sub, dd_sub,
                             n = c_select_sub)
      })
    })
  
    updateSelectInput(session, inputId = "select_output_sub", label = "Some label", 
                      choices = c("Full_cluster" = 0, c_full_sub))
    
    if (is.null(input$select_output_sub)) {
      c_full = as.list(c_full)
      c_full[c_select] = NULL
      updateSelectInput(session, inputId = "select_output", label = "some label", 
                        choices = c_full)
    }
  })
  
  # Populate the output tab 
  output_tab <- observeEvent(input$output_init, {
    
    c_select = as.integer(input$cluster_select)
    final = read.csv('final.csv')
    
    final_output_main = final[final$clusters %in% input$select_output,]
    write.csv(final_output_main, "main_clusters.csv")
    final_output_main = read.csv('main_clusters.csv', stringsAsFactors = FALSE)
    
    # Outputing the strategies in subclusters  
    if (!is.null(input$select_output_sub)) {
      
      final_sub = read.csv('final_sub.csv')
      
      if(0 %in% input$select_output_sub){
        final_full_cluster = final[final$clusters == c_select,]
        write.csv(final_full_cluster, "full_cluster.csv")
        final_full_cluster = read.csv('full_cluster.csv', stringsAsFactors = FALSE)
     } else{
      final_output_sub = final_sub[final_sub$clusters %in% input$select_output_sub,]
      write.csv(final_output_sub, "sub_clusters.csv")
      final_output_sub = read.csv('sub_clusters.csv', stringsAsFactors = FALSE)
     }
    }
    
    # Appending the Strategies
    if (exists('final_full_cluster')){
      dishun = append(final_output_main$X, final_full_cluster$X)
    } else if (exists('final_output_sub')){
      dishun = append(final_output_main$X, final_output_sub$X)
    } else {
      dishun = final_output_main$X
    }
    
    write.csv(dishun, file = 'dishun.csv')
    data = mtcars
    updateTabsetPanel(session, selected= 'output_tab_panel', inputId= 'main_tabset')
    
    output$download_cluster = downloadHandler(filename = function(){ paste('cluster_data', '.csv', sep='')},  
                                              content = function(file) {write.csv(dishun, file)})
  })
 })
}
#  
# Create Shiny app ----
shinyApp(ui, server)
