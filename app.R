library(shiny)
library(ggplot2)
library(dplyr)

# Read the sales data from the CSV file and clean it
salesdata = read.csv("sales_data_sample.csv")
cleandata = na.omit(salesdata)

# Extract the relevant features for clustering
X = cleandata[, c("QUANTITYORDERED", "PRICEEACH", "SALES", "MSRP")]

# Standardize the data to have mean=0 and variance=1
X_std = scale(X)

# Perform clustering
k = 4
kmeans_model = kmeans(X_std, centers = k, nstart = 25)

# Add cluster labels to the data frame
cleandata$Cluster = as.factor(kmeans_model$cluster)

# Define the UI for the Shiny app
ui = fluidPage(
  titlePanel("Sales Data Clustering"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_axis", "Select X-axis variable:", choices = colnames(X)),
      selectInput("y_axis", "Select Y-axis variable:", choices = colnames(X)),
      numericInput("cluster_num", "Number of Clusters:", value = k)
    ),
    mainPanel(
      plotOutput("scatter_plot")
    )
  )
)

# Define the server for the Shiny app
server = function(input, output) {
  output$scatter_plot = renderPlot({
    # Get the selected x-axis and y-axis variables
    x_var = input$x_axis
    y_var = input$y_axis
    
    # Get the number of clusters from the input
    k = input$cluster_num
    
    # Perform clustering again with the updated number of clusters
    kmeans_model = kmeans(X_std, centers = k, nstart = 25)
    
    # Add updated cluster labels to the data frame
    cleandata$Cluster = as.factor(kmeans_model$cluster)
    
    # Create the scatter plot using ggplot2
    ggplot(cleandata, aes(x = .data[[x_var]], y = .data[[y_var]], color = Cluster)) +
      geom_point(size = 3) +
      labs(title = paste("Scatter Plot - Clusters based on", x_var, "and", y_var),
           x = x_var, y = y_var, color = "Cluster") +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui, server)
