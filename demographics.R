library(readxl)
library(data.table)
library(ggplot2)
library(shiny)

demographics <- setDT(read_excel(
    "C:/Users/KangJ.WGTNNZ/Downloads/20191010 Dataset Population Ages by TA to Jie Kang.xlsx",
    sheet = "data",
    skip = 3
  ))

age_levels <- c("Median", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
demographics$`Age group` <- factor(demographics$`Age group`, levels = age_levels)


## All
ggplot(demographics, aes(x = ifelse(Sex == "Male", Number, -Number), y = `Age group`, fill = Sex)) +
  geom_bar(stat = "identity", position = "identity") +
  labs(x = "Percentage at Each Age Cohort", y = "Age Group", title = "Tornado Plot by Age Group") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  scale_x_continuous(labels = abs)   # Remove negative sign on x-axis for clarity
# facet_wrap(~ Area)  # Facet the plot by 'Area' (Urban vs Rural)

## Function to choose where and when 
create_tornado_plot <- function(year, area, data) {
  # Filter data based on Year and Area
  plot_data <- subset(data, Year == year & Area == area)
  
  # Generate the tornado plot
  plot <- ggplot(plot_data, aes(x = ifelse(Sex == "Male", Number, -Number), 
                                y = `Age group`, fill = Sex)) +
    geom_bar(stat = "identity", position = "identity") +
    labs(x = "Percentage at Each Age Cohort", y = "Age Group", 
         title = paste("Tornado Plot by Age Group, Year:", year, "Area:", area)) +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
    scale_x_continuous(labels = abs)  # Remove negative sign on x-axis for clarity
  
  # Return the plot
  return(plot)
}

# Example usage: create a tornado plot for Year = 2020 and Area = "Urban"
create_tornado_plot(1996, "Palmerston North", demographics)
create_tornado_plot(2013, "Dunedin", demographics)

## R shiny app
ui <- fluidPage(
  titlePanel("Tornado Plot Generator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("area", "Select Area:", choices = sort(unique(demographics$Area))),
      selectInput("year", "Select Year:", choices = sort(unique(demographics$Year)))
    ),
    
    mainPanel(
      plotOutput("tornadoPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  output$tornadoPlot <- renderPlot({
    # Filter data based on selected Area and Year
    plot_data <- subset(demographics, Area == input$area & Year == input$year)
    
    # Generate the tornado plot
    ggplot(plot_data, aes(x = ifelse(Sex == "Male", Number, -Number), y = `Age group`, fill = Sex)) +
      geom_bar(stat = "identity", position = "identity") +
      labs(x = "Percentage at Each Age Cohort", y = "Age Group", 
           title = paste("Tornado Plot for Area:", input$area, "Year:", input$year)) +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
      scale_x_continuous(labels = abs)  # Remove negative sign on x-axis for clarity
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
