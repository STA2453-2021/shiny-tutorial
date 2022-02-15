




# load the libraries ------------------------------------------------------
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(bslib)


# load our data -----------------------------------------------------------

df <- read_csv('data/covid_data.csv')

# Want we want

# - Plot the number of cases per day
# - Include a line for each age group (checkbox)
# - Show a bar chart of outcomes (select input)
# - date filter on the plot
# - modal with info on button




# Define UI 
ui <- fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    
    titlePanel('COVID Cases Toronto'),
    
    
    sidebarLayout(
        sidebarPanel(checkboxInput(inputId = "by_age",
                                   label = "By Age Group", value = F),
                     selectInput(inputId = "show_outcomes",
                                 label = "Show outcomes plot", 
                                 selected = "No", choices = c("Yes", "No")),
                     
                     dateRangeInput(inputId = 'date_range',
                                    label = "Select date range",
                                    start = min(df$reported_date),
                                    end = max(df$reported_date)),
                     actionButton(inputId = "show", 
                                  label = "Show Instructions")),
        
        mainPanel(plotlyOutput("daily_cases"),
                  plotlyOutput("daily_outcomes"))
    )
)

# Define server logic
server <- function(input, output) {
    
    
    output$daily_cases <- renderPlotly({
        
        filter_data <- df %>% 
            filter(reported_date >= input$date_range[1],
                   reported_date <= input$date_range[2])
        
        if(!input$by_age) {
            
            our_plot <- filter_data %>% 
                count(reported_date) %>% 
                ggplot(aes(reported_date, n)) +
                geom_line() + 
                labs(title = "Number of Reported Cases per day",
                     x = "Date",
                     y = "Number of Cases")
            
            our_plotly_plot <- ggplotly(our_plot)
        } else {
            our_plot <- filter_data %>% 
                count(age_group, reported_date) %>% 
                ggplot(aes(reported_date, n, color = age_group)) +
                geom_line() + 
                labs(title = "Number of Reported Cases per day",
                     x = "Date",
                     y = "Number of Cases")
            
            our_plotly_plot <- ggplotly(our_plot)
        }
        
        
        return(our_plotly_plot)
    })
    
    output$daily_outcomes <- renderPlotly( {
        
        filter_data <- df %>% 
            filter(reported_date >= input$date_range[1],
                   reported_date <= input$date_range[2])
        
        
        if(input$show_outcomes == "Yes") {
            our_plot <- filter_data %>% 
                count(reported_date, outcome) %>% 
                ggplot(aes(reported_date, n, fill = outcome)) +
                geom_bar(stat= 'identity') + 
                labs(title = "Number of Reported Cases per day",
                     x = "Date",
                     y = "Number of Cases")
            
            our_plotly_plot <- ggplotly(our_plot)
            
            return(our_plotly_plot)
        } else {
            
            return(NULL)
        }
        
    }) 
    
    
    observeEvent(input$show, {
        
        print(input$date_range)
        
        showModal(modalDialog(
            title = "Important message",
            "This is an important message!"
        ))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
