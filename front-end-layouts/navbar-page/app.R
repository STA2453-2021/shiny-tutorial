#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("My Application", theme = my_theme,
                 tabPanel("Histogram plot",
                          fluidRow(
                              column(4, 
                                  sliderInput("bins",
                                              "Number of bins:",
                                              min = 1,
                                              max = 50,
                                              value = 30)),
                              column(8, plotOutput("plot"))
                          )),
                 tabPanel("Component 2",
                          fluidRow(
                              column(6, 
                                     selectInput("spec",
                                                 "Select Species:",
                                                 choices = unique(iris$Species),
                                                 selected = "setosa")),
                              column(6, plotOutput("iris_plot"))
                          )),
                 tabPanel("Component 3",
                          sidebarLayout(
                              
                              sidebarPanel(
                                  sliderInput("bins2",
                                              "Number of bins:",
                                              min = 1,
                                              max = 50,
                                              value = 30)
                              ),
                              
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Plot", plotOutput("plot2")), 
                                      tabPanel("Summary", verbatimTextOutput("summary2")), 
                                      tabPanel("Table", tableOutput("table2"))
                                  )
                              )
                          ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$iris_plot <- renderPlot({
        
        p <- iris %>% 
            dplyr::filter(Species == input$spec) %>% 
            ggplot(aes(Sepal.Length, Sepal.Width)) +
            geom_point()
        
        return(p)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
