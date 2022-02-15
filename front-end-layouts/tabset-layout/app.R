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
ui <- fluidPage(
    
    titlePanel("Tabsets"),
    
    sidebarLayout(
        
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("plot")), 
                tabPanel("Summary", verbatimTextOutput("summary")), 
                tabPanel("Table", dataTableOutput("table"))
            )
        )
    )
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
    
    output$summary <- renderPrint({
        
        text <- glue::glue("You have selected {input$bins} number of bins")
        return(text)
    })
    
    output$table <- renderDataTable({
        
        tab <- head(faithful)
        
        tab <- datatable(tab)
        return(tab)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
