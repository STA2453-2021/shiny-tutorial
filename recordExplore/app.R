

# load the required libraries ---------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(reactable)


# source our helper functions
source('utils/utils.R')

# load the full record data
records <- readr::read_csv('data/nba_season_data.csv')
# get the current standings

standings <- prepare_standings_data(records)


ui <- dashboardPage(
    header <-  dashboardHeader(title = "NBA Win/Loss Record Explorer"),
    
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Current NBA record", tabName = "current", icon = icon("dashboard")),
            menuItem("Historical Record Explorer", icon = icon("th"),
                     tabName = "historical", badgeColor = "green")
        )
    ),
    
    body <- dashboardBody(
        tabItems(
            tabItem(tabName = "current",
                    fluidRow(
                        tabBox(width = 10,
                               title = "Standings",
                               id = "tabset1", height = "650px",
                               tabPanel("Western Conference Standings",
                                        reactableOutput('western_standings')),
                               tabPanel("Eastern Conference Standings",
                                        reactableOutput('eastern_standings'))
                        )
                    )
            ),
            
            tabItem(tabName = "historical",
                    h2("Historical Records"),
                    fluidRow(
                        column(width = 4,
                               box(
                                   title = "Inputs", width = NULL,
                                   solidHeader = TRUE,
                                   status = "primary",
                                   numericInput(inputId = "num_wins",
                                                label = "Select Number of wins",
                                                value = 0,
                                                min = 0,
                                                max = 82,
                                                step = 1),
                                   numericInput(inputId = "num_losses",
                                                label = "Select Number of losses",
                                                value = 0,
                                                min = 0,
                                                max = 82,
                                                step = 1),
                                   numericInput(inputId = "current_pm",
                                                label = "Select Current plus minus differential",
                                                value = 0,
                                                min = -1000,
                                                max = 1000,
                                                step = 1),
                                   numericInput(inputId = "accept_pm",
                                                label = "Keep records within X units of current plus minus",
                                                value = 250,
                                                min = 0,
                                                max = 2000,
                                                step = 1),
                                   actionButton(inputId = "run_check",
                                                label = "Extract Matching Records",
                                                icon = icon('basketball-ball'))
                               ),
                               valueBoxOutput("matching_records", width = 8)
                        ),
                        
                        column(width = 8,
                               tabBox(id = "tabs2", width = 12,
                                      height = '500px',
                                      title = "Results",
                                      tabPanel(title = "Matching Seasons",
                                               dataTableOutput('matching_table')),
                                      tabPanel(title  = "Summary",
                                               br(),
                                               valueBoxOutput('avg_wins', width = 8),
                                               valueBoxOutput('avg_losses', width = 8),
                                               valueBoxOutput('made_playoffs', width = 8)),
                                      tabPanel(title  = "Plot",
                                               br(),
                                               plotlyOutput('record_plot'))
                                      
                               )
                        )
                    )
            ))
    ),
    
    # Put them together into a dashboardPage
    dashboardPage(
        header,
        sidebar,
        body
    )
)

server <- function(input, output) {
    
    selection <- reactiveValues(vec=NULL)
    
    output$eastern_standings <- renderReactable({
        create_standings_table(standings$eastern, select = 'single')
    })
    
    output$western_standings <- renderReactable({
        create_standings_table(standings$eastern, select = 'single')
    })
    
    
    
    
    
    get_data <- eventReactive(input$run_check, {
        
        
        num_wins <- input$num_wins
        num_losses <- input$num_losses
        current_pm <- input$current_pm
        accept_pm <- input$accept_pm
        
        
        acceptable_records <- records %>%
            mutate(contains_record = wins == num_wins &
                       losses == num_losses &
                       plus_minus_pre >= (current_pm - accept_pm) &
                       plus_minus_pre <= (current_pm + accept_pm) ) %>%
            group_by(name_team, year_season) %>%
            arrange(date_game) %>%
            mutate(keep = cumsum(contains_record)) %>%
            filter(keep > 0) %>%
            ungroup() %>%
            group_by(name_team, year_season) %>%
            mutate(starting_record = record[row_number() == 1]) %>%
            ungroup() %>%
            mutate(id = paste(name_team, year_season))
        
        return(acceptable_records)
        
    })
    
    output$matching_records <- renderValueBox({
        
        matching_data <- get_data()
        
        n <- n_distinct(matching_data$id)
        
        valueBox(
            value = n,
            subtitle = "Mathcing Historical Records",
            icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
        
        # Add conditional colors
    })
    
    output$matching_table <- renderDataTable({
        matching_data <- get_data()
        
        matching_data_sum <- matching_data %>%
            select(id,
                   wins = final_wins,
                   losses = final_losses,
                   made_playoffs) %>%
            group_by(season = id) %>%
            slice(1) %>%
            ungroup()
        
        DT::datatable(matching_data_sum)
        
    })
    
    output$avg_wins <- renderValueBox({
        
        
        matching_data <- get_data()
        
        matching_data_sum <- matching_data %>%
            select(id,
                   wins = final_wins,
                   losses = final_losses,
                   made_playoffs) %>%
            group_by(season = id) %>%
            slice(1) %>%
            ungroup()
        
        mean_wins <- median(matching_data_sum$wins)
        q25_wins <- quantile(matching_data_sum$wins, .25)
        q75_wins <- quantile(matching_data_sum$wins, .75)
        
        win_value <- paste(mean_wins,
                           paste0("(", q25_wins, "-", q75_wins, ")"))
        
        valueBox(
            value = win_value,
            subtitle = "Median Wins in all matching seasons (IQR)",
            icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
        # Add conditional colors
    })
    
    output$avg_losses <- renderValueBox({
        
        matching_data <- get_data()
        
        matching_data_sum <- matching_data %>%
            select(id,
                   wins = final_wins,
                   losses = final_losses,
                   made_playoffs) %>%
            group_by(season = id) %>%
            slice(1) %>%
            ungroup()
        
        mean_wins <- median(matching_data_sum$wins)
        q25_wins <- quantile(matching_data_sum$wins, .25)
        q75_wins <- quantile(matching_data_sum$wins, .75)
        
        win_value <- paste(mean_wins,
                           paste0("(", q25_wins, "-", q75_wins, ")"))
        
        valueBox(
            value = win_value,
            subtitle = "Median Wins in all matching seasons (IQR)",
            icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
        
        # Add conditional colors
    })
    
    output$made_playoffs <- renderValueBox({
        
        matching_data <- get_data()
        
        matching_data_sum <- matching_data %>%
            select(id,
                   wins = final_wins,
                   losses = final_losses,
                   made_playoffs) %>%
            group_by(season = id) %>%
            slice(1) %>%
            ungroup()
        
        playoff_percentage <- sum(matching_data_sum$made_playoffs)/nrow(matching_data_sum)
        playoff_percentage <- paste0(round(100*playoff_percentage, 3), "%")
        
        valueBox(
            value = playoff_percentage,
            subtitle = "Percentage of teams making the playoffs",
            icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
        
        # Add conditional colors
    })
    
    output$record_plot <- renderPlotly({
        
        matching_data <- get_data()
        
        p1 <- matching_data %>%
            ggplot(aes(number_game_team_season, wins, color = id)) +
            geom_line() +
            labs(x = "Game number of season",
                 y = "Number of wins")  +
            theme_minimal()+
            theme(legend.position='none') +
            theme(axis.text.x = element_text(colour = "#006bb8"),
                  axis.text.y = element_text(colour = "#006bb8"),
                  axis.title.x = element_text(colour = "#032f4f"),
                  axis.title.y = element_text(colour = "#032f4f"))
        
        p1 <- plotly::ggplotly(p1)
        return(p1)
    })
    
    
}


shinyApp(ui, server)
