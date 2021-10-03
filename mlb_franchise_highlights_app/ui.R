

library(shiny)
library(Lahman)
data(TeamsFranchises)

team_names <- TeamsFranchises %>%
    filter(active == "Y") %>%
    select(franchName)
    

shinyUI(fluidPage(

    titlePanel("MLB Franchise Leaders"),

    # Sidebar with a slider input for number of bins
    verticalLayout(
        #sidebarPanel(
        #    selectInput("team_select", "Select a Franchise",
        #                 team_names$franchName)
        #    
        #),

        #mainPanel(
            selectInput("team_select", "Select a Franchise",
                        team_names$franchName),
            plotOutput("plot1"),
            plotOutput("plot2"),
            plotOutput("plot3"),
            plotOutput("plot4")

        #)
    )
))
