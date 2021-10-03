

library(shiny)
library(Lahman)
library(dplyr)
data(TeamsFranchises)

team_names <- TeamsFranchises %>%
    filter(active == "Y") %>%
    select(franchName)
    

shinyUI(fluidPage(

    titlePanel(
        h1("MLB Franchise Leaders", align = "center")
        ),

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
