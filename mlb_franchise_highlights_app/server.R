#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Lahman)
library(ggplot2)

data(Batting)
data(People)
data(TeamsFranchises)

battingSeasons <- left_join(Batting, Teams, 
                            by = c("teamID" = "teamID", "yearID" = "yearID") ) %>%
    left_join(TeamsFranchises, by = c("franchID" = "franchID")) %>%
    left_join(People, by = c("playerID" = "playerID")) %>%
    filter(active == "Y") %>%
    select(yearID, franchID, franchName, playerID, nameFirst, nameLast, AB.x, H.x,
           HR.x, RBI, SB.x) %>%
    rename(AB = AB.x, H = H.x, HR = HR.x, SB = SB.x)

battingSeasons$nameFull <- paste(seasons$nameFirst, seasons$nameLast)

avg <- battingSeasons %>% 
    group_by(nameFull, franchName) %>% 
    summarize(ABs = sum(AB), Hs = sum(H)) 

avg$avg <- avg$Hs / avg$ABs 

avg_10 <- avg %>%
    group_by(franchName) %>%
    filter(ABs >= 1000) %>%
    slice_max(order_by = avg, n = 10) %>%
    select(nameFull, franchName, avg)

h_10 <- avg %>%
    group_by(franchName) %>%
    slice_max(order_by = Hs, n = 10) %>%
    select(nameFull, franchName, Hs)

hrs <- battingSeasons %>% 
    group_by(nameFull, franchName) %>% 
    summarize(HRs = sum(HR))

hrs_10 <- hrs %>%
    group_by(franchName) %>%
    slice_max(order_by = HRs, n = 10)

rbis <- battingSeasons %>% 
    group_by(nameFull, franchName) %>% 
    summarize(RBIs = sum(RBI))

rbis_10 <- rbis %>%
    group_by(franchName) %>%
    slice_max(order_by = RBIs, n = 10)

sbs <- battingSeasons %>% 
    group_by(nameFull, franchName) %>% 
    summarize(SBs = sum(SB))

sb_10 <- sbs %>%
    group_by(franchName) %>%
    slice_max(order_by = SBs, n = 10)

team_names <- TeamsFranchises %>%
    filter(active == "Y") %>%
    select(franchName)

# Define MLB team colors
colors <- c("#BA0021", "#A71930", "#CE1141", "#DF4601", "#BD3039", "#0E3386",
            "#27251F", "#C6011F", "#0C2340", "#33006F", "#0C2340", "#00A3E0",
            "#002D62", "#004687", "#005A9C", "#FFC52F", "#002B5C", "#002D72",
            "#003087", "#003831", "#E81828", "#FDB827", "#2F241D", "#005C5C",
            "#FD5A1E", "#C41E3A", "#092C5C", "#003278", "#134A8E", "#AB0003")

teamColors <- data.frame(team_names$franchName, colors) %>%
    rename(franchName = team_names.franchName)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$plot1 <- renderPlot({
        
        team <- input$team_select
        teamColor <- teamColors[teamColors$franchName == team, 2]
        
        teamAVG <- avg_10[avg_10$franchName == team, ] %>%
            arrange(avg)
        
        y_min <- min(teamAVG$avg) - 0.080
        y_max <- max(teamAVG$avg) + 0.020
        
        #y_min <- .250
        #y_max <- .400
        
        p <- ggplot(data = teamAVG, aes(x = reorder(nameFull, avg), y = avg)) +
            coord_cartesian(ylim = c(y_min, y_max)) +
            geom_bar(stat="identity", color = "black", fill = teamColor) +
            labs(title = paste(team, "Top Batting Averages (min. 1000 ABs)"), x = "Player", y = "Batting Average") +
            theme(plot.title = element_text(size=18, face="bold",
                                            margin = margin(10, 0, 10, 0)),
                  axis.text.x = element_text(angle=50, size=11, vjust=0.5),
                  axis.text.y = element_text(size=11, vjust=0.5),
                  axis.ticks.x = element_blank()) +
            scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
            geom_text(aes(label = sprintf("%.3f", avg), y = avg + 0.005), position = position_dodge(0.9))
            
        p

    })
    
    output$plot2 <- renderPlot({
        
        team <- input$team_select
        teamColor <- teamColors[teamColors$franchName == team, 2]
        
        teamHR <- hrs_10[hrs_10$franchName == team, ] %>%
            arrange(HRs)
        
        offsetHR <- max(teamHR$HRs) / 30
        
        p <- ggplot(data = teamHR, aes(x = reorder(nameFull, HRs), y = HRs)) +
            #coord_cartesian(ylim = c(y_min, y_max)) +
            geom_bar(stat="identity", color = "black", fill = teamColor) +
            labs(title = paste(team, "Most Home Runs"), x = "Player", y = "Home Runs") +
            theme(plot.title = element_text(size=18, face="bold",
                                            margin = margin(10, 0, 10, 0)),
                  axis.text.x = element_text(angle=50, size=11, vjust=0.5),
                  axis.text.y = element_text(size=11, vjust=0.5),
                  axis.ticks.x = element_blank()) +
            geom_text(aes(label = HRs, y = HRs + offsetHR), position = position_dodge(0.9))
        
        p
        
    })
    
    output$plot3 <- renderPlot({
        
        team <- input$team_select
        teamColor <- teamColors[teamColors$franchName == team, 2]
        
        teamRBI <- rbis_10[rbis_10$franchName == team, ] %>%
            arrange(RBIs)
        
        offsetRBI <- max(teamRBI$RBIs) / 25
        
        p <- ggplot(data = teamRBI, aes(x = reorder(nameFull, RBIs), y = RBIs)) +
            #coord_cartesian(ylim = c(y_min, y_max)) +
            geom_bar(stat="identity", color = "black", fill = teamColor) +
            labs(title = paste(team, "Most Runs Batted In"), x = "Player", y = "Runs Batted In") +
            theme(plot.title = element_text(size=18, face="bold",
                                            margin = margin(10, 0, 10, 0)),
                  axis.text.x = element_text(angle=50, size=11, vjust=0.5),
                  axis.text.y = element_text(size=11, vjust=0.5),
                  axis.ticks.x = element_blank()) +
            geom_text(aes(label = RBIs, y = RBIs + offsetRBI), position = position_dodge(0.9))
        
        p
        
    })
    
    output$plot4 <- renderPlot({
        
        team <- input$team_select
        teamColor <- teamColors[teamColors$franchName == team, 2]
        
        teamSB <- sb_10[sb_10$franchName == team, ] %>%
            arrange(SBs)
        
        offsetSB <- max(teamSB$SBs) / 30
        
        p <- ggplot(data = teamSB, aes(x = reorder(nameFull, SBs), y = SBs)) +
            #coord_cartesian(ylim = c(y_min, y_max)) +
            geom_bar(stat="identity", color = "black", fill = teamColor) +
            labs(title = paste(team, "Most Stolen Bases"), x = "Player", y = "Stolen Bases") +
            theme(plot.title = element_text(size=18, face="bold",
                                            margin = margin(10, 0, 10, 0)),
                  axis.text.x = element_text(angle=50, size=11, vjust=0.5),
                  axis.text.y = element_text(size=11, vjust=0.5),
                  axis.ticks.x = element_blank()) +
            geom_text(aes(label = SBs, y = SBs + offsetSB), position = position_dodge(0.9))
        
        p
        
    })

})
