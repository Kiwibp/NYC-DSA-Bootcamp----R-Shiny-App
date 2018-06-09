
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
  
  output$players_table = renderDataTable({RD_Players[,1:20]})
  output$hollinger_table = renderDataTable({hollinger_stats})
  output$teams_table = renderDataTable({RD_Team[,1:20]})
  

  output$scatterplot = renderPlot({
    # cat(input$teams_checkbox)
    ggplot(teamPER_and_winningPerc[teamPER_and_winningPerc$Tm %in% input$teams_checkbox,], aes(x=team_PER, y=team_win_percentage, label= Tm)) +
      geom_image(aes(image = images), size=.05, by='height')#what should I adjust here?
  })


})
