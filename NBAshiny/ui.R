
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)

test = as.list(teamPER_and_winningPerc$Tm)
names(test) = teamPER_and_winningPerc$Tm

shinyUI(fluidPage(

    # Application title
    titlePanel(strong(h1("NBA 16-17 Season Analysis", align="center"))
               
    ), 
  
    sidebarLayout(
      sidebarPanel(checkboxGroupInput("teams_checkbox", 
                                      label=h3("Select Teams (Team PER ~ Team Winning %)"), 
                                      choices= test, selected = test[1])
      ),
      
    # Show a plot of the generated distribution
      mainPanel(
        img(src="nba.png", height=100, width=100),
        helpText(h4("Players Advanced Stats"), 
                 'TS%: True Shooting Percentage
                   AST: Assist Ratio 
                   TO: Turnover Ratio 
                   USG: Usage Rate 
                   ORR: Offensive rebound rate
                   DRR: Defensive rebound rate
                   REBR: Rebound Rate 
                   PER: Player Efficiency Rating 
                   VA: Value Added 
                   EWA: Estimated Wins Added'
        ),
        tabsetPanel(
          tabPanel("Players Stats", dataTableOutput("players_table")), 
          tabPanel("Players Advanced Stats", dataTableOutput("hollinger_table")),
          tabPanel("Teams Stats", dataTableOutput("teams_table")),
          tabPanel("Team PER ~ Team Winning %")
        ),
        plotOutput("scatterplot")
      )
    )
))
