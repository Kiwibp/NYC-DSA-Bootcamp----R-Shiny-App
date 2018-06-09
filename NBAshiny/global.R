# biocLite("EBImage")
library(EBImage)
library(dplyr)
library(ggplot2)
library(ggimage)
library(stringr)
library(tidyverse)

#install.packages(c("EBImage"), repos = getOption("https://bioconductor.org/biocLite.R"))

RD_Players = read.table("16-17RD_Players.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
RD_Team = read.table("16-17RD.Team.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)

hollinger_stats = read.csv('hollinger_stats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
hollinger_stats = hollinger_stats[,-c(1)]
hollinger_stats$TEAM = trimws(hollinger_stats$TEAM, which = c("both"))

season_advanced_stats = read.csv('16-17AdvancedStats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
season_advanced_stats$Player = str_to_lower(season_advanced_stats$Player)
season_advanced_stats$Player = gsub("\\\\.*", "", season_advanced_stats$Player)

#save these pipelines as csv's, comment out, read back in.  cut out dplyr
team_PER = season_advanced_stats %>%
  filter(Tm != "TOT" & MP > 200) %>%
  select(Player, Pos, Tm, MP, PER) %>%
  group_by(Tm) %>%
  summarise(team_PER = sum(PER)) 
team_PER

team_winning_perc = RD_Team %>%
  select(team, won, lost) %>%
  group_by(team) %>%
  mutate(team_win_percentage = won/sum(won, lost)) %>%
  arrange(team)
team_winning_perc$team = c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
names(team_winning_perc)[names(team_winning_perc) == 'team'] = 'Tm'
team_winning_perc

teamPER_and_winningPerc = merge(team_winning_perc, team_PER, by=c('Tm'), all=TRUE)

 
images = c("www/atlanta.png", "www/boston.png", "www/brooklyn.png", "www/charlotte.png",
           "www/chicago.png", "www/cleveland.png", "www/dallas.png", "www/denver.png",
           "www/detroit.png", "www/goldenstate.png", "www/houston.png", "www/indiana.png",
           "www/la_c.png", "www/la_l.png", "www/memphis.png", "www/miami.png",
           "www/milwaukee.png", "www/minnesota.png", "www/neworleans.png", "www/newyork.png", 
           "www/okcity.png", "www/orlando.png", "www/philadelphia.png", "www/phoenix.png", 
           "www/portland.png", "www/sacramento.png", "www/sanantonio.png", "www/toronto.png", 
           "www/utah.png", "www/washington.png")

teamPER_and_winningPerc$images = images

# #static version. adjusted in server.R for reactivity
# ggplot(teamPER_and_winningPerc, aes(x=team_PER, y=team_win_percentage, label= Tm)) +
#   geom_image(aes(image=images), size=.05, by='height')

test = as.list(teamPER_and_winningPerc$Tm)
names(test) = teamPER_and_winningPerc$Tm

