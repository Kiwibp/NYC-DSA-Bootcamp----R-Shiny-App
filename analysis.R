library(dplyr)
library(ggplot2)
library(stringr)

summary(RD_Player_and_Teams)

#1.what is the correlation between minutes and primary stats?
cor(RD_Players$Min, RD_Players$PTS)
plot(RD_Players$Min, RD_Players$PTS, main="Scatterplot")

cor(RD_Players$Min, RD_Players$OR)
plot(RD_Players$Min, RD_Players$OR, main="Scatterplot")

cor(RD_Players$Min, RD_Players$TR)
plot(RD_Players$Min, RD_Players$TR, main="Scatterplot")

cor(RD_Players$Min, RD_Players$AS)
plot(RD_Players$Min, RD_Players$AS, main="Scatterplot")

cor(RD_Players$Min, RD_Players$ST)
plot(RD_Players$Min, RD_Players$ST, main="Scatterplot")

cor(RD_Players$Min, RD_Players$BK)
plot(RD_Players$Min, RD_Players$BK, main="Scatterplot")

cor(RD_Players$Min, RD_Players$TO)
plot(RD_Players$Min, RD_Players$TO, main="Scatterplot")


#2.what is the mean and SD of primary stats. Per team? Per position?
RD_Player_and_Teams %>% 
  group_by(Team) %>%
  summarise(mean(PTS), mean(OR), mean(TR), mean(AS), mean(ST), mean(BK), mean(TO))

RD_Player_and_Teams %>%
  group_by(Team) %>%
  summarise(sd(PTS), sd(OR), sd(TR), sd(AS), sd(ST), sd(BK), sd(TO))

RD_Player_and_Teams %>% 
  filter(!PS == "??") %>%
  group_by(PS) %>%
  summarise(mean(PTS), mean(OR), mean(TR), mean(AS), mean(ST), mean(BK), mean(TO))

RD_Player_and_Teams %>%
  filter(!PS == "??") %>%
  group_by(PS) %>%
  summarise(sd(PTS), sd(OR), sd(TR), sd(AS), sd(ST), sd(BK), sd(TO))


#3.what players have the best PER's in the league per team? per position? 
#per postion on team?
season_advanced_stats %>%
  filter(Tm != "TOT" & MP > 200) %>%
  select(Player, Pos, Tm, MP, PER) %>%
  arrange(desc(PER))

season_advanced_stats %>%
  filter(Tm != "TOT" & MP > 200) %>%
  select(Player, Pos, Tm, PER) %>%
  group_by(Pos) %>%
  arrange(PER) %>%
  top_n(n=1)

season_advanced_stats %>%
  filter(Tm != "TOT" & MP > 200) %>%
  select(Player, Tm, PER) %>%
  group_by(Tm) %>%
  arrange(Tm) %>%
  top_n(n=1)

season_advanced_stats %>%
  filter(Tm != "TOT" & MP > 200) %>%
  select(Player, Tm, Pos, PER) %>%
  group_by(Tm, Pos) %>%
  arrange(Tm) %>%
  top_n(n=1)

#4. what is each teams total PER?
team_PER = season_advanced_stats %>%
  filter(Tm != "TOT" & MP > 200) %>%
  select(Player, Pos, Tm, MP, PER) %>%
  group_by(Tm) %>%
  summarise(team_PER = sum(PER)) 
team_PER
  
#5. what are the central tendencies of primary stats? Per team? Per Position? 
summary(RD_Players$PTS)
sd(RD_Players$PTS)
hist(RD_Players$PTS)
boxplot(PTS~PS, RD_Players)
boxplot(PTS~Team, RD_Players)

summary(RD_Players$OR)
sd(RD_Players$OR)
hist(RD_Players$OR)
boxplot(OR~PS, RD_Players)
boxplot(OR~Team, RD_Players)

summary(RD_Players$TR)
sd(RD_Players$TR)
hist(RD_Players$TR)
boxplot(TR~PS, RD_Players)
boxplot(TR~Team, RD_Players)

summary(RD_Players$AS)
sd(RD_Players$AS)
hist(RD_Players$AS)
boxplot(AS~PS, RD_Players)
boxplot(AS~Team, RD_Players)

summary(RD_Players$ST)
sd(RD_Players$ST)
hist(RD_Players$ST)
boxplot(ST~PS, RD_Players)
boxplot(ST~Team, RD_Players)

summary(RD_Players$BK)
sd(RD_Players$BK)
hist(RD_Players$BK)
boxplot(BK~PS, RD_Players)
boxplot(BK~Team, RD_Players)

summary(RD_Players$TO)
sd(RD_Players$TO)
hist(RD_Players$TO)
boxplot(TO~PS, RD_Players)
boxplot(TO~Team, RD_Players)

#6.who were the league leader in primary stats?
RD_Players %>%
  select(Player, Team, PS, PTS) %>%
  arrange(desc(PTS)) %>%
  top_n(n=30)
  
RD_Players %>%
  select(Player, Team, PS, OR) %>%
  arrange(desc(OR)) %>%
  top_n(n=30)

RD_Players %>%
  select(Player, Team, PS, TR) %>%
  arrange(desc(TR)) %>%
  top_n(n=30)

RD_Players %>%
  select(Player, Team, PS, AS) %>%
  arrange(desc(AS)) %>%
  top_n(n=30)

RD_Players %>%
  select(Player, Team, PS, ST) %>%
  arrange(desc(ST)) %>%
  top_n(n=30)

RD_Players %>%
  select(Player, Team, PS, BK) %>%
  arrange(desc(BK)) %>%
  top_n(n=30)

RD_Players %>%
  select(Player, Team, PS, TO) %>%
  arrange(desc(TO)) %>%
  top_n(n=30)

#7.what was each teams winning %?
team_winning_perc = RD_Team %>%
  select(team, won, lost) %>%
  group_by(team) %>%
  mutate(team_win_percentage = won/sum(won, lost)) %>%
  arrange(team)
team_winning_perc

#8.What is the correlation between team PER and team win ratio?
team_winning_perc$team = c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
names(team_winning_perc)[names(team_winning_perc) == 'team'] = 'Tm'
team_winning_perc

teamPER_and_winningPerc = merge(team_winning_perc, team_PER, by=c('Tm'), all=TRUE)
teamPER_and_winningPerc
cor(teamPER_and_winningPerc$team_PER, teamPER_and_winningPerc$team_win_percentage)
