library(dplyr)
library(ggplot2)
library(readxl)
HomeRD_Team_Opp = read.table("16-17.HomeRD.Team.Opp.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
AwayRD_Team_Opp = read.table("16-17.AwayRD.Team.Opp.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
RD_Team_Opp = read.table("16-17RD.Team.Opp.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)

HomeRD_Team = read.table("16-17.HomeRD.Team.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
AwayRD_Team = read.table("16-17.AwayRD.Team.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
RD_Team = read.table("16-17RD.Team.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)

HomeRD_Players = read.table("16-17.HomeRD.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
AwayRD_Players = read.table("16-17.AwayRd.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
RD_Players = read.table("16-17RD_Players.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)

RD_Player_and_Teams = read.table("16-17RD.Each.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
RD_Player_and_Teams$Team = as.factor(RD_Player_and_Teams$Team)

Season_Standings = read.table("NBA Standings through games of 5_23_17.txt", header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
Eastern_Standings = Season_Standings[ , 1:7]
colnames(Eastern_Standings) = c("Team", "W", "L", "+/-", "GB", "PCT")
Eastern_Standings <- Eastern_Standings[!is.na(names(Eastern_Standings))]
Western_Standings = Season_Standings[ , 8:ncol(Season_Standings)]
colnames(Western_Standings) = c("Team", "W", "L", "+/-", "GB", "PCT")


Top_10_Player_Rankings = read.csv("NBA 16-17 Top 10 Player Rankings.csv", stringsAsFactors = FALSE)

Scoring_Top10 = Top_10_Player_Rankings[1:10 , 1:3]

FG_Pct = Top_10_Player_Rankings[1:10 , 5:7]
colnames(FG_Pct) = c("Player", "Team", "Pct")

Rebounds_Top10 = Top_10_Player_Rankings[13:23 , 1:3]
colnames(Rebounds_Top10) = Rebounds_Top10[1, ]
Rebounds_Top10 = Rebounds_Top10[-1, ] 
row.names(Rebounds_Top10) = 1:10

Off_Rebounds_Top10 = Top_10_Player_Rankings[13:23 , 5:7]
colnames(Off_Rebounds_Top10) = Off_Rebounds_Top10[1, ]
Off_Rebounds_Top10 = Off_Rebounds_Top10[-1, ]
row.names(Off_Rebounds_Top10) = 1:10

Three_pointers_Top10 = Top_10_Player_Rankings[25:35 , 1:3]
colnames(Three_pointers_Top10) = Three_pointers[1, ]
Three_pointers_Top10 = Three_pointers_Top10[-1, ]
row.names(Three_pointers_Top10) = 1:10

Three_point_perc_Top10 = Top_10_Player_Rankings[25:35 , 5:7]
colnames(Three_point_perc_Top10) = Three_point_perc_Top10[1, ]
Three_point_perc_Top10 = Three_point_perc_Top10[-1, ]
row.names(Three_point_perc_Top10) = 1:10

FT_perc_Top10 = Top_10_Player_Rankings[37:47 , 1:3]
colnames(FT_perc_Top10) = FT_perc_Top10[1 ,]
FT_perc_Top10 = FT_perc_Top10[-1, ]
row.names(FT_perc_Top10) = 1:10

FT_attempts_Top10 = Top_10_Player_Rankings[37:47 , 5:7]
colnames(FT_attempts_Top10) = FT_attempts_Top10[1, ]
FT_attempts_Top10 = FT_attempts_Top10[-1, ]
row.names(FT_attempts_Top10) = 1:10

Assists_Top10 = Top_10_Player_Rankings[49:59 , 1:3]
colnames(Assists_Top10) = Assists_Top10[1 ,]
Assists_Top10 = Assists_Top10[-1, ]
row.names(Assists_Top10) = 1:10

Turnovers_Bottom10 = Top_10_Player_Rankings[49:59 , 5:7]
colnames(Turnovers_Bottom10) = Turnovers_Bottom10[1 ,]
Turnovers_Bottom10 = Turnovers_Bottom10[-1, ]
row.names(Turnovers_Bottom10) = 1:10

Assists_to_TO_Top10 = Top_10_Player_Rankings[61:71 , 1:3]
colnames(Assists_to_TO_Top10) = Assists_to_TO_Top10[1 ,]
Assists_to_TO_Top10 = Assists_to_TO_Top10[-1, ]
row.names(Assists_to_TO_Top10) = 1:10

Turnovers_Top10 = Top_10_Player_Rankings[61:71 , 5:7]
colnames(Turnovers_Top10) = Turnovers_Top10[1 ,]
Turnovers_Top10 = Turnovers_Top10[-1, ]
row.names(Turnovers_Top10) = 1:10

Steals_Top10 = Top_10_Player_Rankings[73:83 , 1:3]
colnames(Steals_Top10) = Steals_Top10[1 ,]
Steals_Top10 = Steals_Top10[-1, ]
row.names(Steals_Top10) = 1:10

Blocks_Top10 = Top_10_Player_Rankings[73:83 , 5:7]
colnames(Blocks_Top10) = Blocks_Top10[1 ,]
Blocks_Top10 = Blocks_Top10[-1, ]
row.names(Blocks_Top10) = 1:10

Technicals_Top10 = Top_10_Player_Rankings[85:95 , 1:3]
colnames(Technicals_Top10) = Technicals_Top10[1 ,]
Technicals_Top10 = Technicals_Top10[-1, ]
row.names(Technicals_Top10) = 1:10

Min_per_game_Top10 = Top_10_Player_Rankings[97:107 , 1:3]
colnames(Min_per_game_Top10) = Min_per_game_Top10[1 ,]
Min_per_game_Top10 = Min_per_game_Top10[-1, ]
row.names(Min_per_game_Top10) = 1:10

Fouls_per_game_Top10 = Top_10_Player_Rankings[97:107 , 5:7]
colnames(Fouls_per_game_Top10) = Fouls_per_game_Top10[1 ,]
Fouls_per_game_Top10 = Fouls_per_game_Top10[-1, ]
row.names(Fouls_per_game_Top10) = 1:10

ScoringStatistics = read_excel("16-17Stats.xlsx", sheet=1, col_names = c("Player",	"Pos",	"G",	"M/Gm",	"FGm",	"FGa",	"FGPCT",	"3m",	"3a",	"3PCT",	"FTm",	"FTA",	"FTPCT",	"+/-/G",	"AVG"))
AtlantaScoring = ScoringStatistics[3:22,]
BostonScoring = ScoringStatistics[25:39,]
BrooklynScoring = ScoringStatistics[322:342,]
CharlotteScoring = ScoringStatistics[41:59,]
ChicagoScoring = ScoringStatistics[61:78,]
ClevelandScoring = ScoringStatistics[80:100,]
DallasScoring = ScoringStatistics[102:125,]
DenverScoring = ScoringStatistics[127:145,]
DetroitScoring = ScoringStatistics[147:161,]
GoldenStateScoring = ScoringStatistics[163:179,]
HoustonScoring = ScoringStatistics[181:198,]
IndianaScoring = ScoringStatistics[200:215,]
LAClippersScoring = ScoringStatistics[217:231,]
LALakersScoring = ScoringStatistics[233:250,]
MemphisScoring = ScoringStatistics[252:268,]
MiamiScoring = ScoringStatistics[270:284,]
MilwaukeeScoring = ScoringStatistics[286:304,]
MinnesotaScoring = ScoringStatistics[306:320,]
NewOrleansScoring = ScoringStatistics[344:369,]
NewYorkScoring = ScoringStatistics[371:386,]
OrlandoScoring = ScoringStatistics[388:406,]
OKCityScoring = ScoringStatistics[502:520,]
PhiladelphiaScoring = ScoringStatistics[408:428,]
PhoenixScoring = ScoringStatistics[430:447,]
PortlandScoring = ScoringStatistics[449:463,]
SacramentoScoring = ScoringStatistics[465:483,]
SanAntonioScoring = ScoringStatistics[485:500,]
TorontoScoring = ScoringStatistics[522:538,]
UtahScoring = ScoringStatistics[540:554,]
WashingtonScoring = ScoringStatistics[556:573,]

ReboundingStatistics = read_excel("16-17Stats.xlsx", sheet=2, col_names = c("Player",	"AS",	"AS/g",	"ST",	"ST/G",	"TO",	"TO/g",	"BK",	"BK/g",	"BA",	"BA/g",	"TND",	"OR",	"OR/g",	"TR",	"TR/g"))
AtlantaRebounding = ReboundingStatistics[3:22,]
BostonRebounding = ReboundingStatistics[24:38,]
BrooklynRebounding = ReboundingStatistics[319:339,]
CharlotteRebounding = ReboundingStatistics[40:58,]
ChicagoRebounding = ReboundingStatistics[60:77,]
ClevelandRebounding = ReboundingStatistics[79:99,]
DallasRebounding = ReboundingStatistics[101:124,]
DenverRebounding = ReboundingStatistics[126:144,]
DetroitRebounding = ReboundingStatistics[146:160,]
GoldenstateReboudning = ReboundingStatistics[162:178,]
HoustonRebounding = ReboundingStatistics[180:197,]
IndianaRebounding = ReboundingStatistics[199:214,]
LAClippersRebounding = ReboundingStatistics[216:230,]
LALakersRebounding = ReboundingStatistics[232:249,]
MemphisRebounding = ReboundingStatistics[251:267,]
MiamiRebounding = ReboundingStatistics[269:283,]
MilwaukeeRebounding = ReboundingStatistics[285:303,]
MinnesotaRebounding = ReboundingStatistics[305:317,]
NewOrleansRebounding = ReboundingStatistics[341:366,]
NewYorkRebounding = ReboundingStatistics[368:383,]
OrlandoRebounding = ReboundingStatistics[385:403,]
OKCityRebounding = ReboundingStatistics[499:517,]
PhiladelphiaRebounding = ReboundingStatistics[405:425,]
PhoenixRebounding = ReboundingStatistics[427:444,]
PortlandRebounding = ReboundingStatistics[446:460,]
SacramentoRebounding = ReboundingStatistics[462:480,]
SanAntonioRebounding = ReboundingStatistics[482:497,]
TorontoRebounding = ReboundingStatistics[519:535,]
UtahRebounding = ReboundingStatistics[537:551,]
WashingtonRebounding = ReboundingStatistics[553:570,]

ScoringStats48min = read_excel("16-17Stats.xlsx", sheet=3, col_names = c("Player", "Min", "Pts", "Tnd/48", "TC", "PF", "DQ", "STA", "+/-", "PTS/48"))
AtlantaScoring48min = ScoringStats48min[3:22, ]
BostonScoring48min = ScoringStats48min[24:38, ]
BrooklynScoring48min = ScoringStatistics48min[322:342,]
CharlotteScoring48min = ScoringStatistics48min[41:59,]
ChicagoScoring48min = ScoringStatistics48min[61:78,]
ClevelandScoring48min = ScoringStatistics48min[80:100,]
DallasScoring48min = ScoringStatistics48min[102:125,]
DenverScoring48min = ScoringStatistics48min[127:145,]
DetroitScoring48min = ScoringStatistics48min[147:161,]
GoldenStateScoring48min = ScoringStatistics48min[163:179,]
HoustonScoring48min = ScoringStatistics48min[181:198,]
IndianaScoring48min = ScoringStatistics48min[200:215,]
LAClippersScoring48min = ScoringStatistics48min[217:231,]
LALakersScoring48min = ScoringStatistics48min[233:250,]
MemphisScoring48min = ScoringStatistics48min[252:268,]
MiamiScoring48min = ScoringStatistics48min[270:284,]
MilwaukeeScoring48min = ScoringStatistics48min[286:304,]
MinnesotaScoring48min = ScoringStatistics48min[306:320,]
NewOrleansScoring48min = ScoringStatistics48min[344:369,]
NewYorkScoring48min = ScoringStatistics48min[371:386,]
OrlandoScoring48min = ScoringStatistics48min[388:406,]
OKCityScoring48min = ScoringStatistics48min[502:520,]
PhiladelphiaScoring48min = ScoringStatistics48min[408:428,]
PhoenixScoring48min = ScoringStatistics48min[430:447,]
PortlandScoring48min = ScoringStatistics48min[449:463,]
SacramentoScoring48min = ScoringStatistics48min[465:483,]
SanAntonioScoring48min = ScoringStatistics48min[485:500,]
TorontoScoring48min = ScoringStatistics48min[522:538,]
UtahScoring48min = ScoringStatistics48min[540:554,]
WashingtonScoring48min = ScoringStatistics48min[556:573,]


ReboundingStats48min = read_excel("16-17Stats.xlsx", sheet=4, col_names = c("Player",	"AS/48",	"ST/48",	"TO/48",	"BK/48",	"BA/48",	"OR/48",	"TR/48"))
AtlantaRebounding48min = ReboundingStats48min[3:22,]
BostonRebounding48min = ReboundingStats48min[24:38,]
BrooklynRebounding48min = ReboundingStats48min[322:342,]
CharlotteRebounding48min = ReboundingStats48min[40:58,]
ChicagoRebounding48min = ReboundingStats48min[60:77,]
ClevelandRebounding48min = ReboundingStats48min[79:99,]
DallasRebounding48min = ReboundingStats48min[101:124,]
DenverRebounding48min = ReboundingStats48min[126:144,]
DetroitRebounding48min = ReboundingStats48min[146:160,]
GoldenstateReboudning48min = ReboundingStats48min[162:178,]
HoustonRebounding48min = ReboundingStats48min[180:197,]
IndianaRebounding48min = ReboundingStats48min[199:214,]
LAClippersRebounding48min = ReboundingStats48min[216:230,]
LALakersRebounding48min = ReboundingStats48min[232:249,]
MemphisRebounding48min = ReboundingStats48min[251:267,]
MiamiRebounding48min = ReboundingStats48min[269:283,]
MilwaukeeRebounding48min = ReboundingStats48min[285:303,]
MinnesotaRebounding48min = ReboundingStats48min[305:320,]
NewOrleansRebounding48min = ReboundingStats48min[344:369,]
NewYorkRebounding48min = ReboundingStats48min[371:386,]
OrlandoRebounding48min = ReboundingStats48min[388:406,]
OKCityRebounding48min = ReboundingStats48min[502:520,]
PhiladelphiaRebounding48min = ReboundingStats48min[408:428,]
PhoenixRebounding48min = ReboundingStats48min[430:447,]
PortlandRebounding48min = ReboundingStats48min[449:463,]
SacramentoRebounding48min = ReboundingStats48min[465:483,]
SanAntonioRebounding48min = ReboundingStats48min[485:500,]
TorontoRebounding48min = ReboundingStats48min[521:538,]
UtahRebounding48min = ReboundingStats48min[540:554,]
WashingtonRebounding48min = ReboundingStats48min[556:573,]

guards_advanced_stats = read.csv('guards_advanced_stats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
guards_advanced_stats = guards_advanced_stats[,-c(1)]

guards_touches_stats = read.csv('guards_touches_stats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
guards_touches_stats = guards_touches_stats[,-c(1)]

forwards_advanced_stats = read.csv('forwards_advanced_stats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
forwards_advanced_stats = forwards_advanced_stats[,-c(1)]

forwards_touches_stats = read.csv('forwards_touches_stats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
forwards_touches_stats = forwards_touches_stats[,-c(1)]

centers_advanced_stats = read.csv('centers_advanced_stats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
centers_advanced_stats = centers_advanced_stats[,-c(1)]

centers_advanced_stats = read.csv('centers_advanced_stats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
centers_advanced_stats = centers_advanced_stats

season_advanced_stats = read.csv('16-17AdvancedStats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
season_advanced_stats$Player = str_to_lower(season_advanced_stats$Player)
season_advanced_stats$Player = gsub("\\\\.*", "", season_advanced_stats$Player)

hollinger_stats = read.csv('hollinger_stats.csv', header = TRUE, stringsAsFactors = FALSE, fill = FALSE)
hollinger_stats = hollinger_stats[,-c(1)]
hollinger_stats$TEAM = trimws(hollinger_stats$TEAM, which = c("both"))