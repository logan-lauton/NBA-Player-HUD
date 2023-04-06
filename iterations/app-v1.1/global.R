##Logan Lauton
##global.R - NBA Player HUD App v1.1
##this is the global.R for the first iteration of the NBA Player HUD R Shiny app that I will
##be producing. In this holds my initial exploratory graphs and features that I would like to implement.

##clear your working directory
rm(list = ls())

##load libraries
library("ggplot2")
library("dplyr")


##loading in required data
ptot <- read.csv("~\\NBA Player Stats(1950 - 2022).csv")
psal <- read.csv("~\\NBA Salaries(1990-2023).csv")
tpay <- read.csv("~\\NBA Payroll(1990-2023).csv")

##important stats from empathy and define phases are
##Blocks & assists, Rebounds & 3-pointers, Points , Steals , FG% and FT%

##start by cleaning data before anything
##begin by removing first two columns
ptot <- ptot %>% select(-1, -2)
##I will then remove all instances of TOT from the team(tm) column as this will largely scew data
ptot <- subset(ptot, Tm != "TOT")

##determine if column names are appropriate
colnames(ptot)
##colnames will suffice

##next basic test of gathering all players in a full season, this will help for the end result
dplyr::filter(ptot, Season %in% c(1950))


##here I perform a basic group sumarise to output the number of field goal attempts per season by
##the entire league
ptot_season <- ptot %>% group_by(Season) %>%
  summarise(FGA = sum(FGA),)
##and finally I plot the data in a basic plot
plot(ptot_season$Season, ptot_season$FGA)

##now that I can see that the player data is successfully cleaned I will export it back to the same
##name inorder to ensure no cleaning is needed in the app.R file and to not confuse .csv files
write.csv(ptot, "~\\NBA Player Stats(1950 - 2022).csv", row.names = FALSE)

##from here I can now begin cleaning the other datasets if necessary
##removing first row of psal
psal <- psal %>% select(-1)
##check colnames
colnames(psal)
##colnames fine
##export csv
write.csv(psal, "~\\NBA Salaries(1990-2023).csv", row.names = FALSE)
##removing first colimn of tpay
tpay <- tpay %>% select(-1)
##check colnames
colnames(tpay)
##colnames fine
##export csv
write.csv(psal, "~\\NBA Salaries(1990-2023).csv", row.names = FALSE)


########################################################################################################
##now I can begin exploring graphs and other features I'd like to implement



dplyr::filter(ptot, Season %in% c(Season)) %>%
  select(Player, Pos, Age, Tm, BLK, AST, TRB, STL, FG. , FT.)%>%
  filter(Tm == 'MIL')

teams <- ptot %>% 
  distinct(Tm, .keep_all = FALSE) %>% 
  arrange(Tm)

teams


historical_names <- data.frame(Tm = c("AND", "ATL", "BAL", "BLB", "BOS", "BRK", "BUF", "CAP", "CHA", "CHH", "CHI", "CHO", "CHP",
                                      "CHS", "CHZ", "CIN", "CLE", "DAL", "DEN", "DET", "DNN", "FTW", "GSW", 
                                      "HOU", "IND", "INO", "KCK", "KCO", "LAC", "LAL", "MEM", "MIA", "MIL", 
                                      "MIN", "MLH", "MNL", "NJN", "NOH", "NOJ", "NOK", "NOP", "NYK", "NYN", 
                                      "OKC", "ORL", "PHI", "PHO", "PHW", "POR", "ROC", "SAC", "SAS", "SDC", 
                                      "SDR", "SEA", "SFW", "SHE", "STB", "STL", "SYR", "TOR", "TRI", "UTA", 
                                      "VAN", "WAS", "WAT", "WSB", "WSC"),
                               TeamName = c("Anderson Packers", "Atlanta Hawks", "Baltimore Bullets", "Baltimore Bullets", "Boston Celtics", "Brooklyn Nets", "Buffalo Braves", 
                                            "Capital Bullets", "Charlotte Hornets", "Charlotte Hornets", "Chicago Bulls", 
                                            "Charlotte Hornets", "Chicago Packers", "Chicago Stags", "Chicago Zephyrs", 
                                            "Cincinnati Royals", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets", 
                                            "Detroit Pistons", "Denver Nuggets", "Fort Wayne Pistons", "Golden State Warriors", 
                                            "Houston Rockets", "Indiana Pacers", "Indianapolis Olympians", "Kansas City Kings", 
                                            "Kansas City-Omaha Kings", "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", 
                                            "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "Milwaukee Hawks", 
                                            "Minnesota Lakers", "New Jersey Nets", "New Orleans Hornets", "New Orleans Jazz", 
                                            "New Orleans/Oklahoma City Hornets", "New Orleans Pelicans", "New York Knicks", 
                                            "New York Nets", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", 
                                            "Phoenix Suns", "Philadelphia Warriors", "Portland Trail Blazers", "Rochester Royals", 
                                            "Sacramento Kings", "San Antonio Spurs", "San Diego Clippers", "San Diego Rockets", 
                                            "Seattle SuperSonics", "San Francisco Warriors", "Sheboygan Redskins", "St. Louis Bombers", 
                                            "St. Louis Hawks", "Syracuse Nationals", "Toronto Raptors", "Tri-Cities Blackhawks", 
                                            "Utah Jazz", "Vancouver Grizzlies", "Washington Wizards", "Waterloo Hawks", 
                                            "Washington Bullets", "Washington Capitols"))



historical_names


ptot <- ptot %>% 
  left_join(historical_names, by = "Tm") %>% 
  select(Season, Player, Pos, Age, TeamName, G, GS, MP, FG, FGA, `FG.`, X3P, X3PA, `X3P.`, X2P, X2PA, `X2P.`, `eFG.`, FT, FTA, `FT.`, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS)

write.csv(ptot, "~\\NBA Player Stats(1950 - 2022).csv", row.names = FALSE)


length(unique(ptot$Season))

##plot testing 
player_inp <- "Giannis Antetokounmpo"

plt <- dplyr::filter(ptot, Player == player_inp)

ggplot(plt, aes(x = Season)) +
  geom_line(aes(y = BLK/G, col = 'Blocks')) +
  geom_line(aes(y = AST/G, col = 'Assists')) +
  geom_line(aes(y = TRB/G, col = 'Rebounds')) +
  geom_line(aes(y = STL/G, col = 'Steals')) +
  geom_line(aes(y = PTS/G, col = 'Points')) +
  theme_minimal() +
  ggtitle('Statistics') +
  theme(plot.title = element_text(hjust = 0.4)) +
  scale_color_manual(
    name = "Type",
    values = c(
      "Blocks" = '#fde725',
      "Assists" = '#5ec962',
      "Rebounds" = '#21918c',
      "Steals" = '#3b528b',
      "Points" = '#440154'
    )
  )

player_inp <- "Aaron Henry"


plt <- dplyr::filter(ptot,Season == '2022', Player == player_inp) %>%
  group_by(Player, Season) %>%
  summarise(
    BLK = sum(BLK),
    AST = sum(AST),
    TRB = sum(TRB),
    STL = sum(STL),
    PTS = sum(PTS),
    FG. = sum(FG.),
    FT. = sum(FT.),
    G   = G
  )

ggplot(plt, aes(x = Season)) +
  geom_bar(aes(y = PTS/G, fill = 'Points'), position = 'stack', stat='identity') +
  geom_bar(aes(y = TRB/G, fill = 'Rebounds'), position = 'stack', stat='identity') +
  geom_bar(aes(y = AST/G, fill = 'Assists'), position = 'stack', stat='identity') +
  geom_bar(aes(y = BLK/G, fill = 'Blocks'), position = 'stack', stat='identity') +
  geom_bar(aes(y = STL/G, fill = 'Steals'), position = 'stack', stat='identity') +
  theme_minimal() +
  ggtitle('Statistics') +
  theme(plot.title = element_text(hjust = 0.4)) +
  scale_fill_manual(
    name = "Type",
    values = c(
      "Blocks" = '#fde725',
      "Assists" = '#5ec962',
      "Rebounds" = '#21918c',
      "Steals" = '#3b528b',
      "Points" = '#440154'
    )
  ) +
  scale_x_continuous(breaks = seq(floor(min(plt$Season)), ceiling(max(plt$Season)), 1))


plt
testplt <- ggplot(plt, aes(x = Season))
barplot(plt$BLK/plt$G)
