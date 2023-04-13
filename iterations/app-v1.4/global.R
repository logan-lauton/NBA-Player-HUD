##Logan Lauton
##global.R - NBA Player HUD App v1.4
##this is the global.R for the first iteration of the NBA Player HUD R Shiny app that I will
##be producing. In this holds my initial exploratory graphs and features that I would like to implement.

##clear your working directory
rm(list = ls())

##load libraries
library("ggplot2")
library("ggcorrplot")
library("dplyr")
library("viridis")



##loading in required data
ptot <- read.csv("~\\NBA Player Stats(1950 - 2022).csv")
psal <- read.csv("~\\NBA Salaries(1990-2023).csv")
tpay <- read.csv("~\\NBA Payroll(1990-2023).csv")
pbox <- read.csv("~\\NBA Player Box Score Stats(1950 - 2022).csv")

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

teams_box <- pbox %>% 
  distinct(Tm, .keep_all = FALSE) %>% 
  arrange(Tm)

teams_box

not_in_historical <- teams_box %>% 
  anti_join(historical_names, by = "Tm") %>% 
  select(Tm)

not_in_historical


dplyr::filter(pbox,Tm == '') %>%
  select(PLAYER_NAME, Season, MATCHUP,)
  

historical_names <- data.frame(Tm = c("AND", "ATL", "BAL", "BKN", "BLB", "BLT", "BOM", "BOS", "BRK", "BUF", 
                                      "CAP", "CHA", "CHH", "CHI", "CHO", "CHP", "CHS", "CHZ", "CIN", "CLE", 
                                      "CLR", "DAL", "DEF", "DEN", "DET", "DN" , "DNN", "FTW", "GOS", "GSW", 
                                      "HOU", "HUS", "IND", "INO", "JET", "KCK", "KCO", "LAC", "LAL", "MEM", 
                                      "MIA", "MIH", "MIL", "MIN", "MLH", "MNL", "NJN", "NOH", "NOJ", "NOK", 
                                      "NOP", "NYK", "NYN", "OKC", "ORL", "PHI", "PHL", "PHO", "PHW", "PHX", 
                                      "PIT", "POR", "PRO", "ROC", "SAC", "SAN", "SAS", "SDC", "SDR", "SEA", 
                                      "SFW", "SHE", "STB", "STL", "SYR", "TCB", "TOR", "TRI", "UTA", "UTH",
                                      "VAN", "WAS", "WAT", "WSB", "WSC"),
                               TeamName = c("Anderson Packers", "Atlanta Hawks", "Baltimore Bullets", "Brooklyn Nets", "Baltimore Bullets", 
                                            "Baltimore Bullets", "St. Louis Bombers", "Boston Celtics", "Brooklyn Nets", "Buffalo Braves", 
                                            "Capital Bullets", "Charlotte Hornets", "Charlotte Hornets", "Chicago Bulls", 
                                            "Charlotte Hornets", "Chicago Packers", "Chicago Stags", "Chicago Zephyrs", 
                                            "Cincinnati Royals", "Cleveland Cavaliers", "Cleveland Rebels", "Dallas Mavericks", "Detroit Falcons",
                                            "Denver Nuggets", "Detroit Pistons", "Denver Nuggets", "Denver Nuggets", "Fort Wayne Pistons", "Golden State Warriors", 
                                            "Golden State Warriors", "Houston Rockets", "Toronto Huskies", "Indiana Pacers", "Indianapolis Olympians", 
                                            "Indianapolis Jets", "Kansas City Kings", 
                                            "Kansas City-Omaha Kings", "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", 
                                            "Miami Heat", "Milwaukee Hawks", "Milwaukee Bucks", "Minnesota Timberwolves", "Milwaukee Hawks", 
                                            "Minnesota Lakers", "New Jersey Nets", "New Orleans Hornets", "New Orleans Jazz", 
                                            "New Orleans/Oklahoma City Hornets", "New Orleans Pelicans", "New York Knicks", 
                                            "New York Nets", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Philadelphia 76ers",
                                            "Phoenix Suns", "Philadelphia Warriors", "Phoenix Suns", "Pittsburgh Ironmen", 
                                            "Portland Trail Blazers", "Providence Steamrollers", "Rochester Royals", 
                                            "Sacramento Kings", "San Antonio Spurs", "San Antonio Spurs", "San Diego Clippers", "San Diego Rockets", 
                                            "Seattle SuperSonics", "San Francisco Warriors", "Sheboygan Redskins", "St. Louis Bombers", 
                                            "St. Louis Hawks", "Syracuse Nationals", "Tri-Cities Blackhawks", "Toronto Raptors", "Tri-Cities Blackhawks", 
                                            "Utah Jazz", "Utah Jazz", "Vancouver Grizzlies", "Washington Wizards", "Waterloo Hawks", 
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

plt <-
      dplyr::filter(ptot, Player == player_inp) %>%
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
    gplt <- ggplot(plt, aes(x = Season))
    
      gplt +
        geom_line(aes(y = (FG.), col = 'FG%')) +
        geom_line(aes(y = (FT.), col = 'FT%')) +
        theme_minimal() +
        ggtitle(paste(player_inp, "'s %'s", sep = "")) +
        theme(plot.title = element_text(hjust = 0.4)) +
        theme(
          axis.line = element_line(color = 'lightgrey'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
        ) +
        scale_color_manual(
          name = "Measured %",
          values = c(
            "FG%" = '#7ad151',
            "FT%" = '#481c6e'
          )
        ) +
        scale_x_continuous(breaks = seq(
          min(player_inp$Season),
          max(player_inp$Season),
          by = 2
        ),
        expand = c(0, 0)) +
        ylab("Per Game Totals")
    
   

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

ptot <- readRDS("/cloud/project/ptot_20_data.rds")


ggplot(plt, aes(x = Season)) +
  geom_line(aes(y = FT, col = 'Blocks')) +
  geom_line(aes(y = AST/G, col = 'Assists')) +
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



##introducing new data which I have scraped------------------------------------------------------------
pbox <- pbox %>% select(-1)

colnames(pbox)
colnames(pbox)[colnames(pbox) == 'Team'] <- 'Tm'


pbox <- pbox %>% 
  left_join(historical_names, by = "Tm") %>% 
  select(Season, Game_ID, PLAYER_NAME , TeamName , GAME_DATE, MATCHUP, WL, MIN, FGM, FGA, FG_PCT, FG3M, FG3A, FG3_PCT, FTM, FTA, FT_PCT, OREB, DREB, REB, AST, STL, BLK, TOV, PF, PTS, PLUS_MINUS, VIDEO_AVAILABLE)

write.csv(pbox, "~\\NBA Player Box Score Stats(1950 - 2022).csv", row.names = FALSE)


colnames(pbox)



  dplyr::filter(pbox, Season %in% c(2023)) %>%
    select(PLAYER_NAME, TeamName, GAME_DATE, MATCHUP, WL, MIN, FGM, FGA, FG_PCT, FG3M , FG3A, FG3_PCT, FTM, FTA, FT_PCT, REB, AST, STL, BLK, TOV, PF, PTS, PLUS_MINUS, VIDEO_AVAILABLE) %>%
    mutate(FG_PCT = FG_PCT * 100, FG3_PCT = FG3_PCT * 100, FT_PCT = FT_PCT * 100) %>%
    arrange(GAME_DATE, PLAYER_NAME)

  
  
saveRDS(ptot, "ptot_data.rds")
saveRDS(psal, "psal_data.rds")
saveRDS(tpay, "tpay_data.rds")
saveRDS(pbox, "pbox_data.rds")

##looking to reduce the number of rows in the dataset, so I will only focus on 
##the previous 20 seasons(2003-2023) as no players in the league have been 
##in since 2002 or earlier.

pbox_20 <- pbox[pbox$Season >= 2003 & pbox$Season <= 2023, ]
ptot_20 <- ptot[ptot$Season >= 2003 & ptot$Season <= 2022, ]
psal_20 <- psal[psal$seasonStartYear >= 2003 & psal$seasonStartYear <= 2023, ]
tpay_20 <- tpay[tpay$seasonStartYear >= 2003 & tpay$seasonStartYear <= 2023, ]

saveRDS(ptot_20, "ptot_20_data.rds")
saveRDS(psal_20, "psal_20_data.rds")
saveRDS(tpay_20, "tpay_20_data.rds")
saveRDS(pbox_20, "pbox_20_data.rds")


ptot <- readRDS("/cloud/project/ptot_20_data.rds")
psal <- readRDS("/cloud/project/psal_20_data.rds")
tpay <- readRDS("/cloud/project/tpay_20_data.rds")
pbox <- readRDS("/cloud/project/pbox_20_data.rds")


ptot[is.na(ptot)] <- 0
ptot$TeamName <- as.numeric(factor(ptot$TeamName))
ptot$Pos <- as.numeric(factor(ptot$Pos))

numeric_cols <- select_if(ptot, is.numeric)


cor_mat <- cor(numeric_cols)

ggcorrplot(cor_mat, hc.order = TRUE, type = "lower", lab = TRUE, 
           lab_size = 1.25)+ 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

pbox[is.na(pbox)] <- 0
numeric_cols_2 <- select_if(pbox, is.numeric)


cor_mat_2 <- cor(numeric_cols)

ggcorrplot(cor_mat_2, hc.order = TRUE, type = "lower", lab = TRUE, 
           lab_size = 1.25)+ 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


##Testing MLR & LR models for forecasting tab to best guess points
##basing accuracy on radjsquared
summary(lm(formula = PTS ~ X2PA , data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ X2P , data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ MP , data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ TOV , data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ FTA , data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ FT , data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ FT/FTA , data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ X2P/X2PA , data = ptot))$adj.r.squared

summary(lm(formula = PTS ~ (FT/FTA) + X2PA, data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ (FT/FTA) + X2PA + MP , data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ (FT/FTA) + X2PA + MP + GS, data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ (FT/FTA) + X2PA + MP + TOV, data = ptot))$adj.r.squared
summary(lm(formula = PTS ~ (FT/FTA) + X2PA + MP + TOV + DRB, data = ptot))$adj.r.squared

##Basing model off Correlational heat map this is most accurate made 
ptot_mlr <- lm(formula = PTS ~ (FT/FTA) + X2PA + MP + TOV + DRB + STL, data = ptot)
summ_ptot <- summary(ptot_mlr)
##testing the null model anvoa
ptot_null_model <- lm(PTS ~ 1,
                 data = ptot)
anova(ptot_null_model, ptot_mlr)

##Since the p-value (2.2e-16) is less than the significance level (0.05) 
##we reject the null hypothesis

##checking the confidence interval
confint(ptot_mlr)[2,]

##
summ_ptot$adj.r.squared


new_data <- data.frame(
  FT = c(3) ,
  FTA = c(6),
  X2PA = c(57),      
  MP = c(204),        
  TOV = c(7),        
  DRB = c(26),        
  STL = c(6)         
)

predicted_pts <- predict(ptot_mlr, new_data)
predicted_pts


library(caret)

# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% testing
trainIndex <- createDataPartition(y = ptot$PTS, p = 0.7, list = FALSE)
trainData <- ptot[trainIndex, ]
testData <- ptot[-trainIndex, ]

# Create a new data frame with the predictor variables
new_data <- data.frame(
  FT = testData$FT,
  FTA = testData$FTA,
  X2PA = testData$X2PA,
  MP = testData$MP,
  TOV = testData$TOV,
  DRB = testData$DRB,
  STL = testData$STL
)

# Use the predict function to get the predicted values of PTS
predicted_values <- predict(ptot_mlr, newdata = new_data)


# Calculate RMSE
rmse <- sqrt(mean((testData$PTS - predicted_values)^2))

# Create a scatter plot of predicted vs. actual values
ggplot(data = testData, aes(x = PTS, y = predicted_values)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual PTS", y = "Predicted PTS")

plot(ptot_mlr$fitted.values, ptot_mlr$residuals, 
     xlab = "Fitted Values", ylab = "Residuals")
std_resid <- rstandard(ptot_mlr)
hist(std_resid, breaks = 30)






ptot_mlr <- lm(formula = PTS ~ (FT/FTA) + X2PA + MP + TOV + DRB + STL, data = ptot)



library(caret)

# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% testing
trainIndex <- createDataPartition(y = ptot$PTS, p = 0.7, list = FALSE)
trainData <- ptot[trainIndex, ]
testData <- ptot[-trainIndex, ]

# Create a new data frame with the predictor variables
new_data <- data.frame(
  FT = testData$FT,
  FTA = testData$FTA,
  X2PA = testData$X2PA,
  MP = testData$MP,
  TOV = testData$TOV,
  DRB = testData$DRB,
  STL = testData$STL
)



ptot_mlr$residuals_std <- rstandard(ptot_mlr)
std_resid <- rstandard(ptot_mlr)

ptot_filtered <- ptot %>%
  filter(abs(std_resid) <= 2)

ptot_mlr_filtered <- lm(formula = PTS ~ (FT/FTA) + X2PA + MP + TOV + DRB + STL, data = ptot_filtered)


library(dplyr)


rmse <- function(predicted, actual) {
  sqrt(mean((predicted - actual)^2))
}

rmse_filtered <- rmse(predict(ptot_mlr_filtered), ptot_filtered$PTS)




# Get the standardized residuals
while(rmse_filtered > 4.6){
  ptot_mlr$residuals_std <- rstandard(ptot_mlr)
  std_resid <- rstandard(ptot_mlr_filtered)


# Filter out the outliers with standardized residuals greater than 
  ptot_filtered <- ptot_filtered %>%
    filter(abs(std_resid) <= 2)
  ptot_mlr_filtered <- lm(formula = PTS ~ (FT/FTA) + X2PA + MP + TOV + DRB + STL, data = ptot_filtered)

  rmse_filtered <- rmse(predict(ptot_mlr_filtered), ptot_filtered$PTS)

  predicted_values <- predict(ptot_mlr_filtered, newdata = new_data)
  
}

summary(ptot_mlr_filtered)$adj.r.squared

rmse_filtered

predicted_values

plot(ptot_mlr_filtered$fitted.values, ptot_mlr_filtered$residuals, 
     xlab = "Fitted Values", ylab = "Residuals")
hist(std_resid, breaks = 30)


cooksD <- cooks.distance(ptot_mlr_filtered)

n <- nrow(ptot_filtered)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "steelblue") # add cutoff line


influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])

ptot_filtered <- ptot_filtered[-influential_obs, ]


predicted_values

library(car)

car::qqPlot(ptot_mlr_filtered, id = TRUE, col.lines = "blue", 
            reps = 1000, ylab = "Ordered R-Student Residuals", pch = 16)


vals<-(car::qqPlot(ptot_mlr_filtered, id = TRUE, col.lines = "blue", 
                     reps = 1000, ylab = "Ordered R-Student Residuals", pch = 16))
vals
ptot_filtered <- ptot_filtered[-vals, ]
ptot_mlr_filtered <- lm(formula = PTS ~ (FT/FTA) + X2PA + MP + TOV + DRB + STL, data = ptot_filtered)

summary(ptot_mlr_filtered)$adj.r.squared

rmse_filtered <- rmse(predict(ptot_mlr_filtered), ptot_filtered$PTS)
rmse_filtered

predicted_values <- predict(ptot_mlr_filtered, newdata = new_data)


predicted_values <- abs(round(predicted_values))
max(predicted_values)
min(predicted_values)
round(mean(predicted_values))
round(median(predicted_values))

ggplot(data = testData, aes(x = PTS, y = predicted_values)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  labs(x = "Actual PTS", y = "Predicted PTS")


