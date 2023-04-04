##Logan Lauton
##global.R - NBA Player HUD App v1.0
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
