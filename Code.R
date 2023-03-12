# Attaching necessary libraries
library(data.table)
library(dplyr)
library(xgboost)

# Reading in the data
data <- fread('./nfl_team_stats_2002-2021.csv')

# Creating data for wins, losses, and ties
data$diff <- (data$score_away - data$score_home)
data$winner <- 1
data$winner[data$diff < 0] <- data$home[data$diff < 0]
data$winner[data$diff > 0] <- data$away[data$diff > 0]

data$loser <- 1
data$loser[data$diff > 0] <- data$home[data$diff > 0]
data$loser[data$diff < 0] <- data$away[data$diff < 0]

data$tied[data$diff == 0] <- 1
data$winner[data$winner == 1] <- NA
data$loser[data$loser == 1] <- NA

#Seperating data into each respective season
nfl_2018 <- data[data$date %between% c("2018-08-01", "2019-02-04")]
nfl_2019 <- data[data$date %between% c("2019-08-01", "2020-02-02")]
nfl_2020 <- data[data$date %between% c("2020-08-01", "2021-02-07")]
nfl_2021 <- data[data$date %between% c("2021-08-01", "2022-02-13")]

#######################2018##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2018$winner))
loses <- as.data.table(table(nfl_2018$loser))

season_2018 <- cbind(wins, loses$N)
colnames(season_2018) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2018$Ties <- 0
x <- na.omit(nfl_2018$away[nfl_2018$tied == 1])
y <- na.omit(nfl_2018$home[nfl_2018$tied == 1])
tied_teams <- c(x,y)

season_2018$Ties[season_2018$Teams == tied_teams] <- 1

# Win percentages
season_2018$win_percent <- season_2018$Wins/(season_2018$Wins+season_2018$Loses+season_2018$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$score_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$score_home[which(nfl_2018$home == n)]))
}

season_2018$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$rushing_yards_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$rushing_yards_home[which(nfl_2018$home == n)]))
}

season_2018$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$passing_yards_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$passing_yards_home[which(nfl_2018$home == n)]))
}

season_2018$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2018$total_yards <- season_2018$total_rushing_yards + season_2018$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$turnovers_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$turnovers_home[which(nfl_2018$home == n)]))
}

season_2018$total_turnovers <- mapply("+", arr1, arr2)

######################2019##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2019$winner))
loses <- as.data.table(table(nfl_2019$loser))

season_2019 <- cbind(wins, loses$N)
colnames(season_2019) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2019$Ties <- 0
x <- na.omit(nfl_2019$away[nfl_2019$tied == 1])
y <- na.omit(nfl_2019$home[nfl_2019$tied == 1])
tied_teams <- c(x,y)

season_2019$Ties[season_2019$Teams == tied_teams] <- 1

season_2019$win_percent <- season_2019$Wins/(season_2019$Wins+season_2019$Loses+season_2019$Ties)

arr1 <- c()
arr2 <- c()

# Calculating team records
for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$score_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$score_home[which(nfl_2019$home == n)]))
}

season_2019$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$rushing_yards_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$rushing_yards_home[which(nfl_2019$home == n)]))
}

season_2019$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$passing_yards_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$passing_yards_home[which(nfl_2019$home == n)]))
}

season_2019$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2019$total_yards <- season_2019$total_rushing_yards + season_2019$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$turnovers_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$turnovers_home[which(nfl_2019$home == n)]))
}

season_2019$total_turnovers <- mapply("+", arr1, arr2)
######################2020##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2020$winner))
loses <- as.data.table(table(nfl_2020$loser))

season_2020 <- cbind(wins, loses$N)
colnames(season_2020) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2020$Ties <- 0
x <- na.omit(nfl_2020$away[nfl_2020$tied == 1])
y <- na.omit(nfl_2020$home[nfl_2020$tied == 1])
tied_teams <- c(x,y)

season_2020$Ties[season_2020$Teams == tied_teams] <- 1

season_2020$win_percent <- season_2020$Wins/(season_2020$Wins+season_2020$Loses+season_2020$Ties)

arr1 <- c()
arr2 <- c()

# Calculating team records
for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$score_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$score_home[which(nfl_2020$home == n)]))
}

season_2020$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$rushing_yards_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$rushing_yards_home[which(nfl_2020$home == n)]))
}

season_2020$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$passing_yards_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$passing_yards_home[which(nfl_2020$home == n)]))
}

season_2020$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2020$total_yards <- season_2020$total_rushing_yards + season_2020$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$turnovers_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$turnovers_home[which(nfl_2020$home == n)]))
}

season_2020$total_turnovers <- mapply("+", arr1, arr2)

######################2021##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2021$winner))
loses <- as.data.table(table(nfl_2021$loser))

season_2021 <- cbind(wins, loses$N)
colnames(season_2021) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2021$Ties <- 0
x <- na.omit(nfl_2021$away[nfl_2021$tied == 1])
y <- na.omit(nfl_2021$home[nfl_2021$tied == 1])
tied_teams <- c(x,y)

season_2021$Ties[season_2021$Teams == tied_teams] <- 1

season_2021$win_percent <- season_2021$Wins/(season_2021$Wins+season_2021$Loses+season_2021$Ties)

arr1 <- c()
arr2 <- c()

# Calculating team records
for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$score_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$score_home[which(nfl_2021$home == n)]))
}

season_2021$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$rushing_yards_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$rushing_yards_home[which(nfl_2021$home == n)]))
}

season_2021$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$passing_yards_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$passing_yards_home[which(nfl_2021$home == n)]))
}

season_2021$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2021$total_yards <- season_2021$total_rushing_yards + season_2021$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$turnovers_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$turnovers_home[which(nfl_2021$home == n)]))
}

season_2021$total_turnovers <- mapply("+", arr1, arr2)

#####################################

fwrite(season_2018, "season_2018.csv")
fwrite(season_2019, "season_2019.csv")
fwrite(season_2020, "season_2020.csv")
fwrite(season_2021, "season_2021.csv")
