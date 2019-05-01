##### Load libraries ###############################################################################

library(stringr)
library(plyr)
library(dplyr)
library(lubridate)

##### Scoring progression - one observation equals one score #######################################
##### Data cleaning ################################################################################

# Use df_scoringProgALL as base for new data frame
df_scoringProg_scores <- df_scoringProgALL

# Clean up string values to keep only alphanumeric data
df_scoringProg_scores$homeTeamScores <- gsub("[^[:alnum:] ]", NA, df_scoringProg_scores$homeTeamScores)
df_scoringProg_scores$homeTeamScoreTime <- gsub("[^[:alnum:] ]", NA, df_scoringProg_scores$homeTeamScoreTime)
df_scoringProg_scores$awayTeamScores <- gsub("[^[:alnum:] ]", NA, df_scoringProg_scores$awayTeamScores)
df_scoringProg_scores$awayTeamScoreTime <- gsub("[^[:alnum:] ]", NA, df_scoringProg_scores$awayTeamScoreTime)

# Remove rows that were column names in the HTML table
df_scoringProg_scores <- df_scoringProg_scores[ which(
  df_scoringProg_scores$homeTeam != df_scoringProg_scores$homeTeamScores |
    df_scoringProg_scores$awayTeam != df_scoringProg_scores$awayTeamScores), ]

# Add variable to specify team of scoring player
df_scoringProg_scores$teamAway <- is.na(df_scoringProg_scores$homeTeamScores)
df_scoringProg_scores$teamName <- ifelse(df_scoringProg_scores$teamAway == FALSE,
                                         df_scoringProg_scores$homeTeam,
                                         df_scoringProg_scores$awayTeam)

# Merge name of scoring players into one variable
# (Instead of two variables, split by team)
df_scoringProg_scores$homeTeamScores <- ifelse(df_scoringProg_scores$teamAway == TRUE,
                                               df_scoringProg_scores$awayTeamScores,
                                               df_scoringProg_scores$homeTeamScores)

# Merge time of score into one variable
# (Instead of two, split by team)
df_scoringProg_scores$homeTeamScoreTime <- ifelse(df_scoringProg_scores$teamAway == TRUE,
                                                  df_scoringProg_scores$awayTeamScoreTime,
                                                  df_scoringProg_scores$homeTeamScoreTime)

# Remove unneeded columns and reorder variables
# $season, $matchDetails, $homeTeam, $awayTeam
# $homeTeamScores, $teamName, $homeTeamScoreTime
df_scoringProg_scores <- df_scoringProg_scores[,c(8,1:4,10,5)]

# Rename variables
colnames(df_scoringProg_scores) <- c(
  "season", "matchDetails", "homeTeam", "awayTeam", "scoreInstance", "teamName", "scoreTime")

# Remove rownames
rownames(df_scoringProg_scores) <- NULL

# Split string data in $matchDetails
temp <- data.frame(str_split_fixed(df_scoringProg_scores$matchDetails, " ", 4))
# Merge round number as new variable back into df_scoringProg_scores
df_scoringProg_scores$round <- temp$X2

temp2 <- data.frame(str_split_fixed(temp$X4, " ", 4))
temp2$X1 <- as.character(temp2$X1)
temp2$X2 <- as.character(temp2$X2)
temp2$X3 <- as.character(temp2$X3)
temp2$X1 <- ifelse(temp2$X1 != "Venue:", temp2$X1, temp2$X2)
temp2$X2 <- ifelse(temp2$X2 == "Date:", NA, temp2$X2)
temp2$X3 <- ifelse(temp2$X3 == "Mon," | temp2$X3 == "Tue," | temp2$X3 == "Wed," | temp2$X3 == "Thu," |
                     temp2$X3 == "Fri," | temp2$X3 == "Sat," | temp2$X3 == "Sun," | temp2$X3 == "Date:",
                   NA, temp2$X3)
temp2$X1 <- revalue(temp2$X1, c(
  "Adelaide" = "Adelaide Oval", "Manuka" = "Manuka Oval", "Bellerive" = "Bellerive Oval",
  "Kardinia" = "Kardinia Park", "York" = "York Park", "Sydney" = "Sydney Showground",
  "Traeger" = "Traeger Park", "Marrara" = "Marrara Oval", "Cazaly's" = "Cazaly's Stadium",
  "Stadium" = "Stadium Australia", "Football" = "Football Park"
))
# Merge venue as new variable back into df_scoringProg_scores
df_scoringProg_scores$venue <- temp2$X1

temp3 <- data.frame(str_split_fixed(temp2$X4, " ", 4))
temp3$X1 <- as.character(temp3$X1)
temp3$X2 <- as.character(temp3$X2)
temp3$X3 <- as.character(temp3$X3)
temp3$X1 <- ifelse(temp3$X1 == "Thu," | temp3$X1 == "Fri," | temp3$X1 == "Sat," | temp3$X1 == "Sun," |
                     temp3$X1 == "Date:",
                   NA, temp3$X1)
temp3$X2 <- ifelse(temp3$X2 == "Fri," | temp3$X2 == "Sat," | temp3$X2 == "Sun,",
                   temp3$X3, temp3$X2)
temp3$X5 <- is.na(temp3$X1)
temp3$X1 <- ifelse(temp3$X5 == TRUE, temp3$X2, temp3$X1)
# Merge date as new variable back into df_scoringProg_scores
df_scoringProg_scores$date <- temp3$X1

temp4 <- data.frame(str_split_fixed(temp2$X4, "[()]", 2))
temp4 <- data.frame(str_split_fixed(temp4$X1, " ", 5))
temp4 <- temp4[,c(-1,-5)]
temp4$X2 <- as.character(temp4$X2)
temp4$X3 <- as.character(temp4$X3)
temp4$X4 <- as.character(temp4$X4)
temp4$X2 <- ifelse(temp4$X2 == "1:10" | temp4$X2 == "1:20" | temp4$X2 == "1:40" | temp4$X2 == "1:45" |
                     temp4$X2 == "12:10" | temp4$X2 == "2:10" | temp4$X2 == "2:15" | temp4$X2 == "2:20" |
                     temp4$X2 == "2:35" | temp4$X2 == "2:40" | temp4$X2 == "3:10" | temp4$X2 == "3:15" |
                     temp4$X2 == "3:20" | temp4$X2 == "3:35" | temp4$X2 == "3:40" | temp4$X2 == "3:45" |
                     temp4$X2 == "4:10" | temp4$X2 == "4:35" | temp4$X2 == "4:40" | temp4$X2 == "4:45" |
                     temp4$X2 == "5:10" | temp4$X2 == "5:40" | temp4$X2 == "6:10" | temp4$X2 == "6:20" |
                     temp4$X2 == "6:40" | temp4$X2 == "6:45" | temp4$X2 == "6:50" | temp4$X2 == "6:55" |
                     temp4$X2 == "7:05" | temp4$X2 == "7:10" | temp4$X2 == "7:20" | temp4$X2 == "7:25" |
                     temp4$X2 == "7:30" | temp4$X2 == "7:40" | temp4$X2 == "7:45" | temp4$X2 == "7:50",
                   temp4$X2, NA)
temp4$X3 <- ifelse(temp4$X3 == "1:05" | temp4$X3 == "1:10" | temp4$X3 == "1:15" | temp4$X3 == "1:20" |
                     temp4$X3 == "1:40" | temp4$X3 == "1:45" | temp4$X3 == "12:40" | temp4$X3 == "12:45" |
                     temp4$X3 == "2:10" | temp4$X3 == "2:20" | temp4$X3 == "2:30" | temp4$X3 == "2:40" |
                     temp4$X3 == "2:45" | temp4$X3 == "2:50" | temp4$X3 == "3:20" | temp4$X3 == "3:40" |
                     temp4$X3 == "4:05" | temp4$X3 == "4:10" | temp4$X3 == "4:15" | temp4$X3 == "4:35" |
                     temp4$X3 == "4:40" | temp4$X3 == "4:45" | temp4$X3 == "5:15" | temp4$X3 == "5:20" |
                     temp4$X3 == "5:40" | temp4$X3 == "5:45" | temp4$X3 == "5:50" | temp4$X3 == "6:10" |
                     temp4$X3 == "6:20" | temp4$X3 == "6:40" | temp4$X3 == "7:00" | temp4$X3 == "7:10" |
                     temp4$X3 == "7:15" | temp4$X3 == "7:20" | temp4$X3 == "7:25" | temp4$X3 == "7:30" |
                     temp4$X3 == "7:40" | temp4$X3 == "7:45" | temp4$X3 == "7:50" | temp4$X3 == "8:10" |
                     temp4$X3 == "8:15",
                   temp4$X3, NA)
temp4$X4 <- ifelse(temp4$X4 == "" | temp4$X4 == "Attendance:" | temp4$X4 == "PM",
                   NA, temp4$X4)
temp4$X5 <- is.na(temp4$X3)
temp4$X3 <- ifelse(temp4$X5 == TRUE, temp4$X4, temp4$X3)
temp4$X5 <- is.na(temp4$X2)
temp4$X2 <- ifelse(temp4$X5 == TRUE, temp4$X3, temp4$X2)
# Merge start time as new variable back into df_scoringProg_scores
df_scoringProg_scores$startTimeLocal <- temp4$X2

# Remove interstitial variables
rm(temp, temp2, temp3, temp4)

# Reorder variables
# $season, $date, $round, $homeTeam, $awayTeam, $startTimeLocal, $venue
# $scoreInstance, $teamName, $scoreTime
df_scoringProg_scores <- df_scoringProg_scores[,c(1,10,8,3,4,11,9,5:7)]

# Fix string data in $scoreInstance
# Identify scoring type (goal or behind)
df_scoringProg_scores$scoreType <- sub('^.* ([[:alnum:]]+)$', '\\1', df_scoringProg_scores$scoreInstance)
df_scoringProg_scores$scoreType <- revalue(df_scoringProg_scores$scoreType, c(
  "goal" = "Goal", "behind" = "Behind"
))

# Isolate player name within $scoreInstance values
temp <- data.frame(str_split_fixed(df_scoringProg_scores$scoreInstance, " ", 4))
temp$X1 <- as.character(temp$X1)
temp$X2 <- as.character(temp$X2)
temp$X3 <- as.character(temp$X3)
temp <- temp[,-4]
temp$X3 <- ifelse(temp$X3 == "behind" | temp$X3 == "goal" | temp$X3 == "", NA, temp$X3)
temp$X4 <- is.na(temp$X3)
temp$X5 <- ifelse(temp$X4 == FALSE, paste(temp$X2, temp$X3, sep = " "), temp$X2)
temp$X6 <- paste(temp$X1, temp$X5, sep = " ")
# Merge player names as new variable back into df_scoringProg_scores
df_scoringProg_scores$playerName <- temp$X6
# Drop $scoreInstance variable and reorder variables
# $season, $date, $round, $homeTeam, $awayTeam, $startTimeLocal, $venue
# $teamName, $scoreType, $playerName, $scoreTime
df_scoringProg_scores <- df_scoringProg_scores[,c(1:7,9,11,12,10)]

# Fix score time data
temp <- data.frame(str_split_fixed(df_scoringProg_scores$scoreTime, "s", 2))
temp$X1 <- as.character(temp$X1)
temp <- data.frame(str_split_fixed(temp$X1, "m ", 2))
colnames(temp) <- c("scoreTime_min", "scoreTime_sec")
temp$scoreTime_min <- as.numeric(as.character(temp$scoreTime_min))
temp$scoreTime_sec <- as.numeric(as.character(temp$scoreTime_sec))
temp$scoreTime_mmss <- ms(paste(temp$scoreTime_min, ":", temp$scoreTime_sec, sep = ""))
temp$scoreTime_secInQtr <- as.numeric(temp$scoreTime_mmss)
# Merge $scoreTime_mmss and $scoreTime_secInQtr back into df_scoringProg_scores
df_scoringProg_scores$scoreTime_mmss <- temp$scoreTime_mmss
df_scoringProg_scores$scoreTime_secInQtr <- temp$scoreTime_secInQtr
# Drop $scoreTime variable
df_scoringProg_scores <- df_scoringProg_scores[,-11]

# Format $date
df_scoringProg_scores$date <- parse_date_time(df_scoringProg_scores$date, orders="dmy")

# Add $scorePoints variable
df_scoringProg_scores$scorePoints <- df_scoringProg_scores$scoreType
df_scoringProg_scores$scorePoints <- revalue(df_scoringProg_scores$scorePoints, c(
  "Goal" = 6, "Behind" = 1
))

# Add variable for running scores
df_scoringProg_scores$homeTeamPoints <- ifelse(df_scoringProg_scores$teamName == df_scoringProg_scores$homeTeam,
                                               df_scoringProg_scores$scorePoints, 0)
df_scoringProg_scores$awayTeamPoints <- ifelse(df_scoringProg_scores$teamName == df_scoringProg_scores$awayTeam,
                                               df_scoringProg_scores$scorePoints, 0)

# Create unique $matchID variable
df_scoringProg_scores$matchID <- paste(df_scoringProg_scores$date, "_R", df_scoringProg_scores$round, "_",
                                       df_scoringProg_scores$homeTeam, "-v-", df_scoringProg_scores$awayTeam, sep = "")

# Calculate running points totals for home and away teams
df_scoringProg_scores <- df_scoringProg_scores %>%
  group_by(matchID) %>%
  mutate(homeTeamRunningScore = cumsum(homeTeamPoints))

df_scoringProg_scores <- df_scoringProg_scores %>%
  group_by(matchID) %>%
  mutate(awayTeamRunningScore = cumsum(awayTeamPoints))

##### Scoring progression - quarter and match durations ############################################
##### Data cleaning ################################################################################

# Use df_scoringProgALL as base for new data frame
df_scoringProg_timePeriods <- df_scoringProgALL

durationQtrs <- data.frame(
  read_html(url) %>%
    html_nodes(xpath = '/html/body/center/table[8]') %>%
    html_table(fill = TRUE))

durationQtrs <- data.frame(durationQtrs[-1,3])
colnames(durationQtrs) <- "duration"

durationQtrs2 <- data.frame(str_split_fixed(durationQtrs$duration, "Â", 2))
durationQtrs3 <- data.frame(str_split_fixed(durationQtrs2$X1, "-", 2))
durationQtrs3 <- data.frame(durationQtrs3[ which(durationQtrs3$X2 == ""), 1])
durationQtrs3 <- durationQtrs3[-5,]
durationQtrs3 <- data.frame(durationQtrs3)
colnames(durationQtrs3) <- "V1"
durationQtrs4 <- data.frame(str_split_fixed(durationQtrs3$V1, " ", 3))
durationQtrs5 <- data.frame(str_split_fixed(durationQtrs4$X3, "\\D", 5))
durationQtrs5 <- durationQtrs5[,c(2,4)]
colnames(durationQtrs5) <- c("min", "s")
durationQtrs5$period <- c(1,2,3,4)