##### Load libraries ###############################################################################

library(stringr)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)

##### Scoring progression - one observation equals one score #######################################
##### Data cleaning ################################################################################

# Use df_scoringProgALL as base for new data frame
df_scoringProg_scores <- df_scoringProgALL

# Clean up string values to keep only alphanumeric data
df_scoringProg_scores$homeTeamScores <- gsub("[^[:alnum:] ]",
                                             NA, df_scoringProg_scores$homeTeamScores)
df_scoringProg_scores$homeTeamScoreTime <- gsub("[^[:alnum:] ]",
                                                NA, df_scoringProg_scores$homeTeamScoreTime)
df_scoringProg_scores$awayTeamScores <- gsub("[^[:alnum:] ]",
                                             NA, df_scoringProg_scores$awayTeamScores)
df_scoringProg_scores$awayTeamScoreTime <- gsub("[^[:alnum:] ]",
                                                NA, df_scoringProg_scores$awayTeamScoreTime)

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
temp2$X3 <- ifelse(temp2$X3 == "Mon," | temp2$X3 == "Tue," | temp2$X3 == "Wed," |
                     temp2$X3 == "Thu," | temp2$X3 == "Fri," | temp2$X3 == "Sat," |
                     temp2$X3 == "Sun," | temp2$X3 == "Date:",
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
temp3$X1 <- ifelse(temp3$X1 == "Thu," | temp3$X1 == "Fri," | temp3$X1 == "Sat," |
                     temp3$X1 == "Sun," | temp3$X1 == "Date:",
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
temp4$X2 <- ifelse(temp4$X2 == "1:10" | temp4$X2 == "1:20" | temp4$X2 == "1:40" |
                     temp4$X2 == "1:45" | temp4$X2 == "12:10" | temp4$X2 == "2:10" |
                     temp4$X2 == "2:15" | temp4$X2 == "2:20" | temp4$X2 == "2:35" |
                     temp4$X2 == "2:40" | temp4$X2 == "3:10" | temp4$X2 == "3:15" |
                     temp4$X2 == "3:20" | temp4$X2 == "3:35" | temp4$X2 == "3:40" |
                     temp4$X2 == "3:45" | temp4$X2 == "4:10" | temp4$X2 == "4:35" |
                     temp4$X2 == "4:40" | temp4$X2 == "4:45" | temp4$X2 == "5:10" |
                     temp4$X2 == "5:40" | temp4$X2 == "6:10" | temp4$X2 == "6:20" |
                     temp4$X2 == "6:40" | temp4$X2 == "6:45" | temp4$X2 == "6:50" |
                     temp4$X2 == "6:55" | temp4$X2 == "7:05" | temp4$X2 == "7:10" |
                     temp4$X2 == "7:20" | temp4$X2 == "7:25" | temp4$X2 == "7:30" |
                     temp4$X2 == "7:40" | temp4$X2 == "7:45" | temp4$X2 == "7:50",
                   temp4$X2, NA)
temp4$X3 <- ifelse(temp4$X3 == "1:05" | temp4$X3 == "1:10" | temp4$X3 == "1:15" |
                     temp4$X3 == "1:20" | temp4$X3 == "1:40" | temp4$X3 == "1:45" |
                     temp4$X3 == "12:40" | temp4$X3 == "12:45" | temp4$X3 == "2:10" |
                     temp4$X3 == "2:20" | temp4$X3 == "2:30" | temp4$X3 == "2:40" |
                     temp4$X3 == "2:45" | temp4$X3 == "2:50" | temp4$X3 == "3:20" |
                     temp4$X3 == "3:40" | temp4$X3 == "4:05" | temp4$X3 == "4:10" |
                     temp4$X3 == "4:15" | temp4$X3 == "4:35" | temp4$X3 == "4:40" |
                     temp4$X3 == "4:45" | temp4$X3 == "5:15" | temp4$X3 == "5:20" |
                     temp4$X3 == "5:40" | temp4$X3 == "5:45" | temp4$X3 == "5:50" |
                     temp4$X3 == "6:10" | temp4$X3 == "6:20" | temp4$X3 == "6:40" | 
                     temp4$X3 == "7:00" | temp4$X3 == "7:10" | temp4$X3 == "7:15" |
                     temp4$X3 == "7:20" | temp4$X3 == "7:25" | temp4$X3 == "7:30" |
                     temp4$X3 == "7:40" | temp4$X3 == "7:45" | temp4$X3 == "7:50" |
                     temp4$X3 == "8:10" | temp4$X3 == "8:15",
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
df_scoringProg_scores$scoreType <- sub('^.* ([[:alnum:]]+)$',
                                       '\\1', df_scoringProg_scores$scoreInstance)
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
df_scoringProg_scores$homeTeamPoints <- ifelse(
  df_scoringProg_scores$teamName == df_scoringProg_scores$homeTeam,
  df_scoringProg_scores$scorePoints, 0)
df_scoringProg_scores$awayTeamPoints <- ifelse(
  df_scoringProg_scores$teamName == df_scoringProg_scores$awayTeam,
  df_scoringProg_scores$scorePoints, 0)

# Create unique $matchID variable
df_scoringProg_scores$matchID <- paste(df_scoringProg_scores$date,
                                       "_R",
                                       df_scoringProg_scores$round,
                                       "_",
                                       df_scoringProg_scores$homeTeam,
                                       "-v-",
                                       df_scoringProg_scores$awayTeam,
                                       sep = "")

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

# Keep only observations with quarter durations
df_scoringProg_timePeriods <- filter(df_scoringProg_timePeriods, grepl('quarter', homeTeamScores))
df_scoringProg_timePeriods <- df_scoringProg_timePeriods[,-c(5:7)]
colnames(df_scoringProg_timePeriods)[4] <- "qtrDuration"

# Clean up string values in temp$X3 to keep only alphanumeric data
temp <- df_scoringProg_timePeriods
temp2 <- data.frame(str_split_fixed(temp$qtrDuration, "Â", 2))
temp3 <- data.frame(str_split_fixed(temp2$X1, " ", 3))
temp4 <- data.frame(str_split_fixed(temp3$X3, "\\D", 5))
# Assign data classes
temp3$X1 <- as.character(temp3$X1)
temp4$X2 <- as.numeric(as.character(temp4$X2))
temp4$X4 <- as.numeric(as.character(temp4$X4))
# Merge clean variables into temporary data frame
temp5 <- cbind(temp3$X1, temp4$X2, temp4$X4)
colnames(temp5) <- c(
  "quarter", "min", "s"
)
# Merge back in with df_scoringProg_timePeriods
df_scoringProg_timePeriods <- cbind(df_scoringProg_timePeriods, temp5)

# Drop messy $qtrDuration variable and reorder variables
# $season, $matchDetails, $homeTeam, $awayTeam, $quarter, $min, $s
df_scoringProg_timePeriods <- df_scoringProg_timePeriods[,c(5,1:3,6:8)]

# Remove interstitial objects
rm(temp, temp2, temp3, temp4, temp5)

# Convert quarter duration variables to mmss and sec formats
df_scoringProg_timePeriods$qtrDuration_mmss <- ms(paste(df_scoringProg_timePeriods$min,
                                                        ":",
                                                        df_scoringProg_timePeriods$s,
                                                        sep = ""))
df_scoringProg_timePeriods$qtrDuration_seconds <- as.numeric(
  df_scoringProg_timePeriods$qtrDuration_mmss)
# Drop $min and $s
df_scoringProg_timePeriods <- df_scoringProg_timePeriods[,-c(6,7)]

# Revalue $quarter values
df_scoringProg_timePeriods$quarter <- as.character(df_scoringProg_timePeriods$quarter)
df_scoringProg_timePeriods$quarter <- revalue(df_scoringProg_timePeriods$quarter, c(
  "1st" = "Q1", "2nd" = "Q2", "3rd" = "Q3", "Final" = "Q4"
))

# Split string data in $matchDetails
temp <- data.frame(str_split_fixed(df_scoringProg_timePeriods$matchDetails, " ", 4))
# Merge round number as new variable back into df_scoringProg_timePeriods
df_scoringProg_timePeriods$round <- temp$X2

temp2 <- data.frame(str_split_fixed(temp$X4, " ", 7))
temp2 <- temp2[,-c(1:3,7)]
temp2$X4 <- as.character(temp2$X4)
temp2$X5 <- as.character(temp2$X5)
temp2$X6 <- as.character(temp2$X6)
temp2$X4 <- ifelse(temp2$X4 == "Thu," | temp2$X4 == "Fri," | temp2$X4 == "Sat," |
                     temp2$X4 == "Sun," | temp2$X4 == "Date:",
                   NA, temp2$X4)
temp2$X4 <- ifelse(is.na(temp2$X4) == TRUE, temp2$X5, temp2$X4)
temp2$X4 <- ifelse(temp2$X4 == "Fri," | temp2$X4 == "Sat," | temp2$X4 == "Sun,",
                   NA, temp2$X4)
temp2$X4 <- ifelse(is.na(temp2$X4) == TRUE, temp2$X6, temp2$X4)
# Merge date variable back into df_scoringProg_timePeriods
df_scoringProg_timePeriods$date <- temp2$X4

# Remove interstitial objects
rm(temp, temp2)

# Format $date
df_scoringProg_timePeriods$date <- parse_date_time(df_scoringProg_timePeriods$date, orders="dmy")

# Create unique $matchID variable
df_scoringProg_timePeriods$matchID <- paste(df_scoringProg_timePeriods$date,
                                            "_R",
                                            df_scoringProg_timePeriods$round,
                                            "_",
                                            df_scoringProg_timePeriods$homeTeam,
                                            "-v-",
                                            df_scoringProg_timePeriods$awayTeam,
                                            sep = "")

# Reorder variables
# $matchID, $quarter, $qtrDuration_seconds
df_scoringProg_timePeriods <- df_scoringProg_timePeriods[,c(10,5,7)]

# Cast data frame from long to wide
df_scoringProg_timePeriods <- dcast(df_scoringProg_timePeriods,
                                    matchID ~ quarter,
                                    value.var = "qtrDuration_seconds")

# Calculate total game duration
df_scoringProg_timePeriods$gameDuration <- df_scoringProg_timePeriods$Q1 +
  df_scoringProg_timePeriods$Q2 + df_scoringProg_timePeriods$Q3 + df_scoringProg_timePeriods$Q4

# Rename variables
colnames(df_scoringProg_timePeriods) <- c(
  "matchID", "Q1_s", "Q2_s", "Q3_s", "Q4_s", "gameDuration_s"
)

##### Scoring progression - final data set #########################################################
##### Data cleaning ################################################################################

# Merge df_scoringProg_scores and df_scoringProg_timePeriods, as base for new data frame
df_scoringProg_final <- inner_join(df_scoringProg_scores, df_scoringProg_timePeriods)
df_scoringProg_final <- data.frame(df_scoringProg_final)

# Identify which quarter each score instance belongs to
temp <- df_scoringProg_final
temp2 <- as.data.frame(diff(temp$scoreTime_secInQtr))
temp2 <- rbind(temp2, c(-100))
colnames(temp2) <- "diff"
temp2$endQtr <- ifelse(temp2$diff > 0, 0, 1)

# Merge temp2$endQtr back into df_scoringProg_final
df_scoringProg_final <- cbind(df_scoringProg_final, temp2$endQtr)
colnames(df_scoringProg_final)[24] <- "endQtr"

# Remove interstitial objects
rm(temp, temp2)

# Assign unique value for each quarter across all matches
df_scoringProg_final$group <- rev(cumsum(rev(df_scoringProg_final$endQtr)))

# Calculate max 'group' value for each unique matchID
# This identifies the 1st quarter for each match
df_scoringProg_final <- df_scoringProg_final %>%
  group_by(matchID) %>%
  mutate(Q1_groupVal = max(group))

# Use $Q1_groupVal to calculate value for the 2nd quarter for each match
df_scoringProg_final <- df_scoringProg_final %>%
  group_by(matchID) %>%
  mutate(Q2_groupVal = (Q1_groupVal - 1))

# Use $Q2_groupVal to calculate value for the 3rd quarter for each match
df_scoringProg_final <- df_scoringProg_final %>%
  group_by(matchID) %>%
  mutate(Q3_groupVal = (Q2_groupVal - 1))

# Calculate min 'group' value for each unique matchID
# This identifies the 4th quarter for each match
df_scoringProg_final <- df_scoringProg_final %>%
  group_by(matchID) %>%
  mutate(Q4_groupVal = min(group))

df_scoringProg_final$quarter <- ifelse(
  df_scoringProg_final$group == df_scoringProg_final$Q1_groupVal, 1, ifelse(
    df_scoringProg_final$group == df_scoringProg_final$Q2_groupVal, 2, ifelse(
      df_scoringProg_final$group == df_scoringProg_final$Q3_groupVal, 3, 4
    )
  )
)

# Remove unneeded variables and reorder variables
# $season, $date, $round, $homeTeam, $awayTeam, $startTimeLocal
# $venue, $quarter, $teamName, $scoreType, $playerName, $scoreTime_mmss, $scoreTime_secInQtr
# $scorePoints, $homeTeamPoints, $awayTeamPoints, $homeTeamRunningScore, $awayTeamRunningScore
# $matchID, $Q1_s, $Q2_s, $Q3_s, $Q4_s, $gameDuration_s
df_scoringProg_final <- df_scoringProg_final[,c(1:7,30,8:15,17,18,16,19:23)]

# Create new variables for game duration up to half-time
df_scoringProg_final$Q1Q2_s <- df_scoringProg_final$Q1_s + df_scoringProg_final$Q2_s

# Create new variable for game duration up to three-quarter time
df_scoringProg_final$Q1Q2Q3_s <- df_scoringProg_final$Q1_s +
  df_scoringProg_final$Q2_s + df_scoringProg_final$Q3_s

# Create new variable for identifying the time of each score instance
# As a timestamp based on the running game time
df_scoringProg_final <- df_scoringProg_final %>%
  group_by(matchID) %>%
  mutate(scoreTime_seconds = ifelse(quarter == 2, (Q1_s + scoreTime_secInQtr), ifelse(
    quarter == 3, (Q1Q2_s + scoreTime_secInQtr), ifelse(
      quarter == 4, (Q1Q2Q3_s + scoreTime_secInQtr), scoreTime_secInQtr
    )
  )))

# Create new variable expressing the time of each score instance
# as a percentage of total game duration
df_scoringProg_final <- df_scoringProg_final %>%
  group_by(matchID) %>%
  mutate(scoreTime_pct = round((scoreTime_seconds / gameDuration_s * 100), 1))

# Isolate final score for both teams in every match
finalScores <- ddply(df_scoringProg_final, .(matchID), summarise,
                     homeTeamFinalScore = max(homeTeamRunningScore, na.rm = TRUE),
                     awayTeamFinalScore = max(awayTeamRunningScore, na.rm = TRUE))
# Merge finalScores data frame back into df_scoringProg_final
df_scoringProg_final <- inner_join(df_scoringProg_final, finalScores)

# For each match, identify which team won or lost, or whether it was a draw
df_scoringProg_final$homeMinusAwayScore <- df_scoringProg_final$homeTeamFinalScore -
  df_scoringProg_final$awayTeamFinalScore
df_scoringProg_final$homeTeamResult <- ifelse(
  df_scoringProg_final$homeMinusAwayScore > 0, "Win", ifelse(
    df_scoringProg_final$homeMinusAwayScore < 0, "Loss", "Draw"
  )
)
df_scoringProg_final$winningTeam <- ifelse(
  df_scoringProg_final$homeTeamResult == "Win", df_scoringProg_final$homeTeam, ifelse(
    df_scoringProg_final$homeTeamResult == "Loss", df_scoringProg_final$awayTeam, "Draw"
  )
)
df_scoringProg_final$losingTeam <- ifelse(
  df_scoringProg_final$homeTeamResult == "Loss", df_scoringProg_final$homeTeam, ifelse(
    df_scoringProg_final$homeTeamResult == "Win", df_scoringProg_final$awayTeam, "Draw"
  )
)

# Clean up naming of 'Brisbane Lions' to 'Brisbane' for consistency
df_scoringProg_final$homeTeam <- revalue(df_scoringProg_final$homeTeam, c(
                                         "Brisbane Lions" = "Brisbane"
))
df_scoringProg_final$awayTeam <- revalue(df_scoringProg_final$awayTeam, c(
  "Brisbane Lions" = "Brisbane"
))
df_scoringProg_final$teamName <- revalue(df_scoringProg_final$teamName, c(
  "Brisbane Lions" = "Brisbane"
))
df_scoringProg_final$winningTeam <- revalue(df_scoringProg_final$winningTeam, c(
  "Brisbane Lions" = "Brisbane"
))
df_scoringProg_final$losingTeam <- revalue(df_scoringProg_final$losingTeam, c(
  "Brisbane Lions" = "Brisbane"
))

# Assign data classes
df_scoringProg_final$season <- as.numeric(as.character(df_scoringProg_final$season))
df_scoringProg_final$scorePoints <- as.numeric(as.character(df_scoringProg_final$scorePoints))
df_scoringProg_final$homeTeamPoints <- as.numeric(as.character(df_scoringProg_final$homeTeamPoints))
df_scoringProg_final$awayTeamPoints <- as.numeric(as.character(df_scoringProg_final$awayTeamPoints))

##### Summary ######################################################################################

# Created the following cleaned data frames:
# df_scoringProg_scores
# df_scoringProg_timePeriods
# df_scoringProg_final

# Save to CSV
write.csv(df_scoringProg_scores, "df_scoringProg_scores.csv", row.names = FALSE)
write.csv(df_scoringProg_timePeriods, "df_scoringProg_timePeriods.csv", row.names = FALSE)
write.csv(df_scoringProg_final, "df_scoringProg_final.csv", row.names = FALSE)

# Remove interstitial objects
# rm(df_scoringProg2008, df_scoringProg2009, df_scoringProg2010, df_scoringProg2011, df_scoringProg2012,
   # df_scoringProg2013, df_scoringProg2014, df_scoringProg2015, df_scoringProg2016, listOfURLs,
   # URLs2008, URLs2009, URLs2010, URLs2011, URLs2012, URLs2013, URLs2014, URLs2015, URLs2016,
   # finalScores)