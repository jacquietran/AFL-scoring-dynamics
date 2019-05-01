##### Load libraries ###############################################################################

library(plyr)
library(dplyr)
library(lubridate)

##### Don't need to run the below for the SMA abstract
##### Data wrangling ###############################################################################
##### Pace of scoring - home teams #################################################################

# Isolate final score for both teams in every match
# Use df_scoringProg_final as a base for a new data frame
temp <- ddply(df_scoringProg_final, .(matchID), summarise,
              homeTeamFinalScore = max(homeTeamRunningScore, na.rm = TRUE),
              awayTeamFinalScore = max(awayTeamRunningScore, na.rm = TRUE))

# Create new data frame
# df_scoringProg_homeTeamRunningScore
df_scoringProg_homeTeamRunningScore <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == df_scoringProg_final$homeTeam
), ]

df_scoringProg_homeTeamRunningScore <- full_join(df_scoringProg_homeTeamRunningScore, everySecond)

# Reduce variables
# $matchID, $scoreTime_seconds, $scorePoints
df_scoringProg_homeTeamRunningScore <- df_scoringProg_homeTeamRunningScore[,c(19,27,14)]

# Sort observations
df_scoringProg_homeTeamRunningScore <- df_scoringProg_homeTeamRunningScore[ order(
  df_scoringProg_homeTeamRunningScore$matchID, df_scoringProg_homeTeamRunningScore$scoreTime_seconds
), ]

# Fill NA values in $scorePoints
df_scoringProg_homeTeamRunningScore$scorePoints <- ifelse(
  is.na(df_scoringProg_homeTeamRunningScore$scorePoints) == TRUE,
  0, df_scoringProg_homeTeamRunningScore$scorePoints)

# Calculate running points totals for home teams
# $homeTeamRunningScore
df_scoringProg_homeTeamRunningScore <- df_scoringProg_homeTeamRunningScore %>%
  group_by(matchID) %>%
  mutate(homeTeamRunningScore = cumsum(scorePoints))

##### Pace of scoring - away teams #################################################################

# Create new data frame
# df_scoringProg_awayTeamRunningScore
df_scoringProg_awayTeamRunningScore <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == df_scoringProg_final$awayTeam
), ]

df_scoringProg_awayTeamRunningScore <- full_join(df_scoringProg_awayTeamRunningScore, everySecond)

# Reduce variables
# $matchID, $scoreTime_seconds, $scorePoints
df_scoringProg_awayTeamRunningScore <- df_scoringProg_awayTeamRunningScore[,c(19,27,14)]

# Sort observations
df_scoringProg_awayTeamRunningScore <- df_scoringProg_awayTeamRunningScore[ order(
  df_scoringProg_awayTeamRunningScore$matchID, df_scoringProg_awayTeamRunningScore$scoreTime_seconds
), ]

# Fill NA values in $scorePoints
df_scoringProg_awayTeamRunningScore$scorePoints <- ifelse(
  is.na(df_scoringProg_awayTeamRunningScore$scorePoints) == TRUE,
  0, df_scoringProg_awayTeamRunningScore$scorePoints)

# Calculate running points totals for away teams
# $awayTeamRunningScore
df_scoringProg_awayTeamRunningScore <- df_scoringProg_awayTeamRunningScore %>%
  group_by(matchID) %>%
  mutate(awayTeamRunningScore = cumsum(scorePoints))

##### Pace of scoring - both teams #################################################################

# Merge home and away team running scores data
df_scoringProg_homeTeamRunningScore <- data.frame(df_scoringProg_homeTeamRunningScore)
df_scoringProg_awayTeamRunningScore <- data.frame(df_scoringProg_awayTeamRunningScore)
colnames(df_scoringProg_homeTeamRunningScore)[3] <- "homeTeamPoints"
colnames(df_scoringProg_awayTeamRunningScore)[3] <- "awayTeamPoints"
df_scoringProg_homeTeamRunningScore$homeTeamPoints <- as.numeric(as.character(
  df_scoringProg_homeTeamRunningScore$homeTeamPoints))
df_scoringProg_awayTeamRunningScore$awayTeamPoints <- as.numeric(as.character(
  df_scoringProg_awayTeamRunningScore$awayTeamPoints))

df_scoringProg_runningScore <- inner_join(df_scoringProg_homeTeamRunningScore,
                                          df_scoringProg_awayTeamRunningScore)

# Remove interstitial objects
rm(df_scoringProg_homeTeamRunningScore, df_scoringProg_awayTeamRunningScore)

# Merge $gameDuration_s values in from df_scoringProg_timePeriods
df_scoringProg_runningScore <- inner_join(df_scoringProg_runningScore, df_scoringProg_timePeriods)

# Merge home and away team final scores in from 'temp' data frame
df_scoringProg_runningScore <- inner_join(df_scoringProg_runningScore, temp)

# Remove unneeded variables
df_scoringProg_runningScore <- df_scoringProg_runningScore[,-c(7:10)]

# Create new variable to express time of scoring instance
# as a percentage of total game duration
df_scoringProg_runningScore$scoreTime_pct <- round(
  (df_scoringProg_runningScore$scoreTime_seconds / df_scoringProg_runningScore$gameDuration_s * 100),
  1)

# Create new variables to express home and away team running scores
# as a percentage of home and away team final scores
df_scoringProg_runningScore$homeTeamRunningScore_pct <- round(
  (df_scoringProg_runningScore$homeTeamRunningScore /
     df_scoringProg_runningScore$homeTeamFinalScore * 100), 1)
df_scoringProg_runningScore$awayTeamRunningScore_pct <- round(
  (df_scoringProg_runningScore$awayTeamRunningScore /
     df_scoringProg_runningScore$awayTeamFinalScore * 100), 1)

##### Average pace of scoring per time instance ####################################################

##### JT continue from here #####
# Need to cast the data frame from long to wide first?

# Summarise data to begin calculating average pace of scoring at each given time instance
paceOfScoring <- ddply(df_scoringProg_runningScore, .(scoreTime_pct), summarise,
                       avg_homeTeamRunningScore_pct = mean(homeTeamRunningScore, na.rm = TRUE),
                       avg_awayTeamRunningScore_pct = mean(awayTeamRunningScore, na.rm = TRUE))

paceOfScoringPoints <- ddply(df_scoringProg_runningScore, .(scoreTime_pct), summarise,
                             avg_homeTeamPoints = mean(homeTeamPoints, na.rm = TRUE),
                             avg_awayTeamPoints = mean(awayTeamPoints, na.rm = TRUE))

# May not need the 'running score' / 'pace of scoring' stuff?