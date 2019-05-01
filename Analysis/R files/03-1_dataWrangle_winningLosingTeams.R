##### Load libraries ###############################################################################

library(plyr)
library(dplyr)
library(lubridate)


##### Data wrangling ###############################################################################
##### Split data into relevant subsets for analysis ################################################

# Winning vs. losing teams
df_scoringProg_winningTeams <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == df_scoringProg_final$winningTeam
), ]
df_scoringProg_losingTeams <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == df_scoringProg_final$losingTeam
), ]

##### Create data frame with every second in any match played by winning teams #####################

# Use df_scoringProg_timePeriods as a base for the new data frame
# that includes an observation for every second in any given match
df_scoringProg_everySecond <- df_scoringProg_timePeriods[,c(1,6)]

# Use df_scoringProg_winningTeams to identify subset of associated matchID values
winningTeams_matchIDs <- data.frame(unique(unlist(df_scoringProg_winningTeams$matchID)))
colnames(winningTeams_matchIDs) <- "matchID"
# Then filter df_scoringProg_everySecond to this subset
df_scoringProg_everySecond <- inner_join(winningTeams_matchIDs, df_scoringProg_everySecond)

# Repeat the matchID values for each second in a match
matchIDvec <- data.frame(rep(
  df_scoringProg_everySecond$matchID, times = df_scoringProg_everySecond$gameDuration_s))
colnames(matchIDvec) <- "matchID"

# Create a new variable for each second in a match
everySecond <- matchIDvec %>%
  group_by(matchID) %>%
  mutate(scoreTime_seconds = seq(n()))

# Remove interstitial objects
rm(df_scoringProg_everySecond, matchIDvec)

##### Use df_scoringProg_winningTeams ##############################################################

# Merge everySecond data frame with df_scoringProg_winningTeams to create
# temp_winningTeam
temp_winningTeam <- full_join(df_scoringProg_winningTeams, everySecond)
temp_winningTeam <- temp_winningTeam[ order(
  temp_winningTeam$matchID, temp_winningTeam$scoreTime_seconds), ]

# In temp_winningTeam data frame, reduce down to key variables
# $matchID, $scoreType, $scoreTime_seconds
temp_winningTeam <- temp_winningTeam[,c(19,10,27)]

# Merge df_scoringProg_timePeriods$gameDuration_s in with temp_winningTeam data frames
temp_winningTeam <- inner_join(temp_winningTeam, df_scoringProg_timePeriods)

# Create new variable for expressing time of score instance
# as a percentage of total game duration
temp_winningTeam$scoreTime_pct <- round((
  temp_winningTeam$scoreTime_seconds / temp_winningTeam$gameDuration_s * 100), 1)

# Create new variable to identify score instances in any given second
temp_winningTeam$scoreInstance <- !is.na(temp_winningTeam$scoreType)

# Create new variables to count number of scoring instances per second
# and non-scoring instances per second
temp_winningTeam$scoreYes <- as.character(temp_winningTeam$scoreInstance)
temp_winningTeam$scoreYes <- revalue(temp_winningTeam$scoreYes, c(
  "TRUE" = 1, "FALSE" = 0
))
temp_winningTeam$scoreNo <- as.character(temp_winningTeam$scoreInstance)
temp_winningTeam$scoreNo <- revalue(temp_winningTeam$scoreNo, c(
  "TRUE" = 0, "FALSE" = 1
))
temp_winningTeam$scoreYes <- as.numeric(as.character(temp_winningTeam$scoreYes))
temp_winningTeam$scoreNo <- as.numeric(as.character(temp_winningTeam$scoreNo))

##### Use df_scoringProg_losingTeams ###############################################################

# Merge everySecond data frame with df_scoringProg_finals to create
# temp_losingTeam
temp_losingTeam <- full_join(df_scoringProg_awayTeamOnly, everySecond)
temp_losingTeam <- temp_losingTeam[ order(temp_losingTeam$matchID, temp_losingTeam$scoreTime_seconds), ]

# Remove interstitial objects
# rm(matchIDvec, everySecond, df_scoringProg_everySecond)

# In temp_losingTeam data frame, reduce down to key variables
# $matchID, $scoreType, $scoreTime_seconds
temp_losingTeam <- temp_losingTeam[,c(19,10,27)]

# Merge df_scoringProg_timePeriods$gameDuration_s in with temp_losingTeam data frames
temp_losingTeam <- inner_join(temp_losingTeam, df_scoringProg_timePeriods)

# Create new variable for expressing time of score instance
# as a percentage of total game duration
temp_losingTeam$scoreTime_pct <- round((
  temp_losingTeam$scoreTime_seconds / temp_losingTeam$gameDuration_s * 100), 1)

# Create new variable to identify score instances in any given second
temp_losingTeam$scoreInstance <- !is.na(temp_losingTeam$scoreType)

# Create new variables to count number of scoring instances per second
# and non-scoring instances per second
temp_losingTeam$scoreYes <- as.character(temp_losingTeam$scoreInstance)
temp_losingTeam$scoreYes <- revalue(temp_losingTeam$scoreYes, c(
  "TRUE" = 1, "FALSE" = 0
))
temp_losingTeam$scoreNo <- as.character(temp_losingTeam$scoreInstance)
temp_losingTeam$scoreNo <- revalue(temp_losingTeam$scoreNo, c(
  "TRUE" = 0, "FALSE" = 1
))
temp_losingTeam$scoreYes <- as.numeric(as.character(temp_losingTeam$scoreYes))
temp_losingTeam$scoreNo <- as.numeric(as.character(temp_losingTeam$scoreNo))

##### Scoring probability per time instance - winning vs. losing teams #############################

# Summarise data to begin calculating probability of scoring in any given time instance
scoreProbability_winningTeam <- ddply(temp_winningTeam, .(scoreTime_pct), summarise,
                                      winning_scoreYes = sum(scoreYes, na.rm = TRUE),
                                      winning_scoreNo = sum(scoreNo, na.rm = TRUE))

# Summarise data to begin calculating probability of scoring in any given time instance
scoreProbability_losingTeam <- ddply(temp_losingTeam, .(scoreTime_pct), summarise,
                                     losing_scoreYes = sum(scoreYes, na.rm = TRUE),
                                     losing_scoreNo = sum(scoreNo, na.rm = TRUE))

# Merge scoreProbability_winningTeam and scoreProbability_losingTeam
scoreProbability_winningLosing <- full_join(
  scoreProbability_winningTeam, scoreProbability_losingTeam)

# Calculate probability of scoring for each time instance, for winning and losing teams separately
scoreProbability_winningLosing$probOfScoring_winning <- round(
  (scoreProbability_winningLosing$winning_scoreYes / scoreProbability_winningLosing$winning_scoreNo), 4)
scoreProbability_winningLosing$probOfScoring_losing <- round(
  (scoreProbability_winningLosing$losing_scoreYes / scoreProbability_winningLosing$losing_scoreNo), 4)

# Remove interstitial objects
rm(temp_winningTeam, temp_losingTeam, scoreProbability_winningTeam, scoreProbability_losingTeam)