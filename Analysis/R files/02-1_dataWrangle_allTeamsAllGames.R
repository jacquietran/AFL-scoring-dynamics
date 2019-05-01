##### Load libraries ###############################################################################

library(plyr)
library(dplyr)
library(lubridate)

##### Data wrangling ###############################################################################
##### Split data into relevant subsets for analysis ################################################

# Use df_scoringProg_final as base data frame for 'all teams, all games' analyses

df_scoringProg_final <- read.csv("df_scoringProg_final.csv")
df_scoringProg_timePeriods <- read.csv("df_scoringProg_timePeriods.csv")

##### Data wrangling ###############################################################################
##### Scoring probability per time instance ########################################################

##### Create data frame with every second in any given match #######################################

# Use df_scoringProg_timePeriods as a base for the new data frame
# that includes an observation for every second in any given match
df_scoringProg_everySecond <- df_scoringProg_timePeriods[,c(1,6)]

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

##### Subset data from df_scoringProg_final - home teams ###########################################

# Subset from df_scoringProg_final
df_scoringProg_homeTeamOnly <- df_scoringProg_final[ which(
  df_scoringProg_final$homeTeam == df_scoringProg_final$teamName
), ]

# Merge everySecond data frame with df_scoringProg_finals to create
# temp_homeTeam
temp_homeTeam <- full_join(df_scoringProg_homeTeamOnly, everySecond)
temp_homeTeam <- temp_homeTeam[ order(temp_homeTeam$matchID, temp_homeTeam$scoreTime_seconds), ]

# In temp_homeTeam data frame, reduce down to key variables
# $matchID, $scoreType, $scoreTime_seconds
temp_homeTeam <- temp_homeTeam[,c(19,10,27)]

# Merge df_scoringProg_timePeriods$gameDuration_s in with temp_homeTeam data frames
temp_homeTeam <- inner_join(temp_homeTeam, df_scoringProg_timePeriods)

# Create new variable for expressing time of score instance
# as a percentage of total game duration
temp_homeTeam$scoreTime_pct <- round((
  temp_homeTeam$scoreTime_seconds / temp_homeTeam$gameDuration_s * 100), 1)

# Create new variable to identify score instances in any given second
temp_homeTeam$scoreInstance <- !is.na(temp_homeTeam$scoreType)

# Create new variables to count number of scoring instances per second
# and non-scoring instances per second
temp_homeTeam$scoreYes <- as.character(temp_homeTeam$scoreInstance)
temp_homeTeam$scoreYes <- revalue(temp_homeTeam$scoreYes, c(
  "TRUE" = 1, "FALSE" = 0
))
temp_homeTeam$scoreNo <- as.character(temp_homeTeam$scoreInstance)
temp_homeTeam$scoreNo <- revalue(temp_homeTeam$scoreNo, c(
  "TRUE" = 0, "FALSE" = 1
))
temp_homeTeam$scoreYes <- as.numeric(as.character(temp_homeTeam$scoreYes))
temp_homeTeam$scoreNo <- as.numeric(as.character(temp_homeTeam$scoreNo))

##### Subset data from df_scoringProg_final - away teams ###########################################

##### Subset from df_scoringProg_final
df_scoringProg_awayTeamOnly <- df_scoringProg_final[ which(
  df_scoringProg_final$awayTeam == df_scoringProg_final$teamName
), ]

# Merge everySecond data frame with df_scoringProg_finals to create
# temp_awayTeam
temp_awayTeam <- full_join(df_scoringProg_awayTeamOnly, everySecond)
temp_awayTeam <- temp_awayTeam[ order(temp_awayTeam$matchID, temp_awayTeam$scoreTime_seconds), ]

# Remove interstitial objects
# rm(matchIDvec, everySecond, df_scoringProg_everySecond)

# In temp_awayTeam data frame, reduce down to key variables
# $matchID, $scoreType, $scoreTime_seconds
temp_awayTeam <- temp_awayTeam[,c(19,10,27)]

# Merge df_scoringProg_timePeriods$gameDuration_s in with temp_awayTeam data frames
temp_awayTeam <- inner_join(temp_awayTeam, df_scoringProg_timePeriods)

# Create new variable for expressing time of score instance
# as a percentage of total game duration
temp_awayTeam$scoreTime_pct <- round((
  temp_awayTeam$scoreTime_seconds / temp_awayTeam$gameDuration_s * 100), 1)

# Create new variable to identify score instances in any given second
temp_awayTeam$scoreInstance <- !is.na(temp_awayTeam$scoreType)

# Create new variables to count number of scoring instances per second
# and non-scoring instances per second
temp_awayTeam$scoreYes <- as.character(temp_awayTeam$scoreInstance)
temp_awayTeam$scoreYes <- revalue(temp_awayTeam$scoreYes, c(
  "TRUE" = 1, "FALSE" = 0
))
temp_awayTeam$scoreNo <- as.character(temp_awayTeam$scoreInstance)
temp_awayTeam$scoreNo <- revalue(temp_awayTeam$scoreNo, c(
  "TRUE" = 0, "FALSE" = 1
))
temp_awayTeam$scoreYes <- as.numeric(as.character(temp_awayTeam$scoreYes))
temp_awayTeam$scoreNo <- as.numeric(as.character(temp_awayTeam$scoreNo))

##### Scoring probability per time instance - all teams, all games #################################

# Summarise data to begin calculating probability of scoring in any given time instance
scoreProbability_homeTeam <- ddply(temp_homeTeam, .(scoreTime_pct), summarise,
                                   home_scoreYes = sum(scoreYes, na.rm = TRUE),
                                   home_scoreNo = sum(scoreNo, na.rm = TRUE))

# Summarise data to begin calculating probability of scoring in any given time instance
scoreProbability_awayTeam <- ddply(temp_awayTeam, .(scoreTime_pct), summarise,
                                   away_scoreYes = sum(scoreYes, na.rm = TRUE),
                                   away_scoreNo = sum(scoreNo, na.rm = TRUE))

# Merge scoreProbability_homeTeam and scoreProbability_awayTeam
scoreProbability_allTeamsAllGames <- full_join(scoreProbability_homeTeam, scoreProbability_awayTeam)

# Calculate probability of scoring for each time instance
scoreProbability_allTeamsAllGames$both_scoreYes <- scoreProbability_allTeamsAllGames$home_scoreYes +
  scoreProbability_allTeamsAllGames$away_scoreYes
scoreProbability_allTeamsAllGames$both_scoreNo <- scoreProbability_allTeamsAllGames$home_scoreNo +
  scoreProbability_allTeamsAllGames$away_scoreNo

scoreProbability_allTeamsAllGames$probOfScoring_home <- round(
  (scoreProbability_allTeamsAllGames$home_scoreYes / scoreProbability_allTeamsAllGames$home_scoreNo), 4)
scoreProbability_allTeamsAllGames$probOfScoring_away <- round(
  (scoreProbability_allTeamsAllGames$away_scoreYes / scoreProbability_allTeamsAllGames$away_scoreNo), 4)
scoreProbability_allTeamsAllGames$probOfScoring_both <- round(
  (scoreProbability_allTeamsAllGames$both_scoreYes / scoreProbability_allTeamsAllGames$both_scoreNo), 4)

# Remove interstitial objects
rm(temp_homeTeam, temp_awayTeam, scoreProbability_homeTeam, scoreProbability_awayTeam, everySecond)

##### Summary ######################################################################################

# Created the following data frames:
# scoreProbability_allTeamsAllGames