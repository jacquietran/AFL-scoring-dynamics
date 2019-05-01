library(readr)
library(plyr)

df <- read_csv("df_2016_temp.csv",
               col_names = TRUE,
               col_types = NULL,
               na = "")

quarterScores <- ddply(df, .(matchID, season, round, homeTeam, awayTeam, quarter), summarise,
                       homeTeamRunningScore = max(homeTeamRunningScore, na.rm = TRUE),
                       awayTeamRunningScore = max(awayTeamRunningScore, na.rm = TRUE),
                       homeTeamPointsInQtr = sum(homeTeamPoints, na.rm = TRUE),
                       awayTeamPointsInQtr = sum(awayTeamPoints, na.rm = TRUE))

quarterScores$homeMinusAwayPointsInQtr <- 
  quarterScores$homeTeamPointsInQtr - quarterScores$awayTeamPointsInQtr
quarterScores$qtrWinningTeam <- ifelse(
  quarterScores$homeMinusAwayPointsInQtr > 0, quarterScores$homeTeam,
  ifelse(
    quarterScores$homeMinusAwayPointsInQtr < 0, quarterScores$awayTeam,
    "Draw"
  )
)

write.csv(quarterScores, "AFL 2016 quarter scores.csv", row.names = FALSE)

matchDurations <- df
matchDurations$dup <- duplicated(matchDurations$matchID)
matchDurations <- matchDurations[ which(matchDurations$dup == FALSE), -35]
matchDurations <- matchDurations[,c(19,1,3:5,24)]

write.csv(matchDurations, "AFL 2016 match durations.csv", row.names = FALSE)