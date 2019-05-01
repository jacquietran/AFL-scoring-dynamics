##### Load libraries ###############################################################################

library(plyr)
library(dplyr)
library(lubridate)

##### Data wrangling ###############################################################################
##### Split data into relevant subsets for analysis ################################################

# Year-to-year
df_scoringProg_final2008 <- df_scoringProg_final[ which(
  df_scoringProg_final$season == 2008
), ]
df_scoringProg_final2009 <- df_scoringProg_final[ which(
  df_scoringProg_final$season == 2009
), ]
df_scoringProg_final2010 <- df_scoringProg_final[ which(
  df_scoringProg_final$season == 2010
), ]
df_scoringProg_final2011 <- df_scoringProg_final[ which(
  df_scoringProg_final$season == 2011
), ]
df_scoringProg_final2012 <- df_scoringProg_final[ which(
  df_scoringProg_final$season == 2012
), ]
df_scoringProg_final2013 <- df_scoringProg_final[ which(
  df_scoringProg_final$season == 2013
), ]
df_scoringProg_final2014 <- df_scoringProg_final[ which(
  df_scoringProg_final$season == 2014
), ]
df_scoringProg_final2015 <- df_scoringProg_final[ which(
  df_scoringProg_final$season == 2015
), ]
df_scoringProg_final2016 <- df_scoringProg_final[ which(
  df_scoringProg_final$season == 2016
), ]

# By team
df_scoringProg_CFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Carlton"
), ]
df_scoringProg_RFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Richmond"
), ]
df_scoringProg_GWS <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Greater Western Sydney"
), ]
df_scoringProg_MFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Melbourne"
), ]
df_scoringProg_EFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Essendon"
), ]
df_scoringProg_GCFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Gold Coast"
), ]
df_scoringProg_NMFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "North Melbourne"
), ]
df_scoringProg_AFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Adelaide"
), ]
df_scoringProg_SFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Sydney"
), ]
df_scoringProg_COFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Collingwood"
), ]
df_scoringProg_WBFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Western Bulldogs"
), ]
df_scoringProg_FFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Fremantle"
), ]
df_scoringProg_PAFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Port Adelaide"
), ]
df_scoringProg_SKFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "St Kilda"
), ]
df_scoringProg_BFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Brisbane"
), ]
df_scoringProg_WCFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "West Coast"
), ]
df_scoringProg_HFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Hawthorn"
), ]
df_scoringProg_GFC <- df_scoringProg_final[ which(
  df_scoringProg_final$teamName == "Geelong"
), ]

##### Scoring probability per time instance - year-to-year #########################################

##### Use df_scoringProg_final2008 #################################################################

# Merge everySecond data frame with df_scoringProg_final2008 to create
# temp_2008
temp_2008 <- full_join()
temp_2008 <- temp_2008[ order(
  temp_2008$matchID, temp_2008$scoreTime_seconds), ]

# In temp_2008 data frame, reduce down to key variables
# $matchID, $scoreType, $scoreTime_seconds
temp_2008 <- temp_2008[,c(19,10,27)]

# Merge df_scoringProg_timePeriods$gameDuration_s in with temp_2008 data frames
temp_2008 <- inner_join(temp_2008, df_scoringProg_timePeriods)

# Create new variable for expressing time of score instance
# as a percentage of total game duration
temp_2008$scoreTime_pct <- round((
  temp_2008$scoreTime_seconds / temp_2008$gameDuration_s * 100), 1)

# Create new variable to identify score instances in any given second
temp_2008$scoreInstance <- !is.na(temp_2008$scoreType)

# Create new variables to count number of scoring instances per second
# and non-scoring instances per second
temp_2008$scoreYes <- as.character(temp_2008$scoreInstance)
temp_2008$scoreYes <- revalue(temp_2008$scoreYes, c(
  "TRUE" = 1, "FALSE" = 0
))
temp_2008$scoreNo <- as.character(temp_2008$scoreInstance)
temp_2008$scoreNo <- revalue(temp_2008$scoreNo, c(
  "TRUE" = 0, "FALSE" = 1
))
temp_2008$scoreYes <- as.numeric(as.character(temp_2008$scoreYes))
temp_2008$scoreNo <- as.numeric(as.character(temp_2008$scoreNo))