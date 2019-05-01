## year by scoring progressions

library(ggplot2)

## Home computer load
setwd("C:/Users/ryletter/Dropbox/2017-03 Pace of scoring/Analysis/R files")

## Uni computer load
setwd("C:/Users/ryletter/Dropbox/2017-03 Pace of scoring/Analysis/R files")



df_scoringProg_final <- read.csv("df_scoringProg_final.csv")




## Score Difference worm

df_scoringProg_final$homeScoreDiff <- df_scoringProg_final$homeTeamRunningScore - df_scoringProg_final$awayTeamRunningScore

df_scoringProg_final$awayScoreDiff <- df_scoringProg_final$awayTeamRunningScore - df_scoringProg_final$homeTeamRunningScore


## Score Diff for win and loss


# add draw factor level into teamName
levels(df_scoringProg_final$teamName) <- c(levels(df_scoringProg_final$teamName), "Draw")

# Make column of score diff for winning teams

df_HomeWin <- df_scoringProg_final[ which(
  df_scoringProg_final$homeTeamResult == "Win"
), ]

df_HomeLoss <- df_scoringProg_final[ which(
  df_scoringProg_final$homeTeamResult == "Loss"
), ]

# Winning team score diff

df_HomeWin$winningScoreDiff <- df_HomeWin$homeTeamRunningScore - df_HomeWin$awayTeamRunningScore

df_HomeLoss$winningScoreDiff <- df_HomeLoss$awayTeamRunningScore - df_HomeLoss$homeTeamRunningScore

df_scoringDiff_final <- rbind(df_HomeWin, df_HomeLoss)

## All scoring porgression plot


p <- ggplot(df_scoringDiff_final, aes(x = scoreTime_pct, y = winningScoreDiff))

p <- p + geom_smooth(method = "loess", span = 0.1, se = FALSE, size = 1, aes(y = winningScoreDiff))

p <- p + scale_x_continuous(breaks = c(0,25,50,75,100),
                            labels = c("0%", "25%", "50%", "75%", "100%"),
                            minor_breaks = seq(0,100,5))
p <- p + theme_bw() + theme(legend.title = element_blank()) + xlab("% Game Time") + ylab("Winning Score Difference")

p <- p + facet_wrap(~season)

ggsave("scoringDiffProg.png", plot = p, width = 24, height = 16, units = "cm", dpi = 300)
