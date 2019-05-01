## Year for each team + top and bottomw teams

library(ggplot2)
library(dplyr)
library(plyr)

## Uni computer load
setwd("C:/Users/ryletter/Dropbox/2017-03 Pace of scoring/Analysis/R files")

df_scoringProg_final <- read.csv("df_scoringProg_final.csv")


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


df_scoreDiff <- df_scoringDiff_final[c(1,2,3, 28, 33, 34, 35)]

remove(df_HomeLoss, df_HomeWin, df_scoringProg_final, df_scoringDiff_final)

## Add losing team scoring in

lossScoreDiff <- df_scoreDiff$winningScoreDiff * -1


df_scoreDiff$lossScoreDiff <- lossScoreDiff

# Factor levels for time periods scored in. 

df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 1 & df_scoreDiff$scoreTime_pct > 0] <- 1
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 2 & df_scoreDiff$scoreTime_pct > 1] <- 2
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 3 & df_scoreDiff$scoreTime_pct > 2] <- 3
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 4 & df_scoreDiff$scoreTime_pct > 3] <- 4
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 5 & df_scoreDiff$scoreTime_pct > 4] <- 5
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 6 & df_scoreDiff$scoreTime_pct > 5] <- 6
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 7 & df_scoreDiff$scoreTime_pct > 6] <- 7
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 8 & df_scoreDiff$scoreTime_pct > 7] <- 8
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 9 & df_scoreDiff$scoreTime_pct > 8] <- 9
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 10 & df_scoreDiff$scoreTime_pct > 9] <- 10
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 11 & df_scoreDiff$scoreTime_pct > 10] <- 11
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 12 & df_scoreDiff$scoreTime_pct > 11] <- 12
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 13 & df_scoreDiff$scoreTime_pct > 12] <- 13
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 14 & df_scoreDiff$scoreTime_pct > 13] <- 14
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 15 & df_scoreDiff$scoreTime_pct > 14] <- 15
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 16 & df_scoreDiff$scoreTime_pct > 15] <- 16
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 17 & df_scoreDiff$scoreTime_pct > 16] <- 17
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 18 & df_scoreDiff$scoreTime_pct > 17] <- 18
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 19 & df_scoreDiff$scoreTime_pct > 18] <- 19
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 20 & df_scoreDiff$scoreTime_pct > 19] <- 20
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 21 & df_scoreDiff$scoreTime_pct > 20] <- 21
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 22 & df_scoreDiff$scoreTime_pct > 21] <- 22
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 23 & df_scoreDiff$scoreTime_pct > 22] <- 23
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 24 & df_scoreDiff$scoreTime_pct > 23] <- 24
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 25 & df_scoreDiff$scoreTime_pct > 24] <- 25
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 26 & df_scoreDiff$scoreTime_pct > 25] <- 26
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 27 & df_scoreDiff$scoreTime_pct > 26] <- 27
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 28 & df_scoreDiff$scoreTime_pct > 27] <- 28
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 29 & df_scoreDiff$scoreTime_pct > 28] <- 29
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 30 & df_scoreDiff$scoreTime_pct > 29] <- 30
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 31 & df_scoreDiff$scoreTime_pct > 30] <- 31
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 32 & df_scoreDiff$scoreTime_pct > 31] <- 32
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 33 & df_scoreDiff$scoreTime_pct > 32] <- 33
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 34 & df_scoreDiff$scoreTime_pct > 33] <- 34
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 35 & df_scoreDiff$scoreTime_pct > 34] <- 35
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 36 & df_scoreDiff$scoreTime_pct > 35] <- 36
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 37 & df_scoreDiff$scoreTime_pct > 36] <- 37
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 38 & df_scoreDiff$scoreTime_pct > 37] <- 38
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 39 & df_scoreDiff$scoreTime_pct > 38] <- 39
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 40 & df_scoreDiff$scoreTime_pct > 39] <- 40
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 41 & df_scoreDiff$scoreTime_pct > 40] <- 41
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 42 & df_scoreDiff$scoreTime_pct > 41] <- 42
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 43 & df_scoreDiff$scoreTime_pct > 42] <- 43
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 44 & df_scoreDiff$scoreTime_pct > 43] <- 44
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 45 & df_scoreDiff$scoreTime_pct > 44] <- 45
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 46 & df_scoreDiff$scoreTime_pct > 45] <- 46
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 47 & df_scoreDiff$scoreTime_pct > 46] <- 47
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 48 & df_scoreDiff$scoreTime_pct > 47] <- 48
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 49 & df_scoreDiff$scoreTime_pct > 48] <- 49
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 50 & df_scoreDiff$scoreTime_pct > 49] <- 50
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 51 & df_scoreDiff$scoreTime_pct > 50] <- 51
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 52 & df_scoreDiff$scoreTime_pct > 51] <- 52
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 53 & df_scoreDiff$scoreTime_pct > 52] <- 53
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 54 & df_scoreDiff$scoreTime_pct > 53] <- 54
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 55 & df_scoreDiff$scoreTime_pct > 54] <- 55
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 56 & df_scoreDiff$scoreTime_pct > 55] <- 56
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 57 & df_scoreDiff$scoreTime_pct > 56] <- 57
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 58 & df_scoreDiff$scoreTime_pct > 57] <- 58
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 59 & df_scoreDiff$scoreTime_pct > 58] <- 59
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 60 & df_scoreDiff$scoreTime_pct > 59] <- 60
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 61 & df_scoreDiff$scoreTime_pct > 60] <- 61
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 62 & df_scoreDiff$scoreTime_pct > 61] <- 62
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 63 & df_scoreDiff$scoreTime_pct > 62] <- 63
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 64 & df_scoreDiff$scoreTime_pct > 63] <- 64
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 65 & df_scoreDiff$scoreTime_pct > 64] <- 65
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 66 & df_scoreDiff$scoreTime_pct > 65] <- 66
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 67 & df_scoreDiff$scoreTime_pct > 66] <- 67
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 68 & df_scoreDiff$scoreTime_pct > 67] <- 68
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 69 & df_scoreDiff$scoreTime_pct > 68] <- 69
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 70 & df_scoreDiff$scoreTime_pct > 69] <- 70
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 71 & df_scoreDiff$scoreTime_pct > 70] <- 71
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 72 & df_scoreDiff$scoreTime_pct > 71] <- 72
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 73 & df_scoreDiff$scoreTime_pct > 72] <- 73
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 74 & df_scoreDiff$scoreTime_pct > 73] <- 74
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 75 & df_scoreDiff$scoreTime_pct > 74] <- 75
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 76 & df_scoreDiff$scoreTime_pct > 75] <- 76
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 77 & df_scoreDiff$scoreTime_pct > 76] <- 77
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 78 & df_scoreDiff$scoreTime_pct > 77] <- 78
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 79 & df_scoreDiff$scoreTime_pct > 78] <- 79
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 80 & df_scoreDiff$scoreTime_pct > 79] <- 80
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 81 & df_scoreDiff$scoreTime_pct > 80] <- 81
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 82 & df_scoreDiff$scoreTime_pct > 81] <- 82
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 83 & df_scoreDiff$scoreTime_pct > 82] <- 83
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 84 & df_scoreDiff$scoreTime_pct > 83] <- 84
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 85 & df_scoreDiff$scoreTime_pct > 84] <- 85
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 86 & df_scoreDiff$scoreTime_pct > 85] <- 86
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 87 & df_scoreDiff$scoreTime_pct > 86] <- 87
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 88 & df_scoreDiff$scoreTime_pct > 87] <- 88
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 89 & df_scoreDiff$scoreTime_pct > 88] <- 89
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 90 & df_scoreDiff$scoreTime_pct > 89] <- 90
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 91 & df_scoreDiff$scoreTime_pct > 90] <- 91
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 92 & df_scoreDiff$scoreTime_pct > 91] <- 92
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 93 & df_scoreDiff$scoreTime_pct > 92] <- 93
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 94 & df_scoreDiff$scoreTime_pct > 93] <- 94
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 95 & df_scoreDiff$scoreTime_pct > 94] <- 95
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 96 & df_scoreDiff$scoreTime_pct > 95] <- 96
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 97 & df_scoreDiff$scoreTime_pct > 96] <- 97
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 98 & df_scoreDiff$scoreTime_pct > 97] <- 98
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 99 & df_scoreDiff$scoreTime_pct > 98] <- 99
df_scoreDiff$scoreTime_per[df_scoreDiff$scoreTime_pct <= 100 & df_scoreDiff$scoreTime_pct > 99] <- 100

df_scoreDiff$scoreTime_per <- as.factor(df_scoreDiff$scoreTime_per)

df_winningTeam <- df_scoreDiff[c(1,2,3,5,7,9,4)]
df_winningTeam$result <- c(rep("win", by = length(df_winningTeam$season)))
df_losingTeam <- df_scoreDiff[c(1,2,3,6,8,9,4)]
df_losingTeam$result <- c(rep("loss", by = length(df_losingTeam$season)))

## rename columns

colnames(df_winningTeam) <- c("season", "date", "round", "team", "scoreDiff", "scoreTime_per", "scoreTime_pct","result")
colnames(df_losingTeam) <- c("season", "date", "round", "team", "scoreDiff", "scoreTime_per", "scoreTime_pct", "result")

df_scoreDiffFinal <- rbind(df_winningTeam, df_losingTeam)

df_scoreDiffFinal$scoreTime_per <- as.numeric(df_scoreDiffFinal$scoreTime_per)

## Further Cleaning
df_scoreDiffFinal$season <- as.factor(df_scoreDiffFinal$season)       

df_scoreDiffFinal$result <- as.factor(df_scoreDiffFinal$result) 

df_scoreDiffFinal$scoreTime_per <- as.factor(df_scoreDiffFinal$scoreTime_per) 

df_scoreDiffFinal$scoreTime_perTemp <- c(tail(df_scoreDiffFinal$scoreTime_per,-1),NA)

df_scoreDiffFinal$scoreTime_perFinal <- ifelse(df_scoreDiffFinal$scoreTime_perTemp == df_scoreDiffFinal$scoreTime_per | (is.na(df_scoreDiffFinal$scoreTime_per) & is.na(df_scoreDiffFinal$scoreTime_perTemp)),NA,df_scoreDiffFinal$scoreTime_per )

df_scoreDiffFinal <- na.omit(df_scoreDiffFinal)

df_plotScoreDiff <- ddply(df_scoreDiffFinal, .(season, team, round, scoreTime_perFinal), summarise,
              scoreDiff = round(scoreDiff)
)

df_plotScoreDiff$scoreTime_per <- as.numeric(df_plotScoreDiff$scoreTime_per)

# Plot
p <- ggplot(df_plotScoreDiff, aes(x = scoreTime_perFinal, y = scoreDiff))
p <- p + geom_line(lwd = 0.2, aes(linetype = round), col = "#666666")
p <- p + geom_smooth(method = "loess", span = 0.1, lwd = 0.9, se = FALSE, aes(y = scoreDiff), col = "blue2")
p <- p + scale_x_continuous(breaks = c(0,25,50,75,100),
                            labels = c("0", "25", "50", "75", "100"),
                            minor_breaks = seq(0,100,5))
p <- p + scale_y_continuous(breaks = c(-150, -100, -50, 0, 50, 100, 150),
                            labels = c(-150, -100, -50, 0, 50, 100, 150),
                            minor_breaks = seq(-150,150,25))
p <- p + theme_bw() + theme(legend.title = element_blank(), legend.position = "blank", panel.grid = element_blank(), axis.text.x = element_text(size = "12")) + xlab("% Game Time") + ylab("Winning Score Difference")


p <- p + facet_grid(team~season)

ggsave("plotScoreDiff1.png", width = 500, height = 500, units = "mm", dpi = 600)











































###############################################

# This code chunk creates the plot that is included in the SMA 2017 poster
labels <- c(
	Adelaide = "Adelaide", Brisbane = "Brisbane", Carlton = "Carlton", Collingwood = "Collingwood",
	Essendon = "Essendon", Fremantle = "Fremantle", Geelong = "Geelong", `Gold Coast` = "Gold Coast",
	`Greater Western Sydney` = "GWS", Hawthorn = "Hawthorn", Melbourne = "Melbourne",
	`North Melbourne` = "North Melb.", `Port Adelaide` = "Port Adel.", Richmond = "Richmond",
	`St Kilda` = "St Kilda", Sydney = "Sydney", `West Coast` = "West Coast",
	`Western Bulldogs` = "W. Bulldogs")
p <- ggplot(df_scoreDiffFinal, aes(x = scoreTime_pct, y = scoreDiff)) 
p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "red")
p <- p + geom_smooth(method = "loess", span = 0.1, se = FALSE, size = 0.8, aes(y = scoreDiff)) 
p <- p + scale_x_continuous(breaks = c(0,25,50,75,100),
                            labels = c("0", "", "50", "", "100"),
                            minor_breaks = seq(0,100,5))
p <- p + xlab("% Game time")
p <- p + ylab("Score margin")
p <- p + facet_grid(season~team, labeller = labeller(team = labels))
p <- p + theme_bw()
p <- p + theme(legend.title = element_blank(),
							 panel.grid.minor = element_blank(),
							 panel.grid.major.y = element_blank(),
							 axis.title = element_text(size = 14),
							 axis.text = element_text(size = 10),
							 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
							 strip.text.x = element_text(size = 8))

ggsave("scoreWormsByTeamByYear.png", width = 40, height = 20, units = "cm", dpi = 600)



p <- ggplot(subset(df_scoreDiffFinal, (Team == "Geelong")), aes(x = scoreTime_pct, y = scoreDiff, color = round))

p <- p + geom_line(aes(y = scoreDiff, linetype = round))

p <- p + geom_smooth(method = "loess", span = 0.1, se = FALSE, size = 1, aes(y = scoreDiff, color = c("black"))) 

p <- p + scale_x_continuous(breaks = c(0,25,50,75,100),
                            labels = c("0%", "25%", "50%", "75%", "100%"),
                            minor_breaks = seq(0,100,5))
p <- p + scale_y_continuous(breaks = c(0, 10,20,30,40,50,60,70,80,100),
                          labels = c(0, 10,20,30,40,50,60,70,80,100),
                          minor_breaks = seq(0,100,5))


p <- p + theme_bw() + theme(legend.title = element_blank()) + xlab("% Game Time") + ylab("Winning Score Difference")

p <- p + facet_grid(~season)

ggsave("Scoring_Worm.png", width = 24, height = 16, units = "cm", dpi = 600)
