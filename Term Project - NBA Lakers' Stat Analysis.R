# MATH E-23C
# Term Project Initial EDA

# Submitted by: Maria Cristina An, Daniel Lebedinsky
# Date: March 23, 2022

# 1. Reference: https://www.basketball-reference.com/teams/LAL/2020/gamelog/
Lakers <- read.csv("Lakers 2019-20 Game log.csv"); head(Lakers)

# Glossary
# Rk -- Rank
# G -- Season Game
# W_E -- opponent is from East or West
# Opp -- Opponent
# W_L -- Lakers team won or lost
# Tm -- Lakers team Points
# Opp_pts -- Opponent Points

# Lakers 
# FG_LA -- Field Goals
# FGA_LA -- Field Goal Attempts
# FGpc_LA -- Field Goal Percentage
# X3P_LA -- 3-Point Field Goals
# X3PA_LA -- 3-Point Field Goal Attempts
# X3Ppc_LA -- 3-Point Field Goal Percentage
# FT_LA -- Free Throws
# FTA_LA -- Free Throw Attempts
# FTpc_LA -- Free Throw Percentage
# ORB_LA -- Offensive Rebounds
# TRB_LA -- Total Rebounds
# AST_LA -- Assists
# STL_LA -- Steals
# BLK_LA -- Blocks
# TOV_LA -- Turnovers
# PF_LA -- Personal Fouls

# Opponent
# FG -- Opponent Field Goals
# FGA -- Opponent Field Goal Attempts
# FGpc -- Opponent Field Goal Percentage
# X3P -- Opponent 3-Point Field Goals
# X3PA -- Opponent 3-Point Field Goal Attempts
# X3Ppc -- Opponent 3-Point Field Goal Percentage
# FT -- Opponent Free Throws
# FTA -- Opponent Free Throw Attempts
# FTpc -- Opponent Free Throw Percentage
# ORB -- Opponent Offensive Rebounds
# TRB -- Opponent Total Rebounds
# AST -- Opponent Assists
# STL -- Opponent Steals
# BLK -- Opponent Blocks
# TOV -- Opponent Turnovers
# PF -- Opponent Personal Fouls


# Convert to proper date format
Dates <- as.Date(Lakers$Date)

# 2. and 3. Summary
summary(Lakers)

# 4. Number of columns and rows
length(Lakers)
nrow(Lakers)

# Creating categorical columns
WonLost <- Lakers$W_L == "W"; head(WonLost) 
sum(WonLost) # total wins

# Graphical Displays

# 1. Barplot of wins vs losses for our team
barplot(table(WonLost), main = "Bar Plot of Lakers' 2019 Wins vs Losses", col = "orange", ylab="Count")
# This plot shows the number of losses compared to the number wins by our team. 
# The y-axis gives the count of wins and losses. The x-label answers the question
# "did the team win?", FALSE means losing and TRUE means winning.

# Barplot of 3 point shots from our team
barplot(table(Lakers$X3P_LA), main = "Bar Plot of Lakers' Number of 3-Pointers per Game", xlab="Number of 3-pointers", ylab="Frequency", col = "yellow")
# This plot shows the number of times the team scores a 3-point shot in a game.
# How often a 3-pointer occurs ranges from the minimum of 2 to a maximum of 19
# in a single game.

# Barplot of Opponent blocks 
barplot(table(Lakers$BLK), main = "Bar Plot of Lakers' Number of Opponent Blocks per Game", xlab="Number of Blocks", ylab="Frequency", col = "green")
# This plot shows how often an opponent successfully blocks our team in a game.
# Most often (the median), an opponent is able to block 3 times and, of course, 
# depending on the opponent's abilities, blocks can range from 0 to 9 in a game.

# 2.  Histogram of 3 point shots
hist(Lakers$X3P_LA, xlab = "3 point shots", main = "Histogram of Lakers' 3 Pointers", col = "cyan", breaks=(0:25))
# This is a histogram version of the barplot which shows the number of times the team scores a 3-point shot in a game.
# The data is the same, and so is the result, a 3-pointer event ranges from the minimum of 2 to a maximum of 19.

# Histogram of Opponent blocks
hist(Lakers$BLK, main = "Histogram of Lakers' Opponent Blocks", xlab="Number of Blocks", col = "magenta", breaks=(0:12)) 
# This is a histogram version that shows how often an opponent successfully blocks our team in a game.
# Same results, an opponent is able to block 3 times and ranges from 0 to 9 in a game.

# Histogram of our team's number of Free Throws
hist(Lakers$FT_LA, probability=TRUE, main="Histogram of Lakers' Number of Free Throws per Game", xlab="Number of Free Throws", col = "pink", breaks=(0:35)) # free throws from the team per game
# This histogram shows the probability of the number of free throws done by our team in a game.
# The range is from a minimum of 7 to a maximum of 33 with a maximum density at 14 and 17.

# 3. Probability density graph overlay on the our team's total number of rebounds in a game
hist(Lakers$TRB_LA, probability=TRUE, main = "Histogram of Lakers' Total Number of Rebounds per Game", ylim=c(0,0.08), xlab="Number of Rebounds", col = "pink", breaks=(20:65)) # total rebounds per game  
curve(dnorm(x, 45, 5), col="purple", add=TRUE, lwd=2) # normal distribution
# Here we have our team's probability of the total number of rebounds in a game.
# Ranging from a minimum of 25 to a maximum of 62, the graph seems like it came from a 
# normal distribution. Overlaying a normal distribution shows like it does.

# Probability density graph on the our team's scores in a game
hist(Lakers$Tm,breaks=seq(from=80, to = 150, by =5), probability=TRUE, main = "Histogram of Lakers' Scores per Game", xlab="Scores", col = "light blue") # team's score frequencies
curve(dnorm(x, 115, 10), col="dark blue", add=TRUE, lwd=2)
# We go back to our histogram of our team's score and its probability in a game.
# It also seems as if it fits a normal distribution like it shows with a normal distribution overlay.
# We can provide more proof later with a chi-square goodness of fit test.

# 4. Contingency table of how many times opponents scored > 100 in a game
Opp100 <- Lakers$Opp_pts > 100; Opp100
table(Opp100)
# This table answers the question "how many times the opponents scored greater than 100 points in a game with our team".
# This does not say that the opponent won or not. But in the times that they did not, it only shows that our team scored more and that
# such a high scoring game would have been a truly awesome event to watch.

# Contingency table where our team finished with a score < 100 
Less100 <- Lakers$Tm < 100; Less100
table(Less100)
# This table shows the opposite of the table above, that despite being the all-around champion during that year, our team scored
# less than 100 in some games and this table shows that.

# 1. Permutation test
# Our permutation test will be comparing our team's scores in games played 
# vs the opponents from the East and the opponents from the West

# H_o: The mean score of the opponents coming from the Eastern Conference 
# is equal to the mean score of the opponents coming from the Western Conference
# H_a: The mean score of the opponents coming from the Eastern Conference 
# is not equal to the mean score of the opponents coming from the Western Conference

# Let's get our data for the teams from the Eastern Conference
# how many opponents were from there and their mean score 
Eastern <- Lakers$W_E == "E"; head(Eastern) 
East <- sum(Eastern); East # count of Eastern opponents = 31
ScorevsEast <- mean(Lakers$Tm*Eastern); ScorevsEast # mean team score vs East = 37.03261

# Let's get our data for the teams from the Western Conference
# how many opponents were from there and their mean score 
Western <- Lakers$W_E == "W"; head(Western) 
West <- sum(Western); West # Western opponents = 61
ScorevsWest <- mean(Lakers$Tm*Western); ScorevsWest # mean team score vs West = 76.26087

# It shows that there are more opponents coming from the West and that their mean score
# obviously would have a higher range.

# Let's get the total number of games played as well as the difference of their mean score 
# which will be our Observed value.
EW <- sum(Eastern) + sum(Western); EW # total number of games = 92
Score_diff <- ScorevsEast - ScorevsWest; Score_diff # -39.22826
Observed <- Score_diff; Observed

# Let's see if this score difference is significant
# We repeat 10^6 times
N <- 10^6
Score_diffs <- numeric(N)
for (i in 1:N){
  # Permute West indices
  E <- sample(EW, East, replace = FALSE)
  
  # Get the difference of the 2 opponent groups
  Score_diffs[i] <- mean(Lakers$Tm[E]) - mean(Lakers$Tm[-E])
}

head(Score_diffs)
summary(Score_diffs)
mean(Score_diffs) # 0.001794507 close to zero
hist(Score_diffs, main="Mean Score difference between games against Eastern vs games against Western Opponents",
     col="light gray", xlab="Mean difference", xlim=c(-55, 55))

#Now display the observed value on the histogram
abline(v = Observed, col = "purple")

#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- (sum(Score_diffs >= Observed)+1)/(N+1); pvalue # 1
# This goes to shows that the data observed has a very big chance 
# to have come about by chance. Therefore, there is insufficient evidence to
# to reject the null hypothesis.

# The End


