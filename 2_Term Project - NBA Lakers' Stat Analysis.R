# MATH E-23C
# Term Project 

# Submitted by: Maria Cristina An, Daniel Lebedinsky, Julio Solis Arce
# Date: May 6, 2022

# 1. Reference: https://www.basketball-reference.com/teams/LAL/2020/gamelog/
Lakers <- read.csv("Lakers 2019-20 Game log.csv"); head(Lakers)

# Libraries
# need to be run at most once
# install.packages("PerformanceAnalytics") 
library("PerformanceAnalytics") # for correlations
#install.packages("stats4")   
library(stats4) # for logistic regression


# Glossary
# 1 Rk -- Rank
# 2 G -- Season Game
# 3 Date
# 4 W_E -- opponent is from East or West
# 5 Opp -- Opponent
# 6 W_L -- Lakers team won or lost
# 7 Tm -- Lakers team Points
# 8 Opp_pts -- Opponent Points

# Lakers 
# 9 FG_LA -- Field Goals
# 10 FGA_LA -- Field Goal Attempts
# 11 FGpc_LA -- Field Goal Percentage
# 12 X3P_LA -- 3-Point Field Goals
# 13 X3PA_LA -- 3-Point Field Goal Attempts
# 14 X3Ppc_LA -- 3-Point Field Goal Percentage
# 15 FT_LA -- Free Throws
# 16 FTA_LA -- Free Throw Attempts
# 17 FTpc_LA -- Free Throw Percentage
# 18 ORB_LA -- Offensive Rebounds
# 19 TRB_LA -- Total Rebounds
# 20 AST_LA -- Assists
# 21 STL_LA -- Steals
# 22 BLK_LA -- Blocks
# 23 TOV_LA -- Turnovers
# 24 PF_LA -- Personal Fouls

# Opponent
# 25 FG -- Opponent Field Goals
# 26 FGA -- Opponent Field Goal Attempts
# 27 FGpc -- Opponent Field Goal Percentage
# 28 X3P -- Opponent 3-Point Field Goals
# 29 X3PA -- Opponent 3-Point Field Goal Attempts
# 30 X3Ppc -- Opponent 3-Point Field Goal Percentage
# 31 FT -- Opponent Free Throws
# 32 FTA -- Opponent Free Throw Attempts
# 33 FTpc -- Opponent Free Throw Percentage
# 34 ORB -- Opponent Offensive Rebounds
# 35 TRB -- Opponent Total Rebounds
# 36 AST -- Opponent Assists
# 37 STL -- Opponent Steals
# 38 BLK -- Opponent Blocks
# 39 TOV -- Opponent Turnovers
# 40 PF -- Opponent Personal Fouls


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
pvalue <- (sum(Score_diffs <= Observed)+1)/(N+1); pvalue # 1
# This goes to shows that the data observed has a very small chance 
# to have come about by chance. Therefore, there is insufficient evidence to
# to reject the null hypothesis.


# Correlations
# We will use the Performance analytics package to show correlations.
# p-values (0.001, 0.01, 0.05, 0.1, 1) <=> symbols("***", "**", "*", ".", "")

# The correlation of Lakers' score against the the opponents score in general
chart.Correlation(Lakers[,7:8], histogram=TRUE, pch=19)

# The correlation of Lakers' score against the scores of the opponents from the West
West <- Lakers[Western, ]; West
chart.Correlation(West[,7:8], histogram=TRUE, pch=19)

# The correlation of Lakers' score against the scores of the opponents from the East
East <- Lakers[Eastern, ]; East
chart.Correlation(East[,7:8], histogram=TRUE, pch=19)

# The correlation of Lakers' score with their Field Goal, 3-pointers, Free throws, and Fouls
chart.Correlation(Lakers[,c(7,9,12,15,24)], histogram=TRUE, pch=19)

# The correlation of the Opponents' score with their Field Goal, 3-pointers, Free throws, and Fouls
chart.Correlation(Lakers[,c(8,25,28,31,40)], histogram=TRUE, pch=19)


# Multiple Linear Regression

# Let us see how the Lakers performed against Western opponents
# Let's extract only the Western games and use the columns for 3-point Shots and Field Goals
WestPts <- Lakers$Tm[Western]; length(WestPts)
West3P <- Lakers$X3P_LA[Western]; head(West3P)
WestFG <- Lakers$FG_LA[Western]; head(WestFG)
plot(WestPts,West3P)  
plot(WestPts,WestFG) 
West <- Lakers[Western,]; nrow(West)

cor(WestPts, West3P)     
chart.Correlation(West[,c(7,12)], histogram=TRUE, pch=19)
cor(WestPts,WestFG)       
chart.Correlation(West[,c(7,9)], histogram=TRUE, pch=19)

# To minimize residuals, project onto the subspace spanned by constant
# 3 pointers, and free throws
m1 <- rep(1,length(West$X3P_LA))    #constant
m2 <- West$X3P_LA    
m3 <- West$FG_LA

A <- cbind(m1,m2,m3)   # columns span a 3 dimensional subspace
# Make an invertible square matrix
B <- t(A)%*%A; B
# Invert it
BInv <- solve(B); BInv
# Make the projection matrix.
P <- A%*%BInv%*%t(A)

PredictScore <- P%*%West$Tm   # the projection of the score onto subspace M
coeff <- BInv%*%t(A)%*%West$Tm; coeff

Resid <- West$Tm - coeff[1]*m1 - coeff[2]*m2 - coeff[3]*m3        # the residuals
sum(Resid*m1); sum(Resid*m2) ; sum(Resid*m3) # orthogonal to all three basis vectors
sqrt(sum(Resid^2))                   # this is the minimum possible length
# Splitting up the variance
varObs <- var(West$Tm); varObs # variance of observations
varPred <- var(PredictScore); varPred # variance of predicted values
varResid <-var(Resid); varResid # variance of residuals

# The variance of the observations is the sum of the two pieces
varObs; varPred +  varResid
# Our predictors now explain a greater fraction of the variance
varPred/varObs

# Of course this has all been automated
FG_3P <- lm(West$Tm~West$X3P_LA+West$FG_LA, data =West);FG_3P
coeff   # we got the same coefficients
summary(FG_3P)


# Regression with a higher-degree polynomial
# Let's try linear regression on multiple variables, we'll use FT, FG and 3P 
West_stat <- West$FT_LA+2*West$FG_LA+3*West$X3P_LA; head(Score); head(West$Tm)  # stats per game
hist(West$Tm)   
plot(West_stat, West$Tm, pch = '.', cex = 3)
# Let's try a linear model
m1 <- rep(1,length(West_stat))    #constant
m2 <- West_stat    

A <- cbind(m1,m2)   #columns span a 2 dimensional subspace
#Make an invertible square matrix
B <- t(A)%*%A; B
#Invert it
BInv <- solve(B); BInv
coeff <- BInv%*%t(A)%*%West$Tm; coeff
f <- function(x) coeff[1]+coeff[2]*x
curve(f, add = TRUE)    

#Just include West_stat^2 as an extra predictor.
m3 <- West_stat^2    
A <- cbind(m1,m2,m3)   #columns span a 3 dimensional subspace
#Make an invertible square matrix
B <- t(A)%*%A; B
#Invert it
BInv <- solve(B); BInv
coeff <- BInv%*%t(A)%*%West$Tm; coeff
f <- function(x) coeff[1]+coeff[2]*x + coeff[3]*x^2
curve(f, add = TRUE, col = "red")   


# Logistic regression

# Extract the scores column of the Eastern opponents and on which ones the Lakers won
EastPts <- Lakers$Opp_pts[Eastern]; EastPts
EastWL <- Lakers$W_L[Eastern] == "W"; head(EastWL) 
plot(EastPts,EastWL)  # not a great candidate for a straight-line approximation, but let's try
# Get the slope
b <- cov(EastPts,EastWL)/var(EastPts); b   
# Get the intercept
a <- mean(EastWL) - b*mean(EastPts);a  
# Add this regression line to the plot of the data
abline(a, b, col = "red")
# This is not a good approximation.

# Get the predictor
pred <- EastPts
# Get the response
resp <- EastWL
# Let's use the function for multiple logistic regression
MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*pred)/(1+exp(alpha+beta*pred)) )*resp
        + log(1/(1+exp(alpha+beta*pred)))*(1-resp) )
}
# Maximize the function of alpha and beta
results<-mle(MLL, start = list(alpha = 0, beta = 0)) # an initial guess is required
results@coef
plot(EastPts,EastWL) # get a clean plot  
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
#The curve shows a good model for the probability of winning as a function of the opponents' scores
abline(h=0.5)
abline(v=110)

index <- which(pred == 110); head(index)   # games with Eastern opponents scoring 110 pts
mean(EastWL[index])   # The Lakers won 100% of the time.
index <- which(pred <= 110)    # games with Eastern opponents scoring <= 110 pts
mean(EastWL[index])   # The Lakers won 80% of the time.
index <- which(pred > 110)    # games with Eastern opponents scoring > 110 pts
mean(EastWL[index])   # The Lakers only won 36.3% of the time.


# The End


