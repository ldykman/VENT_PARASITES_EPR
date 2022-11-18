# PARASITE RICHNESS WITHIN HOST SPECIES STATISTICS

# This script runs statistics comparing parasite diversity from vents, atoll lagoon sandflats, and kelp forest.
# The first test runs an ANCOVA with an ordered alternative to test the hypothesis that the well-connected system (kelp forest) has the highest diversity, 
# the island-like undisturbed system (Atoll lagoon sandflat) has intermediate diversity, and the island-like disturbed system (vents) has the lowest.
# The second test uses an ANCOVA to test the hypothesis that diversity within host species is different in the three ecosystems.

# All tests use rank richness and rank mean host length instead of numerical values Lawson, A. (1983). Rank analysis of covariance: Alternative approaches. Journal of the Royal Statistical Society: Series D (The Statistician), 32(3), 331-337.

# Modified July 28, 2021
# Modified July 19, 2022
# Lauren Dykman

rm(list=ls())

# INSTALLING PACKAGES

#install.packages("nloptr")
#install.packages("dplyr")
#install.packages("permute")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("rstatix")
#install.packages("car")
#install.packages("coneproj")

library(nloptr)
library(dplyr)
library(permute)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(car)
library(coneproj)

# SETTING WORKING DIRECTORY

# Re-name to your working directory
path <- "/Users/laurendykman/Desktop/Mullineaux_Lab/R-Python/VENT_PARASITES_EPR"

getwd()
setwd(path)
getwd()

# IMPORTING DATA

input_file = paste0("STATISTICS_Data_Table_Chao2_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
data.summary <- read.csv(paste(path, "Output", input_file, sep = "/"), header=TRUE)

data.summary$ECOSYSTEM <- factor(data.summary$ECOSYSTEM, levels = c("Kelp forest", "Atoll lagoon sandflat", "Vent"))
data.summary$HOST_GROUP_COMMON_NAME <- factor(data.summary$HOST_GROUP_COMMON_NAME, levels = c("Crustacean", "Fish", "Mollusk", "Polychaete"))

data.summary$RANK_RICHNESS <- rank(data.summary$CHAO2, na.last = TRUE, ties.method = c("average"))
data.summary$RANK_LENGTH <- rank(data.summary$HOST_LENGTH_MEAN, na.last = TRUE, ties.method = c("average"))

data.summary <- data.summary[with(data.summary, order(RANK_LENGTH)),]

# STATISTICAL ANALYSIS 1: TESTING AGAINST AN ORDERED ALTERNATIVE

# Extract x (explanatory variable, in this case rank mean host length per host species)

x <- data.summary$RANK_LENGTH

# Extract y (response variable, in this case rank parasite richness per host species)

y <- data.summary$RANK_RICHNESS

# A function to create the matrix xmat for the equation . 
# This matrix has six columns: 
#   The first is rank host length.
#   The next three contain 1 or 0 designating to which ecosystem the species belongs ((1,0,0), (0,1,0), or (0,0,1)).
#   The final four contain 1 or 0 designating to which host group the species belongs ((1,0,0), (0,1,0), or (-1,-1,-1)).

make.xmat <- function(X) {
  
  # Creating matrix X
  
  ecogroups <- paste(X$ECOSYSTEM, X$HOST_GROUP_COMMON_NAME)
  
  #print(ecogroups)
  
  elements <- as.data.frame(matrix(nrow = 0, ncol = 5))
  
  for (combo in ecogroups) {
    
    if (combo == "Kelp forest Crustacean") {
      
      elements <- rbind(elements, c(1,0,0,-1,-1,-1))
      
    }
    
    if (combo == "Kelp forest Fish") {
      
      elements <- rbind(elements, c(1,0,0,1,0,0))
      
    }
    
    if (combo == "Kelp forest Mollusk") {
      
      elements <- rbind(elements, c(1,0,0,0,1,0))
      
    }
    
    if (combo == "Kelp forest Polychaete") {
      
      elements <- rbind(elements, c(1,0,0,0,0,1))
      
    }
    
    if (combo == "Atoll lagoon sandflat Crustacean") {
      
      elements <- rbind(elements, c(0,1,0,-1,-1,-1))
      
    }
    
    if (combo == "Atoll lagoon sandflat Fish") {
      
      elements <- rbind(elements, c(0,1,0,1,0,0))
      
    }
    
    if (combo == "Atoll lagoon sandflat Mollusk") {
      
      elements <- rbind(elements, c(0,1,0,0,1,0))
      
    }
    
    if (combo == "Atoll lagoon sandflat Polychaete") {
      
      elements <- rbind(elements, c(0,1,0,0,0,1))
      
    }
    
    if (combo == "Vent Crustacean") {
      
      elements <- rbind(elements, c(0,0,1,-1,-1,-1))
      
    }
    
    if (combo == "Vent Fish") {
      
      elements <- rbind(elements, c(0,0,1,1,0,0))
      
    }
    
    if (combo == "Vent Mollusk") {
      
      elements <- rbind(elements, c(0,0,1,0,1,0))
      
    }
    
    if (combo == "Vent Polychaete") {
      
      elements <- rbind(elements, c(0,0,1,0,0,1))
      
    }
    
  }
  
  xmat <- as.matrix(cbind(data.summary$RANK_LENGTH, elements))
  
  return(xmat)
  
}

xmat <- make.xmat(data.summary)
colnames(xmat) <- c("RANK_LENGTH", "X0", "X1", "X2", "X3", "X4", "X5")

# Make the q matrix defined as xmat'xmat

q <- crossprod(xmat)
  
# Make the c vector defined as xmat'y
c <- crossprod(xmat, y)

# Make the constraint matrix to constrain the regression to the hypothesized ecosystem order.

amat <- matrix(0, 2, 7)
amat[1, 2] <- 1; amat[1, 3] <- -1
amat[2, 3] <- 1; amat[2, 4] <- -1

b <- rep(0, 2)

# Call qprog

ans <- qprog(q, c, amat, b)

# Get the constrained fit of y

betahat <- fitted(ans)
fitc <- crossprod(t(xmat), betahat)

# Get the unconstrained fit of y

fitu <- lm(y ~ x + I(x^2) + I(x^3))

# Calculating the test statistic of the observed data

test.stat.observed <- as.numeric(t(y - xmat %*% ans$thetahat) %*% (y - xmat %*% ans$thetahat))

# Make a plot to compare fitc and fitu

qprog.data <- cbind(x,y,fitc)
colnames(qprog.data) <- c("X", "Y", "FitY")

par(mar = c(4, 4, 1, 1))
plot(x, y, cex = 1, xlab = "Rank Mean Host Length", ylab = "Rank Estimated Parasite Richness")
lines(x, fitc, col = 2, lty = 4)
legend("topleft", bty = "n", c("Predicted Results"), lty = c(4), col = c(2))
title("Qprog Model Fit")

# Make a plot to compare observed richness to fitted richness

par(mar = c(4, 4, 1, 1))
plot(y, fitc, cex = 1, xlab = "Observed", ylab = "Predicted", xlim = c(0,100), ylim = c(0,100))
lines(c(0:100), c(0:100))
title("Observed Versus Predicted Parasite Richness")

# Checking output of qprog by explicitly calculating the fits

solve(t(xmat) %*% xmat) %*% t(xmat) %*% y # Checked on 2022-07-19 and outputs were equivalent.

# R-squared calculation (1 - minimized RSS)/SS

SS <- sum((y - mean(y))**2)
RSS <- sum((y - fitc)**2)

Rsq <- 1 - RSS/SS

# DEFINING FUNCTIONS

# This function randomizes the ecosystem of species in a given host group while maintaining the number of species in each ecosystem in each host group.

shuffle.ecosystem <- function(X) {
  
  data.new <- as.data.frame(matrix(nrow = 0, ncol = dim(X)[2]))
  
  groups <- unique(X$HOST_GROUP_COMMON_NAME)
  
  for (g in groups) {
    
    group.subset <- unique(X[X$HOST_GROUP_COMMON_NAME == g,])
    
    group.subset$ECOSYSTEM <- group.subset$ECOSYSTEM[shuffle(group.subset$ECOSYSTEM)]
    
    data.new <- rbind(data.new, group.subset)
    
  }
  
  data.new <- data.new[with(data.new, order(RANK_LENGTH)),]
  
  return(data.new)
  
}

test.shuffle <- shuffle.ecosystem(data.summary)

# RANDOMIZATIONS FOR ORDERED ECOSYSTEM EFFECT ALL HOST GROUPS

final.data.table <- as.data.frame(matrix(nrow = 0, ncol = 1))

while (dim(final.data.table)[1] < 1000) {
  
  randomized.data <- shuffle.ecosystem(data.summary)
  
  #print(head(randomized.data))
  
  print(c(dim(final.data.table)[1]))
  
  xmat.rand <- make.xmat(randomized.data)
  
  #print(head(xmat.rand))
  
  q.rand <- crossprod(xmat.rand)
  
  #print(head(q.rand))
  
  c.rand <- crossprod(xmat.rand, y)
  
  #print(c.rand)
  
  ans.rand <- qprog(q.rand, c.rand, amat, b)
  
  print(ans.rand$thetahat)
  
  #print(randomized.data$RANK_RICHNESS)
  
  test.stat.rand <- as.numeric(t(y - xmat.rand %*% ans.rand$thetahat) %*% (y - xmat.rand %*% ans.rand$thetahat))
  
  #print(test.stat.rand)
  
  final.data.table <- rbind(final.data.table, c(test.stat.rand))
  
}

colnames(final.data.table) <- c("TEST_STATISTIC")

filename4 = paste0("STATISTICS_ORDERED_Randomizations_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(final.data.table, paste(path, "Output", filename4, sep = "/"))

p_value_list <- final.data.table$TEST_STATISTIC < test.stat.observed

p_value_ordered <- sum(p_value_list)/length(p_value_list) # Using anova_test got P-value 0.6 with randomizations got p value of 0.4 with function aov

# ANCOVA TEST AGAINST AN UNORDERED ALTERNATIVE

ancova.lm <- lm(RANK_RICHNESS ~ RANK_LENGTH + ECOSYSTEM + HOST_GROUP_COMMON_NAME, data = data.summary)
ancova.observed <- Anova(ancova.lm, type=2)

F.observed <- ancova.observed$`F value`[2] # 0.20778

# RANDOMIZATIONS FOR OVERALL ECOSYSTEM EFFECT ALL HOST GROUPS

final.data.table <- as.data.frame(matrix(nrow = 0, ncol = 2))

while (dim(final.data.table)[1] < 1000) {
  
  randomized.data <- shuffle.ecosystem(data.summary)
  
  print(c(dim(final.data.table)[1]))
  
  ancova.lm.rand <- lm(RANK_RICHNESS ~ RANK_LENGTH + ECOSYSTEM + HOST_GROUP_COMMON_NAME, data = randomized.data)
  
  ancova.rand <- Anova(ancova.lm.rand, type=2)
  
  final.data.table <- rbind(final.data.table, c(ancova.rand$`F value`[2], ancova.rand$`Pr(>F)`[2]))
  
}

colnames(final.data.table) <- c("F_STATISTIC", "P_VALUE")

filename4 = paste0("STATISTICS_ANCOVA_Randomizations_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(final.data.table, paste(path, "Output", filename4, sep = "/"))

p_value_list <- final.data.table$F_STATISTIC > F.observed

p_value_unordered <- sum(p_value_list)/length(p_value_list) # p = 0.79
