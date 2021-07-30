library(e1071)
library(readr)

# read data, perform basic statistics and visualization

setwd("c:/Users/duquesne/Downloads/")

data <- read_csv("2622217.csv")

m <- mean(data$TOBS, na.rm = TRUE)
s <- sd(data$TOBS, na.rm = TRUE)
sk <- skewness(data$TOBS, na.rm = TRUE)
kr <- kurtosis(data$TOBS, na.rm = TRUE)

h <- hist(data$TOBS, breaks = c(-30, -20, -10, 0, 10, 20, 30))
