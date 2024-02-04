dataset<-read.csv("2019.csv")
dataset


#objective 1:
# correlation test between the Happiness Score and GDP per capita
# Define the variables
Happiness_Score <- dataset$Score
Happiness_Score
GDP_Per_Capita <- dataset$GDP.per.capita
GDP_Per_Capita 
# Perform correlation test
correlation_test <- cor.test(Happiness_Score, GDP_Per_Capita)
correlation_test
# Print the results
cat("Correlation Test Results:\n")
cat("Correlation coefficient:", correlation_test$estimate, "\n")
cat("p-value:", correlation_test$p.value, "\n")
cat("Test statistic:", correlation_test$statistic, "\n")
cat("Degrees of freedom:", correlation_test$parameter, "\n")



#objective 2:
#average Happiness Scores between Finland and Denmark 

  # Define the data for Finland and Denmark
finland_happiness <- c(7.769, 7.6, 7.554, 7.494)
denmark_happiness <- c(7.6) # One data point for Denmark

# Combine the data for Finland and Denmark
happiness_data <- list(Finland = finland_happiness, Denmark = denmark_happiness)

# Perform ANOVA
anova_result <- aov(unlist(happiness_data) ~ as.factor(rep(names(happiness_data), lengths(happiness_data))))

# Print the ANOVA result
print(summary(anova_result))




#objective 2
#Test the hypothesis that the proportion of countries in the world with a happiness score above 5.5 is greater than 50%.

library(tidyverse)
happiness <- read.csv("2019.csv", header = TRUE)
happiness_score <- happiness$Happiness.Score

prop <- length(happiness_score[happiness_score > 5.5])/length(happiness_score)
prop

prop.test(x = length(happiness_score[happiness_score > 5.5]), n = 156, p = 0.5, alternative = "greater")
