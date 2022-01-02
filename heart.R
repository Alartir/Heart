setwd("~/Estudio R")

df <- read.csv('heart.csv')

# df['Oldpeak'] <- factor(df['Oldpeak'])
# df['HeartDisease'] <- factor(df['HeartDisease'])

library(dplyr)

# Import Library
library(caTools)

# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(df$HeartDisease, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(df, sample == TRUE)

# Testing Data
test = subset(df, sample == FALSE)

model = glm(HeartDisease ~ ., family = binomial(logit), data = train)

fitted.probabilities <- predict(model,newdata=test,type='response')

fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

misClasificError <- mean(fitted.results != test$HeartDisease)
print(paste('Accuracy',1-misClasificError))

print(table(test$HeartDisease, fitted.probabilities > 0.5))