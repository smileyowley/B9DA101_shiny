
library(ggplot2)
#library(kernlab)
#library(arules)
#library(InformationValue)
#library(readxl)
#library(readr)

mydata <- read.csv("StudentPerformance.csv")

# Creating a data frame
mydata <- as.data.frame(mydata)
summary(mydata) # summary
# There are no NA or missing values


#creating train and test data

n=nrow(mydata)  
size_sample=0.8
indexes = sample(n,n*(size_sample))  

trainData = mydata[indexes,]  

testData = mydata[-indexes,]  

### Prediction model

 dim(trainData)
 head(trainData)


 dim(testData)   # check test data set
 head(testData)

#------------------------------------------------------lm model linear regression
model <- lm( writing.score~ math.score + gender + race.ethnicity + lunch + parental.level.of.education + test.preparation.course,data=trainData)
summary(model)
lmPred <- predict(model,testData,interval = "prediction", level=0.95)
summary(lmPred)
head(lmPred)

# 1. Add predictions 
mydata1 <- cbind(testData, lmPred)
head(mydata1)
# 2. Regression line + confidence intervals
p <- ggplot(mydata1, aes( fit, writing.score)) +
  geom_point() +
  stat_smooth(method = lm)

# 3. Add prediction intervals
p<-p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  xlab("Predicted Scores") + ylab("Test Scores")
p


#---------------glm function
# Fit the full model  

actual=trainData$writing.score  

full.model <- glm(trainData$writing.score ~., data = trainData, family='gaussian')  

summary(full.model)  

yhat=predict( full.model, testData[1:7])  

rmse_full = sqrt(sum((yhat -actual)^2)/(nrow(testData)))  

# apply stepAIC to reduce model and find rmse for reduced model  

library(MASS)  

reduced.model=stepAIC(full.model)  

yhat_r=predict( reduced.model, testData[1:7])  

rmse_reduced = sqrt(sum((yhat_r -actual)^2)/(nrow(testData))) 



rmse <- c('Full'=rmse_full,'Reduced'=rmse_reduced) 



rmse 

