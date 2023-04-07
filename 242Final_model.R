
library(dplyr)
library(caTools)
library(rpart)
library(caret)
library(ggplot2)
library(lubridate)

rm(list = ls())

OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}


fulldata <- read.csv("full data.csv", stringsAsFactors = TRUE)
fulldata[,c(1, 2, 11, 15, 16, 18, 17, 19:22, 23:43)] <- NULL
fulldata$Date <- ymd(fulldata$Date)
fulldata$nTripYesterday <- NA


for (i in fulldata$station.name) {
  a <- filter(fulldata, fulldata$station.name == i)
  a$nTripYesterday <- c(NA, head(a$nTrip,-1))
  fulldata[fulldata$station.name == i,"nTripYesterday"] <- a$nTripYesterday
}

fulldata <- mutate(fulldata, DayofWeek = factor(wday(Date,label = TRUE),ordered = FALSE))




#for (i in 19:23){fulldata[,c(19:43)] <- as.numeric(fulldata[,i])}

#fulldata[,c(1, 2, 3, 7, 11, 15, 16, 18, 17)] <- NULL

#fulldata$Date <- NULL
fulldata$day <- as.factor(fulldata$day)
fulldata$month <- as.factor(fulldata$month)


#set.seed(123)
#split <-  sample.split(fulldata$nTrip, SplitRatio = 0.7)
#train <- filter(fulldata, split == TRUE)
#test <- filter(fulldata, split == FALSE)


train <- filter(fulldata, year < 2015)
test <- filter(fulldata, year >= 2015)


### linear regression 

#-month-day
model.lm <- lm(nTrip ~ . -day-Date, data = train)
summary(model.lm)

pred.lm <- predict(model.lm, newdata = test)
lm.osr2 <- OSR2(pred.lm, train$nTrip, test$nTrip)
lm.osr2



### CART

## Cart model
cpvalue <- data.frame(cp = seq(0,0.1,by = 0.002))
set.seed(123)
model.cart <- train(nTrip ~ ., 
                    data = train,
                    method = "rpart", 
                    tuneGrid = cpvalue, 
                    trControl = trainControl(method = "cv", number = 5),
                    metric = "RMSE")

model.cart
model.bestcart <- model.cart$finalModel
ggplot(model.cart$results, aes(x=cp, y=RMSE)) + geom_point(size=1) +
  xlab("Complexity Parameter (cp)") + geom_line()

test.mm <- as.data.frame(model.matrix(nTrip~.+0, data = test))
pred.cart <- predict(model.bestcart, newdata = test.mm)
cart.osr2 <- OSR2(pred.cart, train$nTrip, test$nTrip)





### Random Forest

set.seed(123)
model.rf <- train(nTrip ~ .,
                  data = train,
                  method = "rf",
                  tuneGrid = data.frame(mtry=1:3),
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "RMSE")

#model.rf$results
model.bestRF <- model.rf$finalModel
pred.rf <- predict(model.bestRF, newdata = test.mm) # can use same model matrix

ggplot(train.rf$results, aes(x = mtry, y = Rsquared)) + geom_point(size = 3) + 
  ylab("CV Rsquared") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

RF.osr2 <- OSR2(pred.rf, train$nTrip, test$nTrip)













