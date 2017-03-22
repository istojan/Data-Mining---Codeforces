library(rpart)
library(rpart.plot) # need to download rpart.plot package first

setwd("D:/FINKI/6 semestar/Data Mining/Proekt/Data") #  Needs to be set to the absolute path of the
                                                     #  directory of Final Data.csv file
dataWithMeta <- read.csv("Final Data.csv", header = TRUE)

data <- dataWithMeta[ , 8:99] # data without Rating and Meta Attributes

s <- sample(dim(data)[1], dim(data)[1]*0.7)
data_train <- data[s,]
data_test <- data[-s,]

dtm_Max.Rating <- rpart(Max.Rating~. , data_train, method="anova")
rpart.plot(dtm_Max.Rating, type = 4, extra = 101) # some additional options for better plots

p <- predict(dtm_Max.Rating, data_test)
error <- (sum(abs(p - data_test[,1]))/dim(data_test)[1])

print( sprintf("Absolute difference error: %.5f", error))
