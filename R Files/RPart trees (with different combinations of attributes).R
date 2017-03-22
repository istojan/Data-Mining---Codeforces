library(rpart)
library(rpart.plot) # need to download rpart.plot package first

setwd("D:/FINKI/6 semestar/Data Mining/Proekt/Data") #  Needs to be set to the absolute path of the
#  directory of Final Data.csv file
dataWithMeta <- read.csv("Final Data.csv", header = TRUE)




data <- dataWithMeta[ , c(8:15,16:22)] # data with specific Attributes

s <- sample(dim(data)[1], dim(data)[1]*0.7)
data_train <- data[s,]
data_test <- data[-s,]

m1_Max.Rating <- rpart(Max.Rating~. , data_train, method="anova")
rpart.plot(m1_Max.Rating, type = 4, extra = 101) # some additional options for better plots

p <- predict(m1_Max.Rating, data_test)
error_Without_Categories <- (sum(abs(p - data_test[,1]))/dim(data_test)[1])
# I am using MAE(Mean Absolute Error) to calculate error




# Now we will also use the 35 Contest Categories
data <- dataWithMeta[ , c(8:15,16:22, 30:64)] # data with specific Attributes

s <- sample(dim(data)[1], dim(data)[1]*0.7)
data_train <- data[s,]
data_test <- data[-s,]

m2_Max.Rating <- rpart(Max.Rating~. , data_train, method="anova")
rpart.plot(m2_Max.Rating, type = 4, extra = 101) # some additional options for better plots

p <- predict(m2_Max.Rating, data_test)
error_With_Categories <- (sum(abs(p - data_test[,1]))/dim(data_test)[1])





# Now we will use the 7 Practise Clusters
data <- dataWithMeta[ , c(8:15,16:22, 23:29, 30:64)] # data with specific Attributes

s <- sample(dim(data)[1], dim(data)[1]*0.7)
data_train <- data[s,]
data_test <- data[-s,]

m3_Max.Rating <- rpart(Max.Rating~. , data_train, method="anova")
rpart.plot(m3_Max.Rating, type = 4, extra = 101) # some additional options for better plots

p <- predict(m3_Max.Rating, data_test)
error_With_Practise_Clusters <- (sum(abs(p - data_test[,1]))/dim(data_test)[1])





# Now we will also use practise categories (all data)
data <- dataWithMeta[ , c(8:ncol(dataWithMeta))] # data with specific Attributes

s <- sample(dim(data)[1], dim(data)[1]*0.7)
data_train <- data[s,]
data_test <- data[-s,]

m4_Max.Rating <- rpart(Max.Rating~. , data_train, method="anova")
rpart.plot(m4_Max.Rating, type = 4, extra = 101) # some additional options for better plots

p <- predict(m4_Max.Rating, data_test)
error_With_All_Data <- (sum(abs(p - data_test[,1]))/dim(data_test)[1])




print( sprintf("Mean Absolute error without problem categories: %.5f", error_Without_Categories))
print( sprintf("Mean Absolute error with problem categories: %.5f", error_With_Categories)) # usually better that without categories for about 7-8
print( sprintf("Mean Absolute error with problem categories and practise clusters: %.5f", error_With_Practise_Clusters)) # usually the same as before
print( sprintf("Mean Absolute error with all data: %.5f", error_With_All_Data)) # usually a slight increase in precision
# Best result is around 105.Basically, only the first model differs by a significant margin then the rest.
# I don't use prunning here because I noticed in previous examples that it doesn't make a difference