library(party)  # need to install package first

setwd("D:/FINKI/6 semestar/Data Mining/Proekt/Data") #  Needs to be set to the absolute path of the
#  directory of Final Data.csv file
dataWithMeta <- read.csv("Final Data.csv", header = TRUE)

data <- dataWithMeta[ , 8:ncol(dataWithMeta)] # data without Rating and Meta Attributes
#data <- dataWithMeta[ , c(8:15,16:22)]


s <- sample(dim(data)[1], dim(data)[1]*0.7)
data_train <- data[s,]
data_test <- data[-s,]

tree_model = ctree(Max.Rating~. , data_train)

p_party <- predict(tree_model, data_test)
error <- (sum(abs(p_party - data_test[,1]))/dim(data_test)[1]) # I am using MAE(Mean Absolute Error) to calculate error

#plot(tree_model, main="Conditional Inference Tree for Codeforces") 
# The plot is unecessary. The tree is usually very big( > 600 nodes) and is impossible
# to visualize properly. It just slows down the execution of the program

print( sprintf("Mean Absolute error: %.5f", error))
# Gives the best result of all other tree models(80 - 85). If we don't use all attributes, the result is usually 90-95.
# Prunning in this model should not be required because tree growth is based on statistical stopping rules.