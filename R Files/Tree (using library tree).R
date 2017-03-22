library(tree) # need to install package first

setwd("D:/FINKI/6 semestar/Data Mining/Proekt/Data") #  Needs to be set to the absolute path of the
#  directory of Final Data.csv file
dataWithMeta <- read.csv("Final Data.csv", header = TRUE)

data <- dataWithMeta[ , 8:ncol(dataWithMeta)] # data without Rating and Meta Attributes

s <- sample(dim(data)[1], dim(data)[1]*0.7)   # Using 70% of data for train and 30% for test.
data_train <- data[s,]
data_test <- data[-s,]

tree_model = tree(Max.Rating~. , data_train)
plot(tree_model)
text(tree_model, pretty = 0)

p <- predict(tree_model, data_test)
error <- (sum(abs(p - data_test[,1]))/dim(data_test)[1]) # I am using MAE(Mean Absolute Error) to calculate error

# prune the tree to ideal size by Deviation using Cross Validation
cv_tree = cv.tree(tree_model)

plot(cv_tree$size, cv_tree$dev, type = 'b', xlab='Tree Size', ylab = 'Dev')

# Producting a pruned model. It can give a better result, but not necessarily. In this case, it doesn't.
pruned_model = prune.tree(tree_model, best = cv_tree$size[which.min(cv_tree$dev)])
plot(pruned_model)
text(pruned_model, pretty = 0)

# check the accuracy of the model using testing data
tree_pred = predict(pruned_model, data_test)
error_Prunned_Tree <- (sum(abs(tree_pred - data_test[,1]))/dim(data_test)[1]) # I am using MAE(Mean Absolute Error) to calculate error

print( sprintf("Mean Absolute error: %.5f", error))
print( sprintf("Mean Absolute error for prunned tree: %.5f", error_Prunned_Tree))
