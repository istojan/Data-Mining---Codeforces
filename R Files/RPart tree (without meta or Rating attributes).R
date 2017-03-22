library(rpart)
library(rpart.plot) # need to download rpart.plot package first

setwd("D:/FINKI/6 semestar/Data Mining/Proekt/Data") #  Needs to be set to the absolute path of the
                                                     #  directory of Final Data.csv file
dataWithMeta <- read.csv("Final Data.csv", header = TRUE)

data <- dataWithMeta[ , 8:ncol(dataWithMeta)] # data without Rating and Meta Attributes

s <- sample(dim(data)[1], dim(data)[1]*0.7)
data_train <- data[s,]
data_test <- data[-s,]

tree_model <- rpart(Max.Rating~. , data_train, method="anova")
rpart.plot(tree_model, type = 4, extra = 101) # some additional options for better plots

p <- predict(tree_model, data_test)
error <- (sum(abs(p - data_test[,1]))/dim(data_test)[1]) # I am using MAE(Mean Absolute Error) to calculate error


# prune the tree 
tree_model_prunned<- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]) # from cptable   

# plot the pruned tree 
plot(tree_model_prunned, uniform=TRUE, 
     main="Pruned Regression Tree for Codeforces")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

prunned_values <- predict(tree_model_prunned, data_test)
error_Prunned_Tree <- (sum(abs(prunned_values - data_test[,1]))/dim(data_test)[1]) # I am using MAE(Mean Absolute Error) to calculate error


print( sprintf("Mean Absolute error: %.5f", error))
print( sprintf("Mean Absolute error for prunned tree: %.5f", error_Prunned_Tree))
# The prunning in this case doesn't give a better result.
