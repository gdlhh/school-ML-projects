library(RCurl)

df = read.csv(file ='C:\\Users\\Bob\\Desktop\\AML\\archive\\df_cleaned.csv', header = T)
df = df[,c(-1)]

library(formattable)
formattable(df)

# Numerical values in the response variable are converted to labels
formattable(df)

df$is_canceled <- factor(df$is_canceled, levels=c(0,1), labels=c("Confimed", "Cancelled"))

print(summary(df))

# Proportions of the class values
table(df$is_canceled)
prop.table(table(df$is_canceled)) 


# Dividing the dataset into training and validation sets.

set.seed(123)
ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.7, 0.3))
train_df <- df[ind==1,]
validation_df <- df[ind==2,]
table(train_df$is_canceled)
table(validation_df$is_canceled)

# Proportions of the class values
prop.table(table(train_df$is_canceled)) 

# install.packages('rpart') --> (Recursive Partitioning And Regression Trees) and the R implementation of the CART algorithm
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
library(party)

"Can generate different types of trees with rpart
Default split is with Gini index"

tree = rpart(is_canceled~ ., data=train_df)
tree
prp(tree) # plot Rpart Model
prp (tree, type = 5, extra = 100)
rpart.plot(tree, extra = 101, nn = TRUE)


# Split with entropy information
ent_Tree = rpart(is_canceled ~ ., data=train_df, method="class", parms=list(split="information"))
ent_Tree
prp(tree)
prp(ent_Tree)

library(rpart.plot)
plotcp(tree)

# Here we use tree with parameter settings.
# This code generates the tree with training data
tree_with_params = rpart(is_canceled ~ ., data=train_df, method="class", minsplit = 1, minbucket = 1000, cp = -1)
prp (tree_with_params)
print(tree_with_params)
summary(tree_with_params)
plot(tree_with_params)
text(tree_with_params)
plotcp(tree_with_params)

# Now we predict and evaluate the performance of the trained tree model 
Predict = predict(tree_with_params, validation_df)
# Now examine the values of Predict. These are the class probabilities
Predict


Predict = predict(tree_with_params, validation_df, type = "class")
Predict


# Producing confusion matrix
Confusion_matrix = table(Predict, validation_df$is_canceled)
Confusion_matrix

# Calculating the accuracy using the cofusion matrix
Accuracy = sum(diag(Confusion_matrix))/sum(Confusion_matrix)
Accuracy

# ROC curve
# install.packages("ROCR")
library(ROCR)
# install.packages("gplots")

# To draw ROC we need to predict the prob values. So we run predict again
# Note that PredictROC is same as Predict with "type = prob"

Predict_ROC = predict(tree_with_params, validation_df)
Predict_ROC
Predict_ROC[,2]

pred = prediction(Predict_ROC[,2], validation_df$is_canceled)
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc = round(auc, 3)
auc
