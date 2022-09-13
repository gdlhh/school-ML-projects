library(ISLR)
library(dplyr)


df = read.csv(file ='C:\\Users\\Bob\\Desktop\\AML\\archive\\df_cleaned.csv', header = T)
df = df[,c(-1)]


df$hotel = as.factor(df$hotel)
df$deposit_type = as.factor(df$deposit_type)
df$customer_type = as.factor(df$customer_type)
df$is_repeated_guest = as.factor(df$is_repeated_guest)
df$is_canceled = as.factor(df$is_canceled)


#imputing the values


df[is.na.data.frame(df)] = 0




#Logistic regression

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caret)
library(caTools)
set.seed(123)
split = sample.split(df$is_canceled, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Checking Class distribution
table(df$is_canceled)
prop.table(table(df$is_canceled))
prop.table(table(training_set$is_canceled))
prop.table(table(test_set$is_canceled))

# Building classifier
classifier = glm(is_canceled ~.,
                 training_set,
                 family = binomial)
summary(classifier)

# Predicting the Training set results
pred_prob_training = predict(classifier, type = 'response', training_set[ ,-2] )
pred_prob_training
pred_class_training = ifelse(pred_prob_training > 0.5, 1, 0)
pred_class_training
cm_training = table(training_set$is_canceled, pred_class_training)
cm_training

accuracy_training <- sum(diag(cm_training))/sum(cm_training)


# Predicting the Test set results
pred_prob_test <- predict(classifier, type = 'response', test_set[ ,-2] )
pred_prob_test
pred_class_test = ifelse(pred_prob_test > 0.5, 1, 0)
pred_class_test
cm_test = table(test_set$is_canceled, pred_class_test)
cm_test

accuracy_test <- sum(diag(cm_test))/sum(cm_test)

# Using formulae compute all other evaluation metrics

# ROC curve on test set
# install.packages("ROCR")
library(ROCR)
# install.packages("gplots")

# To draw ROC we need to predict the prob values. 

pred = prediction(pred_prob_test, test_set$is_canceled)
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
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc




'''
glm.fit = glm(is_canceled ~., data = df, family = binomial)

glm.probs = predict(glm.fit,type = "response")
glm.probs[1:5]

glm.pred = ifelse(glm.probs > 0.5, "Cancelled", "Not Cancelled")


table(glm.pred,is_canceled)

mean(glm.pred == is_canceled)

summary(glm.fit)
'''






#SVM
