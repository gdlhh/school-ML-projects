

df = read.csv(file ='C:\\Users\\Bob\\Desktop\\AML\\archive\\df_cleaned.csv', header = T)
df = df[,c(-1)]

library(ggplot2)
library(e1071)

# labelling the target variable values

df$is_canceled <- factor(df$is_canceled, levels=c(0,1), labels=c("Confimed", "Cancelled"))
table(df$is_canceled)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$is_canceled, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
table(training_set$is_canceled)
table(test_set$is_canceled)

# ~~~~~~~~~~~~~~~~~~~~  Default SVM Model using the RBF kernel ~~~~~~~~~~~~~~~~~~~~~
svm_rbf <- svm(is_canceled~., data = training_set)
summary(svm_rbf)
svm_rbf$gamma

# Confusion Matrix on the training set


# Pred on test set
pred = predict (svm_rbf, test_set)
pred

length(pred)
length(test_set$is_canceled)
test_set$is_canceled

cm = table(Predicted = pred, Actual = test_set$is_canceled)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

# ~~~~~~~~~~~~~~~~~~~~   SVM model using the Linear model  ~~~~~~~~~~~~~~~~~~~~~
svm_linear = svm (is_canceled~., data = training_set, kernel = "linear")
summary (svm_linear)

# Confusion Matrix
pred = predict (svm_linear, test_set)
pred
cm = table(Predicted = pred, Actual = test_set$is_canceled)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

# ~~~~~~~~~~~~~~~~~~~~   SVM model using sigmoid kernal  ~~~~~~~~~~~~~~~~~~~~~
svm_sigmoid = svm (is_canceled~., data = training_set, kernel = "sigmoid")
summary (svm_sigmoid)

# Confusion Matrix
pred = predict (svm_sigmoid, test_set)
cm = table(Predicted = pred, Actual = test_set$is_canceled)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy



# ~~~~~~~~~~~~~~~~~~~~   SVM model using polynomial kernal  ~~~~~~~~~~~~~~~~~~~~~
svm_polynomial = svm (is_canceled~., data = training_set, kernel = "poly")
summary (svm_polynomial)

# Confusion Matrix
pred = predict (svm_polynomial, test_set)
cm_poly = table(Predicted = pred, Actual = test_set$is_canceled)
cm_poly

accuracy = sum(diag(cm_poly))/sum(cm_poly)*100
accuracy

misclassificarion = (1-sum(diag(cm_poly))/sum(cm_poly))*100
misclassificarion


# ~~~~~~~~~~~~~~~~~~~~  Model Tuning  ~~~~~~~~~~~~~~~~~~~~
set.seed(123)
# tune function tunes the hyperparameters of the model using grid search method
tuned_model = tune(svm, is_canceled~., data=training_set,
                   ranges = list(epsilon = seq (0, 1, 0.1), cost = 2^(0:2)))
plot (tuned_model)
summary (tuned_model)
tuned_model$best.parameters
opt_model = tuned_model$best.model
summary(opt_model)

# Building the best model
svm_best <- svm (is_canceled~., data = training_set, epsilon = 0, cost = 1)
summary(svm_best)






