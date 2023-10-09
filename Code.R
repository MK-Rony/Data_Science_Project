mydata<-read.csv("E:/Academic/AIUB/Semester 9/Data Science/Final/Project/MyProject/Dataset/diabetes.csv",header=TRUE,sep=",")
mydata

summary(mydata)
str(mydata)

is.na(mydata)
colSums(is.na(mydata))

install.packages("caret")
library(caret)
install.packages("class")
library(class)

ctrl <- trainControl(method = "cv", number = 10)
knn_model <- train(
  Outcome ~ .,
  data = mydata,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k =10)
)
knn_model

conf_matrix(knn_model)
recall <- conf_matrix$byClass["Sensitivity"]
recall
precision <- conf_matrix$byClass["Precision"]
precision


mydata
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
mydata <-as.data.frame(lapply(mydata[1:9],normalize))
mydata

correlation<-cor(mydata)
correlation

install.packages("corrplot")
library(corrplot)
cor_matrix <- cor(mydata)
corrplot(cor_matrix, method = "number")

train_indices<-sample(nrow(mydata), 0.8 * nrow(mydata))
train_indices
train_data<-mydata[train_indices, ]
train_data
test_data<-mydata[-train_indices, ]
test_data

NROW(train_data)
sqrt(NROW(train_data))

knn.24 <- knn(train = train_data[, -9], test = test_data[, -9], cl = train_data$Outcome, k = 24)
knn.25 <- knn(train = train_data[, -9], test = test_data[, -9], cl = train_data$Outcome, k = 25)

Accuracy.24 <- 100 * sum(test_data$Outcome == knn.24) / nrow(test_data)
Accuracy.24
Accuracy.25 <- 100 * sum(test_data$Outcome == knn.25) / nrow(test_data)
Accuracy.25

confusion_matrix.24 <- table(knn.24, test_data$Outcome)
confusion_matrix.24
confusion_matrix_summary.24 <- confusionMatrix(confusion_matrix.24)
confusion_matrix_summary.24
recall.24 <- confusion_matrix_summary.24$byClass["Sensitivity"]
recall.24
precision.24 <- confusion_matrix_summary.24$byClass["Pos Pred Value"]
precision.24

confusion_matrix.25 <- table(knn.25, test_data$Outcome)
confusion_matrix.25
confusion_matrix_summary.25 <- confusionMatrix(confusion_matrix.25)
confusion_matrix_summary.25
recall.25 <- confusion_matrix_summary.25$byClass["Sensitivity"]
recall.25
precision.25 <- confusion_matrix_summary.25$byClass["Pos Pred Value"]
precision.25
