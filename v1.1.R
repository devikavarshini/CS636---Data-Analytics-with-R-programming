library(rpart)
setwd("C:/Users/devik/Documents/R/Proj2")
train <- read.csv("train.csv")
train_labels <- read.csv("train_labels.csv") 
mod_train_df <- merge(train, train_labels, by="sequence", all=TRUE)
test <- read.csv("test.csv")
mod_train_df$state <- factor(mod_train_df$state)
str(mod_train_df)

head(mod_train_df)
head(test)

tree <- rpart(state~sensor_00+sensor_01+sensor_02+sensor_03+sensor_04+sensor_05+sensor_06+sensor_07+sensor_08+sensor_09+sensor_10+sensor_11+sensor_12, cp=0.001, data=mod_train_df)

prediction_train <- predict(tree,train,type='class')

library(caret)
confusionMatrix(prediction_train, mod_train_df$state, positive='1')

class(prediction_train)
prediction_train

mod_train_df$state

library(dplyr)
grouped_train_df <- mod_train_df %>% group_by(sequence) %>% summarise(across(contains("sensor"),list(min=min, max=max, mean=mean, sd=sd)))

grouped_train_df
str(grouped_train_df)

grouped_train_df <- merge(grouped_train_df, train_labels, by="sequence", all=TRUE)
grouped_train_df$state <- factor(grouped_train_df$state)

###Feature Selection###

library(mlr)


task = makeClassifTask(data = grouped_train_df, target = "state")

library(FSelectorRcpp)
fv = generateFilterValuesData(task, method="FSelectorRcpp_information.gain")
fv

####stop feature selection###

tree <- rpart(state~. -sequence, data=grouped_train_df)

tree <- rpart(state~. -sequence-sensor_04_median-sensor_12_median-sensor_02_min-sensor_10_median-sensor_00_median-sensor_07_median-sensor_05_max-sensor_11_median-sensor_03_median-sensor_05_min-sensor_08_mean-sensor_06_median-sensor_05_median-sensor_01_median-sensor_08_median-sensor_09_median, data=grouped_train_df)

grouped_train01_df <- train %>% group_by(sequence) %>% summarise(across(contains("sensor"),list(min=min, max=max, mean=mean, median=median, sd=sd)))
str(grouped_train01_df)

prediction_train01 <- predict(tree,grouped_train01_df,type='class')
confusionMatrix(prediction_train01, grouped_train_df$state, positive='1')

grouped_test_df <- test %>% group_by(sequence) %>% summarise(across(contains("sensor"),list(min=min, max=max, mean=mean, median=median, sd=sd)))
str(grouped_test_df)
str(test)

prediction_test <- predict(tree,grouped_test_df,type='class')
head(prediction_test)


grouped_test_df$state<-prediction_test
output <- as.data.frame(c(grouped_test_df$sequence,grouped_test_df$state))
class(output)
write.csv(grouped_test_df,"submissionv1.csv",row.names=FALSE)

