# load required libraries

library(ROSE)
library(MASS)
library(tidyverse)
library(e1071)

# look at the data frame
head(data)

# see the summarised data
summary(data)

str(data)

# check for missing values
sum(is.na(data))

# checking for imbalances based HeartDisease variable
table(data$HeartDisease)

prop.table(table(data$HeartDisease))

# create new balanced data frame
df <- ovun.sample(HeartDisease ~ ., data, method = "under", N = 54746)$data
# confirm data is balanced
table(df$HeartDisease)

# create five folds from the df data frame
folds <- sample(1:5, nrow(df), replace = TRUE)

# create matrix to compute error rates
model_err <- matrix(0, nrow=5, ncol=3)

# assign column names for respective models
colnames(model_err) <- c("Logistic", "QDA", "NaiveBayes")

# Create a loop to evaluate all the 4 models
for (f in 1:5) {
  df_train <- df[folds != f, ]
  df_test <- df[folds == f, ]
  
  # Logistic regression
  dfGLM.fit <- glm(HeartDisease ~ .,
                   data = df_train,
                   family = "binomial")
  
  # predict with Logistic regression
  dfGLM.prob <- predict(dfGLM.fit,
                        df_test,
                        type = "response")
  
  dfGLM.pred <- rep("No", nrow(df_test))
  dfGLM.pred[dfGLM.prob>0.5] = "Yes"
  model_err[f, 1] = mean(df_test$HeartDisease != dfGLM.pred)
  
  # Quadratic Discriminant analysis(QDA)
  dfQDA.fit <- qda(HeartDisease ~ .,
                   data = df_train)
  
  # predict wth QDA
  dfQDA.pred <- predict(dfQDA.fit, df_test)$class
  model_err[f, 2] = mean(df_test$HeartDisease != dfQDA.pred)
  
  # Naive Bayes
  nb.fit <- naiveBayes(HeartDisease ~ .,
                       data = df_train)
  
  # predict with Naive Bayes
  nb.pred <- predict(nb.fit, df_test, type="class")
  
  model_err[f, 3] = mean(df_test$HeartDisease != nb.pred)
  
}

# display matrix with with computed errors rates
model_err

set.seed(1)

# training and testing data
obs <- sample(1:nrow(data), 0.8*nrow(data))
df.train <- data[obs, ]
df.test <- data[-obs, ]

# Logistic regression
dfGLM.fit <- glm(HeartDisease ~ .,
                 data = df.train,
                 family = "binomial")
  
# predict with Logistic regression
dfGLM.prob <- predict(dfGLM.fit,
                      df.test,
                      type = "response")
  
dfGLM.pred <- rep("No", nrow(df.test))
dfGLM.pred[dfGLM.prob>0.5] = "Yes"
model_err = mean(df.test$HeartDisease != dfGLM.pred)

# display error
model_err

# summary of the logistic model
summary(glm(HeartDisease ~ ., data = df, family = "binomial"))

