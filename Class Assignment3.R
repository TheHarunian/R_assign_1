#class activity 4
data(iris)

library(rpart)
install.packages("caret")
library(caret)

model <- train(mpg ~ hp, iris,
               method = "ctree",
               trControl = trainControl(
                 method = "cv", number = 10, verboseIter = TRUE
               )
)



tree_default <- rpart(type ~ ., data = Zoo)
tree_default
tree_full <- rpart(type ~., data=Zoo, control=rpart.control(minsplit=2, cp=0))