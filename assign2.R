data(iris)
head(iris)

summary(iris)

library(rpart)

tree_default <- rpart(Species ~ ., data = iris)
tree_default

nrow(iris)

library(rpart.plot)
rpart.plot(tree_default, extra = 2, under = TRUE, varlen=0, faclen=0)

#confusion table
pred1 <- predict(tree_default, iris, type="class")
confusion_table1 <- table(iris$Species, pred1)
confusion_table1

#accuracy and Resubmission Error
sum(diag(confusion_table1))/sum(confusion_table1)

#Generalization Error
#(6+3*.5)/150 = 5%


#part 2
tree_full <- rpart(Species ~., data=iris, control=rpart.control(minsplit=2, cp=0))
tree_full
rpart.plot(tree_full, extra = 2, under = TRUE,  varlen=0, faclen=0)

#confusion table
pred2 <- predict(tree_full, iris, type="class")
confusion_table2 <- table(iris$Species, pred2)
confusion_table2

#accuracy and Resubmission Error
sum(diag(confusion_table2))/sum(confusion_table2)

#Generalization Error
#(0+9*.5)/150 = 3%


#Part 3
#Sepal.Length 4.3-7.9
#Sepal.Width 2-4.4
#Petal.Length 1-6.9
#Petal.Width 0.1-2.5
#randomly generated new flower to check against tree
element <- data.frame(Sepal.Length=runif(1, 4.3, 7.9),Sepal.Width=runif(1, 2, 4.4), Petal.Length=runif(1, 1, 6.9), Petal.Width=runif(1, 0.1, 2.5),Species=NA)
element
pred3 <- predict(tree_default, element, type="class")
pred3
pred4 <- predict(tree_full, element, type="class")
pred4

#Part 4
#trainsetN <- as.integer(nrow(iris)*.66)
trainsetN <- sample(1:nrow(iris), as.integer(nrow(iris)*.66))
trainset <- iris[trainsetN,]
holdOutTree <- rpart(Species ~., data=trainset, control=rpart.control(minsplit=2, cp=0))
rpart.plot(holdOutTree, extra = 2, under = TRUE,  varlen=0, faclen=0)

#Resubmission Error
pred5 <- predict(holdOutTree, trainset, type="class")
confusion_table3 <- table(trainset$Species, pred5)
confusion_table3
sum(diag(confusion_table3))/sum(confusion_table3)

#Generalization error
gtable<- table(iris[-trainsetN, "Species"], predict(holdOutTree, iris[-trainsetN, colnames(iris) != "Species"], type="class"))
sum(1,-sum(diag(gtable))/sum(gtable))


#Part 5
#Generally the error seems fairly low for all of these, nothing seems to be lower than .92 when it comes to accuracy
#and that was with the holdout tree's generalization error.
#In fact the only time the confusion table showed any issue was on the first plot.
#I did notice that running my randomly generated element that it did end up being split between two species which were
#the virginica and the versicolor as the discrepancy between the two trees. Meaning that I obtained one of the niche cases
#in which the element generated was misclassified. 
#The holdout tree that was generated looked extremely close to the full_tree that had been generated earlier.
#I found it kind of interesting that the generalization error was less for the full tree than the simplified one we started off with
#I had kinda expected it would be the otherway around.