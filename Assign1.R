#William Ryder - wsr160030

library(utils)
adults <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"))

names(adults) <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation", 
                   "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "income")

nrow(adults)  #number of rows
#number of attributes, type of each attribute,  
dimnames(adults)  #displays names
str(adults)   #number and type of attributes


hist(adults$age)

#mode of each attribute, frequency of each attribute, mean of each attribute, and max/min for each attribute
summary(adults, maxsum=50) #displays the frequency, mean, min, and max of each attribute
#Showing the Relationships
cor(adults[,c("age", "fnlwgt", "education-num", "capital-gain", "capital-loss", "hours-per-week")])

library(ggplot2)

barplot(table(adults$workclass))

ggplot(adults, aes(x=adults$`hours-per-week`, y=adults$`education-num`, col=adults$`marital-status`)) +geom_point()
ggplot(adults, aes(x=adults$`age`, y=adults$`education-num`, col=adults$`marital-status`)) +geom_point()
#younger groups are less likely to have been married unsurprisingly
#The more well educated groups seem to be older but not elderly (more middle aged)
ggplot(adults, aes(x=adults$`education-num`, y=adults$`capital-gain`, col=adults$workclass)) +geom_point()
#the more well educated seems to have a higher variability to higher capital gains
#interestingly the collectopm of thpse who never worked seem to have had a moderate education as
#then again appears a substantial portion of the dataset is young people


#Part 2 of Homework
which(is.na(adults)==TRUE)  #shows the row number of NA values
##There are no NA values in this dataset
which(duplicated(adults)==TRUE) #Shows the row numbers of duplicated rows
clean.adults <- adults[which(duplicated(adults)==FALSE),] #create dataset without the duplicated rows

##Data set contained no null values
##There were around 24 duplicated rows which were promptly removed from the dataset


#Part 3 of Homework
#Aggregation
library(arules)

aggregate(clean.adults[,c("age", "fnlwgt", "education-num", "capital-gain", "capital-loss", "hours-per-week")], by = list(clean.adults$income), FUN = mean)
#Those with less than 50k income were younger and generally less educated and worked less time on average
#They had considerably less capital-gain and capital-loss than their counterpart as well on average.


#Sampling
sampleids <- sample(1:nrow(clean.adults), 100)  #sample of size 100
sampleids
plot(x=clean.adults[sampleids,]$`education-num`, y=clean.adults[sampleids,]$`capital-gain`, col=clean.adults$workclass)

#Subset Selection
newadults<- subset(clean.adults, select=c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation", 
                                          "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "income"))
head(newadults)
#excluded native_country


#Discretization
hist(clean.adults$age,
     main = "Discretization: interval", sub = "Blue lines are boundaries")
abline(v=discretize(clean.adults$age, method="frequency",
                    breaks=5,onlycuts=TRUE), col="blue")
#ages seem concentrated around early adulthood and early to mid middle age (*-40)




