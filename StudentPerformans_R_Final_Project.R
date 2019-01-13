
# By 29/12/2018  Created by Serap Aydogdu.

###################### STEP 1 : Underdtanding Dataset ######################

# Loading Libraries
library(readxl)
library(Rcmdr)
library(psych)
library(dummies)
library(corrplot)
library(Hmisc)

# Loading Data
data <- read.csv("StudentsPerformance.csv")
View(data)

# Count missing observations
sapply(data, function(x)(sum(is.na(x)))) # NA counts

# Variables names 
names(data)

# Dimensions of data
dim(data)   

# Summary data
summary(data)

# What we found: 
#  .	Dataset has 1000 records
#  .	Any column does not have null values.
#  .	gender, race.ethinicity, parental.level.of.education, lunch, test.preparation.course columns have categorical data and math.score, reasing.score and writing score columns have numerical data.



###################### STEP 2 : Data Preparation ######################

# Renaming the columns for the ease of use
names(data)[c(3)] <- c("parent_level_edu") 
View(data)
names(data)[c(5)] <- c("test_prep_course")
View(data)

# Generate id column
id <- rownames(data)
data <- cbind(id=id, data)
View(data)

# Dimension & summary of variables
dim(data)
summary(data$math.score)
summary(data$reading.score)
summary(data$writing.score)
summary(data$gender)
summary(data$race.ethnicity)
summary(data$parent_level_edu)
summary(data$lunch)
summary(data$test_prep_course)




###################### STEP 3 : Desciptive Statistic ######################
attach(data)
describe(math.score)
describe(writing.score)
describe(reading.score)
numSummary(data[,"math.score", drop=FALSE], statistics=c("mean", "sd", "IQR",  "quantiles", "skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")
numSummary(data[,"writing.score", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles", "skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")
numSummary(data[,"reading.score", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles", "skewness", "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")

boxplot(math.score) 
mean(math.score)
boxplot(reading.score)
mean(reading.score)
boxplot(writing.score)
mean(writing.score)

hist(math.score)          # Does math score distrubute normal?
shapiro.test(math.score)  # Shapiro test p value: less than 0.05. So no, it is non-normal distributed.
wilcox.test (math.score, mu=100, alternative="less")   #Does math.score's mean equal or greater then 100 ?
hist(reading.score)
shapiro.test(reading.score)
wilcox.test (reading.score, mu=100, alternative="less")
hist(writing.score)
shapiro.test(writing.score)
wilcox.test (writing.score, mu=100, alternative="less")


#What we found: 
#  .	math.score,  reading.score, writing.score are not normal distribution.


#From this boxplot we can see that except of some outliers students who completed the test preparation course have higher median of math score than students who did not complete the course.
boxplot(math.score~test_prep_course,data=data, col=rainbow(7), xlab="Test Preparation Course", ylab="Math Score")

#From this boxplot we can see that except of some outliers students who completed the test preparation course have higher median of reading score than students who did not complete the course.
boxplot(reading.score~test_prep_course,data=data, col=rainbow(9), xlab="Test Preparation Course", ylab="Reading Score")

#From this boxplot we can see that except of some outliers students who completed the test preparation course have higher median of writing score than students who did not complete the course.
boxplot(writing.score~test_prep_course,data=data, col=rainbow(5), xlab="Test Preparation Course", ylab="Writing Score")


# Independent t test (Equal variances not assumed)
boxplot(mean(math.score),mean(reading.score),col=rainbow(7),xlab="Math_score & Reading_score", ylab="Average of scores")
t.test(math.score,reading.score)
t.test(math.score,reading.score, alternative='less',mu=70)
t.test(math.score,reading.score, alternative='greater',mu=70)

#What we found: 
#  .	math.score average is less than reading.score average. 

###################### STEP 4 : Deep Dive Analysis ########################


library(tidyverse)   # metapackage with lots of helpful functions
library(ggplot2)

# Do male students score higher at math than female students?
y<- math.score[gender=="male"]   
x<- math.score[gender=="female"]  
boxplot(math.score~gender,data=data, col=rainbow(3), xlab="Gender", ylab="Math Score")   
t.test(y,x, alternative = "less")   
# Answer : Male students score significantly higher at the math test than female students.


  
# Do male students score higher at reading than female students?
yy <- reading.score[gender=="male"]  
xx <- reading.score[gender=="female"]  
boxplot(reading.score~gender,data=data, col=rainbow(9), xlab="Gender", ylab="Reading Score") 
t.test(yy,xx, alternative = "less")  
# Answer : Female students score significantly higher at the reading test than male students.


#	Do male students score higher at writing than female students?
yyy <- writing.score[gender=="male"]  #male
xxx <- writing.score[gender=="female"]  #female
boxplot(writing.score~gender,data=data, col=rainbow(9), xlab="Gender", ylab="Writing Score") 
t.test(yyy,xxx, alternative = "less")   
# Answer : Female students score significantly higher at the writing test than male students. 



# Does the lunch influence the test score?
mutate(data, score=rowMeans(data[,2:4])) -> data_new  #score= mean(math.score, reading.score, writing.score)
new_data <- filter(data_new, lunch=="free/reduced")     
new_data_2 <- filter(data_new, lunch=="standard")   
rlunch<- select(new_data,
                       score)   #reduced_lunch
slunch<- select(new_data_2,
                       score)   #standard_lunch
shapiro.test(rlunch$score)          #Is it normal distribution?
shapiro.test(slunch$score)          #Is it normal distribution?
t.test(slunch,rlunch,alternative  = "less")
plot_data <- select(data_new,
                    score,lunch=="free/reduced")
# Answer: Students with a standard lunch score significantly higher.


table(plot_data$score)
ggplot(data = plot_data, aes(x =lunch)) + geom_histogram(bins = 100, color = 'green')
       
# Anova
mutate(data, score=rowMeans(data[,7:9])) -> data_new  #let's calculate average score.
View(data_new)
x <- subset(data_new, (parent_level_edu=="bachelor's degree" | parent_level_edu=="some college"  
                       | parent_level_edu=="master's degree" | parent_level_edu=="associate's degree" 
                       | parent_level_edu=="high school"  | parent_level_edu=="some high school") )  
model<-aov(data_new$score~data_new$parent_level_edu,data = x)
summary(model)  
TukeyHSD(model)
ggplot(data=data_new, aes(y=score, x= parent_level_edu))+geom_boxplot(aes(fill=parent_level_edu))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Average Score V/S Parent Education")+xlab("Parent Education Level")+ylab("Average Score")


# Correlation of Scores
ggplot(data, aes(x = math.score, y = reading.score)) + geom_point()    # math.score & reading.sccore have a positive correlation.
ggplot(data, aes(x = writing.score, y = reading.score)) + geom_point() # writing.score & reading.score have a positive correlation.


###################### STEP 5 : Creation Regression Model  #########################

attach(data)

# Generate dummies for Categorical variables
data <- cbind(data, dummy(data$gender, sep = "_"))
data <- cbind(data, dummy(data$race.ethnicity, sep = "_"))
data <- cbind(data, dummy(data$parent_level_edu, sep = "_"))
data <- cbind(data, dummy(data$lunch, sep = "_"))
data <- cbind(data, dummy(data$test_prep_course, sep = "_"))
# Remove the first 5 columns from dataset 
data[1:5]<- NULL  
# Remove unnecessary dummy variables (for preventing multicollineratiy)
data$data_female <- NULL
data$`data_group E` <- NULL
data$`data_some college` <-NULL
data$data_standard <- NULL
data$data_none <- NULL
View(data)

#Creating math.score model
mdl <- lm(math.score~. , data=data)
summary(mdl)   # Adjusted-R: 0.875

#Creating Testing and Training datasets.
set.seed(1)  
sample = sample(nrow(data), floor(nrow(data) * 0.8))  #split the data as %80 for training and %20 dor test.
train = data[sample,]
test = data[-sample,]
#cheking dimension of training and test data
dim(train)
dim(test)

#creating model with training data for prediction of math score

model_math1 = lm(math.score~., data = train)
summary(model_math1)
pred_math = predict(model_math1, newdata = test)
sqrt(mean((test$math.score - pred_math)^2))   #Residual Standard Error
#RMSE is 5.22, which means that on average model is wrong by 5.22. The model is  predicting acceptable average math score on testing dataset.


#Let's create new model without non essential variables;
mdl <- lm(math.score~. -`data_bachelor's degree` -`data_high school` -`data_some high school`  -`data_associate's degree`, data=data)
summary(mdl)   # Adjusted-R: 0.8744 : SO no such a thing changed actually.

#Creating model for predicting reading score.
model_reading = lm(reading.score~., data = train)
summary(model_reading)
pred_reading = predict(model_reading, newdata = test)
sqrt(mean((test$reading.score - pred_reading)^2))
#Root Mean Square Error is 3.885, which means that on average the model is wrong by 3.885.

#Creating model fro predicting writing score
model_writing = lm(writing.score~., data = train)
summary(model_writing)
pred_writing = predict(model_writing, newdata = test)
sqrt(mean((test$writing.score - pred_writing)^2))
#RMSE is 3.633, which means that on average model is wrong by 3.663




